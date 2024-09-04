-- SPDX-License-Identifier: MIT
-- SPDX-FileCopyrightText: Copyright © 2016 Luke Murphy
-- SPDX-FileCopyrightText: Copyright © 2020-2022 John Ky
-- SPDX-FileCopyrightText: Copyright © 2022 Possehl Analytics GmbH
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Char qualified
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (mtimesDefault)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Word (Word8)
import GHC.Utils.Encoding.UTF8 qualified as GHC
import Numeric.Natural (Natural)
import PyF (fmt)
import System.Console.GetOpt qualified as Opt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath qualified as File
import System.FilePath.Glob (compile, globDir1)
import System.IO
  ( hPutStrLn,
    stderr,
  )
import System.IO qualified as IO
import Prelude hiding (lines)

data Config = Config
  { configModuleName :: String,
    configSourceDirectory :: String
  }

-- | Each option overrides the Config struct with some data (@Config -> Config@)
opts :: [Opt.OptDescr (Config -> Config)]
opts =
  [ Opt.Option
      ['m']
      ["module-name"]
      (Opt.OptArg parseModuleName "NAME")
      "Name to use for the generated module"
  ]
  where
    parseModuleName :: Maybe String -> Config -> Config
    parseModuleName =
      \case
        Nothing -> id
        Just nm -> \conf -> conf {configModuleName = nm}

main :: IO ()
main =
  getArgs
    <&> Opt.getOpt Opt.Permute opts
    >>= \case
      (confModifiers, [srcDir, _, dst], []) -> do
        let config =
              List.foldl'
                (\cfg modifier -> modifier cfg)
                ( Config
                    { configModuleName = "Main",
                      configSourceDirectory = File.takeDirectory srcDir
                    }
                )
                confModifiers

        -- allow the customary “`-` is stdout” use-case
        let writeDestination bytes = case dst of
              "-" -> Builder.hPutBuilder IO.stdout bytes
              path -> Builder.writeFile path bytes

        -- XXX(l-epple): get hs-source-dirs from cabal file?
        findAllTestsInSourceDirectory config
          <&> renderTestModule config.configModuleName
          >>= writeDestination
      (_, _, errs) -> do
        prog <- getProgName
        hPutStrLn stderr $ concat errs
        hPutStrLn stderr $
          Opt.usageInfo (prog <> " SRC IGNORED DST [--module-name=NAME]\n") opts
        exitFailure

renderTestModule ::
  -- | Module name
  String ->
  -- | Discovered test functions in the code
  [Test] ->
  -- | Resulting source code of the module
  Builder.Builder
renderTestModule moduleName testList =
  runPrinter $
    lines
      [ "{-# LANGUAGE NoImplicitPrelude #-}",
        [fmt|module {moduleName} (main) where|],
        "import qualified Prelude",
        "import qualified Test.Hspec",
        testMap
          & Map.toList
          <&> importTest
          & lines,
        "main :: Prelude.IO ()",
        "main = Test.Hspec.hspec Prelude.$ do",
        addIndent $
          testMap
            & Map.toList
            <&> callTest
            & foldMap lines
      ]
  where
    testMap =
      testList
        & map
          ( \Test {testModule, testFunction} ->
              (testModule, Set.singleton testFunction)
          )
        & Map.fromListWith Set.union
    importTest (testModule, testFunctions) =
      [fmt|import qualified {testModule} ({testFunctions & foldMap (\name -> name <> ", ")})|]

    callTest (testModule, testFunctions) =
      [ [fmt|Test.Hspec.describe "{testModule}" Prelude.$ do|],
        addIndent $
          testFunctions
            & Set.toList
            <&> (\name -> [fmt|Test.Hspec.describe "{List.stripPrefix "test_" name & fromMaybe name}" {testModule}.{name}|])
            & lines
      ]

-- | A simple printing abstraction which remembers the current indentation level
newtype Printer = Printer
  {lines :: Natural -> Builder}
  deriving newtype (Semigroup, Monoid)

runPrinter :: Printer -> Builder
runPrinter p = p.lines 0

-- Increase inner printer by one indentation level
addIndent :: Printer -> Printer
addIndent p = Printer {lines = \nat -> p.lines (nat + 2)}

-- | put each printer’s contents onto a new line
lines :: [Printer] -> Printer
lines ps = Printer {lines = \nat -> ps <&> (\p -> p.lines nat <> "\n") & mconcat}

-- | Accepts a multi-line string and indents every line according to current indentation level
stringLine :: String -> Printer
stringLine s =
  Printer
    { lines = \nat ->
        s
          & stringToBytesUtf8
          & Bytes.split (charToWordUnsafe '\n')
          <&> (\line -> mtimesDefault nat " " <> (line & Builder.byteString))
          & List.intersperse "\n"
          & fold
    }

stringToBytesUtf8 :: String -> ByteString
stringToBytesUtf8 = GHC.utf8EncodeByteString

charToWordUnsafe :: Char -> Word8
{-# INLINE charToWordUnsafe #-}
charToWordUnsafe = fromIntegral . Data.Char.ord

instance IsString Printer where
  fromString = stringLine

-- Below is the actual test discovery logic which finds test_* functions in a
-- module hierarchy. It has been derived from tasty-discover-5.0.0.

-- | The test type.
data Test = Test
  { -- | Module name.
    testModule :: String,
    -- | Function name.
    testFunction :: String
  }
  deriving stock (Eq, Show, Ord)

-- | 'Test' constructor.
mkTest :: FilePath -> String -> Test
mkTest modulePath testFunction =
  Test
    { testModule =
        modulePath
          & File.dropExtension
          & replacePathSeparatorWith '.',
      ..
    }
  where
    replacePathSeparatorWith newSep path =
      map (\c -> if File.isPathSeparator c then newSep else c) path

-- | Discover the tests modules.
findAllTestsInSourceDirectory :: Config -> IO [Test]
findAllTestsInSourceDirectory config = do
  let directory = config.configSourceDirectory
  allModules <- List.sort <$> globDir1 (compile "**/*.hs") directory
  concat <$> traverse (extract directory) allModules
  where
    extract directory filePath = do
      tests <-
        extractTestsFromModule (dropDirectory directory filePath)
          <$> readFile filePath
      seq (List.length tests) (pure tests)
    dropDirectory directory filePath =
      fromMaybe filePath $
        List.stripPrefix (directory ++ [File.pathSeparator]) filePath

-- | Extract the test names from discovered modules.
extractTestsFromModule :: FilePath -> String -> [Test]
extractTestsFromModule file = mkTestDeDuped . filter ("test_" `List.isPrefixOf`) . parseTest
  where
    mkTestDeDuped = map (mkTest file) . nubOrd
    parseTest = map fst . concatMap lex . List.lines
