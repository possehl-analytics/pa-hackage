{-# LANGUAGE QuasiQuotes #-}

module RunCommand where

import Control.Monad.Logger.CallStack (MonadLogger, logError, logInfo)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as Text
import PossehlAnalyticsPrelude
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Exit qualified as Exit
import System.Exit qualified as System
import System.IO (Handle)
import System.Process.Typed qualified as Process

-- | Given a a command, the executable and arguments,
-- spawn the tool as subprocess and collect its stdout (stderr will go to our stderr).
--
-- Will strip the stdout of trailing newlines.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommand :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m (Exit.ExitCode, ByteString)
runCommand executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.readProcessStdout
    <&> second toStrictBytes
    <&> second stripWhitespaceFromEnd

-- | Given a a command, the executable and arguments,
-- spawn the tool as subprocess and run it to conclusion.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommandNoStdout :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m Exit.ExitCode
runCommandNoStdout executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.runProcess

-- TODO: This is reversing the whole string *twice*. Can we strip from end without doing that?
stripWhitespaceFromEnd :: ByteString -> ByteString
stripWhitespaceFromEnd = ByteString.reverse . ByteString.dropWhile (\w -> w == charToWordUnsafe '\n') . ByteString.reverse

-- | Like `runCommand`, but takes a Bytestring that provides the command with streamed input on stdin.
runCommandWithStdin :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> Lazy.ByteString -> m (Exit.ExitCode, ByteString)
runCommandWithStdin executable args stdin = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.setStdin (Process.byteStringInput stdin)
    & Process.readProcessStdout
    <&> second toStrictBytes
    <&> second stripWhitespaceFromEnd

-- | Like 'runCommandExpect0', but don’t capture stdout,
-- connect stdin and stdout to the command until it returns.
--
-- This is for interactive subcommands.
runCommandInteractiveExpect0 :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m ()
runCommandInteractiveExpect0 executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running interactively: $ {bashArgs}|]
  ( liftIO $
      Process.runProcess $
        Process.proc
          executable
          (args <&> textToString)
    )
    >>= checkStatus0 executable

-- | Given a name of a command, the executable and arguments,
-- spawn the tool as subprocess and pipe its stdout to the given 'Handle'.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommandPipeToHandle :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> Handle -> m Exit.ExitCode
runCommandPipeToHandle executable args handle = do
  -- TODO log the output file?
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running: $ {bashArgs}|]
  liftIO $
    Process.runProcess
      ( Process.proc
          executable
          (args <&> textToString)
          & Process.setStdout (Process.useHandleClose handle)
      )

-- | Like 'runCommand' but exit if the command returns a non-0 status.
runCommandExpect0 :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m ByteString
runCommandExpect0 executable args =
  runCommand executable args >>= \case
    (ex, stdout) -> do
      checkStatus0 executable ex
      pure stdout

-- | Like 'runCommandNoStdout' but exit if the command returns a non-0 status.
runCommandExpect0NoStdout :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m ()
runCommandExpect0NoStdout executable args =
  runCommandNoStdout executable args >>= \case
    ex -> checkStatus0 executable ex

-- | Like 'runCommandWithStdin' but exit if the command returns a non-0 status.
runCommandWithStdinExpect0 :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> Lazy.ByteString -> m ByteString
runCommandWithStdinExpect0 executable args stdin =
  runCommandWithStdin executable args stdin >>= \case
    (ex, stdout) -> do
      checkStatus0 executable ex
      pure stdout

-- | Check whether a command exited 0 or crash.
checkStatus0 :: (MonadLogger m, MonadIO m) => FilePath -> ExitCode -> m ()
checkStatus0 executable = \case
  ExitSuccess -> pure ()
  ExitFailure status -> do
    logCritical [fmt|Command `{executable}` did not exit with status 0 (success), but status {status}|]

-- | Log the message with 'logError', and call 'System.exitFailure'.
logCritical :: (HasCallStack, MonadLogger m, MonadIO m) => Text -> m b
logCritical msg = do
  logError msg
  liftIO $ System.exitFailure

-- | Pretty print a command line in a way that can be copied to bash.
prettyArgsForBash :: [Text] -> Text
prettyArgsForBash = Text.intercalate " " . map simpleBashEscape

-- | Simple escaping for bash words. If they contain anything that’s not ascii chars
-- and a bunch of often-used special characters, put the word in single quotes.
simpleBashEscape :: Text -> Text
simpleBashEscape t = do
  case Text.find (not . isSimple) t of
    Just _ -> escapeSingleQuote t
    Nothing -> t
  where
    -- any word that is just ascii characters is simple (no spaces or control characters)
    -- or contains a few often-used characters like - or .
    isSimple c =
      Char.isAsciiLower c
        || Char.isAsciiUpper c
        || Char.isDigit c
        -- These are benign, bash will not interpret them as special characters.
        || List.elem c ['-', '.', ':', '/']
    -- Put the word in single quotes
    -- If there is a single quote in the word,
    -- close the single quoted word, add a single quote, open the word again
    escapeSingleQuote t' = "'" <> Text.replace "'" "'\\''" t' <> "'"
