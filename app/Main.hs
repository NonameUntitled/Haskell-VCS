module Main where

import Control.Exception
import Data.Time.Clock.System
import FileSystem
import System.FilePath.Posix
import Vcs
import UploadAndSave


main :: IO ()
main = do
  (dirPath, fileSystem) <- catch uploadFileSystem uploadHandler
  finalFileSystem <- runTerminal (fileSystem, fileSystem)
  catch (releaseFileSystem dirPath finalFileSystem) releaseHandler
    where
      uploadHandler :: SomeException -> IO (FilePath, File)
      uploadHandler ex = do
        print ex
        putStrLn "Try again."
        catch uploadFileSystem uploadHandler
      releaseHandler :: SomeException -> IO ()
      releaseHandler ex = putStrLn $ show ex ++ "Cannot update file system."


runTerminal :: FMState -> IO File
runTerminal (root, fileSystem) = do
     (putStr . normalise) $ parent fileSystem </> fileName fileSystem ++ "> "
     command <- words <$> getLine
     case command of
          [] -> runTerminal (root, fileSystem)
          ("exit":_) -> return root
          (x:xs) -> do
               newState <- processCommand (root, fileSystem) x xs
               runTerminal newState


printCommandResults :: FileManager [String] -> FMState -> IO FMState
printCommandResults fileManager oldState =
  let (results, newState) = fmRun fileManager oldState in
    case results of
      Left errorMsg -> putStrLn errorMsg >> return oldState
      Right msgs    -> mapM_ putStrLn msgs >> return newState


executeCommandResults :: FileManager a -> FMState -> IO FMState
executeCommandResults fileManager oldState =
  let (results, newState) = fmRun fileManager oldState in
    case results of
               Left errorMsg -> putStrLn errorMsg >> return oldState
               Right _       -> return newState


helpCommand :: IO ()
helpCommand = putStrLn $ unlines
  [ "Commands:"
  , "cd <folder>                         -- change directory"
  , "cdmk <folder>                       -- change directory recursively creating directories"
  , "ls                                  -- show current directory content"
  , "mkdir 'folder_name'                 -- create folder in the current directory"
  , "cat <file>                          -- show file's content"
  , "touch 'file_name'                   -- create file in the current folder"
  , "rmdir <folder>                      -- delete folder"
  , "rm <file>                           -- delete file"
  , "write <file> 'text'                 -- write text into the file"
  , "find 'file'                         -- find file in the current directory and subdirectories"
  , "fileInfo <file>                     -- show information about the file"
  , "dirInfo <folder>                    -- show information about the folder"
  , "vcs_init                            -- init VCS in the current directory"
  , "vcs_add <file|folder>               -- add file or folder in the VCS"
  , "vcs_update <file> 'message'         -- update file in the VCS"
  , "vcs_hist <file>                     -- show file's changes history"
  , "vcs_cat <file> 'index'              -- show current file's content"
  , "vcs_delete <file> 'index'           -- remove particular file version"
  , "vcs_remove <file>                   -- remove file from the VCS"
  , "vcs_all                             -- show all VCS history"
  , "help                                -- show help commands"
  , "exit                                -- turn off the FS and exit "]


processCommand :: FMState -> String -> [String] -> IO FMState
processCommand currentState command args = do
  time <- getSystemTime
  case command of
    "help" -> helpCommand >> return currentState
    "cd" -> case args of
      [x] -> executeCommandResults (cdCommand x) currentState
      _   -> putStrLn "Wrong arguments" >> return currentState
    "cdmk" -> case args of
      [x] -> executeCommandResults (cdMkCommand x) currentState
      _   -> putStrLn "Wrong arguments" >> return currentState
    "ls" -> case args of
      [] -> processCommand currentState "ls" ["."]
      _  -> printCommandResults (traverse lsCommand args) currentState
    "mkdir" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> executeCommandResults (traverse mkdirCommand args) currentState
    "cat" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> printCommandResults (traverse catCommand args) currentState
    "touch" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> executeCommandResults (traverse (touchCommand time) args) currentState
    "rmdir" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> executeCommandResults (traverse rmdirCommand args) currentState
    "rm" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> executeCommandResults (traverse rmCommand args) currentState
    "write" -> case args of
      (filePath : text : texts) -> executeCommandResults (writeCommand filePath (unwords (text : texts)) time) currentState
      _ -> putStrLn "Wrong arguments" >> return currentState
    "fileInfo" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> printCommandResults (traverse fileInfoCommand args) currentState
    "dirInfo" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> printCommandResults (traverse dirInfoCommand args) currentState
    "find" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _ -> let (results, newState) = fmRun (traverse findCommand args) currentState in
        case results of
          Left errorMsg -> putStrLn errorMsg >> return currentState
          Right msgs -> do
            let matches = zip args msgs
            mapM_ (\(x, y) -> do
              putStrLn $ "Found " ++ (show . length) y ++ " entrances of " ++ x ++ ":"
              mapM_ putStrLn y) matches
            return newState
    "vcs_init" -> case args of
      [] -> executeCommandResults (vcsInitCommand time) currentState
      _  -> putStrLn "Wrong arguments" >> return currentState
    "vcs_add" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> executeCommandResults (traverse vcsAddCommand args) currentState
    "vcs_update" -> case args of
      (filePath :message: messages) -> executeCommandResults (vcsUpdateCommand (unwords (message : messages)) filePath) currentState
      _ -> putStrLn "Wrong arguments" >> return currentState
    "vcs_remove" -> case args of
      [] -> putStrLn "Wrong arguments" >> return currentState
      _  -> executeCommandResults (traverse vcsRemoveCommand args) currentState
    "vcs_cat" -> case args of
      [filePath, index] -> let (result, newState) = fmRun (vcsCatCommand filePath (read index)) currentState in
        case result of
          Left errorMsg -> putStrLn errorMsg >> return currentState
          Right msg     -> putStrLn msg >> return newState
      _ -> putStrLn "Wrong arguments" >> return currentState
    "vcs_delete" -> case args of
      [filePath, index] -> executeCommandResults (vcsDeleteVersionCommand filePath (read index)) currentState
      _ -> putStrLn "Wrong arguments" >> return currentState
    "vcs_hist" -> case args of
      [filePath] ->  let (result, newState) = fmRun (vcsHistoryCommand filePath) currentState in
        case result of
          Left errorMsg -> putStrLn errorMsg >> return currentState
          Right msg     -> putStrLn msg >> return newState
      _ -> putStrLn "Wrong arguments" >> return currentState
    "vcs_all" -> case args of
      [] -> let (result, newState) = fmRun vcsAllInfoCommand currentState in
        case result of
          Left errorMsg -> putStrLn errorMsg >> return currentState
          Right msg     -> putStrLn msg >> return newState
      _ -> putStrLn "Wrong arguments" >> return currentState
    "vcs_merge" -> case args of
      [filePath, index1, index2, policy] -> executeCommandResults
          (vcsMergeCommand time filePath (read index1) (read index2) policy) currentState
      _ -> putStrLn "Wrong arguments" >> return currentState
    _ -> putStrLn "Unknown command" >> return currentState
