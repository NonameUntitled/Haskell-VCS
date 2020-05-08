module FileSystem
  ( File(..)
  , FMState
  , FileManager
  , FMCommandResult
  , fmRun
  , createNewFile
  , createNewFolder
  , updateFile
  , updateSubdirectory
  , updateFileSystem
  , cdHelper
  , cdCommand
  , cdMkCommand
  , lsCommand
  , mkdirCommand
  , catCommand
  , rmdirCommand
  , rmCommand
  , writeCommand
  , touchCommand
  , fileInfoCommand
  , dirInfoCommand
  , findCommand
  , moveToDir
  ) where

import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List
import Data.Time.Clock.System
import Exceptions
import System.FilePath.Posix


data File
  = Directory
  { fileName    :: FilePath
  , documents   :: [File]
  , directories :: [File]
  , parent      :: FilePath
  , vcs         :: Maybe File
  } | Document
  { fileName     :: FilePath
  , creationTime :: SystemTime
  , updateTime   :: SystemTime
  , parent       :: FilePath
  , content      :: String
  , size         :: Int
  } | VcsFile
  { fileName :: FilePath
  , versions :: [(Int, String, Maybe File)]
  , parent   :: FilePath
  } deriving Show

type FMState = (File, File)
type FMCommandResult a = (Either String a, FMState)
type FileManager a = ExceptT String (State FMState) a


fmRun :: FileManager a -> FMState -> FMCommandResult a
fmRun fileManager = runState (runExceptT fileManager)


createNewFile :: String -> SystemTime -> FilePath -> FileManager File
createNewFile name time parentPath = return $ Document name time time parentPath "" 0


createNewFolder :: String -> String -> FileManager File
createNewFolder folderName mParent = return $ Directory folderName [] [] mParent  Nothing


updateFile :: File -> SystemTime -> String -> FileManager File
updateFile oldFile time mContent = return $ oldFile{updateTime = time, content = mContent, size = length mContent}


updateSubdirectory :: File -> FileManager File
updateSubdirectory ~newDirectory@(Directory newFileName _ _ newParent  _) = do
  (root, ~currentDirectory@(Directory mFileName _ mFolders mParent _)) <- get
  if newParent == mParent && newFileName == mFileName then
    return newDirectory
  else do
    newFolders <- mapM (
      \x -> do
        put (root, x)
        updateSubdirectory newDirectory
        )
      mFolders
    return $ currentDirectory{directories = newFolders}


updateFileSystem :: File -> FilePath -> FileManager ()
updateFileSystem changedDirectory currentDirectoryPath = do
  currentRoot <- gets fst
  put (currentRoot, currentRoot)
  newRoot <- updateSubdirectory changedDirectory
  put (newRoot, changedDirectory)
  newCurrentRoot <- gets fst
  put (newCurrentRoot, newCurrentRoot)
  cdCommand currentDirectoryPath


computeSize :: File -> FileManager Int
computeSize dir = do
  directorySizes <- mapM computeSize (directories dir)
  let fileSizes = map size (documents dir)
  return $ sum $ fileSizes ++ directorySizes


moveToDir :: FilePath -> (FilePath -> FileManager ()) -> FileManager (FilePath, FilePath)
moveToDir filePath policy  = do
  (_, currentDirectory) <- get
  let currentDirectoryPath = parent currentDirectory </> fileName currentDirectory
  let (path, name) = splitFileName filePath
  policy path
  return (currentDirectoryPath, name)


cdHelperTemplate :: FilePath -> FileManager () -> FileManager ()
cdHelperTemplate nextDirectory policy = do
  (root, ~(Directory _ _ mDirs mParent _)) <- get
  case nextDirectory of
    "/" -> put (root, root)
    "." -> return ()
    ".." -> put (root, root) >> cdCommand mParent
    _ -> case find (\x -> fileName x == nextDirectory) mDirs of
      Just value -> put (root, value)
      _          -> policy

cdCommandTemplate :: FilePath -> (FilePath -> FileManager ()) -> FileManager ()
cdCommandTemplate nextDirectories policy = do
  let currentDirs = splitDirectories nextDirectories
  (root, currentDirectory) <- get
  helper (root, currentDirectory) currentDirs
    where
      helper _ [] = return ()
      helper (root, dir) (d:ds) = do
        put (root, dir)
        policy d
        (newRoot, newdir) <- get
        helper (newRoot, newdir) ds


cdHelper :: FilePath -> FileManager ()
cdHelper nextDirectory = cdHelperTemplate nextDirectory $ throwE $ show NoDirectoryInFileSystem


cdMkHelper :: FilePath -> FileManager ()
cdMkHelper nextDirectory = do
  (_, ~currentDirectory@(Directory mName _ mDirs mParent _)) <- get
  cdHelperTemplate nextDirectory
    $ createNewFolder nextDirectory (mParent </> mName) >>=
      \newDirectory -> createNewFolder nextDirectory (mParent </> mName)
      >> updateFileSystem currentDirectory{directories = newDirectory:mDirs} (mParent </> mName)
      >> cdMkHelper nextDirectory


cdCommand :: FilePath -> FileManager ()
cdCommand nextDirectories = cdCommandTemplate nextDirectories cdHelper


cdMkCommand :: FilePath -> FileManager ()
cdMkCommand nextDirectories = cdCommandTemplate nextDirectories cdMkHelper


lsCommand :: FilePath -> FileManager String
lsCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  cdCommand name
  ~lsDirectory@(Directory _ mFiles mFolders _ _) <- gets snd
  updateFileSystem lsDirectory currentDirectoryPath
  return $ unlines $ map fileName (mFiles ++ mFolders)


mkdirCommand :: FilePath -> FileManager ()
mkdirCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdMkCommand
  ~mkdirDirectory@(Directory mName _ mFolders mParent _) <- gets snd
  case find (\x -> fileName x == name) mFolders of
    Just _ -> throwE $ show DirectoryExists
    _ -> do
      newDirectory <- createNewFolder name (mParent </> mName)
      let newCurrentDirectory = mkdirDirectory{directories = newDirectory : mFolders}
      updateFileSystem newCurrentDirectory currentDirectoryPath


catCommand :: FilePath -> FileManager String
catCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  ~catDirectory@(Directory _ mFiles _ _ _) <- gets snd
  updateFileSystem catDirectory currentDirectoryPath
  case find (\x -> fileName x == name) mFiles of
    Just ~Document{content = mContent} -> return mContent
    _                                  -> throwE $ show NoFileInFileSystem


touchCommand :: SystemTime -> FilePath -> FileManager ()
touchCommand time filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdMkCommand
  ~touchDirectory@(Directory mName mFiles _ mParent _) <- gets snd
  case find (\x -> fileName x == name) mFiles of
    Just _ -> throwE $ show FileExists
    _ -> do
      newFile <- createNewFile name time (mParent </> mName)
      let newCurrentDirectory = touchDirectory{documents = newFile : mFiles}
      updateFileSystem newCurrentDirectory currentDirectoryPath


rmdirCommand :: FilePath -> FileManager ()
rmdirCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  ~rmdirDirectory@Directory{directories = mFolders} <- gets snd
  let newCurrentDirectory = rmdirDirectory{directories = filter (\x -> fileName x /= name) mFolders}
  updateFileSystem newCurrentDirectory currentDirectoryPath


rmCommand :: FilePath -> FileManager ()
rmCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  ~rmDirectory@Directory{documents = mFiles} <- gets snd
  let newCurrentDirectory = rmDirectory{documents = filter (\x -> fileName x /= name) mFiles}
  updateFileSystem newCurrentDirectory currentDirectoryPath


writeCommand :: FilePath -> String -> SystemTime -> FileManager ()
writeCommand filePath text updatedTime = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  ~writeDirectory@(Directory _ mFiles _ _ _) <- gets snd
  case find (\x -> fileName x == name) mFiles of
    Just ~value@Document{} -> do
      updatedFile <- updateFile value updatedTime text
      let otherFiles = filter (\x -> fileName x /= name) mFiles
      let newCurrentDirectory = writeDirectory{documents = updatedFile : otherFiles}
      updateFileSystem newCurrentDirectory currentDirectoryPath
    _ -> do
      touchCommand updatedTime name
      writeCommand filePath text updatedTime


findCommand :: FilePath -> FileManager [String]
findCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  ~findDirectory@(Directory _ mFiles mFolders _ _) <- gets snd
  subDirectoryEntries <- concatMapM (\x -> do
    root <- gets fst
    put (root, x)
    findCommand ("." </> name)) mFolders
  updateFileSystem findDirectory currentDirectoryPath
  return $ map (\x -> parent x </> fileName x ) (filter (\x -> fileName x == name) mFiles) ++ subDirectoryEntries


fileInfoCommand :: String -> FileManager String
fileInfoCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  ~fileInfoDirectory@Directory{documents = mFiles} <- gets snd
  updateFileSystem fileInfoDirectory currentDirectoryPath
  case find (\x -> fileName x == name) mFiles of
    Just ~(Document dName dCreationTime dUpdateTime dParent _ dSize) ->
      let fileInfo = name
            ++ "\nPath: " ++ (dParent </> dName)
            ++ "\nCreation time: " ++ show dCreationTime
            ++ "\nLast modified: " ++ show dUpdateTime
            ++ "\nSize: " ++ show dSize
            ++ "\nPermissions: rw"
            in return fileInfo
    _ -> throwE $ show NoFileInFileSystem


dirInfoCommand :: String -> FileManager String
dirInfoCommand filePath = do
  (currentDirectoryPath, name) <- moveToDir filePath cdCommand
  cdCommand name
  ~dirInfoDirectory@(Directory mName mFiles mFolders mParent _) <- gets snd
  updateFileSystem dirInfoDirectory currentDirectoryPath
  dirSize <- computeSize dirInfoDirectory
  let dirInfo = mName
        ++ "\nPath: " ++ normalise (mParent </> mName)
        ++ "\nFiles inside: " ++ show (length mFiles + length mFolders)
        ++ "\nSize: " ++ show dirSize
        ++ "\nPermissions: rw"
        in return dirInfo
