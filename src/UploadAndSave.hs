module UploadAndSave 
  ( uploadFileSystem
  , uploadFileSystemRecursive
  , fromPathsToFiles
  , fromPathsToDirectories
  , releaseFileSystem
  , createDirectoryRecursive
  , createDocument
  ) where

import           Control.Monad.State
import           Data.Foldable
import           Data.Time.Clock.System
import           FileSystem
import           System.Directory
import           System.FilePath.Posix


uploadFileSystem :: IO (FilePath, File)
uploadFileSystem = do
  directoryPath <- getLine
  fileSystem <- uploadFileSystemRecursive directoryPath "." "."
  return (directoryPath, fileSystem)


uploadFileSystemRecursive :: FilePath -> FilePath -> FilePath -> IO File
uploadFileSystemRecursive base currentParent currentDir = do
  dirContent <- listDirectory (base </> currentParent </> currentDir)
  let newParent = normalise (currentParent </> currentDir)
  let newPathPrefix = normalise (base </> newParent)
  fileNames <- filterM (doesFileExist . combine newPathPrefix) dirContent
  directoryNames <- filterM (doesDirectoryExist . combine newPathPrefix) dirContent
  files <- fromPathsToFiles base newParent fileNames
  dirs <- fromPathsToDirectories base newParent $ filter (/=".vcs") directoryNames
  case find (==".vcs") directoryNames of
    Just _ -> do
      vcsDir <- head <$> fromPathsToDirectories (base </> ".vcs") "." ["."]
      return $ Directory currentDir files dirs currentParent (Just vcsDir)
    _ -> return $ Directory currentDir files dirs currentParent  Nothing


fromPathsToFiles :: FilePath -> FilePath -> [FilePath] -> IO [File]
fromPathsToFiles base currentParent filePaths =  do
  time <- getSystemTime
  mapM (\x -> do
    contents <- readFile (base </> currentParent </> x)
    return $ Document x time time currentParent contents (length contents)) filePaths


fromPathsToDirectories :: FilePath -> FilePath -> [FilePath] -> IO [File]
fromPathsToDirectories base currentParent = mapM (uploadFileSystemRecursive base currentParent)


releaseFileSystem :: FilePath -> File -> IO ()
releaseFileSystem startDirectory fileSystem = do
     removeDirectoryRecursive startDirectory
     createDirectoryRecursive startDirectory fileSystem


createDirectoryRecursive :: FilePath -> File -> IO ()
createDirectoryRecursive base ~(Directory name docs dirs fileParent _) = do
     createDirectory $ normalise $ base </> fileParent </> name
     traverse_ (createDirectoryRecursive base) dirs
     traverse_ (createDocument base) docs


createDocument :: FilePath -> File -> IO ()
createDocument base ~(Document name _ _ fileParent fileContent _) = writeFile (normalise $ base </> fileParent </> name) fileContent

