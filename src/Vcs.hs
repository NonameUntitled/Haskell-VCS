module Vcs
  ( vcsInitCommand
  , vcsAddCommand
  , vcsUpdateCommand
  , vcsRemoveCommand
  , vcsCatCommand
  , vcsDeleteVersionCommand
  , vcsHistoryCommand
  , vcsMergeCommand
  , vcsAllInfoCommand
  ) where

import           Control.Monad.Extra
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Time.Clock.System
import           Exceptions
import           FileSystem
import           System.FilePath.Posix

data MergePolicy = LeftPolicy | RightPolicy | BothPolicy

mergePolicies :: [(String, MergePolicy)]
mergePolicies = [("left", LeftPolicy), ("right", RightPolicy), ("both", BothPolicy)]

createVcs :: SystemTime -> FileManager File
createVcs time = return $ Directory "." [Document ".history" time time "." "" 0] [] "." Nothing

createFileVersion :: File -> FileManager File
createFileVersion ~file@(Document name _ _ mParent _ _) = return $ VcsFile name [(0, "initial", Just file)] mParent

updateFileVersion :: String -> File -> File -> FileManager File
updateFileVersion message ~fileInfo@VcsFile{versions=fileVersions} ~file@Document{} =
  return $ fileInfo{versions = fileVersions ++ [(length fileVersions, message, Just file)]}


addDocument :: File -> FileManager ()
addDocument ~file@(Document mName _ _ mParent _ _) = do
  currentDirectory <- gets snd
  cdMkCommand mParent
  ~addedDirectory@(Directory _ dFiles _ _ _) <- gets snd
  case find (\x -> fileName x == mName) dFiles of
    Just _ -> throwE $ show FileAddedVcs
    _ -> do
      addedFile <- createFileVersion file
      let newAddDirectory = addedDirectory{documents = dFiles ++ [addedFile]}
      updateFileSystem newAddDirectory (parent currentDirectory </> fileName currentDirectory)

addDirectory :: File -> FileManager ()
addDirectory ~(Directory _ mDocuments mDirectories _ _) = do
  mapM_ addDocument mDocuments
  mapM_ addDirectory mDirectories


updateDocument :: String -> File -> FileManager ()
updateDocument message ~file@(Document mName _ _ mParent _ _) = do
  currentDirectory <- gets snd
  cdCommand mParent
  ~updatedDirectory@(Directory _ dFiles _ _ _) <- gets snd
  case find (\x -> fileName x == mName) dFiles of
    Just ~value@VcsFile{} -> do
      updatedFile <- updateFileVersion message value file
      let otherFiles = filter (\x -> fileName x /= mName) dFiles
      let newUpdateDirectory = updatedDirectory{documents = otherFiles ++ [updatedFile]}
      updateFileSystem newUpdateDirectory (parent currentDirectory </> fileName currentDirectory)
    _ -> throwE $ show NoFileVcs

updateDirectory :: String -> File -> FileManager ()
updateDirectory message ~(Directory _ mDocuments mDirectories _ _) = do
  mapM_ (updateDocument message) mDocuments
  mapM_ (updateDirectory message) mDirectories


vcsCommandtemplate :: String -> (File -> FileManager a) -> FileManager a -> FileManager a
vcsCommandtemplate message justAction nothingAction = do
  currentDirectory <- gets snd
  forM_ (vcs currentDirectory) (vcsAddHistory message)
  case vcs currentDirectory of
    Just newValue -> justAction newValue
    _             -> nothingAction


updateVcs :: File -> File -> FileManager ()
updateVcs root directory = do
  newVcsRoot <- gets fst
  let newCurrentDirectory = directory{vcs = Just newVcsRoot}
  put (root, newCurrentDirectory)
  updateFileSystem newCurrentDirectory (parent directory </> fileName directory)


vcsAddHistory :: String -> File -> FileManager ()
vcsAddHistory message vcsDir = do
  (currentRoot, currentDirectory) <- get
  case (head . documents) vcsDir of
    (Document mName mTime _ _ mContent _) -> do
      put (vcsDir, vcsDir)
      writeCommand mName (mContent ++ message) mTime
      updateVcs currentRoot currentDirectory
    _ -> throwE $ show NoHistoryFileVcs


vcsFsActionsHelper :: FilePath -> (File -> FileManager ()) -> (File -> FileManager ()) -> File -> FileManager ()
vcsFsActionsHelper filePath docsPolicy dirsPolicy vcsDir = do
  (currentRoot, currentDirectory) <- get
  (_, name) <- moveToDir filePath cdCommand
  fsFilesDirectory <- gets snd
  let filteredDocuments = filter (\x -> fileName x == name) (documents fsFilesDirectory)
  let filteredDirectories = filter (\x -> fileName x == name) (directories fsFilesDirectory)
  put (vcsDir, vcsDir)
  traverse_ docsPolicy filteredDocuments
  traverse_ dirsPolicy filteredDirectories
  case filteredDocuments ++ filteredDirectories of
    [] -> throwE $ show NoFileVcs
    _  -> updateVcs currentRoot currentDirectory

vcsInitHelper :: SystemTime -> FileManager ()
vcsInitHelper time = do
  currentDirectory <- gets snd
  newVcs <- createVcs time
  let newCurrentDirectory = currentDirectory{vcs = Just newVcs}
  updateFileSystem newCurrentDirectory (parent currentDirectory </> fileName currentDirectory)

vcsInitCommand :: SystemTime -> FileManager ()
vcsInitCommand time = vcsCommandtemplate
  "init vcs\n"
  (const $ throwE $ show VcsExists) (vcsInitHelper time)


vcsAddHelper :: FilePath -> File -> FileManager ()
vcsAddHelper filePath = vcsFsActionsHelper filePath addDocument addDirectory

vcsAddCommand :: FilePath -> FileManager ()
vcsAddCommand filePath = vcsCommandtemplate
  ("add file " ++ filePath ++ " to vcs\n")
  (vcsAddHelper filePath) (throwE $ show NoVcs)


vcsUpdateHelper :: String -> FilePath -> File -> FileManager ()
vcsUpdateHelper message filePath = vcsFsActionsHelper filePath (updateDocument message) (updateDirectory message)

vcsUpdateCommand :: String -> FilePath -> FileManager ()
vcsUpdateCommand message filePath = vcsCommandtemplate
  ("update file " ++ filePath ++ "\n")
  (vcsUpdateHelper message filePath) (throwE $ show NoVcs)


vcsRemoveHelper :: FilePath -> File -> FileManager()
vcsRemoveHelper filePath vcsDir = do
  (root, currentDirectory) <- get
  put (vcsDir, vcsDir)
  let vcsFilePath = parent currentDirectory </> fileName currentDirectory </> filePath
  rmCommand vcsFilePath
  rmdirCommand vcsFilePath
  updateVcs root currentDirectory

vcsRemoveCommand :: FilePath -> FileManager ()
vcsRemoveCommand filePath = vcsCommandtemplate
  ("remove file " ++ filePath ++ " from vcs\n")
  (vcsRemoveHelper filePath) (throwE $ show NoVcs)


vcsCatHelper :: FilePath -> Int -> File -> FileManager String
vcsCatHelper filePath catVersion vcsDir = do
  (currentRoot, currentDirectory) <- get
  put (vcsDir, vcsDir)
  (_, name) <- moveToDir filePath cdCommand
  givenDirectory <- gets snd
  put (currentRoot, currentDirectory)
  case find (\x -> fileName x == name) (documents givenDirectory) of
    Just (VcsFile _ mVersions _) -> case find (\(x, _, _) -> x == catVersion) mVersions of
        Just (_, _, Just ~fileVersion@Document{}) -> return $ content fileVersion
        _ -> throwE $ show NoVersionVcs
    _ -> throwE $ show NoFileVcs

vcsCatCommand :: FilePath -> Int -> FileManager String
vcsCatCommand filePath index = vcsCommandtemplate
  "" (vcsCatHelper filePath index) (throwE $ show NoVcs)


vcsDeleteVersionHelper :: FilePath -> Int -> File -> FileManager ()
vcsDeleteVersionHelper filePath index vcsDir = do
  (currentRoot, currentDirectory) <- get
  let vcsFilePath = parent currentDirectory </> fileName currentDirectory </> filePath
  put (vcsDir, vcsDir)
  (path, name) <- moveToDir vcsFilePath cdCommand
  currentVcsDirectory <- gets snd
  let otherFiles = filter (\x -> fileName x /= name) (documents currentVcsDirectory)
  case find (\x -> fileName x == name) (documents currentVcsDirectory) of
    Just documentVersion@VcsFile{} -> do
      let changedVersions =
            take index (versions documentVersion)
            ++ (index, "deleted", Nothing) : drop (index + 1) (versions documentVersion)
      let newDocumentVersions = documentVersion{versions = changedVersions}
      let newCurrentVcsDirectory = currentVcsDirectory{documents = newDocumentVersions : otherFiles}
      updateFileSystem newCurrentVcsDirectory path
      updateVcs currentRoot currentDirectory
    _ -> throwE $ show NoFileVcs

vcsDeleteVersionCommand :: FilePath -> Int -> FileManager ()
vcsDeleteVersionCommand filePath index = vcsCommandtemplate
  ("delete version " ++ show index ++ " of file " ++ filePath ++ "\n")
  (vcsDeleteVersionHelper filePath index) (throwE $ show NoVcs)


vcsHistoryHelper :: FilePath -> File -> FileManager String
vcsHistoryHelper filePath vcsDir = do
  (currentRoot, currentDirectory) <- get
  put (vcsDir, vcsDir)
  (_, name) <- moveToDir filePath cdCommand
  givenDirectory <- gets snd
  put (currentRoot, currentDirectory)
  case find (\x -> fileName x == name) (documents givenDirectory) of
    Just documentVersion@VcsFile{} -> do
      let filesInfo = map (\(x, y, _) -> show x ++ ". " ++ y)  (versions documentVersion)
      return $ unlines filesInfo
    _ -> throwE $ show NoFileVcs

vcsHistoryCommand :: FilePath -> FileManager String
vcsHistoryCommand filePath = vcsCommandtemplate
  "" (vcsHistoryHelper filePath) (throwE $ show NoVcs)


vcsMergeCommand :: SystemTime -> FilePath -> Int -> Int -> String -> FileManager ()
vcsMergeCommand time filePath index1 index2 policy = do
  firstContent <- vcsCatCommand filePath index1
  secondContent <- vcsCatCommand filePath index2
  case lookup policy mergePolicies of
    Just LeftPolicy -> writeCommand filePath firstContent time
    Just RightPolicy -> writeCommand filePath secondContent time
    Just BothPolicy -> writeCommand filePath (firstContent ++ "\n>>>>>>>>>>>>\n" ++ secondContent) time
    _ -> throwE $ show UnknownPolicy


vcsAllInfoHelper :: File -> FileManager String
vcsAllInfoHelper vcsDir = return $ (content . head . documents) vcsDir

vcsAllInfoCommand :: FileManager String
vcsAllInfoCommand = vcsCommandtemplate
  "" vcsAllInfoHelper (throwE $ show NoVcs)
