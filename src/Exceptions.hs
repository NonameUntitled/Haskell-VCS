module Exceptions
  ( ErrorTypes(..)
  ) where

data ErrorTypes
  = NoFileInFileSystem
  | DirectoryExists
  | NoDirectoryInFileSystem
  | FileExists
  | VcsExists
  | NoVcs
  | FileAddedVcs
  | NoFileVcs
  | NoHistoryFileVcs
  | NoVersionVcs
  | UnknownPolicy

instance Show ErrorTypes where
  show NoFileInFileSystem      = "No file in file system"
  show DirectoryExists         = "Directory is already exists"
  show NoDirectoryInFileSystem = "No directory in file system"
  show FileExists              = "File is already exists"
  show VcsExists               = "Vcs is already exists"
  show NoVcs                   = "No vcs in current directory"
  show FileAddedVcs            = "This file is already in vcs"
  show NoFileVcs               = "This file isn't exist"
  show NoHistoryFileVcs        = "There is no history file"
  show NoVersionVcs            = "There is no such file version"
  show UnknownPolicy           = "Unknown merge policy"
