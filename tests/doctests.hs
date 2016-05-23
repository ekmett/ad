module Main where

import Build_doctests (autogen_dir, deps)
import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

main :: IO ()
main = getSources >>= \sources -> doctest $
    "-isrc"
  : ("-i" ++ autogen_dir)
  : "-optP-include"
  : ("-optP" ++ autogen_dir ++ "/cabal_macros.h")
  : "-optP-I"
  : "-optPinclude"
  : "-hide-all-packages"
  : map ("-package="++) deps ++ sources

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "src"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
