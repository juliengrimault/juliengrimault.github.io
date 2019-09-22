module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Slick

outputDirectory:: FilePath
outputDirectory = "./docs"

site :: FilePath
site = "./site"

copyStaticFiles:: Action ()
copyStaticFiles = 
    in do
    assetPaths <- getDirectoryFiles site ["images/*"]
    void $ forP assetPaths $ \path ->
         copyFileChanged (site </> path) (outputDirectory </> path)

main :: IO ()
main = slick copyStaticFiles
