{-|
Module      : Slick.Sass
Description : Slick utilities for working with sass
Copyright   : (c) Julien Grimault, 2019
License     : MIT
-}

module Slick.Sass (
    compileFile'
) 
where

import Development.Shake
import Text.Sass

-- | Like 'compileFile' from <https://hackage.haskell.org/package/hsass-0.8.0/docs/Text-Sass-Compilation.html#g:1 hsass> but tracks changes to scss files within Shake for cache-busting.
compileFile' :: FilePath -> SassOptions -> Action String
compileFile' file options =  do
    need [file]
    result <- liftIO $ compileFile file options
    case result of
        Right css -> do
            return css
        Left error -> 
            fail $ show error