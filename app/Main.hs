{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Text
import Development.Shake
import Development.Shake.FilePath
import GHC.Generics
import Site
import Slick
import Slick.Sass
import Text.Sass.Options

-- Methods to generate website

generateSite :: SitePaths -> SiteInfo -> Action ()
generateSite site siteInfo = do
    index <- generateIndex (source site) siteInfo
    writeIndex (output site) index
    copyCSSFile site
    copyStaticFiles site
    
-- Generates the index page from the template filed with the given `SiteInfo`
generateIndex :: FilePath -> SiteInfo -> Action Text
generateIndex site siteInfo = do
    template <- compileTemplate' (site </> "index.html")
    let rendered = substitute template (toJSON siteInfo)
    return rendered

writeIndex :: FilePath -> Text -> Action ()
writeIndex destination html = writeFileChanged (destination </> "index.html") (unpack html)

copyStaticFiles :: SitePaths -> Action ()
copyStaticFiles site = 
    let src = (source site) in
    do
    assetPaths <- getDirectoryFiles src ["images/*"]
    void $ forP assetPaths $ \path ->
         copyFileChanged (src </> path) ((output site) </> path)


copyCSSFile :: SitePaths -> Action ()
copyCSSFile site = 
    let src = (source site) </> "stylesheets" </> "application.scss" in
    do
    css <- compileFile' src defaultSassOptions
    writeFileChanged ((output site) </> "application.css") css

-- | Main

main :: IO ()
main = slick (generateSite paths julien)

-- | Defines site specific data

paths :: SitePaths
paths = SitePaths {
    source = "./site",
    output = "./docs"
}

julien :: SiteInfo
julien = SiteInfo {
    author = "Julien Grimault",
    title = "Julien Grimault",
    twitter = "https://twitter.com/juliengrimault",
    github = "https://github.com/juliengrimault",
    flickr = "https://www.flickr.com/photos/128553968@N04",
    apps = [
        App {
            title = "NewtonApples",
            thumbnail = "./images/newtonapples.jpg",
            url = "https://itunes.apple.com/us/app/newtonapples/id41769831"
        },
        App {
            title = "Schedaroo",
            thumbnail = "./images/schedaroo.jpg",
            url = "https://itunes.apple.com/us/app/schedaroo/id514603464"
        },
        App {
            title = "LotteryTicket",
            thumbnail = "./images/lotteryticket.jpg",
            url = "https://itunes.apple.com/us/app/lotteryticket/id444928739?mt=8"
        },
        App {
            title = "BetterLetter",
            thumbnail = "./images/betterletter.jpg",
            url = "https://itunes.apple.com/us/app/betterletter/id426913117"
        },
        App {
            title = "Bet Buttler",
            thumbnail = "./images/betbuttler.jpg",
            url = "https://itunes.apple.com/sg/app/bet-butler/id520033345"
        },
        App {
            title = "TradeHero",
            thumbnail = "./images/tradehero.jpg",
            url = "https://itunes.apple.com/us/app/tradehero-mobile/id572226383"
        },
        App {
            title = "Maps",
            thumbnail = "./images/maps.jpg",
            url = "https://www.apple.com/ios/maps"
        }
    ],
    contributions = [
        Contribution {
            title = "ModelLite",
            description = "Alternative to CoreData on top of SQLite",
            url = "https://github.com/juliengrimault/ModelLite"
        },
        Contribution {
            title = "Life",
            description = "Conway's <a href=\"http://en.wikipedia.org/wiki/Conway's_Game_of_Life\" rel='external nofollow'>Game of Life</a>",
            url = "http://github.com/juliengrimault/GameOfLifeiOS"
        },
        Contribution {
            title = "MarkdownEditor",
            description = "A simple markdown text editor",
            url = "https://github.com/juliengrimault/MarkdownEditor"
        },
        Contribution {
            title = "TwittApp.net",
            description = "Find your Twitter friends on App.net",
            url = "https://github.com/juliengrimault/AppNetChecker"
        },
        Contribution {
            title = "Rdio2DayOne",
            description = "Create <a href=\"http://dayoneapp.com\" rel='external nofollow'>Day One</a> entries from your Rdio collection",
            url = "https://github.com/juliengrimault/Slogger/commits/rdio"
        },
        Contribution {
            title = "Portfolio-Unicorn",
            description = "Learning Rails!",
            url = "https://github.com/juliengrimault/portfolio-unicorn"
        },
        Contribution {
            title = "LargeNumberFormatter",
            description = "Format large numbers with abreaviations",
            url = "https://github.com/juliengrimault/JGLargeNumberFormatter"
        }
    ]
}

