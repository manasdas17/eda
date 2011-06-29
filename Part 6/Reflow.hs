-- Could definitely be smarter, but watcha gonna do.
-- Look out for Windows-style line endings
-- https://twitter.com/scanline/status/11456119146

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import Data.List

sourceDir = "abstracts"
destDir = "reflowed_abstracts"

readDirectory dir = liftM (filter (not . (isPrefixOf "."))) $ getDirectoryContents dir

main = do
    createDirectoryIfMissing False destDir
    files <- readDirectory sourceDir
    mapM (reflowFile sourceDir destDir) files

reflowFile sourceDir destDir filename =
    TIO.readFile (sourceDir </> filename) >>=
    \f -> TIO.writeFile (destDir </> filename) (reflow f)

reflow = T.unwords . delimitParagraphs . map T.strip . T.lines
    where
        delimitParagraphs = map paragraphNewLine
        paragraphNewLine str    | T.null str = "\n\n"
                                | otherwise = str
        