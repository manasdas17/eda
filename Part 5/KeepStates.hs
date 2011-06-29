{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad

table = map (T.split "\t") . T.lines
untable = T.unlines . map (T.intercalate "\t")

keepStates :: [T.Text] -> [[T.Text]] -> [[T.Text]]
keepStates states = filter (\[_, _, _, state] -> state `elem` states)

keepInformation = filter (\[_, org, _, _] -> org `elem` ["III", "IIS"])

main = do
    t <- liftM table $ TIO.readFile "orgs_states.tsv"
    states <- liftM T.lines $ TIO.readFile "states.txt"
    TIO.writeFile "info_states_for_real.tsv" $ untable $ keepInformation $ keepStates states t