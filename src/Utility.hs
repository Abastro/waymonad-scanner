module Utility (
  capitalizeFirst,
  replaceUnder,
  cleanName,
  hsVarName,
  hsConstrName,
)
where

import Data.Char (toUpper)

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (c : str) = toUpper c : str

replaceUnder :: String -> String
replaceUnder [] = []
replaceUnder "_" = []
replaceUnder ('_' : c : xs) = toUpper c : replaceUnder xs
replaceUnder (x : xs) = x : replaceUnder xs

cleanName :: String -> String
cleanName = capitalizeFirst . replaceUnder

hsVarName :: String -> String
hsVarName = replaceUnder

hsConstrName :: String -> String
hsConstrName = cleanName
