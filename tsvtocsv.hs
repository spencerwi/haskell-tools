#!/usr/bin/env runhaskell
import Data.List
import Text.Regex
 
encloseField :: String -> String
encloseField field
    | "," `isInfixOf` field     = "\"" ++ (escapeField field) ++ "\""
    | otherwise                 = escapeField field

escapeField :: String -> String
escapeField field
    | "\"" `isInfixOf` field    = subRegex (mkRegex "\"") field "\"\"" 
    | otherwise                 = field

fieldsToCSV :: [String] -> String
fieldsToCSV = (intercalate ",") . (map encloseField)

splitFields :: String -> [String]
splitFields = splitRegex (mkRegex "\t")

main = interact $ fieldsToCSV . splitFields
