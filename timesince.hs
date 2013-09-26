import Data.List (isInfixOf, zip)
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)
import Data.Time

parseAsLocalTime :: String  -> LocalTime
parseAsLocalTime dateStr 
    | ":" `isInfixOf` dateStr       = parseWithFormatStr "%F %H:%M" dateStr
    | otherwise                     = parseWithFormatStr "%F" dateStr
    where parseWithFormatStr = readTime defaultTimeLocale

breakDownToFriendlyOutput :: Integer -> String
breakDownToFriendlyOutput seconds
    | seconds == 0                     = ""
    | toMinutes seconds == 0           = (show seconds) ++ " seconds "
    | toHours seconds == 0             = (show $ toMinutes seconds) ++ " minutes " ++ breakDownToFriendlyOutput (remMinutes seconds)
    | toDays seconds == 0              = (show $ toHours seconds) ++ " hours " ++ breakDownToFriendlyOutput (remHours seconds)
    | otherwise                        = (show $ toDays seconds) ++ " days " ++ breakDownToFriendlyOutput (remDays seconds)
    where 
        toMinutes x = x `div` 60
        toHours x = (toMinutes x) `div` 60
        toDays x = (toHours x) `div` 24 
        remMinutes x = x `rem` 60
        remHours x = x `rem` (60*60)
        remDays x = x `rem` (60*60*24)

formatOutput :: LocalTime -> Integer -> String
formatOutput time difference = (breakDownToFriendlyOutput difference)  ++ "since " ++ (show time)

main = do
    now <- getCurrentTime
    localTimeZone <- getCurrentTimeZone
    
    dates <- fmap (map parseAsLocalTime) getArgs
    -- This next step is ugly. Looks like a refactor candidate.
    let timeDifferences = map (round . (diffUTCTime now) . (localTimeToUTC localTimeZone)) dates
    let outputStrings = zipWith formatOutput dates timeDifferences
    mapM_ putStrLn outputStrings
