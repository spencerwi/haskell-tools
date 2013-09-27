import Data.List (isInfixOf, zip)
import System.Exit (exitFailure)
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

diffFromNow :: LocalTime -> IO Integer
diffFromNow date = do
    now <- getCurrentTime
    localTimeZone <- getCurrentTimeZone
    return $ round (diffUTCTime now (localTimeToUTC localTimeZone date))

formatOutput :: LocalTime -> Integer -> String
formatOutput time difference = (breakDownToFriendlyOutput difference)  ++ "since " ++ (show time)

main = do
    args <- getArgs
    case args of
        [] -> do 
                putStrLn "Usage: timesince DATESTRING..."
                putStrLn "Accepts dates in YYYY-MM-DD or YYYY-MM-DD HH:mm format (24-hour)"
                exitFailure
        _ -> do
                let dates = map parseAsLocalTime args
                
                timeDifferences <- mapM diffFromNow dates
                let outputStrings = zipWith formatOutput dates timeDifferences
                mapM_ putStrLn outputStrings
