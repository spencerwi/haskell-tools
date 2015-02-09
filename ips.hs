module IPs where

import Network.Info as Net
import Control.Monad (filterM)

isIPv4Up :: Net.NetworkInterface -> Bool
isIPv4Up = ((/=) "0.0.0.0") . show . ipv4

formatInterface :: Net.NetworkInterface -> String
formatInterface iface = (Net.name iface) ++ ": " ++ ((show . Net.ipvr) iface)

main = Net.getNetworkInterfaces >>= filterM (return . isIPv4UP) >>= mapM_ (putStrLn . formatInterface)
