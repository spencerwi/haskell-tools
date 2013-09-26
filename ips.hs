import qualified Network.Info as Net

extractIpV4 :: Net.NetworkInterface -> String -- formatted like "interface_name: ipv4_addr"
extractIpV4 iface = (Net.name iface) ++ ": " ++ ((show . Net.ipv4) iface)

formatInterfaces :: [Net.NetworkInterface] -> [String] -- format each interface and separate results by newlines
formatInterfaces = map extractIpV4

main = do
    addrs <- fmap formatInterfaces Net.getNetworkInterfaces
    mapM_ putStrLn addrs
