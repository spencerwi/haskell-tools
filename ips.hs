import qualified Network.Info
import qualified Data.List

extractIpV4 :: Network.Info.NetworkInterface -> String -- formatted like "interface_name: ipv4_addr"
extractIpV4 iface = (Network.Info.name iface) ++ ": " ++ ((show . Network.Info.ipv4) iface)

formatInterfaces :: [Network.Info.NetworkInterface] -> String -- format each interface and separate results by newlines
formatInterfaces = (Data.List.intercalate "\n") . map extractIpV4

main = do
    addrs <- fmap formatInterfaces Network.Info.getNetworkInterfaces
    putStrLn $ addrs
