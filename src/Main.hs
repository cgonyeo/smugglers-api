import Network.Wai.Handler.Warp

import Smugglers.API

main :: IO ()
main = run 8080 app
