import Network.Wai.Handler.DevelServer (runQuit)

main = runQuit 3000 "Testapp" "testapp" (const $ return [])
