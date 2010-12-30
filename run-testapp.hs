import Network.Wai.Handler.DevelServer (runQuit)

main = runQuit 3000 "TestApp" "testApp" (const $ return ["hamlet/testapp.hamlet"])
