import Distribution.Simple
import Distribution.MacOSX

main :: IO ()
main = defaultMainWithHooks macHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps
       }

guiApps :: [MacApp]
guiApps = [MacApp "Pinhole"
                  (Just "res/Pinhole.icns")
                  Nothing
                  []
                  []
                  ChaseWithDefaults
          ]
