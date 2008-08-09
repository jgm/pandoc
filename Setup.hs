import Distribution.Simple
import System.Process ( runCommand, waitForProcess )
import System.Directory ( setCurrentDirectory )

main = defaultMainWithHooks (simpleUserHooks {runTests = runTestSuite})

testDir = "tests"

runTestSuite _ _ _ _ = do
  setCurrentDirectory testDir
  runCommand "runhaskell RunTests.hs" >>= waitForProcess
  return ()

