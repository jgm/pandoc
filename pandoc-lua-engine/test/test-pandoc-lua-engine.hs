module Main (main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Tests.Lua
import qualified Tests.Lua.Module
import qualified Tests.Lua.Writer
import System.Directory (withCurrentDirectory)

main :: IO ()
main = withCurrentDirectory "test" $ defaultMain tests

tests :: TestTree
tests = testGroup "pandoc Lua engine"
  [ testGroup "Lua filters" Tests.Lua.tests
  , testGroup "Lua modules" Tests.Lua.Module.tests
  , testGroup "Custom writers" Tests.Lua.Writer.tests
  ]
