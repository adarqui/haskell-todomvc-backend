module Main (
  main
) where



import           System.Environment      (getArgs)
import           Web.TodoMVC.Servant.API (runServer)



main :: IO ()
main = do
  getArgs >>= \args ->
    case args of
      (port:[]) -> runServer (read port)
      _         -> runServer 1080
