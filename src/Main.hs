module Main where
import Control.Monad
import Control.Exception
import System.IO
import GameIO

-- to meet eof character
controlD :: IOError -> IO ()
controlD eofErrorType = exit

-- main function of the project
main :: IO ()
main = do
  putStrLn  "Welcome to Functional Adventure!"
  ia <- eval $ forever repl
  handle controlD ia
  