import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec (exec)


main :: IO()
main =
  do
    putStrLn "\n\nType 'help' if you need to\nType ':quit' to quit\n\nExamples:\n\n  > stats whitman.txt\n  > dfreq ship.txt 0 20\n  > test ship-digram.csv\n\n"
    loop

loop :: IO ()
loop = do
  input <- read'
  unless (input == ":quit")
       $ exec input -- print' (eval' input)
      >> loop


read' :: IO String
read' = putStr "REPL> "
     >> hFlush stdout
     >> getLine


eval' :: String -> String
eval' input =
  "input: " ++ input


print' :: String -> IO ()
print' = putStrLn
