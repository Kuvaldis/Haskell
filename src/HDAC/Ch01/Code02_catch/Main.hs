import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  let fileName = case args of
        (a:_) -> a -- pattern matching. basically it means, if a is not empty
        _ -> "input.txt" -- otherwise 

  -- example of checking if file exists
  exists <- doesFileExist fileName
  input2 <- if exists then readFile filename else return ""

  input <- catch (readFile fileName)
    $ \err -> print (err::SomeException) >> return ""
  print $ countWords input
  
countWords :: String -> [Int]
countWords input = map (length.words) (lines input)
