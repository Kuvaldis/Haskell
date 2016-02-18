import Text.CSV

main :: IO ()
main = do
  let fileName = "input.csv"
  input <- readFile fileName

  let csv = parseCsv fileName input
  either handleError doWork csv
  
