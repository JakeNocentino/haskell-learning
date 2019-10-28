-- example of forever I/O function
import Control.Monad -- location of forever
import Data.Char

main = forever $ do
	putStr "Give me some input: "
	l <- getLine
	putStrLn $ map toUpper l