-- Program that takes a line as input and reverses the characters in each word
-- unwords - creates a String from a list of Strings and inserts spaces
--			 betewen all of them
-- words - creates an list of Strings from the original String with whitespace
--		 - in the original String acting as the delimiter.

main = do
	line <- getLine
	if null line
		then return ()
		else do
			putStrLn $ reverseWords line
			main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words