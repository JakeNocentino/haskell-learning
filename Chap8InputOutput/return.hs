-- showcasing what 'return' actually means in Haskell. See
-- book for details.

main = do
	return ()
	return "HAHAHA"
	line <- getLine
	return "Blah!"
	return 4
	putStrLn line
	a <- return "hell"
	b <- return "yeah!"
	putStrLn $ a ++ " " ++ b