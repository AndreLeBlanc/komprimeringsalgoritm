module Comandecrypt where
{- 	lzcomp position squashMe
	PURPOSE: lzcomp does the compression for the program.
	PRE: Position must be equal or less than the length of squashMe. None of the numbers in squash me can be larger than 127. 
	POST: squashMe compressed with the lz77 algorithm.
	EXAMPLES: 	lzcomp 0 [1,2,3,4,5,6,7] == [1,2,3,4,5,6,7]
				lzcomp 0 [1,2,3,4,1,2,3,4,6,7,8,1,2,3,4] == [1,2,3,4,127,4,4,6,7,8,127,11,4]
-}
lzcomp :: Int -> [Int] -> [Int]
lzcomp pos squashMe
	| pos >= length squashMe = [] 	
    | snd compressable > 3 = [127, fst compressable, snd compressable] ++ (lzcomp (pos + (snd compressable)) squashMe)
	| otherwise =  (squashMe !! pos):(lzcomp (pos + 1) squashMe)
	where
		{- 	compressable
			PURPOSE: compressable find the number of jumps back that there is a pattern that matches the elements at and following squashMe at position pos.
			PRE: True
			Post: A tuple where the first Int represents the number of elements that match and the second elements represents the number of jumps back from 
				  pos that the matching elements start. If there are no matching elements (0,0) is returned.
			EXAMPLES: squashMe == [a,b,c,d,e,f,a,b,c,d,e], pos == 6
					  compressable == (0,5)
					  squashMe == [a,b,c,d,e,f,g,h,i], pos == 5
					  compressable == (0,0)
		-}
		compressable :: (Int, Int)
		compressable = search (largest $ reverse compressable') compressable'
		
		{- 	search findMe (x:xs)
			PURPOSE: To find the tuple that contains the largest number of jumps.
			PRE: FindMe has to be the second value in one of the tuples.
			EXAMPLES: 	search 5 [(6,5),(2,2),(1,1)] == (3,5)
						search 7 [(2,3),(6,5),(8,7)] == (8,7) 
		-}
		search :: Int -> [(Int, Int)] -> (Int, Int) 
		search _ [] = (0,0)
		search findMe (x:xs) = if findMe == snd x then x else search findMe xs 

		{-  largest lista
			PURPOSE: To find the the number of jumps back that you can find the longest number of consecutive elements that are the same as 
		             squashMe at the position pos and the following elements. 
			PRE: True
			POST: The largest Int that is the second element in a tuple in the list lista. 
			EXAMPLES largest [(1,2),(7,8),(5,6] == 8
					 largest [(5,6),(2,3)] == 6	
		-}
		largest :: [(Int, Int)] -> Int
		largest lista = maximum $ map snd lista
		
		{- 	compresssable'
			PURPOSE: compressable' makes a list tuples where the first elements represents the number of matching elements and the second element represents the number of 
					 steps back from pos that these elements start.
			PRE: True
			Post: A list of
		-}
		compressable' :: [(Int, Int)]
		compressable' 	
			| pos > 127 = map (numberofjumps pos 0) [(pos-126)..(pos-4)]
			| otherwise = map (numberofjumps pos 0) [0..(pos-4)]

		numberofjumps :: Int -> Int -> Int -> (Int, Int)
		numberofjumps pos jumps checkPos	
			| checkPos + jumps >= pos || length squashMe <= (pos + jumps) = (pos - checkPos, jumps)
			| squashMe !! (checkPos + jumps) == squashMe !! (pos +  jumps) = (pos - checkPos, (snd (numberofjumps pos (jumps+1) checkPos)))
			| otherwise = (pos - checkPos, jumps)

lzdecomp :: Int -> [Int] -> [Int]
lzdecomp pos unSquash 
	| pos >= length unSquash -1 = unSquash	
	| unSquash !! pos /= 127 =  lzdecomp (pos + 1) unSquash
	| otherwise = lzdecomp (pos + (length (dezip 0))) ((take pos unSquash) ++ (dezip 0) ++ (drop (pos + 3) unSquash))
	where
		steps :: Int
		steps = unSquash !! (pos + 2)

		getPos :: Int
		getPos = pos - (unSquash !! (pos + 1))

		dezip :: Int -> [Int]
		dezip jump	| steps > jump + 1 =  [unSquash !! (getPos + jump)] ++ (dezip (jump + 1))
					| otherwise = [unSquash !! (getPos + jump)]

hash :: Int -> [Int] -> [Int]
hash alice message = toCharList $ map hasch $ byte message
	where 
		hasch :: Int -> Int
		hasch meddelande = (alice + meddelande) `mod` 268435456

dehash :: Int -> [Int] -> [Int]
dehash password hash = toCharList $ map smoke $ byte hash
	where
		smoke :: Int -> Int
		smoke unHash = (unHash - password) `mod` 268435456

byte :: [Int] -> [Int]
byte meddelande 
	| length meddelande >= 4 = (makeByte $ take 4 meddelande):(byte (drop 4 meddelande))
	| length meddelande == 0 = []
	| otherwise = [makeByte meddelande]

makeByte :: [Int] -> Int
makeByte lista 	
	| length lista >= 1 = 128*(makeByte (init lista)) + last lista
	| otherwise = 0

toCharList :: [Int] -> [Int]
toCharList [] = []
toCharList (x:xs) = (four $ toChar x) ++ toCharList xs

toChar :: Int -> [Int]
toChar 0 = []
toChar number = (toChar (number `div` 128)) ++ [number `mod` 128]

four :: [Int] -> [Int]
four lista
	| length lista >= 4 = lista
	| otherwise = four (0:lista)