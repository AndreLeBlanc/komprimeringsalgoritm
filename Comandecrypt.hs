module Comandecrypt where
lzcomp :: Int -> [Int] -> [Int]
lzcomp pos squashMe
	| pos >= length squashMe = [] 	
    | snd compressable > 3 = [127, fst compressable, snd compressable] ++ (lzcomp (pos + (snd compressable)) squashMe)
	| otherwise =  (squashMe !! pos):(lzcomp (pos + 1) squashMe)
	where
		compressable :: (Int, Int)
		compressable = search (largest $ reverse compressable') compressable'

		search :: Int -> [(Int, Int)] -> (Int, Int) 
		search _ [] = (0,0)
		search findMe (x:xs) = if findMe == snd x then x else search findMe xs 

		largest :: [(Int, Int)] -> Int
		largest lista = maximum $ map snd lista

		compressable' :: [(Int, Int)]
		compressable' 	
			| pos > 127 = map (numberofjumps pos 0) [(pos-126)..(pos-4)]
			| otherwise = map (numberofjumps pos 0) [0..(pos-4)]

		numberofjumps :: Int -> Int -> Int -> (Int, Int)
		numberofjumps pos jumps checkPos	
			| checkPos + jumps >= pos || length squashMe <= (pos + jumps) = (pos - checkPos, jumps)
			| squashMe !! (checkPos + jumps) == squashMe !! (pos +  jumps) = (pos - checkPos, (snd (numberofjumps pos (jumps+1) checkPos)))
			| otherwise = (0, 0)

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
hash alice message = toCharList $ map hasch (byte message)
	where 
		hasch :: Int -> Int
		hasch meddelande = (alice + meddelande) `mod` 260144641
		
		byte :: [Int] -> [Int]
		byte meddelande | length meddelande >= 4 = (makeByte $ take 4 meddelande):(byte (drop 4 meddelande))
						| length meddelande == 0 = []
						| otherwise = [makeByte meddelande]

dehash :: Int -> [Int] -> [Int]
dehash password hash = toCharList $ map smoke $ byte hash
	where
		smoke :: Int -> Int
		smoke unHash = (unHash - password) `mod` 260144641

byte :: [Int] -> [Int]
byte meddelande 
	| length meddelande >= 4 = (makeByte $ take 4 meddelande):(byte (drop 4 meddelande))
	| length meddelande == 0 = []
	| otherwise = [makeByte meddelande]

makeByte :: [Int] -> Int
makeByte lista 	
	| length lista >= 1 = 127*(makeByte (init lista)) + last lista
	| otherwise = 0

toCharList :: [Int] -> [Int]
toCharList [] = []
toCharList (x:xs) = (toChar x) ++ toCharList xs

toChar :: Int -> [Int]
toChar 0 = []
toChar number = (toChar (number `div` 127)) ++ [number `mod` 127]