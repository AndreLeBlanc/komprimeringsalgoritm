module Comandecrypt where

lzcomp :: [Int] -> [Int]
lzcomp squashMe = lzcomp1 (drop 4 squashMe) 0 (take 4 squashMe)

lzcomp1 :: [Int] -> Int -> [Int] -> [Int]
lzcomp1 squashMe pos acc
	| pos >= length squashMe - 1 = acc 	
    | snd compressable > 3 = lzcomp1 squashMe (pos +1) ([127, fst compressable, snd compressable]++ acc)
	| otherwise = lzcomp1 squashMe (pos + 1) ([squashMe !! pos] ++ acc)
	where
		compressable :: (Int, Int)
		compressable = search (largest $ reverse compressable') compressable'

		search :: Int -> [(Int, Int)] -> (Int, Int) 
		search _ [] = (0,0)
		search findMe (x:xs) = if findMe == snd x then x else search findMe xs 

		largest :: [(Int, Int)] -> Int
		largest lista = maximum $ map snd lista

		compressable' :: [(Int, Int)]
		compressable' 	| pos > 132 = map (numberofjumps pos 0) [(pos-132)..(pos-4)]
						| otherwise = map (numberofjumps pos 0) [0..(pos-4)]

		numberofjumps :: Int -> Int -> Int -> (Int, Int)
		numberofjumps pos jumps checkPos	
											| (checkPos + jumps + 1 == pos) = (checkPos, jumps)
											| (checkPos +  jumps) < (pos) && squashMe !! (checkPos + jumps) == squashMe !! (pos +  jumps) = (checkPos, (1 + (snd (numberofjumps pos (jumps+1) checkPos))))
											| otherwise = (0, 0)

ldecomp a= [1,2,3,4]
lzdecomp :: [Int] -> [Int]
lzdecomp unSquash = lzdecomp1 unSquash 0 []

lzdecomp1 :: [Int] -> Int -> [Int] -> [Int]
lzdecomp1 unSquash pos acc
	| pos >= length unSquash = acc	
	| unSquash !! pos /= 127 = lzdecomp1 unSquash (pos + 1) (acc ++ [unSquash !! pos])
	| otherwise = lzdecomp1 unSquash (pos + 3) (acc ++ (dezip 0))
	where
		steps = unSquash !! (pos + 2)
		getPos = pos - (unSquash !! (pos + 1))
		dezip :: Int -> [Int]
		dezip jump	| steps > jump + 1 =  [unSquash !! (getPos + jump)] ++ (dezip (jump + 1))
					| otherwise = [unSquash !! (getPos + jump)]
hash :: [Int] -> [Int]
hash = undefined

dehash :: [Int] -> [Int]
dehash = undefined