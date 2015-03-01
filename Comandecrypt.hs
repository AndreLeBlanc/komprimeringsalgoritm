module Comandecrypt where
import Data.List
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
			| checkPos + jumps >= pos || length squashMe <= (pos + jumps) = (checkPos, jumps)
			| squashMe !! (checkPos + jumps) == squashMe !! (pos +  jumps) = (checkPos, (snd (numberofjumps pos (jumps+1) checkPos)))
			| otherwise = (0, 0)

lzdecomp :: Int -> [Int] -> [Int]
lzdecomp pos unSquash 
	| pos >= length unSquash = unSquash	
	| genericIndex unSquash pos /= 127 =  lzdecomp (pos + 1) unSquash
	| otherwise = lzdecomp (pos + (length (dezip 0)) +1) ((take pos unSquash) ++ (dezip 0) ++ (drop (pos + 3) unSquash))
	where
		steps :: Int
		steps = unSquash !! (pos + 2)

		getPos :: Int
		getPos = pos - (unSquash !! (pos + 1))

		dezip :: Int -> [Int]
		dezip jump	| steps > jump + 1 =  [unSquash !! (getPos + jump)] ++ (dezip (jump + 1))
					| otherwise = [unSquash !! (getPos + jump)]

hash :: [Int] -> [Int]
hash = undefined

dehash :: [Int] -> [Int]
dehash = undefined