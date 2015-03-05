module Comandecrypt where
import Test.HUnit

{- lzcomp position squashMe
   PURPOSE: lzcomp does the compression for the program.
   PRE: Position must be equal or less than the length of squashMe. None of the numbers in squash me can be larger than 126. 
   POST: squashMe compressed with the lz77 algorithm.
   EXAMPLES: lzcomp 0 [1,2,3,4,5,6,7] == [1,2,3,4,5,6,7]
             lzcomp 0 [1,2,3,4,1,2,3,4,6,7,8,1,2,3,4] == [1,2,3,4,127,4,4,6,7,8,127,11,4]
-}
lzcomp :: Int -> [Int] -> [Int]
lzcomp pos squashMe
 | pos >= length squashMe = [] 	
 | snd compressable > 3 = [127, fst compressable, snd compressable] ++ (lzcomp (pos + (snd compressable)) squashMe)
 | otherwise =  (squashMe !! pos):(lzcomp (pos + 1) squashMe)
 where
		
  {- compressable
  PURPOSE: compressable finds the number of jumps back that there is a sequence of elements that match the elements at and
           following squashMe at position pos.
  PRE: True
  Post: A tuple where the first Int represents the number of elements that match and the second elements represents the
        number of jumps back from pos that the matching elements start. If there are no matching elements (0,0) is returned.
  EXAMPLES: squashMe == [a,b,c,d,e,f,a,b,c,d,e], pos == 6
            compressable == (0,5)
            squashMe == [a,b,c,d,e,f,g,h,i], pos == 5
            compressable == (0,0)
        -}
  compressable :: (Int, Int)
  compressable = search (largest compressable') compressable'

  {- search findMe (x:xs)
     PURPOSE: To find the tuple that contains the longest repeating pattern.
     PRE: FindMe has to be equal to the second value in one of the tuples.
     EXAMPLES: search 5 [(6,5),(2,2),(1,1)] == (3,5)
               search 7 [(2,3),(6,5),(8,7)] == (8,7) 
  -}
  search :: Int -> [(Int, Int)] -> (Int, Int) 
  search _ [] = (0,0)
  search findMe (x:xs) = if findMe == snd x then x else search findMe xs 

  {- largest lista
	 PURPOSE: To find the the number of jumps back that you can find the longest number of consecutive elements that are the same as 
		      squashMe at the position pos and the following elements. 
	 PRE: True
	 POST: The largest Int that is in the second element in a tuple in the list lista. 
	 EXAMPLES largest [(1,2),(7,8),(5,6] == 8
			  largest [(5,6),(2,3)] == 6	
  -}
  largest :: [(Int, Int)] -> Int
  largest lista = maximum $ map snd lista
		
  {- compresssable'
	 PURPOSE: compressable' makes a list of tuples where the first elements represents the number of matching elements and the second element represents the number of 
			  positions back from pos that these elements start.
	 PRE: True
	 POST: A list of tuples where the first element represents the number of positions back from pos that a matching sequnce starts and the second element represents the number of matching elements.
		   The list starts at position 0 in squashMe or pos - 127 if pos is greater than 127.
	 EXAMPLE: squashMe == [2,3,5,3,1,5,5], pos == 5
			  compressable' == [(5,0),(4,0),(3,1)]
			  squashMe == [5,3,23] pos = 3
			  compressable' == []
  -}
  compressable' :: [(Int, Int)]
  compressable' 	
   | pos > 127 = map (numberofjumps pos 0) [(pos-127)..(pos-4)]
   | otherwise = map (numberofjumps pos 0) [0..(pos-4)]

  {- numberofjumps pos jumps checkpos
	 PURPOSE: To find the number of repeating elements in the list starting at poistion checkPos
	 PRE: jumps must be smaller or equal to pos.
	 POST: A tuple with the starting poisition as the first element and the number of matching elements as the second element.
	 EXAMPLES: squashMe = [1,2,3,4,5,6,7,8,9,1,2,3]
			   numberofjumps 9 0 0 == 3
			   numberofjumps 6 0 2 == 0
  -}
  numberofjumps :: Int -> Int -> Int -> (Int, Int)
  numberofjumps pos jumps checkPos	
   | checkPos + jumps >= pos || length squashMe <= (pos + jumps) = (pos - checkPos, jumps)
   | squashMe !! (checkPos + jumps) == squashMe !! (pos +  jumps) = (pos - checkPos, (snd (numberofjumps pos (jumps+1) checkPos)))
   | otherwise = (pos - checkPos, jumps)

{- lzdecomp pos unSquash
   PURPOSE: To decompress files that have been compressed by lzcomp.
   PRE: pos < length unSquash, unSquash must be compressed in the exact same way as lzcomp compresses.
   POST: unSquash with all elements of unSquash from position pos and beyond decompressed.
   EXAMPLES: lzdecomp 0 [1,2,3,4,5,6,7,8] == [1,2,3,4,5,6,7,8]
			 lzdecomp 0 [1,2,3,4,5,6,7,8,127,7,6] == [1,2,3,4,5,6,7,8,2,3,4,5,6,7]	
-}
lzdecomp :: Int -> [Int] -> [Int]
lzdecomp pos unSquash 
 | pos >= length unSquash -1 = unSquash	
 | unSquash !! pos /= 127 =  lzdecomp (pos + 1) unSquash
 | otherwise = lzdecomp (pos + (length (dezip 0))) ((take pos unSquash) ++ (dezip 0) ++ (drop (pos + 3) unSquash))
 where

  {- steps
	 PURPOSE: To provide a maximum limit to the number of elements that should be repeated.
	 PRE: TRUE
	 POST: The Int at position pos + 2 in the list unSquash.
	 EXAMPLES: unSquash == [1,2,3,4,5,6,127,4,3], pos == 6
		   	   steps == 3
			   unSquash == [1,2,3,4,5,6,127,5,5], pos == 6
			   steps == 5
  -}
  steps :: Int
  steps = unSquash !! (pos + 2)

  {- getPos
	 PURPOSE: To find the position where the repeat of elements should start.
	 PRE: True
	 POST: The Int at position pos + 1 in the list unSquash.
	 EXAMPLES: unSquash == [1,2,3,4,5,6,127,4,3], pos == 6
		   	   getPos == 2
			   unSquash == [1,2,3,4,5,6,127,5,5], pos == 6
			   getPos == 1
  -}
  getPos :: Int
  getPos = pos - (unSquash !! (pos + 1))

		{- 	dezip jump
			PURPOSE: To replace references to earlier sections of unSquash with the actual section
			PRE: True
			POST: The list with the references replaced with the elements that are referenced
			EXAMPLES: unSquash == [1,2,3,4,5,6,127,4,3], getPos == 2, steps == 3
					  dezip 0 == [3,4,5]
					  unSquash == [1,2,3,4,5,6,127,5,5], getPos == 1, steps == 5
					  dezip 0 == [2,3,4,5,6]
		-}
  dezip :: Int -> [Int]
  dezip jump	| steps > jump + 1 =  [unSquash !! (getPos + jump)] ++ (dezip (jump + 1))
					| otherwise = [unSquash !! (getPos + jump)]

{- hash password message
   PURPOSE: To hash files.
   PRE: True
   POST: A hashed version of the list message
   EXAMPLES: hash 45345 [23,12,45,23,12,78,120,121] == [23,15,15,56,12,81,91,26]
			 hash 4534567 [123,123,5,3,124,8,12,12] == [126,15,103,42,126,28,110,51]
-}
hash :: Int -> [Int] -> [Int]
hash alice message = toCharList $ map hasch $ byte message
 where 

  {- hasch meddelande 
	 PURPOSE: To hash a number using the caesar cipher.
	 PRE: True	
	 POST: An encrypted version of meddelande.
	 EXAMPLES: alice == 200000000
		       hasch 70000000 == 1564544
		       hasch 12345 == 200012345
  -}
  hasch :: Int -> Int
  hasch meddelande = (alice + meddelande) `mod` 268435456

{- dehash password hash
   PURPOSE: To decrypt files encrypted by the function hash.
   PRE: Files have to be encrypted by the function hash or the same algorithm as in the function hash.
   POST: An unencrypted version of the list hash
   EXAMPLES: dehash 4534567 [126,15,103,42,126,28,110,51] == [123,123,5,3,124,8,12,12]
			 dehash 45345 [23,15,15,56,12,81,91,26] == [23,12,45,23,12,78,120,121]
-}
dehash :: Int -> [Int] -> [Int]
dehash password hash = toCharList $ map smoke $ byte hash
 where

  {- smoke unHash
     PURPOSEE: To decrypt an Int using a reverse Caesar cipher.
	 PRE: True
	 POST: A decrypted version of unHash.
	 EXAMPLES: password == 1234567
		       smoke 9876543 == 8641976
		       smoke 657483 == 267858372
  -}
  smoke :: Int -> Int
  smoke unHash = (unHash - password) `mod` 268435456

{- byte meddelande
   PURPOSE: To turn a list of Ints representing a numbers in base 2^7 into a list representing the same numbers in base 2^28.
   PRE: True
   POST: A list of numbers in base 2^28 instead of a list of numbers in base 2^7.
   EXAMPLES: byte [23,15,15,56,12,81,91,26] == [48482232,26504602]
   			 byte [123,123,5,3,124,8,12,12] == [259965571,260179468]
-}
byte :: [Int] -> [Int]
byte meddelande 
 | length meddelande >= 4 = (makeByte $ take 4 meddelande):(byte (drop 4 meddelande))
 | length meddelande == 0 = []
 | otherwise = [makeByte meddelande]

{- makeByte lista
   PURPOSE: To turn a number represented as a list of 4 numbers in base 2^7 into 1 number in base 2^28 so that the caesar cipher can have 2^28 options instead of 2^7.
   PRE: True
   POST: A number in base 2^28
   EXAMPLES: makeByte [23,54,56,34] == 49126434
-}
makeByte :: [Int] -> Int
makeByte lista 	
 | length lista >= 1 = 128*(makeByte (init lista)) + last lista
 | otherwise = 0

{- toCharList lista
   PURPOSE: To turn a list of numbers in base 2^28 into a list of numbers in base 2^7.
   PRE: True
   POST: A list of Ints in base 2^7.
   EXAMPLES: toCharList [53421345,65432] == [25,60,74,33,0,3,127,24]
   			 toCharList [534232] == [0,32,77,88]
-}
toCharList :: [Int] -> [Int]
toCharList [] = []
toCharList (x:xs)
 | length xs == 0 = toChar x
 | otherwise = (four $ toChar x) ++ toCharList xs

{-  toChar number
	PURPOSE: To turn a number in base 2^28 into base 2^7 and stor the result in a list.
	PRE: True
	POST: A number in base 2^7 with each place value represented as an element in a list.
	EXAMPLE: toChar 53421345 == [25,60,74,33]
			 toChar 534232 == [0,32,77,88]
-}
toChar :: Int -> [Int]
toChar 0 = []
toChar number = (toChar (number `div` 128)) ++ [number `mod` 128]

{- four lista
   PURPOSE: To make sure that each number is represented by 4 place values.
   PRE: A list that can't contain more than 4 members.
   POST: A list of 4 Ints.
   EXAMPLE: four [12,34,56,23] == [12,34,56,23]
            four [34,56,23] == [0,34,56,23]
-}
four :: [Int] -> [Int]
four lista
 | length lista >= 4 = lista
 | otherwise = four (0:lista)


test9 = TestCase (assertEqual "lzcomp," ([116,101,115,116,105,110,103,32,116,104,105,115,32,127,
				 13,4]) (lzcomp 0 [116,101,115,116,105,110,103,32,116,104,105,115,32,116,101,
				 115,116]))

test10 = TestCase (assertEqual "lzdecomp," ([116,101,115,116,105,110,103,32,116,104,105,115,32,
				 116,101,115,116]) (lzdecomp 0 [116,101,115,116,105,110,103,32,116,104,105,115,32,
				 127,13,4]))

test11 = TestCase (assertEqual "hash," ([23,15,15,56,12,81,91,26]) 
				 (hash 45345 [23,12,45,23,12,78,120,121]))
test12 = TestCase (assertEqual "dehash," ([123,123,5,3,124,8,12,12])
				 (dehash 4534567 [126,15,103,42,126,28,110,51]))

testsComandecrypt = TestList [TestLabel "test9" test9, TestLabel "test10" test10,
        			TestLabel "test11" test11, TestLabel "test12" test12]