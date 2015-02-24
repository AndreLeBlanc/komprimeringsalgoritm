{-for binary strings-}
import Data.Binary
import qualified Data.ByteString.Char8 as B
import System.IO

type Mword = (String, Int)
type Library = [Mword]


main = do
  putStr "Enter file name "
  name <- getLine
  contents <- readFile name
  let c = unwords (compress (quicksort (wordCount (words contents))))
  writeFile (name ++ "comp.txt") c
  

{- 	compress String
	PRE: TRUE
	POST: The string compressed and turned into a bytestring.
-}
--compress :: String -> B.ByteString 
compress [] = []
compress (h:t) = fst h : compress t


{- 	Buildlibrary
	PRE: TRUE
	POST: an extention of the standard library for the specific file
-}
--buildlibrary :: B.ByteString -> String -> B.ByteString
buildlibrary = undefined



wordCount :: [String] -> Library
wordCount [] = []
wordCount document = 
  let 
    {- wordCountAux document emptylist
       PURPOSE: Checks if a specific word in a document has already been counted.
       PRE: The second argument is an empty list
       POST: Returns a tally of all the words in the document.
       EXAMPLES:

        wordCountAux ["hello", "hey", "hello", "goodbye"] [] = [("hello",2), ("hey",1), ("goodbye",1)]
        wordCountAux [] [] = []
    -}
    wordCountAux [] _ = []
    wordCountAux (h:t) wordsUsed | elem h wordsUsed = wordCountAux t wordsUsed
                                 | otherwise        = (aux h t 1):wordCountAux t (h:wordsUsed)
       
    {- aux word document counter
       PURPOSE: Counts how many times a specific word occurs in a document.
       PRE: True
       POST: Returns a tuple with the word and its number of occurances as its elements.
       EXAMPLES:

        aux ["hello", "hey", "hello", "goodbye"] "hello" 0 = ("hello", 2) 
    -}
    aux word [] c = (word, c)
    aux word (h:t) c | word == h = aux word t (c+1)
                     | otherwise = aux word t c     
       
  in 
    wordCountAux document []


partition _ [] = ([] , [])
partition p ( x : xs ) =
	let
		( lows , highs ) = partition p xs
	in
		if (snd x) > p
			then ( x : lows , highs )
			else ( lows , x : highs )

quicksort [] = []
quicksort ( x : xs ) =
	let
		( lows , highs ) = partition (snd x) xs
	in
		quicksort lows ++ x : quicksort highs

{- decomress
	PRE: TRUE
	POST: The file decrypted from a binary file to a String
-}
--decompress :: B.ByteString -> String
decompress = undefined

{-	encrypt
	PRE: an encypted file
	POST: The file encrypted with the RSA algorithm
-}
--encrypt :: B.ByteString -> String -> B.ByteString
encrypt = undefined

{-findprime 
	PRE: TRUE
	POST: an industrial grade prime number to be used for encryption by encrypt.
-}
--findprime :: Int
findprime = undefined
{- decrypt
	PRE: True
	Post: If the password is correct the binary file decrypted. Otherwise the same binary file.
-}
--decrypt :: B.ByteString -> B.ByteString
decrypt = undefined

{- Hejsan -}