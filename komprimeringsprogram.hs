{-for binary strings-}
{-# LANGUAGE LambdaCase #-}
import Data.Binary
import qualified Data.ByteString.Char8 as B
import System.IO
import Bib
import Data.Char
import System.Directory
import Comandecrypt
import Control.Monad
data Funct = Compress | Decompress | Encrypt | Decrypt deriving Eq


{- 
PURPOSE: To compress the given file.
PRE: 
POST: The given file compressed.
EXAMPLES:
-}
compressFile :: IO ()
compressFile = do
  putStr "Enter uncompressed file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Compress
  if filePath == "error" then putStr "Error, the file can't be compressed "
  	else
  		writeFile filePath (compress contents)
  where
	  compress :: String -> String
	  compress compressMe = intToStr $ (lzcomp 0) $ strToInt compressMe

{- 
PURPOSE: To decompress the given file.
PRE: 
POST: 
EXAMPLES: The given file decompressed.
-}
deCompressFile :: IO ()
deCompressFile = do
  putStr "Enter compressed file name: "
  name <- getLine
  contents <- readFile name

  let filePath = makeName name Decompress

  if filePath == "error" then putStr "Error, the file needs to be compressed to be decompressed "
     else
        fexist filePath 1 (deCompress contents)

  where
    deCompress :: String -> String
    deCompress deCompressMe = intToStr $ (lzdecomp 0) $ strToInt deCompressMe

    fexist f x c = do
      exist <- doesFileExist f
      if exist 
        then fexist ((take (length f - 4) f) ++ (show x) ++ ".txt") (x + 1) c
      else
        writeFile f c

{- 
PURPOSE: To encrypt the given file.
PRE: 
POST: 
EXAMPLES: The given file encrypted.
-}
encrypt :: IO ()
encrypt = do
  putStr "Enter unencrypted file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Encrypt
  writeFile filePath (crypt contents)
  where
	  crypt :: String -> String
	  crypt cryptMe = intToStr $ hash $ strToInt cryptMe

{- 
PURPOSE: The decrypt the given file.
PRE: 
POST: 
EXAMPLES: The given file decrypted.
-}
decrypt :: IO ()
decrypt = do
  putStr "Enter unencrypted file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Decrypt
  if filePath == "error" then putStr "Error, the file needs to be encrypted to be decrypted "
  	else
  		writeFile filePath (uncrypt contents)
  where
	  uncrypt :: String -> String
	  uncrypt uncryptMe = intToStr $ dehash $ strToInt uncryptMe

{- 
PURPOSE: To generate a new name of the given name based on certain conditions.
PRE: 
POST: 
EXAMPLES: A new version of the given name.
-}
makeName :: String -> Funct -> String
makeName name lastName
	| unEncrypted && not compressed && lastName == Compress = (newName 4) ++ "comp.txt"
	| unEncrypted && compressed && lastName == Decrypt = (newName 9) ++ ".txt"
	| unEncrypted && compressed && lastName == Encrypt = (newName 4) ++ "crypt.txt"
	| unEncrypted && lastName == Decompress = (newName 8) ++ ".txt"
	| otherwise = "error" 
	where
    compressed :: Bool 
    compressed = (drop (length name - 8) name) == "comp.txt"
    
    unEncrypted :: Bool
    unEncrypted = (drop (length name - 9) name) /= "crypt.txt"
    
    newName :: Int -> String
    newName len = take (length name - len) name 


{- 
PURPOSE: 
PRE: 
POST: 
EXAMPLES:
-}
intToStr :: [Int] -> String
intToStr a = map chr a

{- 
PURPOSE: 
PRE: 
POST: 
EXAMPLES:
-}
strToInt :: String -> [Int]
strToInt [] = []
strToInt (x:xs)
  | length xs >= 2 && dou /= 1000 = dou:(strToInt (drop 2 xs))
  | length xs >= 3 && trio /= 1000 = trio:(strToInt (drop 3 xs))
  | length xs >= 5 && penta /= 1000 = penta:(strToInt (drop 5 xs))
  | otherwise = [fromEnum x] ++ (strToInt xs)
  where
    dou = if [x] == "\\" then asci (take 2 xs) else 1000
    trio = if [x] == "\\" then asci (take 3 xs) else 1000
    penta = if [x] == "\\" then asci (take 5 xs) else 1000

{- 
PURPOSE: 
PRE: 
POST: 
EXAMPLES:
-}
asci :: String -> Int
asci "DEL" = 127
asci "NUL" = 0
asci "SOH" = 1
asci "STX" = 2
asci "ETX" = 3
asci "EOT" = 4
asci "ENQ" = 5
asci "ACK" = 6
asci "BEL" = 7
asci "BS" = 8
asci "TAB" = 9
asci "LF" = 10
asci "VT" = 11
asci "FF" = 12
asci "CR" = 13
asci "SO" = 14
asci "SI" = 15
asci "DLE" = 16
asci "DC1" = 17
asci "DC2" = 18
asci "DC3" = 19
asci "DC4" = 20
asci "NAK" = 21
asci "SYN" = 22
asci "ETB" = 23
asci "CAN" = 24
asci "EM" = 25
asci "SUB" = 26
asci "ESC" = 27
asci "FS" = 28
asci "GS" = 29
asci "RS" = 30
asci "US" = 31
asci "Space" = 32
asci _ = 1000
