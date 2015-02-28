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

compressFile :: IO ()
compressFile = do
  putStr "Enter uncompressed file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Compress
  if filePath == "error" then putStr "Error, the file can't be compressed "
  	else
  		writeFile filePath $ compress contents
  where
	  compress :: String -> String
	  compress compressMe = let a = lzcomp $ strToInt compressMe
						in intToStr a

deCompressFile :: IO ()
deCompressFile = do
  putStr "Enter compressed file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Decompress
  if filePath == "error" then putStr "Error, the file needs to be compressed to be decompressed "
     else
        writeFile filePath (deCompress contents)

deCompress :: String -> String
deCompress compressMe = intToStr $ lzdecomp $ strToInt compressMe


encrypt :: IO ()
encrypt = do
  putStr "Enter unencrypted file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Encrypt
  writeFile filePath $ crypt contents
  where
	  crypt :: String -> String
	  crypt cryptMe = let a = hash $ strToInt cryptMe
						in intToStr a

decrypt :: IO ()
decrypt = do
  putStr "Enter unencrypted file name: "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Decrypt
  if filePath == "error" then putStr "Error, the file needs to be encrypted to be decrypted "
  	else
  		writeFile filePath $ uncrypt contents
  where
	  uncrypt :: String -> String
	  uncrypt uncryptMe = let a = dehash $ strToInt uncryptMe
						in intToStr a

makeName :: String -> Funct -> String
makeName name lastName
	| unEncrypted && not compressed = (take (length name - 4) name) ++ "comp.txt"
	| unEncrypted && compressed && lastName == Decrypt = (take (length name - 8) name) ++ ".txt"
	| unEncrypted && compressed && lastName == Encrypt = (take (length name - 6) name) ++ ".crypt"
	| unEncrypted = (take (length name - 8) name) ++ ".txt"
	| otherwise = "error" 
	where
		compressed = (drop (length name - 8) name) == "comp.txt"
		unEncrypted = (drop (length name - 9) name) /= "crypt.txt"

intToStr :: [Int] -> String
intToStr a = map chr a

strToInt :: String -> [Int]
strToInt [] = []
strToInt (x:xs)
  | length xs >= 2 && dou /= 1000 = dou:(strToInt xs)
  | length xs >= 3 && trio /= 1000 = trio:(strToInt xs)
  | length xs >= 5 && penta /= 1000 = penta:(strToInt xs)
  | otherwise = [fromEnum x] ++ (strToInt xs)
  where
    dou = if [x] == head ["\\"] then asci (take 2 xs) else 1000
    trio = if [x] == head ["\\"] then asci (take 3 xs) else 1000
    penta = if [x] == head ["\\"] then asci (take 5 xs) else 1000

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


