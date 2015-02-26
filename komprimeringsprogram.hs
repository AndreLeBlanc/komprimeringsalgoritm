{-for binary strings-}
{-# LANGUAGE LambdaCase #-}
import Data.Binary
import qualified Data.ByteString.Char8 as B
import System.IO
import Bib
import Data.Char
import System.Directory
data Funct = Compress | Decompress | Encrypt | Decrypt deriving Eq

compressFile :: IO ()
compressFile = do
  putStr "Enter uncompressed file name "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Compress
  if filePath == "error" then putStr "Error, the file can't be compressed "
  	else
  		writeFile filePath $ compress contents
  where
	  compress :: String -> String
	  compress compressMe = let a = strToInt compressMe
						in intToStr a
	 
deCompressFile :: IO ()
deCompressFile = do
  putStr "Enter compressed file name "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Decompress
  if filePath == "error" then putStr "Error, the file needs to be compressed to be decompressed "
  	 else
  		writeFile filePath $ deCompress contents
  where
	   deCompress :: String -> String
	   deCompress deCompressMe = let a = strToInt deCompressMe
						in intToStr a

encrypt :: IO ()
encrypt = do
  putStr "Enter unencrypted file name "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Encrypt
  writeFile filePath $ crypt contents
  where
	  crypt :: String -> String
	  crypt cryptMe = let a = strToInt cryptMe
						in intToStr a

decrypt :: IO ()
decrypt = do
  putStr "Enter unencrypted file name "
  name <- getLine
  contents <- readFile name
  let filePath = makeName name Decrypt
  if filePath == "error" then putStr "Error, the file needs to be encrypted to be decrypted "
  	else
  		writeFile filePath $ uncrypt contents
  where
	  uncrypt :: String -> String
	  uncrypt uncryptMe = let a = strToInt uncryptMe
						in intToStr a

makeName :: String -> Funct -> String
makeName name lastName
	| unEncrypted && notCompressed = (take (length name - 4) name) ++ "comp.txt"
	| unEncrypted && not notCompressed && lastName == Decrypt = (take (length name - 8) name) ++ ".txt"
	| unEncrypted && not notCompressed && lastName == Encrypt = (take (length name - 6) name) ++ ".crypt"
	| not unEncrypted = (take (length name - 9) name) ++ ".txt"
	| otherwise = "error" 
	where
		notCompressed = (take (length name - 8) name) /= "comp.txt"
		unEncrypted = (take (length name - 9) name) /= "crypt.txt"

intToStr :: [Int] -> String
intToStr a = map chr a

strToInt :: String -> [Int]
strToInt a = map fromEnum a

