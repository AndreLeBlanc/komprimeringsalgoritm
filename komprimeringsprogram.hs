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
   	 where
	 	deCompress :: String -> String
		deCompress compressMe =  intToStr $ lzdecomp $ strToInt compressMe

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

    {-if exists then go 1 f else f
    where
        go suffix f = do
            exists <- doesFileExist (f ++ show suffix)
            if exists then go (suffix + 1) f else (f ++ show suffix)
-}
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
strToInt a = map fromEnum a

