{-for binary strings-}
{-# LANGUAGE LambdaCase #-}
import Data.Binary
import System.IO
import Data.Char
import System.Directory
import Comandecrypt
import Control.Monad
import Test.HUnit

{-
Funct

REPR. CONV.: Funct is represented by four conditions, Compress, Decompress, Encrypt and Decrypt.
REPR. INVARIANT: None
-}
data Funct = Compress | Decompress | Encrypt | Decrypt deriving Eq


{- 
compressFile

PURPOSE: To compress the given file.
PRE: The text file the user wants to compress needs to be in the same directory as the program.
POST: The given file compressed.
EXAMPLES: compressFile -> "test.txt" -> "testcomp.txt"
-}
compressFile :: IO ()
compressFile = do
  putStr "Enter uncompressed file name: "
  name <- getLine
  contents <- readFile name

  let filePath = makeName name Compress

      {- 
        fexist f x c

        PURPOSE: Checks whether a file with the given name f already exists in the directory,
                 and if so, generates and saves the file with contents c and its new name.
        PRE: True
        POST: The new file with contents c and the given name f if no conflicts of the name 
              is found within the directory, otherwise the new file is saved with contents c
              and a new name.
        EXAMPLES: fexist "test.txt" 1 "hey" -> File already exists ->
                  File saved as "test1.txt" with contents "hey"

                  fexist "test.txt" 1 "hey" -> File does not exist ->
                  File saved as "test.txt" with contents "hey"
      -}
      fexist :: String -> Int -> String -> IO ()
      fexist f x c = do
        exist <- doesFileExist f
        if exist 
          then fexist ((take (length f - 4) f) ++ (show x) ++ ".txt") (x + 1) c
        else
          writeFile f c


  if filePath == "error" then putStr "Error, the file can't be compressed "
    else
      fexist filePath 1 (compress contents)
  where

    {- 
    compress s

    PURPOSE: To compress the given string s.
    PRE: True.
    POST: The given string compressed.
    EXAMPLES: compress "test test" = "test \DEL\ENQ\EOT"
    -}
    compress :: String -> String
    compress compressMe = intToStr $ (lzcomp 0) $ strToInt compressMe


{- 
deCompressFile

PURPOSE: To decompress the given file.
PRE: The text file the user wants to decompress needs to be in the same directory as the program.
POST: The given file decompressed.
EXAMPLES: deCompressFile -> "testcomp.txt" -> "test.txt"
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

    {- 
    deCompress s

    PURPOSE: To decompress the given string s
    PRE: True
    POST: The given string decompressed
    EXAMPLES: deCompress "test \DEL\ENQ\EOT" = "test test"
    -}
    deCompress :: String -> String
    deCompress deCompressMe = intToStr $ (lzdecomp 0) $ strToInt deCompressMe

{- 
fexist f x c

PURPOSE: Checks whether a file with the given name f already exists in the directory,
         and if so, generates and saves the file with contents c and its new name.
PRE: True
POST: The new file with contents c and the given name f if no conflicts of the name 
      is found within the directory, otherwise the new file is saved with contents c
      and a new name.
EXAMPLES: fexist "test.txt" 1 "hey" -> File already exists ->
          File saved as "test1.txt" with contents "hey"

          fexist "test.txt" 1 "hey" -> File does not exist ->
          File saved as "test.txt" with contents "hey"
-}
    fexist :: String -> Int -> String -> IO ()
    fexist f x c = do
      exist <- doesFileExist f
      if exist 
        then fexist ((take (length f - 4) f) ++ (show x) ++ ".txt") (x + 1) c
      else
        writeFile f c



{- 
encrypt

PURPOSE: To encrypt the given file.
PRE: The given file must be in its compressed state, the chosen password must be 1-4 ASCII
     characters. The file needs to be in the same directory as the program.
POST: The given file encrypted.
EXAMPLES: encrypt -> "testcomp.txt" -> "testcompcrypt.txt"
-}
encrypt :: IO ()
encrypt = do
  putStr "Enter unencrypted file name: "
  name <- getLine
  putStr "Enter password, 1-4 ASCII characters "
  password <- getLine
  let pass = makepswd password
  contents <- readFile name
  let filePath = makeName name Encrypt
  if filePath == "error" then putStr "Error, the file needs to be compressed to be encrypted"
    else
      fexist filePath 1 (crypt contents pass)
  where

    {- 
    crypt s p

    PURPOSE: To encrypt the given string s with password p.
    PRE: True.
    POST: The given string s encrypted with password p.
    EXAMPLES: crypt "test" 1234 = "te}F"
    -}
    crypt :: String -> Int -> String
    crypt cryptMe pass = intToStr $ (hash pass) $ strToInt cryptMe

{- 
fexist f x c

PURPOSE: Checks whether a file with the given name f already exists in the directory,
         and if so, generates and saves the file with contents c and its new name.
PRE: True
POST: The new file with contents c and the given name f if no conflicts of the name 
      is found within the directory, otherwise the new file is saved with contents c
      and a new name.
EXAMPLES: fexist "test.txt" 1 "hey" -> File already exists ->
          File saved as "test1.txt" with contents "hey"

          fexist "test.txt" 1 "hey" -> File does not exist ->
          File saved as "test.txt" with contents "hey"
-}
    fexist :: String -> Int -> String -> IO ()
    fexist f x c = do
      exist <- doesFileExist f
      if exist 
        then fexist ((take (length f - 4) f) ++ (show x) ++ ".txt") (x + 1) c
      else
        writeFile f c

{- 
decrypt

PURPOSE: The decrypt the given file.
PRE: The password must be 1-4 ASCII characters. The file needs to be in the same directory
     as the program.
POST: The given file decrypted.
EXAMPLES: decrypt -> "testcompcrypt.txt" -> "testcomp.txt"
-}
decrypt :: IO ()
decrypt = do
  putStr "Enter unencrypted file name: "
  name <- getLine
  contents <- readFile name
  putStr "Enter password, 1-4 ASCII characters "
  password <- getLine
  let pass = makepswd password
  let filePath = makeName name Decrypt
  if filePath == "error" then putStr "Error, the file needs to be encrypted to be decrypted "
    else
      fexist filePath 1 (uncrypt contents pass)
  where

    {- 
    uncrypt s p

    PURPOSE: To decrypt the given string s with the password p.
    PRE: True.
    POST: The string s decrypted with the password p.
    EXAMPLES: uncrypt "te}F" 1234 = "test"
    -}
    uncrypt :: String -> Int -> String
    uncrypt uncryptMe password = intToStr $ (dehash password) $ strToInt uncryptMe

{- 
fexist f x c

PURPOSE: Checks whether a file with the given name f already exists in the directory,
         and if so, generates and saves the file with contents c and its new name.
PRE: True
POST: The new file with contents c and the given name f if no conflicts of the name 
      is found within the directory, otherwise the new file is saved with contents c
      and a new name.
EXAMPLES: fexist "test.txt" 1 "hey" -> File already exists ->
          File saved as "test1.txt" with contents "hey"

          fexist "test.txt" 1 "hey" -> File does not exist ->
          File saved as "test.txt" with contents "hey"
-}
    fexist :: String -> Int -> String -> IO ()
    fexist f x c = do
      exist <- doesFileExist f
      if exist 
        then fexist ((take (length f - 4) f) ++ (show x) ++ ".txt") (x + 1) c
      else
        writeFile f c

{- 
makeName name lastname

PURPOSE: To generate a new name of the given name by looking at its current name.
PRE: True
POST: A new version of the given name.
EXAMPLES: makeName "test.txt" Compress = "testcomp.txt"
          makeName "testcomp.txt" Compress = "error"
-}
makeName :: String -> Funct -> String
makeName name lastName
  | unEncrypted && not compressed && lastName == Compress = (newName 4) ++ "comp.txt"
  | not unEncrypted && lastName == Decrypt = (newName 9) ++ ".txt"
  | unEncrypted && compressed && lastName == Encrypt = (newName 4) ++ "crypt.txt"
  | unEncrypted && lastName == Decompress = (newName 8) ++ ".txt"
  | otherwise = "error" 
  where

    {- 
    compressed

    PURPOSE: To check if the string ends with "comp.txt"
    PRE:
    POST: True if the string ends with "comp.txt", otherwise False.
    EXAMPLES:
    -}
    compressed :: Bool 
    compressed = (drop (length name - 8) name) == "comp.txt"
    
    {- 
    unEncrypted

    PURPOSE: To check if the string ends with "crypt.txt"
    PRE:
    POST: True if the string does not end with "crypt.txt", otherwise False.
    EXAMPLES:
    -}
    unEncrypted :: Bool
    unEncrypted = (drop (length name - 9) name) /= "crypt.txt"

    {- 
    newName len

    PURPOSE: To remove len number of characters from the end of the string.
    PRE:
    POST: The string with len number of characters removed from the end.
    EXAMPLES:
    -}
    newName :: Int -> String
    newName len = take (length name - len) name 


{- 
intToStr a

PURPOSE: To convert a list of integers to its corresponding characters in the ASCII table
         concatenated into a string.
PRE: True
POST: A string of the corresponding characters given from the list of integers.
EXAMPLES: intToStr [116,101,115,116] = "test"
-}
intToStr :: [Int] -> String
intToStr a = map chr a

{- 
strToInt str

PURPOSE: To convert the characters of a string to its corresponding integers in the ASCII table.
PRE: True
POST: A list of the corresponding integers given from the string.
EXAMPLES: strToInt "test" = [116,101,115,116]
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
PURPOSE: To generate a password based on the given string.
PRE: True
POST: A password consisting of integers based on the given string.
EXAMPLES: makepswd "test" = 244939252
-}
makepswd :: String -> Int
makepswd "" = 0
makepswd pass = if length pass == 1
                  then head $ strToInt [last pass]
                else

                    (head $ strToInt [last pass]) + 128 * makepswd (init pass)
{- 
PURPOSE: To match ASCII characters to its corresponding integers
PRE: True
POST: The integer matched to its ASCII character. 
EXAMPLES: asci "DEL" = 127
          asci "test" = 1000
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

test1 = TestCase (assertEqual "makeName," ("testcomp.txt") (makeName "test.txt" Compress))
test2 = TestCase (assertEqual "makeName," ("test.txt") (makeName "testcrypt.txt" Decrypt))
test3 = TestCase (assertEqual "makeName," ("testcompcrypt.txt") (makeName "testcomp.txt" Encrypt))
test4 = TestCase (assertEqual "makeName," ("test.txt") (makeName "testcomp.txt" Decompress))
test5 = TestCase (assertEqual "makeName," ("error") (makeName "testcomp.txt" Compress))
test6 = TestCase (assertEqual "intToStr," ("testing") (intToStr [116,101,115,116,105,110,103]))
test7 = TestCase (assertEqual "strToInt," ([116,101,115,116,105,110,103]) (strToInt "testing"))
test8 = TestCase (assertEqual "makepswd," (244939252) (makepswd "test"))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2,
        TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, 
        TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8]