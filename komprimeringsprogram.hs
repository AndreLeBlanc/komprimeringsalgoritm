{-foor binary strings-}
import Data.Binary
import qualified Data.ByteString as B

data word = (String, Int)
data library = [word]

{- 	compress String
	PRE: TRUE
	POST: The string compressed and turned into a bytestring.
-}
compress :: String -> B.ByteString 
compress text = undefined


{- 	Buildlibrary
	PRE: TRUE
	POST: an extention of the standard library for the specific file
-}
buildlibrary :: B.ByteString -> String -> B.ByteString
buildlibrary = undefined

{- decomress
	PRE: TRUE
	POST: The file decyptet from a binary file to a String
-}
decompress :: B.ByteString -> String
decompress = undefined

{-	encrypt
	PRE: an encypted file
	POST: The file encrypted with the RSA algorithm
-}
encrypt :: B.ByteString -> String -> B.ByteString
encrypt = undefined

{-findprime 
	PRE: TRUE
	POST: an industrial grade prime number to be used for encryption by encrypt.
-}
findprime :: Int

{- decrypt
	PRE: True
	Post: If the password is correct the binary file decrypted. Otherwise the same binary file.
-}
decrypt :: B.ByteString -> B.ByteString
decrypt = undefined