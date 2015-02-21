{-deCompress :: Library -> Compressed -> [String]
deCompress (y:ys) file = map (intToString (y:ys)) file
	where
		 intToString :: Library -> Int -> String
		 intToString (x:xs) a | (snd x) == a = fst x
		 				  	  | otherwise = intToString xs a 
-}
compress :: String -> [Int]
compress text = (compress (drop (fst (fsm 0)) text )) ++ [snd (fsm 0 )]
	where
		fsm :: String -> (Int, Int)
		fsm ch = fsm (filter (isin ch) stdlib)

isin :: (String, Int) -> String -> Bool
isin ch indexera 	| fst ch == fst (fst indexera) = True
					| otherwise = False

stdlib :: [([Char], Int)]
stdlib = [("hej", 1),("jag", 2),("kan", 3),("koda", 4),("haskell", 5),("hejsan", 6)]

{-Tankar och idéer kring projektet:

compressorn:
- Kan man använda sieve of erathostenes för att kolla vilka states som är sanna när man kollar i biblan?
- 





-}

{-Dagbok

20 februari:
Idag samlade vi alla deltare i projektet för första gången och började spåna på projektidéer.
Vi bestämde oss för att göra ett komprimeringsprogram som kan ta in en textfil och sedan komprimera den. 
Programmet ska även kunna kryptera/dekryptera komprimerade filer samt återställa dem. 

Vi insåg vårt första stora problem idag. Utput datan måste sparas som en binär fil för att komprimeringen ska vara effektiv. 
Skriver man till en textfil så blir det mycket ineffektivt.

Vi bestämmde även för att använda git för att hantera filerna och skapade även en facebook chatt. 

21 februari:
Idag har vi lekt med binära siffror och git. Vi har även tittat på andra algoritmer som används för text komprimering och ägnat tid åt annat förberedande arbete. 


-}
