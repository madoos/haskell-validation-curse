module Main where

hasMaxLength :: Int -> String -> Bool 
hasMaxLength n s = (length s) <= n

toValidation :: (a -> Bool) -> a -> Maybe a
toValidation predicate x | predicate x = Just x
                         | otherwise = Nothing

validateMaxLengthTo5 :: String -> Maybe String                        
validateMaxLengthTo5 = toValidation (hasMaxLength 5)

main :: IO ()
main = do
  putStrLn "Please enter your password"
  password <- getLine
  print (validateMaxLengthTo5 password)
  
