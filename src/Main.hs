module Main where

import Data.Char (isAlphaNum, isSpace)

-- Predicates
hasMaxLength :: Int -> String -> Bool 
hasMaxLength n s = (length s) <= n

isAlphaNumeric :: String -> Bool
isAlphaNumeric = (all isAlphaNum)

hasSpace :: String -> Bool
hasSpace = any isSpace

-- Util
toValidation :: (a -> Bool) -> a -> Maybe a
toValidation predicate x | predicate x = Just x
                         | otherwise = Nothing

-- Validations                       
validateMaxLengthTo5 :: String -> Maybe String                        
validateMaxLengthTo5 = toValidation (hasMaxLength 5)

validateAlphanumeric :: String -> Maybe String                        
validateAlphanumeric = toValidation isAlphaNumeric

validateHasSpace :: String -> Maybe String
validateHasSpace = toValidation (not . hasSpace)

-- Main
main :: IO ()
main = do
  putStrLn "Please enter your password"
  password <- getLine
  print (validateMaxLengthTo5 password)
  print (validateAlphanumeric password)
  print (validateHasSpace password)
  
