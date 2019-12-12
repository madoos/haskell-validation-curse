module Main where

import Data.Char (isAlphaNum, isSpace)

-- Predicates
hasMaxLength :: Int -> String -> Bool 
hasMaxLength n s = (length s) <= n

isAlphaNumeric :: String -> Bool
isAlphaNumeric = (all isAlphaNum)

hasSpace :: String -> Bool
hasSpace = any isSpace

-- Util Either
toEitherValidation :: (a -> Bool) -> String -> a -> Either String a
toEitherValidation predicate validationMessage x = case (predicate x) of
                                                    True -> Right x
                                                    False -> Left validationMessage
--Validations
validateHasSpace :: String -> Either String String
validateHasSpace = toEitherValidation (not . hasSpace) "It does not have to contain spaces"

validateMaxLengthTo5 :: String -> Either String String                        
validateMaxLengthTo5 = toEitherValidation (hasMaxLength 5) "The maximum size must be 5"

validateAlphanumeric :: String -> Either String String                        
validateAlphanumeric = toEitherValidation isAlphaNumeric "Only alphanumeric characters are allowed"

--Program
validatePassword :: String -> Either String String
validatePassword password = validateHasSpace password
                            >>= validateAlphanumeric
                            >>= validateMaxLengthTo5

-- Main
main :: IO ()
main = do
  putStrLn "Please enter your password"
  password <- getLine
  print (validatePassword password)