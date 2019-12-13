module Main where

import Data.Char (isAlphaNum, isSpace)

-- Types
newtype Password = Password String deriving (Eq, Show)
newtype Username = Username String deriving (Eq, Show)
newtype Error = Error String deriving (Eq, Show)
data User = User Username Password deriving (Eq, Show)

-- Predicates
hasMaxLength :: Int -> String -> Bool 
hasMaxLength n s = (length s) <= n

isAlphaNumeric :: String -> Bool
isAlphaNumeric = (all isAlphaNum)

hasSpace :: String -> Bool
hasSpace = any isSpace

-- Util Either
toEitherValidation :: (a -> Bool) -> Error -> a -> Either Error a
toEitherValidation predicate validationMessage x = case (predicate x) of
                                                    True -> Right x
                                                    False -> Left validationMessage
--Validations
validateHasSpace :: String -> Either Error String
validateHasSpace = toEitherValidation (not . hasSpace) (Error "It does not have to contain spaces")

validateMaxLengthTo5 :: String -> Either Error String                        
validateMaxLengthTo5 = toEitherValidation (hasMaxLength 5) (Error "The maximum size must be 5")

validateAlphanumeric :: String -> Either Error String                        
validateAlphanumeric = toEitherValidation isAlphaNumeric (Error "Only alphanumeric characters are allowed")

--Program
validateUsername :: String -> Either Error String
validateUsername username = validateAlphanumeric username
                            >>= validateHasSpace
                            

validatePassword :: String -> Either Error String
validatePassword password = validateHasSpace password
                            >>= validateAlphanumeric
                            >>= validateMaxLengthTo5

validateCredentials :: String -> String -> Either Error User
validateCredentials username password = User <$> username' <*> password' where
                                        username' = Username <$> (validateUsername username)
                                        password' = Password <$> (validatePassword password)

-- Main
main :: IO ()
main = do
  putStrLn "Please enter your user name"
  username <- getLine
  putStrLn "Please enter your password"
  password <- getLine
  print (validateCredentials username password)