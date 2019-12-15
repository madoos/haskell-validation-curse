module Main where

import Data.Char (isAlphaNum, isSpace)
import Data.Validation
import Data.Semigroup
import Data.Coerce (coerce)

-- Types
newtype Password = Password String deriving (Eq, Show)
newtype Username = Username String deriving (Eq, Show)
data User = User Username Password deriving (Eq, Show)

newtype Error = Error [String] deriving (Eq, Show)

instance Semigroup Error where 
  Error xs <> Error ys = Error (xs <> ys)

-- Predicates
hasMaxLength :: Int -> String -> Bool 
hasMaxLength n s = length s <= n

isAlphaNumeric :: String -> Bool
isAlphaNumeric = all isAlphaNum

hasSpace :: String -> Bool
hasSpace = any isSpace

-- Util Validation
toValidation :: (a -> Bool) -> Error -> a -> Validation Error a
toValidation predicate validationMessage x | predicate x = Success x
                                           | otherwise = Failure validationMessage

--Validations
validateHasSpace :: String -> Validation Error String
validateHasSpace = toValidation (not . hasSpace) (Error ["It does not have to contain spaces"])

validateMaxLengthTo5 :: String -> Validation Error String                        
validateMaxLengthTo5 = toValidation (hasMaxLength 5) (Error ["The maximum size must be 5"])

validateAlphanumeric :: String -> Validation Error String                        
validateAlphanumeric = toValidation isAlphaNumeric (Error ["Only alphanumeric characters are allowed"])

--Program
validateUsername :: String -> Validation Error String
validateUsername username = validateAlphanumeric username
                            <* validateHasSpace username
                            

validatePassword :: String -> Validation Error String
validatePassword password = validateHasSpace password
                            <* validateAlphanumeric password
                            <* validateMaxLengthTo5 password
                            
identifyLabel :: String -> (a -> Validation Error a) -> a -> Validation Error a
identifyLabel message f x = case f x of 
                              Success x -> Success x  
                              Failure error -> Failure (Error [message] <> error) 
                                                      

validateCredentials :: String -> String -> Validation Error User
validateCredentials username password = User <$> username' <*> password' where
                                        username' = Username <$> identifyLabel "Username Errors:" validateUsername username
                                        password' = Password <$> identifyLabel "Password Errors:" validatePassword password

display :: Validation Error User -> IO ()
display userValidation = case userValidation of
                         Failure error -> print (unlines (coerce error))
                         Success (User username password) -> print ("hello " ++ coerce username)

-- Main
main :: IO ()
main = do
  putStrLn "Please enter your user name:"
  username <- getLine
  putStrLn "Please enter your password:"
  password <- getLine
  putStrLn ""
  display (validateCredentials username password)