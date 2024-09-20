module Menu.Shared where

import ContactsLib
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Menu

type AppState = ContactList

data NameOrNumber = Name | Number

getNameOrNumber :: String -> IO NameOrNumber
getNameOrNumber text = do
  option <- prompt text
  handleOption option
  where
    handleOption opt | '1' `elem` opt = return Name
    handleOption opt | '2' `elem` opt = return Number
    handleOption _ = getNameOrNumber "That's not an option! Try again. > "

notSpace :: (Foldable t) => t Char -> Bool
notSpace str = not $ isSpace `all` str

getContactFromMenu :: (Rubric p) => p -> IO (Maybe Contact)
getContactFromMenu s = do
  nameOrNumber <- getNameOrNumber "By name [1] or number [2]? > "
  key <- prompt "Key: "

  let by property contact =
        notSpace key && isPrefixOf key (property contact)
      maybeContact =
        case nameOrNumber of
          Name -> getContact (by _name) s
          Number -> getContact (by _phoneNumber) s
   in return maybeContact

indexOfContact :: AppState -> (Contact -> Bool) -> Maybe Int
indexOfContact s by = indexOfContact' s by 0

indexOfContact' :: AppState -> (Contact -> Bool) -> Int -> Maybe Int
indexOfContact' (ContactList []) _ _ = Nothing
indexOfContact' (ContactList (c : cs)) by count
  | by c = Just count
  | otherwise = indexOfContact' (ContactList cs) by (count + 1)
