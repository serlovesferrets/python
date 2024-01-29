module Main where

import ContactsLib (Contact (..), ContactList, Rubric (getContact), defaultContacts)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Menu

type AppState = ContactList

state :: AppState
state = defaultContacts

-- Getting all contacts
menuGetAllContacts :: MenuAction AppState
menuGetAllContacts s = return (s, Ok $ show s)

-- Getting certain contacts
data NameOrNumber = Name | Number

getNameOrNumber :: String -> IO NameOrNumber
getNameOrNumber text = do
  option <- prompt text
  if '1' `elem` option
    then return Name
    else
      if '2' `elem` option
        then return Number
        else getNameOrNumber "That's not an option! Try again. > "

menuGetContact :: (Rubric a) => a -> IO (a, MenuResult)
menuGetContact s = do
  nameOrNumber <- getNameOrNumber "By name [1] or number [2]? > "
  key <- prompt "Key: "

  let notSpace str = not $ isSpace `all` str
      predicate str accessor c =
        notSpace str && isPrefixOf key (accessor c)
      contact =
        case nameOrNumber of
          Name -> getContact (predicate key _name) s
          Number -> getContact (predicate key _phoneNumber) s

  let result =
        case contact of
          Nothing -> Error "Contact not found!"
          Just c -> Ok $ "Contact found:\n" ++ show c

  return (s, result)

-- TODO: Adding a contact
-- TODO: Removing a contact
-- TODO: Editing a contact

options :: [MenuOption AppState]
options =
  [ MenuOption
      { _description = "Get all the contacts",
        _action = menuGetAllContacts
      },
    MenuOption
      { _description = "Get a certain contact",
        _action = menuGetContact
      }
  ]

menu :: MenuChooser AppState
menu = makeMenu "Gestore contatti" state options

main :: IO ()
main = do
  runMenu menu
