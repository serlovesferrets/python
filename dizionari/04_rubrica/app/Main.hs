module Main where

import ContactsLib
import Data.Char (isSpace)
import Menu
  ( Menu (..),
    Option (Option, _action, _description),
    runMenu,
  )

menuGetAllContacts :: Option
menuGetAllContacts =
  Option
    { _description = "Get all contacts",
      _action = return $ show defaultContacts
    }

menuAddContact = do
  putStrLn "Inserisci il nome del contatto."
  name <- getLine
  putStrLn "Inserisci il numero."
  phoneNumber <- getLine
  putStrLn "Inserisci l'indirizzo (non necessario)"
  address <- getLine
  putStrLn "Inserisci l'email (non necessario)"
  email <- getLine

  let contact =
        Contact
          { _name = name,
            _phoneNumber = phoneNumber,
            _address = if isSpace `all` address then Nothing else Just address,
            _email = if isSpace `all` email then Nothing else Just email
          }

  return name

menu :: Menu
menu = Menu [menuGetAllContacts]

main :: IO ()
main = do
  runMenu menu "Contacts app"
