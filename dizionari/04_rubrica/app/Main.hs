module Main where

import ContactsLib (ContactList, defaultContacts)
import Menu

type AppState = ContactList

state :: AppState
state = defaultContacts

menuGetAllContacts :: MenuAction AppState
menuGetAllContacts s = return (s, Ok $ show s)

options :: [MenuOption AppState]
options =
  [ MenuOption
      { _description = "Get all the contacts",
        _action = menuGetAllContacts
      }
  ]

menu :: MenuChooser AppState
menu = makeMenu "Gestore contatti" state options

main :: IO ()
main = do
  runMenu menu
