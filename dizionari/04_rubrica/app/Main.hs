module Main where

import ContactsLib (ContactList, defaultContacts)
import Menu

type AppState = ContactList

state :: AppState
state = defaultContacts

options :: [MenuOption AppState]
options =
  [ MenuOption
      { _description = "Get all the contacts",
        _action = \s -> return (s, Ok $ show s)
      }
  ]

menu :: MenuChooser AppState
menu = makeMenu "Gestore contatti" state options

main :: IO ()
main = do
  runMenu menu
