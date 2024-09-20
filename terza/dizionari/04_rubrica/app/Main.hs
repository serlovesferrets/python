module Main where

import ContactsLib (ContactList, defaultContacts)
import Menu (MenuChooser, MenuOption (..), makeMenu, runMenu)
import Menu.EditContact (menuEditContact)
import Menu.GetAllContacts (menuGetAllContacts)
import Menu.GetContact (menuGetContact)

type AppState = ContactList

state :: AppState
state = defaultContacts

options :: [MenuOption AppState]
options =
  [ MenuOption
      { _description = "Get all the contacts",
        _action = menuGetAllContacts
      },
    MenuOption
      { _description = "Get a certain contact",
        _action = menuGetContact
      },
    MenuOption
      { _description = "Edit a contact",
        _action = menuEditContact
      }
  ]

menu :: MenuChooser AppState
menu = makeMenu "Gestore contatti" state options

main :: IO ()
main = runMenu menu
