module Menu.GetAllContacts where

import Menu
import Menu.Shared

menuGetAllContacts :: MenuAction AppState
menuGetAllContacts s = return (s, Ok $ show s)
