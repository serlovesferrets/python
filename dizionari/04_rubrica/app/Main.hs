module Main where

import ContactsLib (
    Contact (Contact, _address, _email, _name, _phoneNumber),
    ContactList (..),
 )

contacts :: ContactList
contacts =
    ContactList
        [ Contact
            { _name = "Giuseppe Gullo"
            , _phoneNumber = "3393527822"
            , _address = Just "Indirizzo 1"
            , _email = Just "ggullo@mail.it"
            }
        , Contact
            { _name = "Antonio Barbera"
            , _phoneNumber = "3393228442"
            , _address = Just "Indirizzo 2"
            , _email = Just "abarbera@mail.it"
            }
        , Contact
            { _name = "Nicola Spina"
            , _phoneNumber = "3383522812"
            , _address = Just "Indirizzo 3"
            , _email = Just "nspina@mail.it"
            }
        ]

main :: IO ()
main = do
    print contacts
