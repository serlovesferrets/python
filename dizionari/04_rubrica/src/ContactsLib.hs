{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module ContactsLib where

import Data.List (findIndex)

data Contact where
  Contact ::
    { _name :: String,
      _phoneNumber :: String,
      _address :: Maybe String,
      _email :: Maybe String
    } ->
    Contact
  deriving (Eq)

instance Show Contact where
  show :: Contact -> String
  show contact =
    let name = "Name: " ++ _name contact
        number = "\n- Number: " ++ _phoneNumber contact
        address = maybe "" ("\n- Address: " ++) (_address contact)
        email = maybe "" ("\n- Email: " ++) (_email contact)
     in name ++ number ++ address ++ email

newtype ValidContact = ValidContact Contact

type ContactValidator = (Contact -> Maybe ValidContact)

validateContact :: ContactValidator
validateContact contact@Contact {_phoneNumber = number, _email = email}
  | emailIsValid && numberIsValid = Just $ ValidContact contact
  | otherwise = Nothing
  where
    emailIsValid =
      case email of
        Nothing -> True
        Just e -> '@' `elem` e && length e >= 3
    numberIsValid = length number == 10

type ContactFinder = (Contact -> Bool)

class Rubric r where
  getContact :: ContactFinder -> r -> Maybe Contact
  addContact :: ValidContact -> r -> r
  editContact :: ContactFinder -> (Contact -> Contact) -> r -> Either String r
  removeContact :: ContactFinder -> r -> Either String r

newtype ContactList = ContactList [Contact]

instance Show ContactList where
  show :: ContactList -> String
  show (ContactList cs) = foldr folder "Contacts:" cs
    where
      folder c acc = acc ++ "\n" ++ show c

instance Rubric ContactList where
  getContact :: ContactFinder -> ContactList -> Maybe Contact
  getContact predicate (ContactList cs) =
    case filter predicate cs of
      (c : _) -> Just c
      [] -> Nothing

  addContact :: ValidContact -> ContactList -> ContactList
  addContact (ValidContact c) (ContactList cs) = ContactList (c : cs)

  editContact ::
    ContactFinder ->
    (Contact -> Contact) ->
    ContactList ->
    Either String ContactList
  editContact predicate update (ContactList cs) =
    case findIndex predicate cs of
      Nothing -> Left "Contact not found!"
      Just index ->
        let left = take index cs
            right = drop (index + 1) cs
            maybeUpdated = update (cs !! index)
         in case validateContact maybeUpdated of
              Nothing ->
                Left "Update function doesn't return a valid contact!"
              Just (ValidContact updated) ->
                Right . ContactList $ right ++ (updated : left)

  removeContact :: ContactFinder -> ContactList -> Either String ContactList
  removeContact predicate (ContactList cs) =
    case findIndex predicate cs of
      Nothing -> Left "Contact not found!"
      Just index ->
        let left = take index cs
            right = drop (index + 1) cs
         in Right . ContactList $ right ++ left

defaultContacts :: ContactList
defaultContacts =
  ContactList
    [ Contact
        { _name = "Giuseppe Gullo",
          _phoneNumber = "3393527822",
          _address = Just "Indirizzo 1",
          _email = Just "ggullo@mail.it"
        },
      Contact
        { _name = "Antonio Barbera",
          _phoneNumber = "3393228442",
          _address = Just "Indirizzo 2",
          _email = Just "abarbera@mail.it"
        },
      Contact
        { _name = "Nicola Spina",
          _phoneNumber = "3383522812",
          _address = Just "Indirizzo 3",
          _email = Just "nspina@mail.it"
        }
    ]
