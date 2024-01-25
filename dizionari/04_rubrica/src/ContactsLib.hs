{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module ContactsLib where

import Data.List (findIndex)

safeLength :: [a] -> Int -> Int
safeLength _ acc | acc == (maxBound :: Int) = maxBound :: Int
safeLength [] _ = 0
safeLength [_] acc = acc + 1
safeLength (_ : xs) acc = safeLength xs (acc + 1)

safeLength' :: [a] -> Int
safeLength' xs = safeLength xs 0

data Contact where
    Contact ::
        { _name :: String
        , _phoneNumber :: String
        , _address :: Maybe String
        , _email :: Maybe String
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
validateContact contact@Contact{_phoneNumber = number, _email = email}
    | emailIsValid && numberIsValid = Just $ ValidContact contact
    | otherwise = Nothing
  where
    emailIsValid =
        case email of
            Nothing -> True
            Just e -> '@' `elem` e && safeLength' e >= 3
    numberIsValid = safeLength' number == 10

type ContactFinder = (Contact -> Bool)

class Rubric r where
    getContact :: ContactFinder -> r -> Maybe Contact
    addContact :: ValidContact -> r -> r
    editContact :: ContactFinder -> (Contact -> Contact) -> r -> Either r String
    removeContact :: ContactFinder -> r -> Either r String

newtype ContactList = ContactList [Contact]

instance Show ContactList where
    show (ContactList cs) = foldr folder "Contacts:" cs
      where
        folder c acc = acc ++ "\n" ++ show c

instance Rubric ContactList where
    getContact predicate (ContactList cs) =
        case filter predicate cs of
            (c : _) -> Just c
            [] -> Nothing

    addContact (ValidContact c) (ContactList cs) = ContactList (c : cs)

    editContact predicate update (ContactList cs) =
        case findIndex predicate cs of
            Nothing -> Right "Contact not found!"
            Just index ->
                let (left, _ : right) = splitAt index cs
                    maybeUpdated = update (cs !! index)
                 in case validateContact maybeUpdated of
                        Nothing ->
                            Right "Update function doesn't return a valid contact!"
                        Just (ValidContact updated) ->
                            Left . ContactList $ left ++ (updated : right)

    removeContact predicate (ContactList cs) =
        case findIndex predicate cs of
            Nothing -> Right "Contact not found!"
            Just index ->
                let (left, _ : right) = splitAt index cs
                 in Left . ContactList $ left ++ right
