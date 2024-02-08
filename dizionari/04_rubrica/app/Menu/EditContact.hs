module Menu.EditContact where

import ContactsLib (Contact (Contact, _address, _email, _name, _phoneNumber))
import Data.Maybe (fromJust, isJust)
import Menu
import Menu.Shared

updatePropertyOr :: String -> [Char] -> IO String
updatePropertyOr previous property = do
  putStrLn $ "[" ++ previous ++ "]"
  newProperty <-
    prompt $
      "Inserisci il nuovo valore per "
        ++ property
        ++ ", spazio non cambia nulla: "

  return $ if notSpace newProperty then newProperty else previous

updateMaybePropertyOr :: Maybe String -> [Char] -> IO (Maybe String)
updateMaybePropertyOr previous property = do
  putStr $ if isJust previous then "[" ++ fromJust previous ++ "]" else ""
  newProperty <-
    prompt $
      "Inserisci il nuovo valore per "
        ++ property
        ++ ", spazio non cambia nulla: "

  return $ if notSpace newProperty then Just newProperty else previous

menuEditContact :: MenuAction AppState
menuEditContact s = do
  maybeContact <- getContactFromMenu s

  case maybeContact of
    Nothing -> return (s, Error "Contact not found!")
    Just contact -> do
      name <- updatePropertyOr (_name contact) "il nome"
      number <- updatePropertyOr (_phoneNumber contact) "il numero"
      address <- updateMaybePropertyOr (_address contact) "l'indirizzo"
      email <- updateMaybePropertyOr (_email contact) "l'email"

      let updatedContact =
            Contact
              { _name = name,
                _phoneNumber = number,
                _address = address,
                _email = email
              }
          oldContactIndex = indexOfContact s (== contact)
          (updatedstate, result) = case oldContactIndex of
            Nothing -> (s, Error "")
       in undefined

  return (s, Termination)
  where
    handleContact Nothing = Error "Contact not found!"
    handleContact (Just contact) = Ok $ "Contact found:\n" ++ show contact
