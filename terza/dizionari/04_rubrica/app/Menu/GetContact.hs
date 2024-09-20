module Menu.GetContact where

import Menu
import Menu.Shared

menuGetContact :: MenuAction AppState
menuGetContact s = do
  maybeContact <- getContactFromMenu s

  let result = handleContact maybeContact
  return (s, result)

  where
    handleContact Nothing = Error "Contact not found!"
    handleContact (Just contact) = Ok $ "Contact found:\n" ++ show contact
