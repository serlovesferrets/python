module Menu where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | The "aftermath" of executing an action.
-- Each action returns a string or signal that the program should terminate.
data MenuResult
  = Ok String
  | Error String
  | Termination

-- | What should be done after an action is executed.
-- The generic `a` is the state that gets updated.
data ActionResult a = Continue a | Terminate

-- | The signature of a function for a menu's action.
type MenuAction a = a -> IO (a, MenuResult)

-- | The option for a menu.
-- The description is what gets printed in the chooser's menu.
-- The action is what interacts with the user and updates the state.
data MenuOption a = MenuOption
  { _description :: String,
    _action :: MenuAction a
  }

-- | The menu itself.
-- Title gets printed once, when the menu is ran.
-- State is the state of the menu.
-- Options is a list of options that get printed automatically.
data MenuChooser a = Menu
  { _title :: String,
    _state :: a,
    _options :: [MenuOption a]
  }

-- | An option that's added automatically to the menu's options list.
-- Returns a termination request, signaling that the program should stop.
exitOption :: MenuOption a
exitOption =
  MenuOption
    { _description = "Exit",
      _action = \s -> return (s, Termination)
    }

-- | Creates a menu, adding an exit option automatically.
makeMenu :: String -> a -> [MenuOption a] -> MenuChooser a
makeMenu title state options =
  Menu
    { _title = title,
      _state = state,
      _options = reverse $ (exitOption :) $ reverse options
    }

-- | Figures out how to print the available options to the console.
showOpts :: [MenuOption a] -> String
showOpts opts =
  let optsWithIndexes = zip [1 .. length opts] opts
      folder acc (i, opt) = acc ++ "\n" ++ show i ++ ". " ++ _description opt
      stringed = foldl folder "" optsWithIndexes
   in stringed

-- | Gets a string from the user after printing a prompt.
prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

-- | Keeps asking a number from the user until it gets one.
-- The length of the options is used to determine whether the number
-- goes above the number of options, ex.: if the options are 6, 8 is
-- not a valid option.
getChoice :: Int -> IO Int
getChoice optsLen = do
  putStrLn "Choose an option:"
  choiceMaybe <- readMaybe <$> prompt "> " :: IO (Maybe Int)

  case choiceMaybe of
    Nothing -> do
      putStrLn "That's not a number!"
      getChoice optsLen
    Just choice | choice < 1 -> do
      putStrLn "That number is too small!"
      getChoice optsLen
    Just choice | choice > optsLen -> do
      putStrLn "That number is too big!"
      getChoice optsLen
    Just choice -> return choice

-- | Given a menu and a choice, it executes said choice and
-- returns the result of the action, alongside the updated state.
executeAction :: MenuChooser a -> Int -> IO (ActionResult (MenuChooser a))
executeAction
  menu@Menu
    { _title = title,
      _state = state,
      _options = opts
    }
  choice =
    let action = _action $ opts !! (choice - 1)
     in do
          (newState, res) <- action state
          case res of
            Error err -> do
              putStrLn $ "ERROR: " ++ err
              return $ Continue menu
            Ok ok -> do
              putStrLn ok
              return . Continue $
                Menu
                  { _title = title,
                    _options = opts,
                    _state = newState
                  }
            Termination -> do
              putStrLn "Goodbye!"
              return Terminate

-- | Prints a menu's title, and starts executing it.
runMenu :: MenuChooser a -> IO ()
runMenu menu@Menu {_title = title} = do
  putStr $ "# " ++ title
  runMenu' menu

-- | Executes the menu until it gets told to termite the program.
runMenu' :: MenuChooser a -> IO ()
runMenu' menu@Menu {_options = opts} = do
  putStrLn $ showOpts opts
  choice <- getChoice $ length opts
  res <- executeAction menu choice

  case res of
    Terminate -> return ()
    Continue newMenu ->
      runMenu' newMenu
