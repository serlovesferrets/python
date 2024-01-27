{-# LANGUAGE GADTs #-}

module Menu where

import Data.Maybe (fromJust)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

newtype State a = State a

data Option a where
  Option ::
    { _description :: String,
      _action :: State a -> (IO String, State a)
    } ->
    Option a

newtype Menu a = Menu [Option a]

-- TODO rewrite using the Writer monad
handleIndex :: Maybe Int -> [Option a] -> State a -> Either String (IO String, State a)
handleIndex Nothing _ _ = Left "That's not an index!"
handleIndex (Just i) menu (State state)
  | i < 1 = Left "Index too small!"
  | i > length menu = Left "Index too big!"
  | otherwise =
      let option = menu !! (i - 1)
          (result, newState) =
            case option of
              Option {_action = action} -> action (State state)
       in Right (result, newState)

printPrompt :: String -> IO ()
printPrompt prompt = do
  putStr prompt
  hFlush stdout

runMenu :: Menu a -> State a -> String -> IO ()
runMenu (Menu menu) state title =
  let menuWithExit =
        Menu . reverse $
          Option
            { _description = "Exit",
              _action = const (return "", state)
            }
            : reverse menu
   in runMenu' menuWithExit state title

runMenu' :: Menu a -> State a -> String -> IO ()
runMenu' (Menu menu) (State state) title =
  let lastIndex = length menu

      indexes = [(1 :: Int) .. lastIndex]
      entries = zip indexes menu

      folder acc opt = acc ++ format opt
      format (i, opt) = "\n" ++ show i ++ ". " ++ _description opt

      entriesAsStrs = foldl folder title entries
   in do
        putStrLn entriesAsStrs
        printPrompt "> "

        choice <- readMaybe <$> getLine :: IO (Maybe Int)

        case handleIndex choice menu (State state) of
          Left err -> do
            putStr $ "---\n" ++ "ERROR: " ++ err ++ "\n---"
            runMenu' (Menu menu) (State state) ""
          Right _ | fromJust choice == lastIndex -> exitSuccess
          Right (res, newState) -> do
            resStr <- res
            putStr $ "---\n" ++ resStr ++ "\n---"
            runMenu' (Menu menu) newState ""
