module Main where

import MyLib
import GameData
import System.Random (randomRIO)
import Data.Char (toUpper)



main :: IO ()
main = do
   putStrLn myLogo
   putStrLn "\n(TYPE \"QUIT\" TO EXIT AND \"?\" FOR HELP).\n"
   n <- randomRIO (0, length secretList - 1)
   playLingo $ initLS (secretList !! n)

playLingo :: LingoState -> IO ()
playLingo ls
   |stopGame ls = putStrLn "Bye!"
   |correctAnswer ls = putStrLn "Hooray :) You got it!"
   |endGame ls = putStrLn $ "Out of guesses :( The word was " <> show (toUpper `fmap` fst (getSecret ls))
   |firstGuess ls = askInput ls
   |showHelp ls = do
      putStrLn "Use the letter clue to guess the word. \nA capital letter is a letter in the right place. \nA lowercase letter is a letter that has an occurence in the word but is not in the correct place.\n"
      askInput ls
   |otherwise = do
      putStrLn "\nNope :( Try again.\n"
      askInput ls

askInput :: LingoState -> IO () -- askInput :: (MonadState LingoState m, MonadIO m) => m ()
askInput ls = do
   putStrLn $ "Complete the word using the letter clue! The category is: " <> show (toUpper `fmap` snd (getSecret ls)) <> (if firstGuess ls then "" else "\nPrevious guesses: " <> show (getGuessList ls) <> "\n") <> "\nYou have " <> show (getGuessesLeft ls) <> " guesses left.\n"
   putStrLn $ addSpaces (getGuessGrid ls)
   g <- getLine
   if gameCommand g then putStr "" else putStrLn $ addSpaces (compareUserGuess ls g)

   playLingo $ updateState ls g