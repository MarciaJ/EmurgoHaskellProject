module MyLib where

import GameData
import Data.Char (toUpper, toLower, isUpper, isSpace)


type ToBeGuessed = String

data LingoState = LS
    {getSecret :: (String, String)
    ,getGuessList :: [String]
    ,getUserGuess :: String
    ,getGuessesLeft :: Int
    ,getGuessGrid :: String
    ,getGuessTemplate :: String
    } deriving  Show


initLS :: (String, String) -> LingoState
initLS s = LS s [] "" 5 (initTemplate $ fst s) (initTemplate $ fst s)

correctAnswer :: LingoState -> Bool
correctAnswer ls = fst (getSecret ls) == getUserGuess ls

endGame :: LingoState -> Bool
endGame ls = (getGuessesLeft ls == 0) && not (correctAnswer ls)

stopGame :: LingoState -> Bool
stopGame ls = toLower `fmap` getUserGuess ls == "quit"

showHelp :: LingoState -> Bool
showHelp ls = getUserGuess ls == "?"

firstGuess :: LingoState -> Bool
--firstGuess ls = getGuessList ls == []
firstGuess ls = null (getGuessList ls)

guessIsWrong :: LingoState -> Bool
guessIsWrong ls
    | getUserGuess ls == "" = False
    | not (correctAnswer ls) = True
    | otherwise = False

initTemplate :: String -> String
initTemplate [] = []
initTemplate (x:xs) = toUpper x : map (const '.') xs

isInWord :: Eq a => a -> [a] -> Bool
isInWord n [] = False
isInWord n (x:xs)
    | n == x = True
    | otherwise = isInWord n xs

compareUserGuess :: LingoState -> String -> String
--compareUserGuess ls g = compareWords (getSecret ls) g (got ETA reduced) kind of weird?
compareUserGuess ls = compareWords ( fst $ getSecret ls)
  where
    compareWords _ [] = []
    compareWords [] _ = []
    compareWords (x:xs) (y:ys)
         | toLower x == y = toUpper y : compareWords xs ys
         | toLower x /= y && isInWord y (x:xs) = toLower y : compareWords xs ys
         | otherwise = '.': compareWords xs ys

adjustCompareLength :: LingoState -> String -> String
adjustCompareLength ls g =
  if templateLength > compareLength
--  then compareUserGuess ls g ++ take (templateLength - compareLength) (cycle ".")
  then compareUserGuess ls g ++ replicate (templateLength - compareLength) '.'
  else compareUserGuess ls g
 where
   templateLength = length $ getGuessTemplate ls
   compareLength = length $ compareUserGuess ls g


constructGuessTemplate :: LingoState -> String -> String
constructGuessTemplate ls g = construct (getGuessTemplate ls) $ adjustCompareLength ls g
  where
    construct [] _ = []
    construct _ [] = []
    construct (x:xs) (y:ys)
      | x /= '.' = toUpper x : construct xs ys
      | x == '.' && isUpper y = toUpper y : construct xs ys
      | otherwise = '.' : construct xs ys

gameCommand :: String -> Bool
gameCommand g
      | g == "?" = True
      | toLower `fmap` g == "quit" = True
      | otherwise = False

updateState :: LingoState -> String -> LingoState
updateState ls g = ls {getGuessesLeft = if gameCommand g then getGuessesLeft ls else getGuessesLeft ls - 1
                   , getUserGuess = g
                   , getGuessList = if gameCommand g then getGuessList ls else getGuessList ls ++ [g]
                   , getGuessTemplate = constructGuessTemplate ls g
                   , getGuessGrid = getGuessGrid ls ++ "\n" ++ addition ls g
                   }
-- updateState :: MonadState LingoState m => String -> m ()
-- updateState g = modify (defined helper function LingoState -> LingoState)


addition :: LingoState -> String -> String
addition ls g =
   if constructGuessTemplate ls g == compareUserGuess ls g
   then constructGuessTemplate ls g
   else compareUserGuess ls g ++ "\n" ++ constructGuessTemplate ls g

addSpaces :: String -> String
addSpaces [] = []
addSpaces (x:xs)
      |isSpace x = x : addSpaces xs
      |otherwise = x : ' ' : addSpaces xs






