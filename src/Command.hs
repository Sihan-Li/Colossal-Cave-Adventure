module Command where
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)
import Item
import Direction

type Parser = ReadP
type Conjunction = [Command]

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

--core function to run a parser
runParser :: ReadP a -> String -> [(a, String)]
runParser = readP_to_S

-- remove the whitespace
whitespacify :: Parser a -> Parser a
whitespacify prsr = skipSpaces *> prsr <* skipSpaces

-- command inventory
inventoryP :: ReadP Command
inventoryP = pure (\_ -> Inventory)
             <* skipSpaces
             <*> string "inventory"
             <* skipSpaces

-- command take
takeP :: ReadP Command
takeP = pure Take
        <* skipSpaces
        <* string "take "
        <*> nounPhrase
        <* skipSpaces
        
-- command exit
exitP :: ReadP Command
exitP = pure (\_ -> Exit)
        <* skipSpaces
        <*> string "exit"
        <* skipSpaces
        <|>
        pure (\_ -> Exit)
        <* skipSpaces
        <*> string "quit"
        <* skipSpaces

--  check if a string is the next input word
haveString :: String -> ReadP ()
haveString s = do
  r <- look
  if r == ""
  then pfail
  else if words r !! 0 == s
    then pure ()
  else pfail

-- command drop
dropP :: ReadP Command
dropP = pure Drop
        <* skipSpaces
        <* string "drop "
        <*> nounPhrase
        <* skipSpaces

-- command look
lookP :: ReadP Command
lookP = pure (\_ -> Look)
        <* skipSpaces
        <*> string "look"
        <* skipSpaces

-- command direction
directionP :: ReadP Direction
directionP = pure (\_ -> N)
            <* skipSpaces
            <*> string "north"
            <* skipSpaces
            <|>
            pure (\_ -> W)
            <* skipSpaces
            <*> string "west"
            <* skipSpaces
            <|>
            pure (\_ -> S)
            <* skipSpaces
            <*> string "south"
            <* skipSpaces
            <|>
            pure (\_ -> E)
            <* skipSpaces
            <*> string "east"
            <* skipSpaces

-- command move
moveP :: ReadP Command
moveP = pure Move
        <* skipSpaces
        <*> directionP
        <* skipSpaces

-- integrated command function
commandP :: ReadP Command
commandP = inventoryP <|> takeP <|> dropP <|> exitP <|> lookP <|> moveP

--helper function for conjunctionP
skipAnd :: ReadP ()
skipAnd = pure ()
      <* skipSpaces
      <* string "and"
      <* skipSpaces

-- parse a conjunction of commands
conjunctionP :: ReadP Conjunction
conjunctionP =  (pure (:[])
                  <*> commandP
                  <|>
                  pure (:) 
                  <*> commandP
                  <* skipAnd
                  <*> conjunctionP
                  )<* eof

--parse string to commands
parse :: String -> Maybe Conjunction
parse str =
  case runParser conjunctionP str of
    [(x,"")] -> Just x
    [] -> Nothing
  
--helper function of nounphrase
noun :: ReadP ItemName
noun = whitespacify $ munch1 isAlpha

--helper function of nounphrase
skipComma :: ReadP ()
skipComma = pure()
         <* skipSpaces
         <* string ","
         <* skipSpaces

--parse a comma-separated list of nouns
nounPhrase :: ReadP [ItemName]
nounPhrase = (
          pure (:[])
          <*> noun
          <|>
          pure (:)
          <*> noun
          <* skipComma
          <*> nounPhrase
          )<* eof

