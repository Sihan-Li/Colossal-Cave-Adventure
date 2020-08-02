module Example where
import Data.List
import System.Random
import qualified Data.Map as M
import Item
import Direction
import Room
import Player
import Initialize
import GameState

class Example a where
    example :: IO a

instance Example Item where
    example = do
        iname <- choose ["pot","jug","sandbag","stove","couch","tarragon",
                        "beans","grill","bed","bomb","submachineGun","broadsword"]
        weight <- randomRIO(0,100)
        let rs = (Item iname weight)
        return rs

instance Example Direction where
    example = do
        rs <- choose [N,W,E,S]
        return rs
--example of exit returns a IO exit
exitExample :: IO Exit
exitExample = do
  loca <- choose [N,S,W,E]
  rm <- choose roomNames
  return (loca,rm)

instance Example Room where
  example = do
    rname <- choose roomNames
    let desc = "You are in the " ++ rname ++ ". It is randomly-generated room."
    let ex = exitExample
    let range = randomRIO (2,5)
    exit <- exampleList ex range
    obj <- exampleList (choose itemNames) range
    let rs = Room rname desc exit obj
    return rs 

instance Example Player where
  example = do
    let ioItem = choose itemNames
    let carryRange = randomRIO (0,10)
    ivt <- exampleList ioItem carryRange
    wt <- randomRIO (95,105)
    loca <- choose roomNames
    let rs = Player (nub ivt) wt loca
    return rs

--helper function for gamestate instance
chooseMessage :: Integer -> Maybe String
chooseMessage 0 = Nothing
chooseMessage 1 = Just "one possible message"
chooseMessage 2 = Just "Yet another possible message"

instance Example GameState where
  example = do
    messageRange <- randomRIO(0,2)
    let m = chooseMessage messageRange
    let mapRoomRange = randomRIO(2,3)
    let ioRoom = example :: IO Room
    exRoom <- exampleList ioRoom mapRoomRange
    let gmap = mkMap exRoom
    let universeItemRange = randomRIO(5,10)
    let i = example :: IO Item
    ioItem <- exampleList i universeItemRange
    let uni = mkUniverse ioItem 
    instancePlayer <- example :: IO Player
    let rs = GameState m gmap uni instancePlayer
    return rs

--random choose from a list
choose :: [a] -> IO a
choose xs = do
  let len = length xs
  idx <- randomRIO (0,len-1)
  return $ xs !! idx

--change style from IO a to a IO [a] list
exampleList :: IO a -> IO Int -> IO [a]
exampleList a i = do
  cnt <- i
  let rs = Data.List.replicate cnt a
  sequence rs
  