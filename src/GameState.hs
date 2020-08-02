module GameState where
import Control.Exception
import qualified Data.Map as M
import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room
type Error a = Either String a

data GameState = 
    GameState{
        message :: Maybe String
        ,gmap :: GameMap
        ,universe :: Universe
        ,player :: Player
    }
    deriving Show

-- helper function for making ap
createRoomlist :: [Room] -> [(RoomName,Room)]
createRoomlist [] = []
createRoomlist (x : xs) = (rname x, x) : createRoomlist xs

-- make the map
mkMap :: [Room] -> GameMap
mkMap asList = M.fromList (createRoomlist asList)


data KeyError = KeyError
  deriving Show

instance Exception KeyError

-- find an item arrcording to name from universe
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

-- get obj from the game map
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- get map
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

-- get current room
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- update map
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap oldname newRoom mp = 
    M.insert oldname newRoom $ M.delete oldname mp

-- update message from
setMessage :: String -> GameState -> GameState
setMessage ms st =  case ms of
                        "" -> st {message = Nothing}
                        _ -> st {message = Just ms}

-- see the inventory
currentInventory :: GameState -> [ItemName]
currentInventory st = inventory $ player st

-- see which roo you are in
currentRoom :: GameState -> Room
currentRoom st = getRoom (location $ player st) st 

-- see the objects in the room
nearbyObjects :: GameState -> [ItemName]
nearbyObjects st = objects $ currentRoom st

-- take an item from current room
takeItem :: ItemName -> GameState -> GameState
takeItem it st = 
   case alreadyHaveTakeCheck it st of
       Left msg -> st {message = Just ("You are already carrying the " ++ it)}
       Right st ->
           case inRoomTakeCheck it st of
                Left msg -> st {message = Just ("There is no " ++ it ++ " in this room.")}
                Right st ->
                    case weightCheck it st of
                        Left msg -> st {message = Just "That's too much weight for you to carry."}
                        Right st ->
                            normalTake it st
    
-- take before check, helper function of takeItem
normalTake :: ItemName -> GameState ->GameState
normalTake it st =  
        st {message = Just ("You take the "++ it ++ "."),
        gmap = setRoomMap (rname $ currentRoom st) (Room.removeItem it $ currentRoom st) (gmap st),
        player = Player.addItem it (player st)}

-- drop an item to current room
dropItem :: ItemName -> GameState ->GameState
dropItem it st = 
    case anywhereDropCheck it st of
        Left msg -> st {message = Just ("What do you mean, drop the \"" ++ it ++ "\"?")}
        Right st ->
            case inRoomDropCheck it st of
                Left msg -> st {message = Just ("You aren't carrying the " ++ it)}
                Right st ->
                    normalDrop it st

-- drop before check, helper function of takeItem
normalDrop :: ItemName -> GameState -> GameState
normalDrop it st = 
    st {message = Just ("You drop the "++ it ++ "."),
        gmap = setRoomMap (rname $ currentRoom st) (Room.addItem it $ currentRoom st) (gmap st),
        player = Player.removeItem it (player st)}

-- weight of all carrying items
inventoryWeight :: GameState -> Integer
inventoryWeight st = foldr (+) 0 ((currentInventory st) >>= \i -> [weight $ getObject i st])

-- see if have already carrying something
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck n st = 
    if elem n (inventory $ player st)
    then Left ("You are already carrying the " ++ n)
    else Right st

-- check whether an item is in a room
inRoomTakeCheck :: ItemName -> GameState -> Error GameState 
inRoomTakeCheck n st = 
    if (elem n (nearbyObjects st)) == False
    then Left ("There is no " ++ n ++ " in this room.")
    else Right st

-- check whether you are capable to carry that much things
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck n st = 
    if (inventoryWeight st) + (weight $ getObject n st) > (maxWeight $ player st)
    then Left "That's too much weight for you to carry."
    else Right st

-- check whether you can drop anywhere
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck n st = 
    if elem n (inventory $ player $ st) || (elem n (nearbyObjects st))
    then Right st
    else Left ("What do you mean, drop the \"" ++ n ++ "\"?")

-- check whether you carry the item before you drop
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck n st = 
    if elem n (nearbyObjects st)
    then Left ("You aren't carrying the " ++ n)
    else Right st

-- check whether an item in th room
roomHasObjects :: GameState -> Bool
roomHasObjects st = hasObjects $ currentRoom st

-- find the room in given direction
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir rm = 
    if elem dir (map fst $ exits rm) == True
        then lookup dir (exits rm)
        else Nothing

-- move to a direction
move :: Direction -> GameState -> GameState
move dir st = 
    case destinationName dir (currentRoom st) of
        Just next -> 
            st {message = Just $concat ["You go ",show dir,"."], player = newLocation next (player st)}
        _ -> 
            st {message = Just "There is no exit in that direction."}

-- check if you have win the game
haveWonGame :: GameState -> Bool
haveWonGame st = 
    if (rname $ currentRoom st) == "castle" 
        then
            case elem "treasure" (currentInventory st) of
                True -> True
                False -> False
        else False

-- check if you are killed
isDead :: GameState -> Bool
isDead st  = 
    if (rname $ currentRoom st) == "treasureRoom"
        then
            case elem "broadsword" (currentInventory st) of
                True -> False
                False ->
                    case elem "bomb" (currentInventory st) of
                        True -> False
                        False -> 
                            case elem "submachineGun" (currentInventory st) of
                                True -> False
                                False -> True
        else False
