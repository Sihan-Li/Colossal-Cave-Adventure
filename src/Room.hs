module Room where
import Item
import Direction


type RoomName = String
type Exit = (Direction,RoomName)

data Room =
    Room{
        rname :: RoomName
        ,desc :: String
        ,exits :: [Exit]
        ,objects :: [ItemName]
    }
    deriving(Show,Eq)

-- add an item to the room
addItem :: ItemName -> Room -> Room
addItem iname room = 
    Room{rname = rname room, desc = desc room, exits = exits room, 
        objects = (objects room) ++ [iname]}

-- remove an item from the room
removeItem :: ItemName -> Room -> Room
removeItem iname room = 
    Room{rname = rname room, desc = desc room, exits = exits room, 
        objects = filter (\x -> x /= iname) (objects room)}

-- check whether a room has items inside
hasObjects :: Room -> Bool
hasObjects rm = 
    case length $ objects rm of
        0 -> False
        _ -> True
        
