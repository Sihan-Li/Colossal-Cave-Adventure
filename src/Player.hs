module Player where
import Item
import Room

data Player 
    = Player{
        inventory :: [ItemName]
        ,maxWeight :: Integer
        ,location :: RoomName
    }
    deriving(Show,Eq)

-- pick an item into the inventory
addItem :: ItemName -> Player -> Player
addItem name p = p {inventory = name : inventory p}

-- drop an otem from the inventory
removeItem :: ItemName -> Player -> Player
removeItem name p = p {inventory = filter (\x -> x/= name) (inventory p)}

-- change location of player 
newLocation :: RoomName -> Player -> Player
newLocation locationName p = p {location = locationName}

-- check whether the inventory is empty
isCarryingAnything :: Player -> Bool
isCarryingAnything p =
    case length (inventory p) of
        0 -> False
        _ -> True


