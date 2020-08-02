module Initialize where
import qualified Data.Map as M
import Room
import Item
import Direction
import Player
import GameState


pot = Item "pot" 8
jug = Item "jug" 7
sandbag = Item "sandbag" 30
stove = Item "stove" 60
couch = Item "couch" 90
tarragon = Item "tarragon" 5
beans = Item "beans" 6
grill = Item "girll" 55
bed = Item "bed" 100
broadsword = Item "broadsword" 25
submachineGun = Item "submachineGun" 50
bomb = Item "bomb" 66
treasure = Item "treasure" 75
sofa = Item "sofa" 59
dragon = Item "dragon" 100

kitchen = Room "kitchen" "This is the kitchen."  
    [(N,"livingRoom"),(E,"pantry"),(S,"backyard")] ["pot","stove"]
pantry = Room "pantry" "This is the pantry." 
    [(W,"kitchen"),(E,"castle")] ["tarragon","beans","bomb"]
backyard = Room "backyard" "This is the backyard of your house." 
    [(N,"kitchen"),(S,"treasureRoom")] ["grill","submachineGun"]
livingRoom = Room "livingRoom" "This is the livingRoom." 
    [(N,"bedroom"),(S,"kitchen")] ["couch","jug","sandbag"]
bedroom = Room "bedroom" "This is the bedroom." 
    [(S,"livingRoom")] ["bed","broadsword"]
treasureRoom = Room "treasureRoom" "This room contains the great treasure saved by gradon." 
    [(N,"backyard")] ["treasure","dragon"]
castle = Room "castle" "This is the castle where princess sleeps." 
    [(W,"pantry")] ["couch","sofa"]

items = [stove,pot,couch,sandbag,jug,grill,bed,tarragon,beans,
          broadsword,submachineGun,bomb,treasure,sofa,dragon]
univ = mkUniverse items

itemNames :: [ItemName]
itemNames = M.keys univ

roomNames :: [RoomName]
roomNames = map rname [kitchen, pantry, backyard, livingRoom, bedroom,treasureRoom,castle]

-- make game map
gameMap :: GameMap
gameMap = mkMap [kitchen,pantry,backyard,livingRoom,bedroom,treasureRoom,castle]

-- calculate max weight you can carry
heaviestWeight :: Integer
heaviestWeight = maximum . map weight $ M.elems univ
 
-- initial player
you :: Player
you = Player
      []
      heaviestWeight
      "livingRoom"

-- initial gamestate
initialState :: GameState
initialState
  = GameState
    Nothing
    gameMap
    univ
    you

