module Item where
import qualified Data.Map as M


type ItemName = String

data Item = 
        Item{iname :: ItemName
            ,weight :: Integer}
    deriving (Show,Eq)

type Universe = M.Map String Item 

-- helper function of making a universe
createAlist :: [Item] -> [(ItemName,Item)]
createAlist [] = []
createAlist (x : xs) = (iname x, x) : createAlist xs

-- create universe containing all the items
mkUniverse :: [Item] -> Universe
mkUniverse asList = M.fromList (createAlist asList)