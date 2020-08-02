module Direction where


data Direction
    = N | E | W | S
    deriving Eq

instance Show Direction where
    show N = "north"
    show S = "south"
    show E = "east"
    show W = "west"

