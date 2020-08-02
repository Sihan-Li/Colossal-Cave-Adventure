module GameIO where
import Control.Monad.State
import System.Exit
import System.IO
import Direction
import GameState
import Player
import Room
import Command
import Item
import Initialize

type GameIO a = StateT GameState IO a

-- access :: GameIO a -> IO GameState
-- access = (flip execStateT) initialState

-- initialstate IO
eval :: GameIO a -> IO a
eval = (flip evalStateT) initialState

-- change style GameState to GameIO
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange f = do
    gm <- get
    put $ f gm
    pure ()

--  "->"" symbol in front of input
prompt :: GameIO()
prompt = liftIO (putStr "->" >> hFlush stdout)

-- print message
printMessage :: GameIO ()
printMessage = do
    gm <- get
    case message gm of
        Nothing -> pure ()
        Just x -> do
            liftIO (putStrLn $ x)
            put $ setMessage "" gm
            pure ()
        
-- print description of room
printDescription :: GameIO ()
printDescription = do
  gm <- get
  liftIO (putStrLn $ desc $ currentRoom gm)

-- helper function for printings
iter :: (a -> IO ()) -> [a] -> IO()
iter ioFunction [] = return ()
iter ioFunction (x : xs) = do
  ioFunction x
  iter ioFunction xs

-- print objects
printObjects :: GameIO ()
printObjects = do
    gm <- get
    if (objects $ currentRoom gm) == []
        then pure ()
    else do
        liftIO (putStrLn "You see the following objects:")
        liftIO (iter putStrLn $ objects $ currentRoom gm)

-- print exits
printExits :: GameIO ()
printExits = do
    gm <- get
    if (exits $ currentRoom gm) == []
        then pure ()
    else do
        liftIO (putStrLn "There are exits in the following directions:")
        liftIO (iter putStrLn $ map show $ map fst $ exits $ currentRoom gm)

-- print inventory
printInventory :: GameIO ()
printInventory = do
    gm <- get
    if (inventory $ player gm) == []
        then liftIO (putStrLn "You aren't carrying anything.")
    else do
        liftIO (putStrLn "You see the following objects:")
        liftIO (iter putStrLn $ inventory $ player gm)

-- do operations to a list
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList func ls = 
    case ls of 
        [] -> pure ()
        (hd : tl) -> do
            effectChange (func hd) >> printMessage
            actionOverList func tl
    
-- exit if success
finishGame :: GameIO ()
finishGame = do
    liftIO(putStrLn"You successfully gave the treasure to the princess.\nCongrats! You win!")
    liftIO(exitWith ExitSuccess)

-- exit at any time
exit :: IO ()
exit = do
    putStrLn "Goodbye!"
    exitWith ExitSuccess

-- check whether success and exit
checkGameOver :: GameIO ()
checkGameOver = do
    gm <- get
    case haveWonGame gm of
        True -> finishGame
        False -> pure ()

-- exit if being killed
failAndExit :: GameIO ()
failAndExit = do
    liftIO(putStrLn "You are killed by the dragon and the game is over.")
    liftIO(exitWith ExitSuccess)

-- check whether is dead and exit
checkDeath :: GameIO ()
checkDeath = do
    gm <- get
    case isDead gm of
        True -> failAndExit
        False -> pure ()

-- report syntax error for commands
syntaxError :: GameIO()
syntaxError = do
    liftIO (putStrLn "I don't understand that.")

-- perform a command
performCommand :: Command -> GameIO()
performCommand c = do
    case c of
        Look -> do
            printDescription
            printObjects
            printExits
        Inventory -> printInventory
        Drop x -> actionOverList dropItem x >> printMessage
        Take x -> actionOverList takeItem x >> printMessage
        Move x -> effectChange (move x)>> printMessage
        Exit -> liftIO (exit)

-- perfrom a conjunction of commands
performConjunction :: Conjunction -> GameIO ()
performConjunction conj = do
    case conj of 
        [] -> pure ()
        (hd:tl) ->
            performCommand hd
            >> performConjunction tl

-- parse a conjunction of commands
parseConjunction :: String -> GameIO ()
parseConjunction str = do
    case parse str of
        Nothing -> syntaxError
        Just x  -> performConjunction $ x

-- core part of the program, the repl
repl :: GameIO()
repl = do
    prompt
    c <- liftIO getLine
    parseConjunction c
    >>checkGameOver
    >>checkDeath