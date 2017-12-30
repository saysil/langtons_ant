
import System.Posix.Unistd

import Data.Fixed
import Data.Complex
import Data.Tree.RBTree

import Control.Concurrent

import UI.HSCurses.Curses

import Debug.Trace

-- Ant Logic

type Direction = Complex Float

rotate :: Direction -> Float -> Direction
rotate dir a = ((fromIntegral.round.realPart) newRot) :+ ((fromIntegral.round.imagPart) newRot)
        where newRot = (cis ((phase dir)+a+(2*Prelude.pi)))


type Ant = (Int, Int, Direction)


rotateMove :: Ant -> Float -> Ant
rotateMove (x, y, dir0) dir = let ndir = rotate dir0 dir
        in (x + (round (realPart ndir)), y + (round (imagPart ndir)), ndir)




-- Board Logic

data NodeType = Clean | Weakened | Infected | Flagged deriving Show

type YTree = (RBTree (Int, NodeType))

type XTree = RBTree (Int, YTree)


compareXTree :: Int -> (Int, YTree) -> Ordering
compareXTree = (flip (compare.fst))

compareYTree :: Int -> (Int, NodeType) -> Ordering
compareYTree = (flip (compare.fst))

newYTree :: Int -> NodeType -> RBTree (Int, NodeType)
newYTree y node = insert (compareYTree.fst) (emptyRB) (y, node)

insertXY :: NodeType -> (Int, Int) -> XTree -> XTree
insertXY n (x, y) tree = case (searchFast (compareXTree) tree x) of 
        Nothing -> insert (compareXTree.fst) tree (x, newYTree y n)
        -- Insert new X value and new Y value
        Just (_, t) -> insert (compareXTree.fst) (delete (compareXTree.fst) tree (x, emptyRB)) (x, (insert (compareYTree.fst) t (y, n)))
        -- Delete current X value, then reinsert the tree with the new y value added

deleteXY :: NodeType -> (Int, Int) -> XTree -> XTree
deleteXY n (x, y) tree = case (searchFast (compareXTree) tree x) of
        Nothing -> error "Tree constructed unsuccessfully" 
        --This shouldnt happen!!!
        Just (_, t) -> insert (compareXTree.fst) (delete (compareXTree.fst) tree (x, emptyRB)) (x, (delete (compareYTree.fst) t (y, n)))
        -- Same methodology as above, we delete the x value then insert the y tree with y deleted


type Board = (XTree, Int)


isBlack :: (Int, Int) -> Board -> Bool
isBlack (x, y) board = case (searchFast (compareXTree) (fst board) x) of
        Nothing -> False
        Just (_, t) -> case (searchFast (compareYTree.fst) t (y, Clean)) of
                Nothing -> False
                Just _ -> True


getStatus :: (Int, Int) -> Board -> NodeType
getStatus (x, y) board = case (searchFast (compareXTree) (fst board) x) of
        Nothing -> Clean
        Just (_, t) -> case (searchFast (compareYTree.fst) t (y, Clean)) of
                Nothing -> Clean
                Just (_, a) -> a

moveAnt :: (Ant, Board) -> (Ant, Board)
moveAnt ((x, y, dir), (board, n)) = case ((getStatus (x, y) (board, n))) of
        Clean -> (rotateMove (x, y, dir) ((Prelude.pi)/2), ((insertXY Weakened (x, y) board), n))
        Weakened -> (rotateMove (x, y, dir) (2*Prelude.pi), ((insertXY Infected (x, y) (deleteXY Weakened (x, y) board)), n+1))
        Infected -> (rotateMove (x, y, dir) (-(Prelude.pi)/2), ((insertXY Flagged (x, y) (deleteXY Infected (x, y) board)), n))
        Flagged -> (rotateMove (x, y, dir) (Prelude.pi), ((deleteXY Flagged (x, y) board), n))

statusToChar :: (Int, Int) -> Board -> Char
statusToChar (x, y) board = case getStatus (x, y) board of
        Clean -> ' '
        Weakened -> 'X'
        Infected -> '#'
        Flagged -> '@'

-- GUI Logic

type BoardAttr = (Board, MVar (Int, Int, Int, Int))

getAntChar :: Direction -> Char
getAntChar dir
        | (r == 1) = '>'
        | (r == (-1)) = '<'
        | (i == 1) = '^'
        | (i == (-1)) = 'V'
        where (r, i) = (round (realPart dir), round (imagPart dir))


showAnt :: Ant -> (Window, BoardAttr) -> IO ()
showAnt (x, y, dir) (win, (_, params)) = do
        (x1, y1, w, h) <- readMVar params
        --putStrLn ((show (x-x1, (y-y1))) ++ (show [getAntChar dir]))
        if ((x > x1 && x < (x1 + w)) && ((y < y1 && y > (y1 - h))))
        then mvWAddStr win (abs (y-y1)) (x-x1) [getAntChar dir]
        else return ()

makeBGChar :: (Int, Int) -> Window -> Char -> IO ()
makeBGChar (x, y) win c = mvWAddStr win y x [c]

makeColor :: (Int, Int) -> Board -> Window -> (Int, Int) -> IO ()
makeColor (x1, y1) board win (x, y) = makeBGChar (x-x1, abs (y-y1)) win search
        where search = (statusToChar (x, y) board)


setBG :: (Window, BoardAttr) -> IO ()
setBG (win, (board, params)) = do
        (x, y, w, h) <- readMVar params
        mapM_ (makeColor (x, y) board win) ([ (a, b) | a<-[x..x+w-1], b<-[y-h..y+1]])

antloop :: Window -> MVar (Int, Int, Int, Int) -> (Ant, Board) -> IO () 
antloop win params (ant, board) = do
        --mvWAddStr win (10) (10) "#Hello World!!"
        setBG (win, (board, params))
        showAnt ant (win, (board, params))
        --makeColor (-10, -10) board win (10, 10)
        --putStrLn ("Ant: " ++ (show x) ++ " " ++ (show y) ++ " " ++ [getAntChar dir])
        refresh
        usleep 1000000
        wclear win
        antloop win params (moveAnt (ant, board))


{-antloop :: Int -> (Ant, Board) -> (Int, Ant, NodeType)
antloop 10000000 (ant, (b, n)) = (n, ant, getStatus (-1, 1) (b, n))
antloop n state = antloop (n+1) (moveAnt state)
-}

adjustBoard :: Int -> Int -> String -> Board -> Board
adjustBoard _ _ [] board = board
adjustBoard x y (s:xs) (board, n)
        | s == '#' = trace ("Thing: " ++ show x ++ ", " ++ show y ++ "\n") adjustBoard (x+1) y xs (insertXY Infected (x, y) board, n)
        | otherwise = adjustBoard (x+1) y xs (board, n)

adjustBoardL :: Int -> Int -> [String] -> Board -> Board
adjustBoardL _ _ [] board = board
adjustBoardL w y (x:xs) board = adjustBoardL w (y-1) xs (adjustBoard (((-w) `div` 2)+1) y x board)

parseTree :: String -> Board
parseTree s = adjustBoardL w (h `div` 2) (lines s) (emptyRB, 0)
        where (h, w) = (Prelude.length (lines s), (Prelude.length.head.lines) s)

--solve1 :: String -> (Int, Ant, NodeType)
--solve1 s = antloop 0 ((0, 0, 0 :+ 1), parseTree s)

--main = (putStrLn.show) ((solve1 "..#\n#..\n..."))
--main = ((readFile "day_22_input.txt") >>= (putStrLn.show.solve1))

main = do
        let ant = (0, 0, 1)
        let board = (emptyRB, 0)

        initCurses
        win <- initScr
        (h, w) <- scrSize

        wclear win
        mvWAddStr win 10 10 ("Size: " ++ (show (w, h)))
        refresh

        usleep 1000000

        params <- newMVar ((-40), 15, w-1, h-1)

        cursSet CursorInvisible

        antloop win params (ant, board)


