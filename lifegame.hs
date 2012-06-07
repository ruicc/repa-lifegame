module Main where
import Data.Array.Repa as R hiding (map)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.Posix.Unistd (usleep)
import Data.Time (getCurrentTime)

type Board t = R.Array t DIM2 Int
type BoardSize = (Int, Int)
type BoardIx = (Int, Int)
type CellStatus = Int

boardSize :: BoardSize
--boardSize = (30,80)
boardSize = (47,193)
--boardSize = (1000,1000)

initLifeGame :: BoardSize -> IO (Board U)
initLifeGame (x,y) = do
    ls <- replicateM (x*y) $ randomRIO (0,1)
    return $ R.fromListUnboxed (R.ix2 x y) ls

step :: BoardSize -> Board U -> Board D
step siz board = R.traverse board id nextStatus
    where
    -- check around cells, count living cells, and return next own status
    nextStatus g curIx@(Z :. x :. y) = 
        lifeCheck (g curIx) $ sum $ map indexToStatus (getAround siz (x, y))
        where
            indexToStatus (x', y') =  g $ R.ix2 x' y'

    getAround :: BoardSize -> BoardIx -> [BoardIx]
    getAround (maxx, maxy) (ixx, ixy) = filter valid nineCells
        where
            nineCells = do
                x <- [ixx-1 .. ixx+1]
                y <- [ixy-1 .. ixy+1]
                return (x,y)
            valid (x, y)
                | x < 0 || y < 0         = False
                | maxx <= x || maxy <= y = False
                | ixx == x && ixy == y   = False
                | otherwise              = True

    lifeCheck :: CellStatus -> Int -> CellStatus
    lifeCheck a 2 = a
    lifeCheck _ 3 = 1
    lifeCheck _ _ = 0

repeatStep :: BoardSize -> Board U -> IO ()
repeatStep siz board = do
    board' <- R.computeP $ step siz board  
--    usleep 10000
    usleep 120000
    putStr "\x1b[2J"
    putStr "\x1b[1;1H"
    printListToRect siz $ R.toList board'
    repeatStep siz board'

    where
        printListToRect :: BoardSize -> [Int] -> IO ()
--        printListToRect _ _ = print =<< getCurrentTime
        printListToRect (x,y) ls
            | length ls < y = return ()
            | otherwise = do
                mapM_ (putStr.show) $ take y ls
                putChar '\n'
                printListToRect (x,y) $ drop y ls

main :: IO ()
main = do
    board <- initLifeGame boardSize
    repeatStep boardSize board
