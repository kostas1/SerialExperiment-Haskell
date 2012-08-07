import Serial
import Bezier
import Geometry
import Data.Word
import Data.List.Split
import Control.Concurrent
import Control.Monad

main :: IO ()
main = 
    -- this is for use with user-input and sending a single coordinate
    if True then
        withDefaultSerial $ \s -> do
            _ <- forkIO $ forever $ whenReadingSerial s print
            forever $ getUserInput >>= \(x, y) ->
                let angles = moveTo robot (Point2D x y)
                    d = map toWord8 angles
                in writeSerial s d >> print d
    -- this is for use with drawing a bezier curve with a robot hand
    else
        withDefaultSerial $ \s -> do
            _ <- forkIO $ forever $ whenReadingSerial s print
            threadDelay 2000000
            let a = map (\[x, y] -> Point2D x y) $ bezierCurve [[5, 5], [-5, 5]] 10
            _ <- writeDelayedSerial s 200000 $ map (map toWord8 . moveTo robot) a
            print "done"

-- this is purely for debugging purposes. I can execute this function from interpreter
-- just to see how it works
execCurve :: [[Float]] -> Float -> Int -> IO ()
execCurve ps n ms = 
    let points = map (\[x, y] -> Point2D x y) $ bezierCurve ps n
    in withDefaultSerial $ \s -> do
        threadDelay 2000000
        _ <- writeDelayedSerial s ms $ map (map toWord8 . moveTo robot) points
        print "done"

toWord8 :: RealFrac a => a -> Word8
toWord8 n = fromIntegral (round n :: Word8)

getUserInput :: IO (Float, Float)
getUserInput = do
    print "Give me two numbers separated by comma:"
    a <- getLine
    let nums = map (\b -> reads b :: [(Float, String)]) $ splitOn "," a
    if [] `elem` nums
        then
            print "Give me numbers!" >> getUserInput
        else
            case length nums of
                2 -> let extract e = fst . head $ head e
                     in  return (extract nums, extract $ tail nums)
                _ -> do
                        print "I asked for two numbers!"
                        getUserInput

robot :: Robot
robot = Robot   (Part (Segment 9) (Joint 1 170))
                (Part (Segment 7.5) (Joint 1 170))