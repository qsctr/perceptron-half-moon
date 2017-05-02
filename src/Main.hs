{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import Data.Bifunctor
import Data.List
import Graphics.Gloss.Interface.IO.Game
import Perceptron
import System.Random

data State = State { perceptron :: Perceptron, points :: [((Float, Float), Bool)] }

windowSize :: Int
windowSize = 600

windowSize', rMin, rMax :: Float
windowSize' = fromIntegral windowSize
rMax = windowSize' / 4
rMin = windowSize' / 8

main :: IO ()
main = do
    putStrLn "Use arrow keys to move the half moons"
    [x, y] <- replicateM 2 $ let a = windowSize' / 2 - rMax in randomRIO (-a, a)
    state <- State <$> perceptronInit <*> liftM2 (++)
        (map ((, True) . bimap (+ x) (+ y)) <$> halfMoon 0 pi)
        (map ((, False) . bimap (subtract x) (subtract y)) <$> halfMoon pi (2 * pi))
    playIO (InWindow "Half moon perceptron" (600, 600) (0, 0)) black 5 state draw listener update
    where
    halfMoon minAngle maxAngle = replicateM 1000 $ do
        r <- randomRIO (rMin, rMax)
        θ <- randomRIO (minAngle, maxAngle)
        return (r * cos θ, r * sin θ)

update :: Float -> State -> IO State
update _ State {..} = do
    perceptron' <- train perceptron points
    return State { perceptron = perceptron', .. }
    where
    train perceptron' [] = return perceptron'
    train perceptron' points' = do
        ((x, y), t) <- (points' !!) <$> randomRIO (0, length points' - 1)
        train (learn ([x, y], t) perceptron') $ delete ((x, y), t) points'

draw :: State -> IO Picture
draw State {..} = return $ Pictures $ map drawPoint points ++ [perceptronLine]
    where
    drawPoint ((x, y), _) = Color col $ Translate x y $ circleSolid 2
        where
        col
            | activate perceptron [x, y] > 0.5 = green
            | otherwise = red
    perceptronLine = Color blue $ Line [(-windowSize' / 2, y), (windowSize' / 2, -y)]
        where
        Perceptron {..} = perceptron
        y = (windowSize' / 2) * ((threshold / last weights) / (threshold / head weights))

listener :: Event -> State -> IO State
listener (EventKey (SpecialKey key) Down _ _) State {..}
    | KeyLeft <- key = shift (-1, 0)
    | KeyRight <- key = shift (1, 0)
    | KeyUp <- key = shift (0, 1)
    | KeyDown <- key = shift (0, -1)
    where
    shift (join bimap (* (windowSize' / 20)) -> (x, y)) =
        return State { points = map move points, ..}
        where
        move point
            | snd point = first (bimap (+ x) (+ y)) point
            | otherwise = first (bimap (subtract x) (subtract y)) point
listener _ state = return state
