{-# LANGUAGE RecordWildCards #-}

module Perceptron (Perceptron (..), activate, learn, perceptronInit) where

import Control.Monad
import System.Random

data Perceptron = Perceptron { weights :: [Float], threshold :: Float }

rate :: Float
rate = 0.5

activate :: Perceptron -> [Float] -> Float
activate Perceptron {..} =
    recip . succ . exp . negate . subtract threshold . sum . zipWith (*) weights

learn :: ([Float], Bool) -> Perceptron -> Perceptron
learn (inputs, target) Perceptron {..} = Perceptron
    { weights = zipWith learnWeight weights inputs
    , threshold = learnWeight threshold (-1) }
    where learnWeight weight input = weight +
              rate * (fromIntegral (fromEnum target) - activate Perceptron {..} inputs) * input

perceptronInit :: IO Perceptron
perceptronInit = Perceptron <$> replicateM 2 randomWeight <*> randomWeight
    where randomWeight = randomRIO (-0.1, 0.1)
