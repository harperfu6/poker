module Main where

import Cards
import Hands

import System.Random.Shuffle
import Control.Monad

main :: IO ()
main = do
	forM_ [1..500] $ \i -> do
		hand <- randomHand
		res <- return $ judgePoker hand
		putStrLn $ show i ++ "  " ++ show hand ++ " -> " ++ show res


randomHand :: IO (Maybe Hand)
randomHand = do
	shuffled <- shuffleM allCards
	return . toHand . take 5 $ shuffled

judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker h = do
	i <- h
	return $ pokerHand i
