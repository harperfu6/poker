module Main where

import Cards
import Hands

import System.Random.Shuffle
import Control.Monad
import Safe
import Data.Char

-- hand test
-- main :: IO ()
-- main = do
-- 	forM_ [1..500] $ \i -> do
-- 		hand <- randomHand
-- 		res <- return $ judgePoker hand
-- 		putStrLn $ show i ++ "  " ++ show hand ++ " -> " ++ show res

main :: IO ()
main = do
	putStrLn "------------------"
	putStrLn "-- simple poker --"
	putStrLn "------------------"
	deck <- shuffleM allCards
	case getHand deck of
		Nothing -> error "予期せぬエラー"
		Just (hand, deck) -> playPoker hand deck
	ynQuestion "-- もう一度やる？" main (putStrLn "-- またね")

playPoker :: Hand -> Deck -> IO ()
playPoker hand deck = do
	discards <- inputDisuse hand
	case drawHand deck discards hand of
		Nothing -> error "予期せぬエラー"
		Just (nhand, _) -> do
			printHand [] nhand
			printResult $ pokerHand nhand

inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
	printHand [] hand 
	putStrLn "-- 捨てるカードを選んでください"
	gotDisuse <- getDiscardList hand
	case gotDisuse of
		Nothing -> do
			putStrLn "-- 1~5の数値を並べて入力してね"
			inputDisuse hand
		Just disuses -> do
			printHand disuses hand
			ynQuestion "-- これでいい?" (return disuses) (inputDisuse hand)


printHand :: DiscardList -> Hand -> IO ()
printHand dis hand = putStrLn $ "-- 手札 : " ++ showChangeHand dis hand

printResult :: (PokerHand, Card) -> IO ()
printResult (ph, card) = putStrLn $ concat
	["***** あなたの手札は ", show ph, " で，最強のカードは ", show card, " でした*****"]

showChangeHand :: DiscardList -> Hand -> String
showChangeHand dis h =
	let
		judge x = if elem x dis then " " ++ show x ++ " " else "[" ++ show x ++ "]"
	in concat $ map judge (fromHand h)


ynQuestion :: String -> IO a -> IO a -> IO a
ynQuestion s yes no = do
	putStrLn $ s ++ "(y/n)"
	input <- getLine
	case input of
		"y" -> yes
		"n" -> no
		_ -> do
			putStrLn "-- `y`か`n`で入力してね"
			ynQuestion s yes no







randomHand :: IO (Maybe Hand)
randomHand = do
	shuffled <- shuffleM allCards
	return . toHand . take 5 $ shuffled

judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker h = do
	i <- h
	return $ pokerHand i




type DiscardList = [Card] -- 捨札
type Deck = [Card] -- 山札

-- 山札から手札を取り出す
getHand :: Deck -> Maybe (Hand, Deck)
getHand deck = do
	hand <- toHand . take 5 $ deck
	return (hand, drop 5 deck)

-- 捨札を作成する
getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do
	input <- getLine
	-- return . Just . selectByIndexes (fromHand h) $ toIntList input
	return $ do
		intList <- toIntList input
		res <- selectByIndexes (fromHand h) intList
		return res


toIntList :: String -> Maybe [Int]
-- map (\x -> read ((:[]) x):: Int) "123"
-- toIntList = map $ read . (:[])
toIntList str =
	if and $ map isDigit str
		then Just $ reads str
		else Nothing
	where
		reads :: String -> [Int]
		reads = map $ read . (:[])

selectByIndexes :: [a] -> [Int] -> Maybe [a]
-- selectByIndexes l = map ((l!!).(subtract 1))
-- atMay: !!の安全版
-- sequence: モナドをリストの外に出す
selectByIndexes l = sequence . map ((atMay l).(subtract 1))

-- 手札の交換処理
drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, [Card])
drawHand deck dis h =
	let
		-- n1 = filter (\c -> notElem c dis) (fromHand h)
		n1 = filter (flip notElem dis) (fromHand h)
		nr = drop (5 - length n1) deck
	-- in do
		-- hand <- toHand . take 5 $ n1 ++ deck
		-- ndeck <- return nr
		-- return (hand, ndeck)
	in (,) <$> toHand (take 5 $ n1 ++ deck) <*> return nr

