module Main where

import Cards
import Hands

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import Safe
import Data.Char
import Data.Maybe

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
		Just res -> matchPoker res
	ynQuestion "-- もう一度やる？" main (putStrLn "-- またね")


data Player =  Player | Enemy deriving Eq


showPlayerName :: Player -> String
showPlayerName Player = "あなた"
showPlayerName Enemy = "あいて"

matchPoker :: (Hand, Deck) -> IO ()
matchPoker (mhand, deck) = do
	(mres, ndeck, nmhand) <- playPoker mhand deck Player
	case getHand ndeck of
		Nothing -> error "予期せぬエラー"
		Just (ehand, odeck) -> do
			(eres, _, nehand) <- playPoker ehand odeck Enemy
			printResult nmhand nehand mres eres


playPoker :: Hand -> Deck -> Player -> IO ((PokerHand, Card), Deck, Hand)
playPoker hand deck player = do
	discards <- if player == Player
		then inputDisuse hand
		else aiDisuse hand
	case drawHand deck discards hand of
		Nothing -> error "予期せぬエラー"
		Just (nhand, ndeck) -> do
			let res = pokerHand hand
			return (res, ndeck, nhand)

inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
  printHand [] hand Player
  putStrLn "-- 捨てるカードを選んでね"
  gotDisuse <- getDiscardList hand
  case gotDisuse of
    Nothing -> do
      putStrLn "-- 1~5の数値を並べて入力してね"
      inputDisuse hand
    Just disuses -> do
      printHand disuses hand Player
      ynQuestion "-- あなた：これでいい？" (return disuses) (inputDisuse hand)

aiDisuse :: Hand -> IO DiscardList
aiDisuse hand = do
	let res = aiSelectDiscards hand
	printHand res hand Enemy
	putStrLn "---あいて:これでいいよ！"
	return res


----
printResult :: Hand -> Hand -> (PokerHand, Card) -> (PokerHand, Card) -> IO ()
printResult mhand ehand mres@(mph, mcard) eres@(eph, ecard) = do
	putStrLn "**** 結果発表！ ****"
	printHand [] mhand Player
	printHand [] ehand Enemy
	putStrLn $ concat ["あなたの手札は ", show mph, " で，最強のカードは ", show mcard, " でした"]
	putStrLn $ concat ["あいての手札は ", show eph, " で，最強のカードは ", show ecard, " でした"]
	case judgeVictory mres eres of
		LT -> putStrLn "あなたの負けです"
		EQ -> putStrLn "引き分けです"
		GT -> putStrLn "あなたの勝ちです"


printHand :: DiscardList -> Hand -> Player -> IO ()
printHand dis hand player = putStrLn $ "-- " ++ showPlayerName player ++ " の手札 : " ++ showChangeHand dis hand


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




----------------------
-- CPU AIの処理
----------------------

-- 入れ替えるカードを判断する
--- とりあえず役が揃っているもの以外は捨てる
aiSelectDiscards :: Hand -> DiscardList
aiSelectDiscards hand =
	case straightHint hand `mplus` flushHint hand *> Just [] of
		Nothing -> nOfKindDiscards hand
		Just xs -> xs

-- 役が揃っているすべてのカードを返す
allNOfKinds :: Hand -> [Card]
allNOfKinds hand = concat . concat
	$ catMaybes [nOfKindHint 2 hand, nOfKindHint 3 hand, nOfKindHint 4 hand]

-- 上記以外を捨てる
nOfKindDiscards :: Hand -> DiscardList
nOfKindDiscards hand = filter (flip notElem $ allNOfKinds hand) $ fromHand hand


-- 場の状況をAiHint,「気まぐれに選ぶ」を乱数（副作用）とする
--AiHint  Hand -> IO DiscardList
--AiHint = undefined


-- 勝敗判定
judgeVictory :: (PokerHand, Card) -> (PokerHand, Card) -> Ordering
judgeVictory l r = compare (pullStrength l) (pullStrength r)
	where
		pullStrength :: (PokerHand, Card) -> (PokerHand, Int)
		pullStrength = fmap cardStrength





















