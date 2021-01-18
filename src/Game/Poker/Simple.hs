module Game.Poker.Simple
( simpleGame
 )
where

import Game.Poker.Cards
import Game.Poker.Hands
import Game.Poker.AI

import System.Random.Shuffle

-- hand test
-- main :: IO ()
-- main = do
-- 	forM_ [1..500] $ \i -> do
-- 		hand <- randomHand
-- 		res <- return $ judgePoker hand
-- 		putStrLn $ show i ++ "  " ++ show hand ++ " -> " ++ show res

simpleGame :: IO ()
simpleGame = do
	putStrLn "------------------"
	putStrLn "-- simple poker --"
	putStrLn "------------------"
	deck <- shuffleM allCards
	case getHand deck of
		Nothing -> error "予期せぬエラー"
		Just res -> matchPoker res
	ynQuestion "-- もう一度やる？" simpleGame (putStrLn "-- またね")


--------------
data Player =  Player | Enemy
	deriving Eq


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


---- 
randomHand :: IO (Maybe Hand)
randomHand = do
	shuffled <- shuffleM allCards
	return . toHand . take 5 $ shuffled

judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker h = do
	i <- h
	return $ pokerHand i

