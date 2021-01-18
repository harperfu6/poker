module Game.Poker.Hands
( Hand -- データコンストラクタはエクスポートしない
, toHand, fromHand -- toHandを使うことで「手」が保証される
, PokerHand(..)
, pokerHand
----
-- hint (後々思考ルーチンを作るのに役立つ可能性があるため)
, straightHint
, flushHint
, nOfKindHint
----
-- hand
, straightFlush
, fourOfAKind
, fullHouse
, flush
, straight
, threeOfAKind
, twoPair
, onePair
----
, DiscardList
, Deck
, getHand
, drawHand
, getDiscardList 
, judgeVictory 
 )
where

import Game.Poker.Cards
import Data.List

import Safe
import Data.Char
import Control.Monad
import Game.Poker.Cards

-- Haskellは部品をつなげる糊がいくらでもあるので，とりあえず必要だと思った部品を作りまくることが大事
-- Haskellでは型を作るのが簡単なので，ちょっとした条件の保証や軽い意味付けを与えるためによく型を定義する
-- decisionを通ったものを「手」として定義することで解釈性を向上させる
newtype Hand = Hand {
	fromHand :: [Card]
} deriving (Show, Eq, Ord)

-- カードの枚数判定と後続処理のためのソート
-- toHand :: [Card] -> Maybe [Card]
toHand :: [Card] -> Maybe Hand
toHand l =
	if length l == 5
  	then Just $ Hand (sort l)
  	else Nothing


-- ポーカーハンド
-- 弱い順から定義することで比較しやすくする
data PokerHand
	= HighCards
	| OnePair
	| TwoPair
	| ThreeOfAKind
	| Straight
	| Flush
	| FullHouse
	| FourOfAKind
	| StraightFlush
	deriving (Show, Read, Eq, Ord, Enum)


-- ハンドの種類が同じ場合，ハンドを構成する最強カードが含まれている方が勝ち
-- fmap ($h) hands :: [Maybe (PokerHand, Card)]
-- 上記の説明：handsの要素をfとすると，f hを各fについて適用していく
-- この中から最強のハンドを取得する．mplusは両辺がJustの場合左返すので強い順から判定する
pokerHand :: Hand -> (PokerHand, Card)
	-- lは[Card]
pokerHand h@(Hand l) =
	case foldl mplus Nothing $ fmap ($h) hands of
		Just pc -> pc
		Nothing -> (HighCards, last l)
	where
		-- ハンド関数のリスト
		-- hadnsにHand型を適用し，[Maybe (PokerHand, Card)]を取得する
		hands :: [Hand -> Maybe (PokerHand, Card)]
		hands =
			[ straightFlush
			, fourOfAKind
			, fullHouse
			, flush
			, straight
			, threeOfAKind
			, twoPair
			, onePair
			]





-- pokerHandの中を手続き的に書くのではなく，必要そうな判定処理を関数として定義していく
onePair :: Hand -> Maybe (PokerHand, Card)
onePair h = do
	cs <- nOfKindHint 2 h
	return (OnePair, last $ concat cs)

twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair h = do
	cs <- nOfKindHint 2 h
	if length cs == 2
		then Just (TwoPair, last $ concat cs)
		else Nothing

threeOfAKind :: Hand -> Maybe (PokerHand, Card)
threeOfAKind h = do
	cs <- nOfKindHint 3 h
	return (ThreeOfAKind, last $ concat cs)

straight :: Hand -> Maybe (PokerHand, Card)
straight h = do
	c <- straightHint h
	return (Straight, c)

flush :: Hand -> Maybe (PokerHand, Card)
flush h = do
	c <- flushHint h
	return (Flush, c)

fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse h = do
	cs1 <- nOfKindHint 3 h
	cs2 <- nOfKindHint 2 h
	return (FullHouse, last $ concat cs1)

fourOfAKind :: Hand -> Maybe (PokerHand, Card)
fourOfAKind h = do
	cs <- nOfKindHint 4 h
	return (FourOfAKind, maximum $ concat cs)

straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush h = do
	c <- straightHint h
	d <- flushHint h
	return (StraightFlush, max c d)



-- ハンドは下記に一般化できる
-- 連続する番号のカードが5枚揃っていること
-- 同じスートのカードが5枚揃っていること
-- 同じ番号のn枚組がmセット以上あること


-- 連続する番号のカードが5枚揃っていること
-- ストレート判定のためにエースの解釈が２通りになる
-- 「どちらかが成功した場合」を扱う仕組みとしてmplusを使う
-- ghci> Just 1 `mplus` Nothing
-- Just 1
-- ghci> Nothing `mplus` Just 1
-- Just 1
-- ghci> Just 1 `mplus` Just 2
-- Just 1
-- ghci> Nothing `mplus` Nothing
-- Nothing
straightHint :: Hand -> Maybe Card
straightHint (Hand l) =
	(judgeStraight . extract cardStrength $ l)
	`mplus`
	(judgeStraight . sort . extract cardNumber $ l)

-- カードが連番で並んでいるか
isStraight :: [Int] -> Bool
isStraight xs@(x:_) = xs == [x .. x+4]
isStraight _ = False

-- ストレート判定した上で，最強のカードを抽出
judgeStraight :: [(Int, Card)] -> Maybe Card
judgeStraight l =
	if isStraight $ map fst l
		then Just . snd . last $ l
		else Nothing

-- [Card]から[(Int, Card)]を作り出す
-- エースの扱いだけ注意する
extract :: (Card -> Int) -> [Card] -> [(Int, Card)]
-- 上記は(b -> a) -> [b] -> [(a, b)]に一般化できるのでHoogle検索してお目当ての関数を探しても良い
extract f cs = map (\c -> (f c, c)) cs



-- 同じスートのカードが5枚揃っていること
-- 返却は最強のカード，ハンドはソート済みであることを考慮
flushHint :: Hand -> Maybe Card
flushHint (Hand (x:xs)) =
	if all ((cardSuit x ==) . cardSuit) xs then Just (last xs) else Nothing

-- 同じ番号のn枚組がmセット以上あること
-- 返却はどういう組み合わせだったか
nOfKindHint :: Int -> Hand -> Maybe [[Card]]
nOfKindHint n (Hand h) =
	if cards /= [] then Just cards else Nothing
	where
		cards :: [[Card]]
		cards = filter ((==n) . length) $ groupBy (\x y -> cardNumber x == cardNumber y) h


-------

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


-- 勝敗判定
judgeVictory :: (PokerHand, Card) -> (PokerHand, Card) -> Ordering
judgeVictory l r = compare (pullStrength l) (pullStrength r)
	where
		pullStrength :: (PokerHand, Card) -> (PokerHand, Int)
		pullStrength = fmap cardStrength

