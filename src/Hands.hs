module Hands
( Hand -- データコンストラクタはエクスポートしない
, toHand, fromHand -- toHandを使うことで「手」が保証される
 )
where

import Data.List
import Control.Monad
import Cards

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
pokerHand :: Hand -> (PokerHand, Card)
pokerHand = undefined

-- pokerHandの中を手続き的に書くのではなく，必要そうな判定処理を関数として定義していく
onePair :: Hand -> Maybe (PokerHand, Card)
onePair = undefined

twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair = undefined

threeOfAKind :: Hand -> Maybe (PokerHand, Card)
threeOfAKind = undefined

straight :: Hand -> Maybe (PokerHand, Card)
straight = undefined

flush :: Hand -> Maybe (PokerHand, Card)
flush = undefined

fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse = undefined

fourOfAKind :: Hand -> Maybe (PokerHand, Card)
fourOfAKind = undefined

straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush = undefined




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
















