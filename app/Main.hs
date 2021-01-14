module Main where

import System.Random.Shuffle


-- トランプの定義
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Read, Eq, Ord, Enum)
-- Ordした際に先の要素が湯煎されるためIntを先にもってくる
-- 好き勝手に生成できないようにするために，Readのインスタンスにしない，Showは独自で定義
data Card = Card Int Suit deriving (Eq, Ord) 

-- 番号を文字列にする
showCardNumber :: Int -> String
showCardNumber 14 = "A_"
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber x = (show x) ++ "_"

instance Show Card where
	show (Card i Hearts) = "H" ++ showCardNumber i
	show (Card i Diamonds) = "D" ++ showCardNumber i
	show (Card i Clubs) = "C" ++ showCardNumber i
	show (Card i Spades) = "S" ++ showCardNumber i


main :: IO ()
main = do
	shuffledList <- shuffleM [1,2,3,4,5]
	print shuffledList
