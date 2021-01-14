module Cards
( Suit(..)
, Card -- カード型はカードを独自に作成されないように，データコンストラクトをあえてエクスポートしない
, allCards
, cardSuit
, cardNumber
 )
where


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

-- すべてのカードを列挙する
allCards :: [Card]
allCards = [Card num suit | suit <- [Hearts ..], num <- [2..14]]

-- カードの取得用
cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card n _) = n
