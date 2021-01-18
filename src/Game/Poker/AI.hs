module Game.Poker.AI
( aiSelectDiscards
 )
where

import Data.Maybe
import Control.Applicative
import Control.Monad

import Game.Poker.Cards
import Game.Poker.Hands

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

