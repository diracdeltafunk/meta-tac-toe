{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Game where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as MapL

data Mark = X | O deriving (Eq, Show, Read)

type Claim = [Mark]

class Claimable a where
    claimed :: a -> Claim
    isClaimed :: Mark -> a -> Bool
    isClaimed = flip ((flip elem).claimed)

data Sector = TL | TM | TR | ML | MM | MR | BL | BM | BR deriving (Eq, Ord, Show, Read)

sectors :: [Sector]
sectors = [TL, TM, TR, ML, MM, MR, BL, BM, BR]

type Coord = (Sector, Sector)

wincons :: [[Sector]]
wincons = [[TL, TM, TR],
           [ML, MM, MR],
           [BL, BM, BR],
           [TL, ML, BL],
           [TM, MM, BM],
           [TR, MR, BR],
           [TL, MM, BR],
           [TR, MM, BL]]

type SubBoard = MapL.Map Sector Mark

instance Claimable SubBoard where
    isClaimed = flip (flip . ((any . all . flip MapL.member) .) . MapL.filter . (==)) wincons
    claimed = flip filter [X,O] . flip isClaimed

instance Show SubBoard where
    show = intercalate "\n" . map concat . chunksOf 3 . flip map (MapL.lookup <$> sectors) . ((fromMaybe "*" . fmap show) .) . flip ($)

type Board = MapL.Map Sector SubBoard

instance Claimable Board where
    isClaimed = flip (flip . ((any . all . flip MapL.member) .) . MapL.filter . isClaimed) wincons
    claimed = flip filter [X,O] . flip isClaimed

instance Show Board where
    show = unlines . intersperse "------------" . map ((intercalate "\n").(map concat).transpose.(intersperse (replicate 3 "|"))) . chunksOf 3 . map (lines . show) . MapL.elems

type Move = State Board Sector

newGame :: Board
newGame = MapL.fromList $ flip (,) MapL.empty <$> sectors

move :: Mark -> Coord -> Move
move mark (superSector, subSector) = do s <- get
                                        put $ MapL.adjust (MapL.insert subSector mark) superSector $ s
                                        return subSector
