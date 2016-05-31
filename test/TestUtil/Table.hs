{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module TestUtil.Table
    ( Table
    , generateTable
    , generateTable'
    , printTable
    ) where

import Data.List (intercalate, foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Strict as State
import Text.Printf
import System.CPUTime (getCPUTime)
import Math.Topology.KnotTh.Tangle



generateTable :: (forall m. (Monad m) => ((Tangle ct, a) -> m ()) -> m ()) -> Table
generateTable =
    generateTable'
        (\ (tangle, _) -> (numberOfVertices tangle, numberOfLegs tangle))
        (const 1)


generateTable' :: (Ord k) => (a -> k) -> (a -> Int) -> (forall m. (Monad m) => (a -> m ()) -> m ()) -> Map.Map k Int
generateTable' key weight generator =
    let yield x = State.modify' $ Map.insertWith (+) (key x) (weight x)
    in State.execState (generator yield) Map.empty


type Table = Map.Map (Int, Int) Int


printTable :: String -> Table -> IO ()
printTable name table = do
    beginTime <- getCPUTime
    putStrLn $ name ++ ":"
    let crs = Set.toList $ Set.fromList $ map fst $ Map.keys table
    let totalTangles = sum $ Map.elems table
    putStr $
        let possibleLegs = Set.toList $ Set.fromList $ map snd $ Map.keys table
            total = "total: " ++ show totalTangles
            header = intercalate "\t" $ "l\\n" : map show crs
            line l = intercalate "\t" $ show l : map (\ c -> cell (c, l)) crs

            allCr =
                let numForCr c = foldl' (\ !carry ((!cc, !_), !cn) -> carry + if cc == c then cn else 0) 0 $ Map.assocs table
                in intercalate "\t" $ "all:" : map (show . numForCr) crs

            cell arg =
                case Map.lookup arg table of
                    Just x | x > 0 -> show x
                    _              -> "."

        in unlines $ [header] ++ map line possibleLegs ++ [allCr, total]
    endTime <- getCPUTime
    let time = (fromInteger (endTime - beginTime) :: Double) / 1.0e12
    printf "Time = %fs (%f tangles/s)\n" time (realToFrac totalTangles / time)
