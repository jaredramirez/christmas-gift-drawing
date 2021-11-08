{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs, NamedFieldPuns #-}

module Main where

import Control.Monad (foldM, replicateM)
import qualified Data.Array as Array
import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import System.Random.Stateful (uniformRM, globalStdGen)


-- list randomization and pairing


randomize :: [a] -> IO [a]                                                                                                                                             
randomize list = do
    randomNumbers <- replicateM (length list) $ uniformRM (1 :: Int, 100000) globalStdGen
    randomNumbers
        & zip list
        & List.sortOn snd
        & fmap fst
        & pure


pair :: [a] -> [(a, a)]
pair list =
    let maxIndex = length list - 1
        arr = Array.listArray (0, maxIndex) list
    in
    foldr
        (\(index, a) acc ->
            (if index /= maxIndex then
                (a, (Array.!) arr (index + 1))
            else 
                (a, (Array.!) arr 0)
            ) : acc
        )
        []
        (Array.assocs arr)


randomizeAndPair :: [a] -> IO [(a, a)]
randomizeAndPair list =
    randomize list
        & fmap pair


uniqRandomizeAndPair :: Eq k => (a -> k) ->  [a] -> [(a, a)] -> IO [(a, a)]
uniqRandomizeAndPair toKey list pairs =
    randomize list
        >>= \next ->
            let nextPairs = pair next
                nextAndPrevPairs = pairs ++ nextPairs
                uniqNextPairs =
                    List.nubBy
                        (\(a1, a2) (b1, b2) ->
                            toKey a1 == toKey b1
                                && toKey a2 == toKey b2
                        )
                        nextAndPrevPairs
            in
            if length nextAndPrevPairs == length uniqNextPairs then
                pure nextPairs
            else
                uniqRandomizeAndPair toKey list pairs


group :: Ord a => [(a,b)] -> [(a, [b])]
group list =
    list
        & fmap (\(a, b) -> (a, [b]))
        & Map.fromListWith (++)
        & Map.toList


-- validation


doesValueCountPerKeyMatch :: Ord k => (b -> k) -> Int ->[(a, [b])] -> Bool
doesValueCountPerKeyMatch toKey expected list =
    list
        & map snd
        & concat
        & map (\b -> (toKey b, 1))
        & Map.fromListWith (+)
        & Map.elems
        & List.all (== expected)


doesTotalCountMatch :: Ord k => (b -> k) -> Int ->[(a, [b])] -> Bool
doesTotalCountMatch toKey expected list =
    list
        & map snd
        & concat
        & map (\b -> (toKey b, 1))
        & Map.fromListWith (+)
        & Map.elems
        & sum
        & (== expected)


areAllValuesUniqForKey :: Ord k => (b -> k) -> [(a, [b])] -> Bool
areAllValuesUniqForKey toKey list =
    list
        & map
            (\(a, bs) ->
                length bs == length (List.nubBy (\b1 b2 -> toKey b1 == toKey b2) bs)
            )
        & List.all id


-- types


data Person =
    Person { name :: String, phone :: String }
    deriving (Show, Eq, Ord)


data GiftsT person =
    Gifts
        { big1 :: person
        , big2 :: person
        , small :: person
        , book :: person
        }
    deriving (Show)


type Gifts = GiftsT Person


type GiftMaybe = GiftsT (Maybe Person)

setGiftsFromList :: [(Person, Person)] -> (Person -> GiftMaybe -> GiftMaybe) -> Map Person GiftMaybe -> Map Person GiftMaybe
setGiftsFromList pairs setGifts giftsMap =
    foldr (\(from, to) -> Map.adjust (setGifts to) from)
        giftsMap
        pairs


giftsMaybeToGifts :: GiftMaybe -> Maybe Gifts
giftsMaybeToGifts giftsMaybe =
    pure Gifts
        <*> giftsMaybe.big1
        <*> giftsMaybe.big2
        <*> giftsMaybe.small
        <*> giftsMaybe.book


assignGifts :: [Person] -> IO (Map Person Gifts)
assignGifts people = do
    let uniqRandomizeAndPairA = uniqRandomizeAndPair (.name) people

    big1GiftsList <- uniqRandomizeAndPairA []
    big2GiftsList <- uniqRandomizeAndPairA big1GiftsList
    smallGiftsList <- uniqRandomizeAndPairA (big1GiftsList ++ big2GiftsList)
    bookGiftsList <- uniqRandomizeAndPairA (big1GiftsList ++ big2GiftsList ++ smallGiftsList)

    giftsMap <-
        people
            & map (\p -> (p, Gifts Nothing Nothing Nothing Nothing))
            & Map.fromList
            & setGiftsFromList big1GiftsList (\p giftsMaybe -> giftsMaybe{big1 = Just p})
            & setGiftsFromList big2GiftsList (\p giftsMaybe -> giftsMaybe{big2 = Just p})
            & setGiftsFromList smallGiftsList (\p giftsMaybe -> giftsMaybe{small = Just p})
            & setGiftsFromList bookGiftsList (\p giftsMaybe -> giftsMaybe{book = Just p})
            & traverse giftsMaybeToGifts
            & maybe (error "There was a problem assigning gifts") pure

    pure giftsMap


assignmentGiftsToString :: Person -> Gifts -> String
assignmentGiftsToString person gifts =
    "Hey " ++ person.name ++ "! The people you're getting gifts for this Christmas are: \n" ++
        "Big gift 1: " ++ gifts.big1.name ++ "\n" ++
        "Big gift 2: " ++ gifts.big2.name ++ "\n" ++
        "Small gift: " ++ gifts.small.name ++ "\n" ++
        "Book: " ++ gifts.book.name


assignmentsToSMSPayload :: Map Person Gifts -> Map Person String
assignmentsToSMSPayload giftsMap =
    giftsMap
        & Map.mapWithKey assignmentGiftsToString


-- data


peopleData :: [Person]
peopleData =
    [ Person { name = "Caleb", phone = "1-940-703-9602" }
    , Person { name = "Sophia", phone = "1-940-435-5754" }
    , Person { name = "Jared", phone = "1-940-368-7410" }
    , Person { name = "Sam", phone = "1-940-595-5845" }
    , Person { name = "Alex", phone = "1-940-595-5829" }
    , Person { name = "Misa", phone = "1-940-435-9542" }
    , Person { name = "Carlos", phone = "1-940-595-5837" }
    ]


-- put it all together


main :: IO ()
main = do
    assignments <- assignGifts peopleData
    let smsPayloads = assignmentsToSMSPayload assignments
    mapM_ putStrLn smsPayloads
