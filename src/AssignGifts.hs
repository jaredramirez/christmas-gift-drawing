{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module AssignGifts (
    assignGifts,
    personGiftsToString,
    giftsToGiftsText,
    Person (..),
    Gifts,
    GiftsT (..),

    -- * Tests
    makePairs,
    makeRandomizedPairs,
    makeRandomizedPairs',
) where

import Control.Monad qualified as Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Array qualified as Array
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Rando qualified

--------------------------------------------------------------------------------
-- list randomization and pairing

{- | Given a list, split them into pairs with their neighbors
The last first and last elements are also paired.

    makePairs [1, 2, 3, 4] == [(1,2),(2,3),(3,4),(4,1)]
-}
makePairs :: [a] -> [(a, a)]
makePairs list =
    let maxIndex = length list - 1
        arr = Array.listArray (0, maxIndex) list
     in foldr
            ( \(index, a) acc ->
                ( if index /= maxIndex
                    then (a, (Array.!) arr (index + 1))
                    else (a, (Array.!) arr 0)
                )
                    : acc
            )
            []
            (Array.assocs arr)

{- | Takes function to determine unique-ness, a list, and a list
of pairs to avoid, return a list of randomized pairs such that
for each element A and the resulting list L, (A, _) exist exactly
once in L and (_, A) exists exactly one in L.

Additionally, this function ensures that it does not generate a pair
if that pair is in the list of existing pairs.
-}
makeRandomizedPairs' ::
    (Eq a) =>
    -- | List to make pairs of
    [a] ->
    -- | Existing pairs
    [(a, a)] ->
    IO [(a, a)]
makeRandomizedPairs' = generate
  where
    generate list existingPairs = do
        result <-
            Monad.foldM
                ( \mbAcc a -> case mbAcc of
                    Nothing -> pure Nothing
                    Just acc -> do
                        let paired = map snd acc
                        mbNextPair <-
                            generatePossiblePair
                                a
                                ( list
                                    & filter (/= a)
                                    & filter (`notElem` paired)
                                )
                                (acc ++ existingPairs)
                        pure $ fmap (: acc) mbNextPair
                )
                (Just [])
                list
        case result of
            Nothing -> generate list existingPairs
            Just pairs -> pure pairs
    generatePossiblePair a subList subExistingPairs = do
        shuffled <- Rando.shuffle subList
        case shuffled of
            [] -> pure Nothing
            b : rest ->
                let possiblePair = (a, b)
                 in if possiblePair `notElem` subExistingPairs
                        then pure $ Just possiblePair
                        else generatePossiblePair a rest subExistingPairs

{- | Takes function to determine unique-ness, a list, and a list
of pairs to avoid, return a list of randomized pairs such that
for each element A and the resulting list L, (A, _) exist exactly
once in L and (_, A) exists exactly one in L.

Additionally, this function ensures that it does not generate a pair
if that pair is in the list of existing pairs.
-}
makeRandomizedPairs ::
    (Eq k) =>
    -- | Equality test
    (a -> k) ->
    -- | List to make pairs of
    [a] ->
    -- | Existing pairs
    [(a, a)] ->
    IO [(a, a)]
makeRandomizedPairs toKey list existingPairs = do
    next <- Rando.shuffle list
    let nextPairs = makePairs next
        nextAndPrevPairs = existingPairs <> nextPairs
        uniqNextPairs =
            List.nubBy
                ( \(a1, a2) (b1, b2) ->
                    toKey a1 == toKey b1
                        && toKey a2 == toKey b2
                )
                nextAndPrevPairs
    if length nextAndPrevPairs == length uniqNextPairs
        then pure nextPairs
        else makeRandomizedPairs toKey list existingPairs

--------------------------------------------------------------------------------
-- types

newtype Person = Person {name :: Text}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON)

data GiftsT person = Gifts
    { small1 :: person
    , small2 :: person
    , big :: person
    , book :: person
    }
    deriving (Show, Generic)

instance (ToJSON p) => ToJSON (GiftsT p)

type Gifts = GiftsT Person

type GiftsMaybe = GiftsT (Maybe Person)

-- | Set the list of pairs for a gift type for each person
setGiftsFromList :: [(Person, Person)] -> (Person -> GiftsMaybe -> GiftsMaybe) -> Map Person GiftsMaybe -> Map Person GiftsMaybe
setGiftsFromList pairs setGifts giftsMap =
    foldr
        (\(from, to) -> Map.adjust (setGifts to) from)
        giftsMap
        pairs

-- | Convert a gifts of mayse to a maybe of gifts
giftsMaybeToGifts :: GiftsMaybe -> Maybe Gifts
giftsMaybeToGifts giftsMaybe =
    Gifts
        <$> giftsMaybe.small1
        <*> giftsMaybe.small2
        <*> giftsMaybe.big
        <*> giftsMaybe.book

-- | Convert a gifts of people to just their text counterparts
giftsToGiftsText :: Gifts -> GiftsT Text
giftsToGiftsText gifts =
    Gifts
        { small1 = gifts.small1.name
        , small2 = gifts.small2.name
        , big = gifts.big.name
        , book = gifts.book.name
        }

{- | Given a list of people, pair them up 4 times for 4 gift types, such that
each person will always recieve and give 1 gift fo each type
-}
assignGifts :: [Person] -> IO (Map Person Gifts)
assignGifts people = do
    let makeRandomizedPairsA = makeRandomizedPairs (.name) people

    small1GiftsList <- makeRandomizedPairsA []
    small2GiftsList <- makeRandomizedPairsA small1GiftsList
    bigGiftsList <- makeRandomizedPairsA (small1GiftsList <> small2GiftsList)
    bookGiftsList <- makeRandomizedPairsA (small1GiftsList <> small2GiftsList <> bigGiftsList)

    people
        & map (,Gifts Nothing Nothing Nothing Nothing)
        & Map.fromList
        & setGiftsFromList small1GiftsList (\p giftsMaybe -> giftsMaybe{small1 = Just p})
        & setGiftsFromList small2GiftsList (\p giftsMaybe -> giftsMaybe{small2 = Just p})
        & setGiftsFromList bigGiftsList (\p giftsMaybe -> giftsMaybe{big = Just p})
        & setGiftsFromList bookGiftsList (\p giftsMaybe -> giftsMaybe{book = Just p})
        & traverse giftsMaybeToGifts
        & maybe (error "There was a problem assigning gifts") pure

--------------------------------------------------------------------------------
-- display helpers

personGiftsToString :: Person -> Gifts -> Text
personGiftsToString person gifts =
    person.name
        <> ("\n    Big: " <> gifts.big.name)
        <> ("\n    Small 1: " <> gifts.small1.name)
        <> ("\n    Small 2: " <> gifts.small2.name)
        <> ("\n    Book: " <> gifts.book.name)
