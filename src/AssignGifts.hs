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
    assignGiftsWithSeed,
    personGiftsToString,
    giftsToGiftsText,
    Person (..),
    Gifts,
    GiftsT (..),
    PreviousGifts,
    giftRecipients,
    maxRepeatRecipients,

    -- * Tests
    makePairs,
    makeRandomizedPairs,
    makeRandomizedPairs',
) where

import Control.Monad qualified as Monad
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Array qualified as Array
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Random (StdGen, mkStdGen, newStdGen)
import System.Random.Stateful (IOGenM, newIOGenM, uniformShuffleListM)

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
makeRandomizedPairs' list existingPairs = do
    gen <- newStdGen >>= newIOGenM
    generate gen list existingPairs
  where
    generate gen items excludedPairs = do
        result <-
            Monad.foldM
                ( \mbAcc a -> case mbAcc of
                    Nothing -> pure Nothing
                    Just acc -> do
                        let paired = map snd acc
                        mbNextPair <-
                            generatePossiblePair
                                gen
                                a
                                ( items
                                    & filter (/= a)
                                    & filter (`notElem` paired)
                                )
                                (acc ++ excludedPairs)
                        pure $ fmap (: acc) mbNextPair
                )
                (Just [])
                items
        case result of
            Nothing -> generate gen items excludedPairs
            Just pairs -> pure pairs
    generatePossiblePair gen a subList subExistingPairs = do
        shuffled <- uniformShuffleListM subList gen
        case shuffled of
            [] -> pure Nothing
            b : rest ->
                let possiblePair = (a, b)
                 in if possiblePair `notElem` subExistingPairs
                        then pure $ Just possiblePair
                        else generatePossiblePair gen a rest subExistingPairs

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
    gen <- newStdGen >>= newIOGenM
    makeRandomizedPairsWithGen gen toKey list existingPairs

makeRandomizedPairsWithGen :: (Eq k) => IOGenM StdGen -> (a -> k) -> [a] -> [(a, a)] -> IO [(a, a)]
makeRandomizedPairsWithGen gen toKey list existingPairs =
    tryRandomizedPairs gen toKey list existingPairs
        >>= maybe (makeRandomizedPairsWithGen gen toKey list existingPairs) pure

-- | Like 'makeRandomizedPairsWithGen', but gives up after a number of attempts
makeRandomizedPairsBounded :: (Eq k) => Int -> IOGenM StdGen -> (a -> k) -> [a] -> [(a, a)] -> IO (Maybe [(a, a)])
makeRandomizedPairsBounded attempts gen toKey list existingPairs
    | attempts <= 0 = pure Nothing
    | otherwise =
        tryRandomizedPairs gen toKey list existingPairs
            >>= maybe (makeRandomizedPairsBounded (attempts - 1) gen toKey list existingPairs) (pure . Just)

-- | Shuffle once, returning the pairs only if none of them are in the existing pairs
tryRandomizedPairs :: (Eq k) => IOGenM StdGen -> (a -> k) -> [a] -> [(a, a)] -> IO (Maybe [(a, a)])
tryRandomizedPairs gen toKey list existingPairs = do
    next <- uniformShuffleListM list gen
    let nextPairs = makePairs next
        pairKey (a1, a2) = (toKey a1, toKey a2)
        -- Existing pairs may overlap each other, so only check the new ones
        existingKeys = map pairKey existingPairs
    pure $
        if all ((`notElem` existingKeys) . pairKey) nextPairs
            then Just nextPairs
            else Nothing

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
    deriving (Show, Eq, Generic)

instance (ToJSON p) => ToJSON (GiftsT p)

instance (FromJSON p) => FromJSON (GiftsT p)

type Gifts = GiftsT Person

-- | A previous draw: who each person gave to, keyed by giver name
type PreviousGifts = Map Text (GiftsT Text)

-- | The most people anyone may give to again from the previous draw
maxRepeatRecipients :: Int
maxRepeatRecipients = 2

giftRecipients :: GiftsT p -> [p]
giftRecipients gifts = [gifts.small1, gifts.small2, gifts.big, gifts.book]

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
assignGifts :: PreviousGifts -> [Person] -> IO (Map Person Gifts)
assignGifts previous people = do
    gen <- newStdGen >>= newIOGenM
    assignGiftsWithGen gen previous people

{- | Reproduce a draw using the same seed, previous draw, and ordered list of
people.
-}
assignGiftsWithSeed :: Int -> PreviousGifts -> [Person] -> IO (Map Person Gifts)
assignGiftsWithSeed seed previous people = do
    gen <- newIOGenM (mkStdGen seed)
    assignGiftsWithGen gen previous people

{- | Draw until no one gives the same gift type to the same person as in the
previous draw, and no one gives to more than 'maxRepeatRecipients' of their
previous recipients.
-}
assignGiftsWithGen :: IOGenM StdGen -> PreviousGifts -> [Person] -> IO (Map Person Gifts)
assignGiftsWithGen gen previous people = do
    let previousPairs giftType =
            [ (Person giver, Person (giftType gifts))
            | (giver, gifts) <- Map.toList previous
            ]
        -- Earlier categories can leave no valid cycle for a later one, so
        -- give up on a category after a while and restart the whole draw
        makeRandomizedPairsA giftType existing =
            makeRandomizedPairsBounded 10000 gen (.name) people (previousPairs giftType <> existing)
        retry = assignGiftsWithGen gen previous people

    mbAssignments <- runMaybeT $ do
        small1GiftsList <- MaybeT $ makeRandomizedPairsA (.small1) []
        small2GiftsList <- MaybeT $ makeRandomizedPairsA (.small2) small1GiftsList
        bigGiftsList <- MaybeT $ makeRandomizedPairsA (.big) (small1GiftsList <> small2GiftsList)
        bookGiftsList <- MaybeT $ makeRandomizedPairsA (.book) (small1GiftsList <> small2GiftsList <> bigGiftsList)

        people
            & map (,Gifts Nothing Nothing Nothing Nothing)
            & Map.fromList
            & setGiftsFromList small1GiftsList (\p giftsMaybe -> giftsMaybe{small1 = Just p})
            & setGiftsFromList small2GiftsList (\p giftsMaybe -> giftsMaybe{small2 = Just p})
            & setGiftsFromList bigGiftsList (\p giftsMaybe -> giftsMaybe{big = Just p})
            & setGiftsFromList bookGiftsList (\p giftsMaybe -> giftsMaybe{book = Just p})
            & traverse giftsMaybeToGifts
            & maybe (error "There was a problem assigning gifts") pure

    case mbAssignments of
        Just assignments | withinRepeatLimit previous assignments -> pure assignments
        _ -> retry

withinRepeatLimit :: PreviousGifts -> Map Person Gifts -> Bool
withinRepeatLimit previous =
    all
        ( \(giver, gifts) -> case Map.lookup giver.name previous of
            Nothing -> True
            Just previousGifts ->
                let recipients = map (.name) (giftRecipients gifts)
                    repeats = filter (`elem` recipients) (giftRecipients previousGifts)
                 in length repeats <= maxRepeatRecipients
        )
        . Map.toList

--------------------------------------------------------------------------------
-- display helpers

personGiftsToString :: Person -> Gifts -> Text
personGiftsToString person gifts =
    person.name
        <> ("\n    Big: " <> gifts.big.name)
        <> ("\n    Small 1: " <> gifts.small1.name)
        <> ("\n    Small 2: " <> gifts.small2.name)
        <> ("\n    Book: " <> gifts.book.name)
