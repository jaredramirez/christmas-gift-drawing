{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module AssignGifts
    ( assignGifts
    , asssignmentsToBrevoRecipients
    , personGiftsToString
    , Person (..)

      -- * Tests
    , GiftsT (..)
    , makeRandomizedPairs
    , getOtherSmall1GiftGiver
    , getOtherSmall2GiftGiver
    ) where

import Brevo qualified

import Control.Monad qualified as Monad
import Data.Aeson (FromJSON, ToJSON, ToJSONKey (..))
import Data.Either.Combinators qualified as Either
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Rando qualified


--------------------------------------------------------------------------------
-- Randomized pairing

-- | Takes function to determine unique-ness, a list, and a list
-- of pairs to avoid, return a list of randomized pairs such that
-- for each element A and the resulting list L, (A, _) exist exactly
-- once in L and (_, A) exists exactly one in L.
--
-- Additionally, this function ensures that it does not generate a pair
-- if that pair is in the list of existing pairs.
makeRandomizedPairs
    :: Eq a
    => [a]
    -- ^ List to make pairs of
    -> [(a, a)]
    -- ^ Existing pairs
    -> IO [(a, a)]
makeRandomizedPairs = generate
  where
    -- Given a list of elements and a list of existing pairs, generate a new
    -- list of existing pairs
    generate list existingPairs = do
        -- Build the list of pairs
        -- This will be @Nothing@ if we failed to find a pair for any single person
        mbPairs <-
            Monad.foldM
                ( \mbAcc a -> case mbAcc of
                    Nothing -> pure Nothing
                    Just acc -> do
                        -- Get the list of elements already paired
                        let paired = map snd acc
                        -- Attempt to select the next possible pair
                        mbNextPair <-
                            generatePossiblePair
                                a
                                ( list
                                    & filter (/= a)
                                    & filter (`notElem` paired)
                                )
                                (acc ++ existingPairs)
                        -- Append the next pair to the accumlator
                        pure $ fmap (: acc) mbNextPair
                )
                (Just [])
                list
        case mbPairs of
            Nothing -> generate list existingPairs
            Just pairs -> pure pairs

    -- Attempt to select a random value for to pair with @a@, ensuring the pair
    -- does not already exists
    generatePossiblePair a subList subExistingPairs = do
        shuffled <- Rando.shuffle subList
        case shuffled of
            [] -> pure Nothing
            b : rest ->
                let possiblePair = (a, b)
                 in if possiblePair `notElem` subExistingPairs
                        then pure $ Just possiblePair
                        else generatePossiblePair a rest subExistingPairs


--------------------------------------------------------------------------------
-- Types

data Person = Person {name :: Text, email :: Text}
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToJSONKey)


data GiftsT person = Gifts
    { small1 :: person
    , small2 :: person
    , big :: person
    , book :: person
    }
    deriving (Show, Generic, ToJSON)


type Gifts person = GiftsT person


type GiftsMaybe person = GiftsT (Maybe person)


-- | Convert a gifts of mayse to a maybe of gifts
giftsMaybeToGifts :: GiftsMaybe person -> Maybe (Gifts person)
giftsMaybeToGifts giftsMaybe =
    Gifts
        <$> giftsMaybe.small1
        <*> giftsMaybe.small2
        <*> giftsMaybe.big
        <*> giftsMaybe.book


--------------------------------------------------------------------------------
-- Gift Assignment

-- | Given a list of people, pair them up 4 times for 4 gift types, such that
-- each person will always recieve and give 1 gift fo each type
--
-- Because there are 4 gifts, you must pass at least 5 persons to this function
assignGifts :: Ord person => [person] -> IO (Map person (Gifts person))
assignGifts people = do
    let makeRandomizedPairsA = makeRandomizedPairs people

    small1GiftsList <- makeRandomizedPairsA []
    small2GiftsList <- makeRandomizedPairsA small1GiftsList
    bigGiftsList <- makeRandomizedPairsA (small1GiftsList <> small2GiftsList)
    bookGiftsList <- makeRandomizedPairsA (small1GiftsList <> small2GiftsList <> bigGiftsList)

    people
        & map (,Gifts Nothing Nothing Nothing Nothing)
        & Map.fromList
        & setGiftsFromList small1GiftsList (\p giftsMaybe -> giftsMaybe {small1 = Just p})
        & setGiftsFromList small2GiftsList (\p giftsMaybe -> giftsMaybe {small2 = Just p})
        & setGiftsFromList bigGiftsList (\p giftsMaybe -> giftsMaybe {big = Just p})
        & setGiftsFromList bookGiftsList (\p giftsMaybe -> giftsMaybe {book = Just p})
        & traverse giftsMaybeToGifts
        & maybe (error "There was a problem assigning gifts") pure


-- | Set the list of pairs for a gift type for each person
setGiftsFromList
    :: Ord person
    => [(person, person)]
    -> (person -> GiftsMaybe person -> GiftsMaybe person)
    -> Map person (GiftsMaybe person)
    -> Map person (GiftsMaybe person)
setGiftsFromList pairs setGifts giftsMap =
    foldr
        (\(from, to) -> Map.adjust (setGifts to) from)
        giftsMap
        pairs


--------------------------------------------------------------------------------
-- Convert gifts to Brevo recipients

-- | Convert gift asignments to sendgrid template data
asssignmentsToBrevoRecipients :: Map Person (Gifts Person) -> Either Text [Brevo.Recipient Brevo.SecretSantaTempleteParams]
asssignmentsToBrevoRecipients giftsMap =
    let giftList = Map.toList giftsMap
     in traverse
            ( \(person, gifts) -> do
                otherSmall1Gift <-
                    getOtherSmall1GiftGiver gifts giftList
                        & Either.maybeToRight ("Could not find other gift giver for small1 for " <> person.name)
                        & fmap (.name)
                otherSmall2Gift <-
                    getOtherSmall2GiftGiver gifts giftList
                        & Either.maybeToRight ("Could not find other gift giver for small2 for " <> person.name)
                        & fmap (.name)
                Right $
                    Brevo.Recipient
                        { email = person.email
                        , params =
                            Brevo.SecretSantaTempleteParams
                                { you = person.name
                                , bigGift = gifts.big.name
                                , smallGift1 = gifts.small1.name
                                , smallGift2 = gifts.small2.name
                                , bookGift = gifts.book.name
                                , otherSmallGift1 = otherSmall1Gift
                                , otherSmallGift2 = otherSmall2Gift
                                }
                        }
            )
            giftList


-- | For @gifts.small1@, find the other person getting the same recipient a small gift
getOtherSmall1GiftGiver :: Eq person => Gifts person -> [(person, Gifts person)] -> Maybe person
getOtherSmall1GiftGiver gifts giftList =
    giftList
        & Maybe.mapMaybe
            ( \(person, personGifts) ->
                if personGifts.small2 == gifts.small1
                    then Just person
                    else Nothing
            )
        & Maybe.listToMaybe


-- | For @gifts.small1@, find the other person getting the same recipient a small gift
getOtherSmall2GiftGiver :: Eq person => Gifts person -> [(person, Gifts person)] -> Maybe person
getOtherSmall2GiftGiver gifts giftList =
    giftList
        & Maybe.mapMaybe
            ( \(person, personGifts) ->
                if personGifts.small1 == gifts.small2
                    then Just person
                    else Nothing
            )
        & Maybe.listToMaybe


personGiftsToString :: Person -> Gifts Person -> Text
personGiftsToString person gifts =
    person.name
        <> ("\n    Big: " <> gifts.big.name)
        <> ("\n    Small 1: " <> gifts.small1.name)
        <> ("\n    Small 2: " <> gifts.small2.name)
        <> ("\n    Book: " <> gifts.book.name)
