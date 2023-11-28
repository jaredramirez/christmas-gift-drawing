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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Brevo
import qualified Control.Exception as Exc
import Control.Monad (foldM, replicateM, void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as Aeson
import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.List ((\\))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Typeable (Typeable)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Rando
import qualified System.Environment as Env

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

Additinally, this function ensures that it does not generate a pair
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

-- types

data Person = Person {name :: Text, email :: Text}
    deriving (Show, Eq, Ord, Generic, FromJSON)

data GiftsT person = Gifts
    { small1 :: person
    , small2 :: person
    , big :: person
    , book :: person
    }
    deriving (Show)

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

-- | Convert gift asignments to sendgrid template data
asssignmentsToEmailRecipients :: Map Person Gifts -> [Brevo.Recipient Brevo.SecretSantaTempleteParams]
asssignmentsToEmailRecipients giftsMap =
    let giftList = Map.toList giftsMap
     in giftsMap
            & Map.mapWithKey
                ( \person gifts ->
                    let
                        otherSmall1Gift = (fst $ List.head $ List.filter (\(p, v) -> v.small2 == gifts.small1) giftList).name
                        otherSmall2Gift = (fst $ List.head $ List.filter (\(p, v) -> v.small1 == gifts.small2) giftList).name
                     in
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
            & Map.elems

-- display helpers

personGiftsToString :: Person -> Gifts -> Text
personGiftsToString person gifts =
    person.name
        <> ("\n    Big: " <> gifts.big.name)
        <> ("\n    Small 1: " <> gifts.small1.name)
        <> ("\n    Small 2: " <> gifts.small2.name)
        <> ("\n    Book: " <> gifts.book.name)

-- put it all together

main :: IO ()
main = do
    brevoApiKey <- Env.getEnv "BREVO_API_KEY" & fmap CBS.pack
    people <- BSL.readFile "people.json" >>= (either (error "Problem parsing people.json") pure . Aeson.eitherDecode @[Person])
    assignments <- assignGifts people
    let recipients = asssignmentsToEmailRecipients assignments
    Brevo.send brevoApiKey 3 recipients
