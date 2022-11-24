{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs, NamedFieldPuns, OverloadedStrings #-}

module Main where

import Control.Monad (foldM, replicateM)
import qualified Data.Array as Array
import qualified Data.Maybe as Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status as HTTPStatus
import qualified System.Environment as Env
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
                nextAndPrevPairs = pairs <> nextPairs
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
        & Map.fromListWith (<>)
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
    Person { name :: ByteString, phone :: ByteString }
    deriving (Show, Eq, Ord)


data GiftsT person =
    Gifts
        { small1 :: person
        , small2 :: person
        , big :: person
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
        <*> giftsMaybe.small1
        <*> giftsMaybe.small2
        <*> giftsMaybe.big
        <*> giftsMaybe.book


assignGifts :: [Person] -> IO (Map Person Gifts)
assignGifts people = do
    let uniqRandomizeAndPairA = uniqRandomizeAndPair (.name) people

    small1GiftsList <- uniqRandomizeAndPairA []
    small2GiftsList <- uniqRandomizeAndPairA small1GiftsList
    bigGiftsList <- uniqRandomizeAndPairA (small1GiftsList <> small2GiftsList)
    bookGiftsList <- uniqRandomizeAndPairA (small1GiftsList <> small2GiftsList <> bigGiftsList)

    giftsMap <-
        people
            & map (\p -> (p, Gifts Nothing Nothing Nothing Nothing))
            & Map.fromList
            & setGiftsFromList small1GiftsList (\p giftsMaybe -> giftsMaybe{small1 = Just p})
            & setGiftsFromList small2GiftsList (\p giftsMaybe -> giftsMaybe{small2 = Just p})
            & setGiftsFromList bigGiftsList (\p giftsMaybe -> giftsMaybe{big = Just p})
            & setGiftsFromList bookGiftsList (\p giftsMaybe -> giftsMaybe{book = Just p})
            & traverse giftsMaybeToGifts
            & maybe (error "There was a problem assigning gifts") pure

    pure giftsMap 


assignmentGiftsToString :: Map Person Gifts -> Person -> Gifts -> ByteString
assignmentGiftsToString giftMap person gifts =
    let
        giftList = Map.toList giftMap
        otherSmall1Gift = (fst $ List.head $ List.filter (\(p, v) -> v.small2 == gifts.small1) giftList).name
        otherSmall2Gift = (fst $ List.head $ List.filter (\(p, v) -> v.small1 == gifts.small2) giftList).name
    in
    "Hey " <> person.name <> "! The people you're getting gifts for this Christmas are: \n" <>
        "Big gift: " <> gifts.big.name <> "\n" <>
        "Small gift 1: " <> gifts.small1.name <> "\n" <>
        "Small gift 2: " <> gifts.small2.name <> "\n" <>
        "Book: " <> gifts.book.name <> "\n\n" <>
        "The other person getting " <> gifts.small1.name <> " a small gift is " <> otherSmall1Gift <> "\n" <>
        "The other person getting " <> gifts.small2.name <> " a small gift is " <> otherSmall2Gift


assignmentsToSMSPayload :: Map Person Gifts -> Map Person ByteString
assignmentsToSMSPayload giftsMap =
    giftsMap
        & Map.mapWithKey (assignmentGiftsToString giftsMap)


-- http


data SendSMSPayload =
    SendSMSPayload { to :: ByteString, from :: ByteString, body :: ByteString }


data TwilioAuth =
    TwilioAuth { sid :: ByteString, token :: ByteString }


sendSMS :: TwilioAuth -> SendSMSPayload -> IO ()
sendSMS twilioAuth payload = do
    initReq <- parseRequest $ "https://api.twilio.com/2010-04-01/Accounts/" <> CBS.unpack twilioAuth.sid <> "/Messages.json"
    let req = initReq
                & HTTP.setRequestBasicAuth twilioAuth.sid twilioAuth.token
                & HTTP.setRequestBodyURLEncoded
                    [ ( "To", payload.to )
                    , ( "From", payload.from )
                    , ( "Body", payload.body )
                    ]
    resp <- HTTP.httpBS req
    if HTTP.getResponseStatus resp == HTTPStatus.created201 then
        pure ()
     else
        error "Invalid status"



-- data


peopleData :: [Person]
peopleData =
    [ ]


-- put it all together


main :: IO ()
main = do
    twilioSid <- Env.getEnv "TWILIO_SID" & fmap CBS.pack
    twilioToken <- Env.getEnv "TWILIO_TOKEN" & fmap CBS.pack
    twilioFrom <- Env.getEnv "TWILIO_FROM_NUMBER" & fmap CBS.pack

    assignments <- assignGifts peopleData
    let smsPayloads = assignmentsToSMSPayload assignments
        sendSMSApplied to body =
            sendSMS (TwilioAuth { sid = twilioSid, token = twilioToken })
                (SendSMSPayload { to = to, from = twilioFrom, body = body })
    -- print smsPayloads
    smsPayloads
        & Map.toList
        & mapM_ (\(person, body) -> sendSMSApplied person.phone body)

