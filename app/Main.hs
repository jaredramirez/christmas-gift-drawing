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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import AssignGifts qualified
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Data.Base64
import Data.ByteString.Base64 qualified as ByteString.Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

main :: IO ()
main = do
    people <- BSL.readFile "people.json" >>= (either (error "Problem parsing people.json") pure . Aeson.eitherDecode @[AssignGifts.Person])
    assignments <- AssignGifts.assignGifts people
    assignments
        & Map.toList
        & mapM_
            ( \(person, gifts) -> do
                let payload = giftsToPayload person gifts
                    payloadJson = BSL.toStrict $ Aeson.encode payload
                putStr . Text.unpack $ person.name <> " : "
                putStrLn . Text.unpack $ baseUrl <> Data.Base64.extractBase64 (ByteString.Base64.encodeBase64 payloadJson)
            )

baseUrl :: Text
baseUrl = "https://omg.lol/@jaredramirez?data="

data Payload = Payload
    { recipient :: Text
    , small1 :: Text
    , small2 :: Text
    , big :: Text
    , book :: Text
    }
    deriving (Show, Generic, Aeson.ToJSON)

giftsToPayload :: AssignGifts.Person -> AssignGifts.Gifts -> Payload
giftsToPayload person gifts =
    Payload
        { recipient = person.name
        , small1 = gifts.small1.name
        , small2 = gifts.small2.name
        , big = gifts.big.name
        , book = gifts.book.name
        }
