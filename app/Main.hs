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
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Options.Applicative qualified as Options

data Args = Args
    { seed :: Maybe Int
    , previous :: Maybe FilePath
    }

main :: IO ()
main = do
    args <- Options.execParser options
    people <- BSL.readFile "people.json" >>= (either (error "Problem parsing people.json") pure . Aeson.eitherDecode @[AssignGifts.Person])
    previous <- maybe (pure Map.empty) (readPrevious people) args.previous
    assignments <- maybe AssignGifts.assignGifts AssignGifts.assignGiftsWithSeed args.seed previous people
    assignments
        & Map.toList
        & mapM_
            ( \(person, gifts) -> do
                let payload = giftsToPayload person gifts
                    payloadJson = BSL.toStrict $ Aeson.encode payload
                putStr . Text.unpack $ person.name <> " : "
                putStrLn . Text.unpack $ baseUrl <> Data.Base64.extractBase64 (ByteString.Base64.encodeBase64 payloadJson)
            )

{- | Read a previous draw, failing if it names anyone not in people.json so a
misspelled name can't silently skip a constraint.
-}
readPrevious :: [AssignGifts.Person] -> FilePath -> IO AssignGifts.PreviousGifts
readPrevious people path = do
    previous <- BSL.readFile path >>= (either (\err -> error ("Problem parsing " <> path <> ": " <> err)) pure . Aeson.eitherDecode @AssignGifts.PreviousGifts)
    let names = map (.name) people
        previousNames = Map.keys previous <> concatMap AssignGifts.giftRecipients (Map.elems previous)
    case filter (`notElem` names) previousNames of
        [] -> pure previous
        unknown -> error ("Names in " <> path <> " not in people.json: " <> show (List.nub unknown))

options :: Options.ParserInfo Args
options =
    Options.info
        ( ( Args
                <$> Options.optional
                    ( Options.option
                        Options.auto
                        ( Options.long "seed"
                            <> Options.metavar "INTEGER"
                            <> Options.help "Reproduce a draw with an integer seed"
                        )
                    )
                <*> Options.optional
                    ( Options.strOption
                        ( Options.long "previous"
                            <> Options.metavar "FILE"
                            <> Options.help "A previous draw to avoid repeating, as JSON mapping each giver to their small1, small2, big, and book recipients"
                        )
                    )
          )
            Options.<**> Options.helper
        )
        (Options.fullDesc <> Options.progDesc "Draw gift assignments from people.json")

baseUrl :: Text
baseUrl = "https://2026.secret-santa.jaredramirez.omg.lol/?data="

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
