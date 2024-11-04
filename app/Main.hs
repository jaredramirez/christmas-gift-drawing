{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import AssignGifts qualified
import Brevo qualified
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Text qualified as Text
import System.Environment qualified as Env


main :: IO ()
main = do
    brevoApiKey <- Env.getEnv "BREVO_API_KEY" & fmap BSC.pack
    people <-
        BSL.readFile "people.json"
            >>= ( either (error "Problem parsing people.json") pure
                    . Aeson.eitherDecode @[AssignGifts.Person]
                )
    assignments <- AssignGifts.assignGifts people
    let eitherRecipients = AssignGifts.asssignmentsToBrevoRecipients assignments
    case eitherRecipients of
        Left errMsg -> do
            putStrLn "Failed to convert gift recipients to Brevo recipients"
            putStrLn $ Text.unpack errMsg
        Right recipients -> do
            let brevoEmailTemplateKey = 4
            Brevo.send brevoApiKey brevoEmailTemplateKey recipients
            BSL.writeFile "gifts.json" $ Aeson.encode assignments
