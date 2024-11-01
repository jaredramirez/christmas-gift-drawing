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

module Main (main) where

import AssignGifts qualified
import Brevo qualified
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as CBS
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import System.Environment qualified as Env

main :: IO ()
main = do
    brevoApiKey <- Env.getEnv "BREVO_API_KEY" & fmap CBS.pack
    people <- BSL.readFile "people.json" >>= (either (error "Problem parsing people.json") pure . Aeson.eitherDecode @[AssignGifts.Person])
    assignments <- AssignGifts.assignGifts people
    let recipients = AssignGifts.asssignmentsToEmailRecipients assignments
    Brevo.send brevoApiKey 4 recipients
