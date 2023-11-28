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
{-# LANGUAGE UndecidableInstances #-}

module Brevo (Recipient (..), SecretSantaTempleteParams (..), send) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Status as HTTPStatus

data RequestBody params = RequestBody
    { templateId :: Int
    , messageVersions :: [RequestMessageVersion params]
    }
    deriving (Show, Generic, ToJSON)

data RequestMessageVersion params = RequestMessageVersion
    { to :: [RequestTo]
    , params :: params
    }
    deriving (Show, Generic, ToJSON)

newtype RequestTo = RequestTo
    { email :: Text
    }
    deriving (Show, Generic, ToJSON)

data Recipient params = Recipient
    { email :: Text
    , params :: params
    }
    deriving (Show)

-- | Send a email with customizations via Brevo
send ::
    (ToJSON params) =>
    -- | API Key
    ByteString ->
    -- | Template ID
    Int ->
    -- | Recipients
    [Recipient params] ->
    IO ()
send apiKey templateId recipients =
    let initReq = HTTP.parseRequestThrow_ "POST https://api.brevo.com/v3/smtp/email"
        req =
            initReq
                & HTTP.addRequestHeader "api-key" apiKey
                & HTTP.setRequestBodyJSON
                    ( RequestBody
                        { templateId = templateId
                        , messageVersions =
                            map
                                ( \recipient ->
                                    RequestMessageVersion
                                        { to = [RequestTo{email = recipient.email}]
                                        , params = recipient.params
                                        }
                                )
                                recipients
                        }
                    )
     in void $ HTTP.httpBS req

data SecretSantaTempleteParams = SecretSantaTempleteParams
    { you :: Text
    , bigGift :: Text
    , smallGift1 :: Text
    , smallGift2 :: Text
    , bookGift :: Text
    , otherSmallGift1 :: Text
    , otherSmallGift2 :: Text
    }
    deriving (Show, Generic, ToJSON)
