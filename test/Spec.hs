{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec (main) where

import AssignGifts qualified

import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Test.Hspec (shouldBe)
import Test.Hspec qualified as H
import Test.Hspec.QuickCheck qualified as QC
import Test.QuickCheck qualified as QC


--------------------------------------------------------------------------------
-- Test

main :: IO ()
main = H.hspec $ do
    H.describe "makeRandomizedPairs" do
        H.describe "each item should appear once in the first position and once in the second" do
            H.it "unit" do
                let items = [1, 2, 3]
                randomized <- AssignGifts.makeRandomizedPairs @Int items []

                let firsts = map fst randomized
                    seconds = map snd randomized

                forM_ items $ \item -> do
                    length (filter (== item) firsts) `shouldBe` 1
                    length (filter (== item) seconds) `shouldBe` 1

            QC.prop "property" $
                \items -> do
                    let uniqItems = List.nub items
                    when (length uniqItems < 2) QC.discard

                    randomized <- AssignGifts.makeRandomizedPairs @Int uniqItems []

                    let firsts = map fst randomized
                        seconds = map snd randomized

                    forM_ items $ \item -> do
                        length (filter (== item) firsts) `shouldBe` 1
                        length (filter (== item) seconds) `shouldBe` 1

    H.describe "assignGifts" do
        H.describe "each person should appear in the gifter list and exactly once in each giftee list " do
            H.it "unit" do
                let persons = ["Caleb", "Sophia", "Jared", "Sam", "Alex"]
                gifts <- AssignGifts.assignGifts persons
                let giftRecipients = Map.elems gifts
                    bigGiftRecipients = map (.big) giftRecipients
                    small1GiftRecipients = map (.small1) giftRecipients
                    small2GiftRecipients = map (.small2) giftRecipients
                    bookGiftRecipients = map (.book) giftRecipients

                forM_ persons \person -> do
                    Map.member person gifts `shouldBe` True
                    let isInListExactlyOnce !recipients =
                            recipients
                                & List.filter (== person)
                                & length
                                & (== 1)
                    isInListExactlyOnce bigGiftRecipients `shouldBe` True
                    isInListExactlyOnce small1GiftRecipients `shouldBe` True
                    isInListExactlyOnce small2GiftRecipients `shouldBe` True
                    isInListExactlyOnce bookGiftRecipients `shouldBe` True

            QC.prop "property" \propPersons -> do
                let persons = List.nub @String propPersons
                when (length persons < 5) QC.discard

                gifts <- AssignGifts.assignGifts persons
                let giftRecipients = Map.elems gifts
                    bigGiftRecipients = map (.big) giftRecipients
                    small1GiftRecipients = map (.small1) giftRecipients
                    small2GiftRecipients = map (.small2) giftRecipients
                    bookGiftRecipients = map (.book) giftRecipients

                forM_ persons \person -> do
                    Map.member person gifts `shouldBe` True
                    let isInListExactlyOnce !recipients =
                            recipients
                                & List.filter (== person)
                                & length
                                & (== 1)
                    isInListExactlyOnce bigGiftRecipients `shouldBe` True
                    isInListExactlyOnce small1GiftRecipients `shouldBe` True
                    isInListExactlyOnce small2GiftRecipients `shouldBe` True
                    isInListExactlyOnce bookGiftRecipients `shouldBe` True

    H.describe "getOtherSmallGiftGiver" do
        H.it "1" do
            let jaredsGiftRecipients =
                    AssignGifts.Gifts
                        { AssignGifts.small1 = "Caleb"
                        , AssignGifts.small2 = "A"
                        , AssignGifts.big = "B"
                        , AssignGifts.book = "C"
                        }

                gifts =
                    [ ("Jared", jaredsGiftRecipients)
                    ,
                        ( "Sophia"
                        , AssignGifts.Gifts
                            { AssignGifts.small1 = "D"
                            , AssignGifts.small2 = "Caleb"
                            , AssignGifts.big = "E"
                            , AssignGifts.book = "F"
                            }
                        )
                    ]
            AssignGifts.getOtherSmall1GiftGiver jaredsGiftRecipients gifts
                `shouldBe` Just "Sophia"

        H.it "2" do
            let jaredsGiftRecipients =
                    AssignGifts.Gifts
                        { AssignGifts.small1 = "A"
                        , AssignGifts.small2 = "Sam"
                        , AssignGifts.big = "B"
                        , AssignGifts.book = "C"
                        }

                gifts =
                    [ ("Jared", jaredsGiftRecipients)
                    ,
                        ( "Alex"
                        , AssignGifts.Gifts
                            { AssignGifts.small1 = "Sam"
                            , AssignGifts.small2 = "D"
                            , AssignGifts.big = "E"
                            , AssignGifts.book = "F"
                            }
                        )
                    ]
            AssignGifts.getOtherSmall2GiftGiver jaredsGiftRecipients gifts
                `shouldBe` Just "Alex"
