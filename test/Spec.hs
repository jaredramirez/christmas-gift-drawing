{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import AssignGifts qualified

import Control.Monad (forM_, when)

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec (shouldBe)
import Test.Hspec qualified as H
import Test.Hspec.QuickCheck qualified as QC
import Test.QuickCheck qualified as QC

main :: IO ()
main = H.hspec $ do
    H.describe "assignGiftsWithSeed" $ do
        let people = map (AssignGifts.Person . Text.pack . show) [1 :: Int .. 8]
            recipients gifts = [gifts.small1, gifts.small2, gifts.big, gifts.book]
        H.it "repeats a draw independently of intervening draws" $ do
            first <- AssignGifts.assignGiftsWithSeed 42 Map.empty people
            _ <- AssignGifts.assignGiftsWithSeed 123 Map.empty people
            repeated <- AssignGifts.assignGiftsWithSeed 42 Map.empty people
            repeated `shouldBe` first
        H.it "uses the seed to vary the draw" $ do
            first <- AssignGifts.assignGiftsWithSeed 42 Map.empty people
            other <- AssignGifts.assignGiftsWithSeed 123 Map.empty people
            first `H.shouldNotBe` other
        H.it "preserves gift assignment constraints across seeds" $ do
            forM_ [-10 .. 10] $ \seed -> do
                assignments <- AssignGifts.assignGiftsWithSeed seed Map.empty people
                Map.keys assignments `shouldBe` people
                forM_ (Map.toList assignments) $ \(giver, gifts) -> do
                    let receivers = recipients gifts
                    length (List.nub receivers) `shouldBe` 4
                    receivers `H.shouldNotContain` [giver]
                forM_ [(.small1), (.small2), (.big), (.book)] $ \giftType ->
                    List.sort (map giftType (Map.elems assignments)) `shouldBe` people
        H.it "avoids repeating a previous draw" $ do
            let morePeople = map (AssignGifts.Person . Text.pack . show) [1 :: Int .. 9]
            prevAssignments <- AssignGifts.assignGiftsWithSeed 0 Map.empty (take 8 morePeople)
            let previous =
                    Map.fromList
                        [ (giver.name, AssignGifts.giftsToGiftsText gifts)
                        | (giver, gifts) <- Map.toList prevAssignments
                        ]
            forM_ [1 .. 5] $ \seed -> do
                assignments <- AssignGifts.assignGiftsWithSeed seed previous morePeople
                forM_ (Map.toList assignments) $ \(giver, gifts) ->
                    forM_ (Map.lookup giver.name previous) $ \prevGifts -> do
                        let current = AssignGifts.giftsToGiftsText gifts
                        forM_ [(.small1), (.small2), (.big), (.book)] $ \giftType ->
                            giftType current `H.shouldNotBe` giftType prevGifts
                        let repeats = filter (`elem` recipients current) (recipients prevGifts)
                        length repeats `H.shouldSatisfy` (<= AssignGifts.maxRepeatRecipients)

    H.describe "makePairs" $ do
        H.it "should suceed" $ do
            AssignGifts.makePairs @Int [1, 2, 3]
                `shouldBe` [(1, 2), (2, 3), (3, 1)]

    H.describe "makeRandomizedPairs'" $ do
        H.describe "unit" $ do
            H.it "each item should appear once in the first position and once in the second" $ do
                let items = [1, 2, 3]
                randomized <- AssignGifts.makeRandomizedPairs' @Int items []

                let firsts = map fst randomized
                    seconds = map snd randomized

                forM_ items $ \item -> do
                    length (filter (== item) firsts) `shouldBe` 1
                    length (filter (== item) seconds) `shouldBe` 1
            H.it "should exclude specified pairs" $ do
                let items = [1, 2, 3]
                randomized <- AssignGifts.makeRandomizedPairs' @Int items [(1, 3)]

                let firsts = map fst randomized
                    seconds = map snd randomized

                forM_ items $ \item -> do
                    length (filter (== item) firsts) `shouldBe` 1
                    length (filter (== item) seconds) `shouldBe` 1
        H.describe "property" $ do
            QC.prop "each item should appear once in the first position and once in the second" $
                \items -> do
                    let uniqItems = List.nub items
                    when (length uniqItems < 2) QC.discard

                    randomized <- AssignGifts.makeRandomizedPairs' @Int uniqItems []

                    let firsts = map fst randomized
                        seconds = map snd randomized

                    forM_ items $ \item -> do
                        length (filter (== item) firsts) `shouldBe` 1
                        length (filter (== item) seconds) `shouldBe` 1

    H.describe "makeRandomizedPairs" $ do
        H.describe "unit" $ do
            H.it "each item should appear once in the first position and once in the second" $ do
                let items = [1, 2, 3]
                randomized <- AssignGifts.makeRandomizedPairs @Int @Int id items []

                let firsts = map fst randomized
                    seconds = map snd randomized

                forM_ items $ \item -> do
                    length (filter (== item) firsts) `shouldBe` 1
                    length (filter (== item) seconds) `shouldBe` 1
        H.describe "property" $ do
            QC.prop "each item should appear once in the first position and once in the second" $
                \items -> do
                    let uniqItems = List.nub items
                    when (length uniqItems < 2) QC.discard

                    randomized <- AssignGifts.makeRandomizedPairs @Int @Int id uniqItems []

                    let firsts = map fst randomized
                        seconds = map snd randomized

                    forM_ items $ \item -> do
                        length (filter (== item) firsts) `shouldBe` 1
                        length (filter (== item) seconds) `shouldBe` 1
