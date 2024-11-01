module Main (main) where

import AssignGifts qualified

import Control.Monad (forM_, when)

import Data.List qualified as List
import Test.Hspec (shouldBe)
import Test.Hspec qualified as H
import Test.Hspec.QuickCheck qualified as QC
import Test.QuickCheck qualified as QC

import Debug.Trace

main :: IO ()
main = H.hspec $ do
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
