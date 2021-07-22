-- ~\~ language=Haskell filename=test/HSpec.hs
-- ~\~ begin <<lit/milkshake.md|test/HSpec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module HSpec (spec) where

import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "sanity" $ do
        it "1 + 1 == 2" $ do
            1 + 1 `shouldBe` (2 :: Int)
        it "2 + 2 != 5" $ do
            2 + 2 `shouldNotBe` 5
-- ~\~ end
