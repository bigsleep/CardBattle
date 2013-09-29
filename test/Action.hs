module ActionSpec where

import Test.Hspec
import Battle.Action
import TestUtil

spec :: Spec
spec = do
    describe "a test"
    it "add int" $ do
        (1 + 1) `shoudeBe` (2 :: Int)
