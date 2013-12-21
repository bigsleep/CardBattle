{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
module Battle.TypeSpec where

import qualified Test.Hspec as H (Spec(..), describe, it, shouldBe)
import Test.QuickCheck (Positive(..))
import qualified Test.QuickCheck.Property as P

import Data.Aeson ((.=))
import qualified Data.Aeson as DA (Value(..), object, encode, decode, toJSON)
import qualified Data.Text as T (Text(..), pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LC (pack, unpack)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Codec.Binary.UTF8.String as U8 (encodeString, decodeString)

import Battle.Types

spec :: H.Spec
spec = do
    H.describe "TypeSpec" $ do

    H.it "JSON to BattleState" $ do
        let result = DA.decode . LC.pack $ battleStateJSON
        let expected = Just expectedBattleState
        result `H.shouldBe` expected

    H.it "BattleState to JSON" $ do
        let result = DA.toJSON expectedBattleState
        let expected = DA.object ["First" .= ([] :: [DA.Value]), "Second" .= ([] :: [DA.Value]), "OneTurnEffects" .= ([] :: [DA.Value]), "Effects" .= ([] :: [DA.Value]), "Turn" .= (1 :: Int)]
        result `H.shouldBe` expected

    H.it "JSON to Card" $ do
        let result = DA.decode . LC.pack $ cardJSON :: Maybe Card
        let expected = Just expectedCard
        result `H.shouldBe` expected

    H.it "Card to JSON" $ do
        let result = DA.toJSON expectedCard
        let expected = DA.object ["Name" .= DA.String (T.decodeUtf8 . BS.pack $ "あああ"), "Properties" .= DA.object ["MaxHp" .= (0 :: Int), "MaxMp" .= (0 :: Int), "Attack" .= (0 :: Int), "Defense" .= (0 :: Int), "Speed" .= (0 :: Int), "Magic" .= (0 :: Int)], "Skills" .= ([] :: [DA.Value])]
        result `H.shouldBe` expected


    H.it "JSON to Skill" $ do
        let result = DA.decode . LC.pack $ skillJSON :: Maybe Skill
        let expected = Just expectedSkill
        result `H.shouldBe` expected

    H.it "Skill to JSON" $ do
        let result = DA.toJSON expectedSkill
        let expected = DA.object ["Action" .= DA.object ["Attack" .= (100 :: Int)], "Target" .= DA.object ["TargetCapacityOpponent" .= ([] :: [DA.Value])]]
        result `H.shouldBe` expected

    H.it "JSON to BattleEffect" $ do
        let result = DA.decode . LC.pack $ battleEffectJSON
        let expected = Just expectedBattleEffect
        result `H.shouldBe` expected

    H.it "BattleEffect to JSON" $ do
        let result = DA.toJSON expectedBattleEffect
        let expected = DA.object ["Target" .= [(0 :: Int), (0 :: Int)], "Property" .= (0 :: Int), "Factor" .= (100 :: Int)]
        result `H.shouldBe` expected



battleStateJSON :: String
battleStateJSON = "{\"First\": [],\"Second\": [],\"OneTurnEffects\": [],\"Effects\": [],\"Turn\": 1}"

expectedBattleState :: BattleState
expectedBattleState = (BattleState [] [] [] [] 1)

cardJSON :: String
cardJSON = "{\"Name\":\"あああ\", \"Properties\":{\"MaxHp\":0,\"MaxMp\":0,\"Attack\":0,\"Defense\":0,\"Speed\":0,\"Magic\":0}, \"Skills\":[]}"

expectedCard :: Card
expectedCard = (Card (BS.pack "あああ") (PropertySet 0 0 0 0 0 0) [])

skillJSON :: String
skillJSON = "{\"Action\":{\"Attack\":100},\"Target\":{\"TargetCapacityOpponent\":[]}}"

expectedSkill :: Skill
expectedSkill = Skill (Attack 100) TargetCapacityOpponent

battleEffectJSON :: String
battleEffectJSON = "{\"Target\":[0,0],\"Property\":0,\"Factor\":100}"

expectedBattleEffect :: BattleEffect
expectedBattleEffect = BattleEffect (FirstPlayer, 0) MaxHpTag 100
