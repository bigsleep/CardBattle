{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
module Battle.TypeSpec where

import qualified Test.Hspec as H (Spec, describe, it, shouldBe)

import Data.Aeson ((.=))
import qualified Data.Aeson as DA (Value(..), object, decode, toJSON, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LC (pack)
import qualified Codec.Binary.UTF8.String as UTF8 (encode)

import Battle.Types

spec :: H.Spec
spec = do
    testJSON "BattleState" battleStateJSON expectedBattleState (DA.object ["First" .= ([] :: [DA.Value]), "Second" .= ([] :: [DA.Value]), "OneTurnEffects" .= ([] :: [DA.Value]), "Effects" .= ([] :: [DA.Value]), "Turn" .= (1 :: Int)])

    testJSON "Card" cardJSON expectedCard (DA.object ["Name" .= DA.String "あああ", "Properties" .= DA.object ["MaxHp" .= (0 :: Int), "MaxMp" .= (0 :: Int), "Attack" .= (0 :: Int), "Defense" .= (0 :: Int), "Speed" .= (0 :: Int), "Magic" .= (0 :: Int)], "Skills" .= ([] :: [DA.Value])])

    testJSON "Skill" skillJSON expectedSkill (DA.object ["Name" .= DA.String "skill", "Action" .= DA.object ["Attack" .= (100 :: Int)], "Target" .= (0 :: Int)])

    testJSON "BattleEffect" battleEffectJSON expectedBattleEffect (DA.object ["Target" .= [(0 :: Int), (0 :: Int)], "Property" .= (0 :: Int), "Factor" .= (100 :: Int)])


battleStateJSON :: String
battleStateJSON = "{\"First\": [],\"Second\": [],\"OneTurnEffects\": [],\"Effects\": [],\"Turn\": 1}"

expectedBattleState :: BattleState
expectedBattleState = (BattleState [] [] [] [] 1)

cardJSON :: String
cardJSON = "{\"Name\":\"あああ\", \"Properties\":{\"MaxHp\":0,\"MaxMp\":0,\"Attack\":0,\"Defense\":0,\"Speed\":0,\"Magic\":0}, \"Skills\":[]}"

expectedCard :: Card
expectedCard = (Card "あああ" (PropertySet 0 0 0 0 0 0) [])

skillJSON :: String
skillJSON = "{\"Name\":\"skill\",\"Action\":{\"Attack\":100},\"Target\":0}}"

expectedSkill :: Skill
expectedSkill = Skill "skill" (Attack 100) TcAlmighty

battleEffectJSON :: String
battleEffectJSON = "{\"Target\":[0,0],\"Property\":0,\"Factor\":100}"

expectedBattleEffect :: BattleEffect
expectedBattleEffect = BattleEffect (FirstPlayer, 0) MaxHpTag 100

testJSON :: (DA.FromJSON a, DA.ToJSON a, Show a, Eq a) => String -> String -> a -> DA.Value -> H.Spec
testJSON name str x json = do
    H.it (name ++ ": decode") $ do
        let result = DA.decode . LC.pack . UTF8.encode $ str
        let expected = Just x
        result `H.shouldBe` expected

    H.it (name ++ ": encode") $ do
        let result = DA.toJSON x
        let expected = json
        result `H.shouldBe` expected

