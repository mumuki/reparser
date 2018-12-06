{-# LANGUAGE RankNTypes #-}

module SplitterSpec (spec) where

import           Test.Hspec
import           Text.Splitter

spec :: Spec
spec = do
  describe "reparse" $ do
    it "splits empty strings" $ do
      split "" `shouldBe` Right []

    it "splits begin..end string" $ do
      split "/*<begin#*/strings/*#end>*/" `shouldBe` Right ["strings"]

    it "splits begin..end string with trailing content" $ do
      split "/*<begin#*/strings/*#end>*/trailing" `shouldBe` Right ["strings"]

    it "splits begin..end string with leading content" $ do
      split "leading/*<begin#*/strings/*#end>*/" `shouldBe` Right ["strings"]

    it "splits multiple begin..end string" $ do
      split "/*<begin#*/strings/*#end>*//*<begin#*/strongs/*#end>*/" `shouldBe` Right ["strings", "strongs"]

    it "splits multiple begin..end string strings with spaces and spureuos content" $ do
      split "/*<begin#*/strings for all/*#end>*/trailing/*<begin#*/strongs/*#end>*/" `shouldBe` Right ["strings for all", "strongs"]

    it "splits incomplete sections" $ do
      split "/*<begin#*/strings/*#end>*/aaaa/*<begin#*/moar" `shouldBe` Right ["strings"]
