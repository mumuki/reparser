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
      split "/*<foo#*/strings/*#end>*/" `shouldBe` Right [["foo", "strings"]]

    it "splits begin..end string with trailing content" $ do
      split "/*<foo#*/strings/*#end>*/trailing" `shouldBe` Right [["foo", "strings"]]

    it "splits begin..end string with leading content" $ do
      split "leading/*<foo#*/strings/*#end>*/" `shouldBe` Right [["foo", "strings"]]

    it "splits multiple begin..end string" $ do
      split "/*<foo#*/strings/*#end>*//*<bar#*/strongs/*#end>*/" `shouldBe` Right [["foo", "strings"], ["bar", "strongs"]]

    it "splits multiple begin..end string strings with spaces and spureuos content" $ do
      split "/*<foo#*/strings for all/*#end>*/trailing/*<bar#*/strongs/*#end>*/" `shouldBe` Right [["foo", "strings for all"], ["bar", "strongs"]]

    it "splits incomplete sections" $ do
      split "/*<foo#*/strings/*#end>*/aaaa/*<begin#*/moar" `shouldBe` Right [["foo","strings"]]

