module LogAnalysisTest where

  import Test.Hspec
  import Test.QuickCheck
  import Control.Exception (evaluate)
  import LogAnalysis
  import Log

  main :: IO ()
  main = hspec $ do

    describe "parseMessage" $ do
      it "should return   LogMessage (Error 2) 562 help help for E 2 562 help help" $ do
        parseMessage "E 2 562 help help" `shouldBe`  LogMessage (Error 2) 562 "help help"

      it "should return LogMessage Info 29 la la la for I 29 la la la" $ do
        parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

      it "should return Unknown This is not in the right format for This is not in the right format" $ do
        parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

      it "should return Warning 5 Flange is due for a check-up for W 5 Flange is due for a check-up" $ do
        parseMessage "W 5 Flange is due for a check-up" `shouldBe` LogMessage Warning 5 "Flange is due for a check-up"

    describe "parse" $ do

      it "should create Logmessage for the given string" $ do
        parse "E 2 562 help help\nI 29 la la la" `shouldBe` [LogMessage (Error 2) 562 "help help", LogMessage Info 29 "la la la"]
