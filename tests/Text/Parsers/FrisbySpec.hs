{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Text.Parsers.FrisbySpec (spec) where

import qualified Data.Semigroup as Semigroup
import Prelude hiding ((<>))
import Test.Hspec
import Control.Exception (evaluate)
import Data.MonoTraversable
import Data.Sequences
import Data.Typeable
import Text.Printf (IsChar(toChar,fromChar))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Parsers.Frisby hiding (runPeg)
import qualified Text.Parsers.Frisby as F
import Data.Functor (void)
import Data.Word (Word8)
import Data.String (IsString)

tests :: forall mono. (IsSequence mono, IsString mono,
                      Show mono, Eq mono, Typeable mono,
                      IsChar (Element mono),
                      Show (Element mono), Enum (Element mono), Eq (Element mono)) => Spec
tests = do
  let runPeg :: (forall s . PM s (P s mono a)) -> mono -> a
      runPeg = F.runPeg @mono
  
  describe (show $ typeRep (Proxy @mono)) $ do
    describe "Basic Parser Operations" $ do
      describe "char" $ do
        it "matches a single character" $ do
          runPeg (return $ void $ char 'a') "a" `shouldBe` ()
          evaluate (runPeg (return $ void $ char 'a') "b") 
            `shouldThrow` anyException

      describe "text" $ do
        it "matches exact text" $ do
          runPeg (return $ void $ text "hello") "hello" `shouldBe` ()
          evaluate (runPeg (return $ void $ text "hello") "hell") 
            `shouldThrow` anyException

      describe "anyChar" $ do
        it "matches any single character" $ do
          runPeg (return anyChar) "x" `shouldBe` fromChar 'x'
          evaluate (runPeg (return anyChar) "") 
            `shouldThrow` anyException

      describe "eof/bof" $ do
        it "detects start and end of input" $ do
          runPeg (return $ void bof) "" `shouldBe` ()
          runPeg (return $ void eof) "" `shouldBe` ()
          runPeg (return $ void $ text "abc" ->> eof) "abc" `shouldBe` ()

    describe "Combinators" $ do
      describe "sequence operations" $ do
        it "combines parsers with <>" $ do
          let parser = runPeg $ return $ void $ char 'a' <> char 'b'
          parser "ab" `shouldBe` ()
          evaluate (parser "ac") 
            `shouldThrow` anyException

      describe "choice operations" $ do
        it "handles alternative with //" $ do
          let parser = runPeg $ return $ (char 'a' ->> unit True) // (char 'b' ->> unit False)
          parser "a" `shouldBe` True
          parser "b" `shouldBe` False

    describe "Repetition" $ do
      describe "many" $ do
        it "matches zero or more occurrences" $ do
          let parser = runPeg $ return $ many (void $ char 'a') ## length
          parser "" `shouldBe` 0
          parser "aaa" `shouldBe` 3

      describe "many1" $ do
        it "matches one or more occurrences" $ do
          let parser = runPeg $ return $ many1 (void $ char 'a') ## length
          parser "aaa" `shouldBe` 3
          evaluate (parser "") `shouldThrow` anyException

    describe "Character Classes" $ do
      describe "oneOf" $ do
        it "matches one of given characters" $ do
          let parser :: mono -> Element mono
              parser = runPeg $ return $ oneOf ['a', 'b', 'c']
          parser "a" `shouldBe` fromChar 'a'
          evaluate (parser "d") `shouldThrow` anyException

    describe "Regular Expressions" $ do
      it "handles basic patterns" $ do
        runPeg (regex "a+b") "aaab" `shouldBe` "aaab"

      it "handles character classes" $ do
        runPeg (regex "[abc]+") "abcabc" `shouldBe` "abcabc"
    
    describe "Complex Parsers" $ do
      it "parses nested structures" $ do
        let 
            parser = runPeg $ mdo
              expr <- newRule $ between (char '(') (char ')') expr // 
                              many1 (oneOf ['0'..'9']) ## id
              return expr
        parser "(123)" `shouldBe` fromChar <$> "123"
        parser "((123))" `shouldBe` fromChar <$> "123"

      it "handles sequential composition" $ do
        let parser = runPeg $ mdo
              num <- newRule $ many1 (oneOf ['0'..'9']) ## id
              return $ num <> char '+' <> num ## \((a,_),b) -> a Semigroup.<> b
        parser "123+456" `shouldBe` fromChar <$> "123456"

spec :: Spec
spec = do
  tests @String
  tests @BS.ByteString
  tests @BL.ByteString
  tests @TS.Text  
  tests @TL.Text

instance IsChar Word8 where
  fromChar = fromIntegral . fromEnum
  toChar = toEnum . fromIntegral
