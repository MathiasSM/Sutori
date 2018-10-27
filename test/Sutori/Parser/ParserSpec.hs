module Sutori.Parser.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Sutori.Parser  (parseExpression)
import Sutori.Lexer   (runLexer')
import Sutori.AST     (SutExpression(..), SutLiteral(..), SutConstructor(..))
import Sutori.Types   (SutType(..), SutPrimitive(..))


parseExpression' s = let Right ((result, _), _) = runLexer' s parseExpression in result

spec :: Spec
spec =
  describe "Sutori.Parser" $ do

    describe "parseExpression" $ do

      context "When parsing a literal value" $ do

        it "parses a Literal True (on)" $
          let input = "on"
              expected = ExprLiteral (SutPrimitiveType SutLight) (SutBool True)
           in parseExpression' input `shouldBe` expected

        it "parses a Literal False (off)" $
          let input = "off"
              expected = ExprLiteral (SutPrimitiveType SutLight) (SutBool False)
           in parseExpression' input `shouldBe` expected

        it "parses a Literal Char" $
          let input = "'a'"
              expected = ExprLiteral (SutPrimitiveType SutLetter) (SutChar "a")
           in parseExpression' input `shouldBe` expected

        it "parses a Literal Int" $
          let input = "1"
              expected = ExprLiteral (SutPrimitiveType SutBag) (SutInt 1)
           in parseExpression' input `shouldBe` expected

        it "parses a Literal Float" $
          let input = "1.0"
              expected = ExprLiteral (SutPrimitiveType SutWallet) (SutFloat 1.0)
           in parseExpression' input `shouldBe` expected

        it "parses a Literal String" $
          let input = "\"I am a string\""
              expected = ExprLiteral (SutPrimitiveType SutPhrase) (SutString "I am a string")
           in parseExpression' input `shouldBe` expected


      context "When parsing a constructed value" $ do

        it "parses an array of Int" $
          let input = "[1; 2]"
              expected = ExprConstructor (SutChain 2 5)
                          (SutArray [ ExprLiteral (SutPrimitiveType SutBag) (SutInt 1),
                                      ExprLiteral (SutPrimitiveType SutBag) (SutInt 2)])
           in parseExpression' input `shouldBe` expected

        it "parses an array of String" $
          let input = "[\"Hi\"; \"St.\"]"
              expected = ExprConstructor (SutChain 2 7)
                          (SutArray [ ExprLiteral (SutPrimitiveType SutPhrase) (SutString "Hi"),
                                      ExprLiteral (SutPrimitiveType SutPhrase) (SutString "St.")])
           in parseExpression' input `shouldBe` expected

        it "fails parsing an empty Array" $
          let input = "[]"
           in evaluate( parseExpression' input) `shouldThrow` anyException

        it "parses a singleton Struct" $
          let input = "{ a: 4 }"
              expected = ExprConstructor (SutMachine [("a",5)])
                          (SutStruct [("a", ExprLiteral (SutPrimitiveType SutBag) (SutInt 4))])
           in parseExpression' input `shouldBe` expected

        it "parses a Struct with some members" $
          let input = "{ a: 4.5; b: \"str\" }"
              expected = ExprConstructor (SutMachine [("a",6), ("b", 7)])
                          (SutStruct [("a", ExprLiteral (SutPrimitiveType SutWallet) (SutFloat 4.5)),
                                      ("b", ExprLiteral (SutPrimitiveType SutPhrase) (SutString "str"))])
           in parseExpression' input `shouldBe` expected



    describe "parseType" $ do

      it "returns the first element of a list" $
        head [23 ..] `shouldBe` (23 :: Int)

      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)

      it "throws an exception if used with an empty list" $
        evaluate (head []) `shouldThrow` anyException
