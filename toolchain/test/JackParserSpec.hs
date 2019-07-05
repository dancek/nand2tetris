{-# LANGUAGE QuasiQuotes #-}

module JackParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.RawString.QQ (r)

import Text.Megaparsec (parse)
import JackParser

-- helper for running individual parsers with proper space consumer etc
expectParse parser input expected =
  parse (wrapParser parser) "" input
    `shouldParse` expected

spec :: Spec
spec = do
  describe "class" $ do
    it "empty body" $
      expectParse jackClass "class Foo {}" $
        JackClass "Foo" [] []

    it "1 static var" $
      expectParse jackClass
        [r|
          class Foo {
            static int foo;
          }
        |] $ JackClass "Foo"
              [StaticDec (VarDec IntType ["foo"])]
              []

    it "multi-declaration" $
      expectParse jackClass
        [r|
          class Foo {
            static int foo, bar, baz;
          }
        |] $ JackClass "Foo"
              [StaticDec (VarDec IntType ["foo", "bar", "baz"])]
              []

    it "fields, class types" $
      expectParse jackClass
        [r|
          class Foo {
            field boolean bar;
            field AnotherClass baz;
          }
        |] $ JackClass "Foo"
              [ FieldDec (VarDec BooleanType ["bar"])
              , FieldDec (VarDec (ClassType "AnotherClass") ["baz"])
              ]
              []

    it "empty subroutine" $
      expectParse jackClass
        [r|
          class Foo {
            method void noop() {
              return;
            }
          }
        |] $ JackClass "Foo"
              []
              [ SubroutineDec VoidType "noop" []
                (SubroutineBody [] [ReturnStatement])
              ]


  describe "subroutine" $ do
    it "no-op" $
      expectParse subroutineDec
        "method void noop() {return;}" $
        SubroutineDec VoidType "noop" [] $
          SubroutineBody [] [ReturnStatement]
