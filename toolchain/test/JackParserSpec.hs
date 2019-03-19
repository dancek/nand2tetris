module JackParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec (parse)
import JackParser

spec :: Spec
spec = do
    describe "class" $ do
        it "empty body" $ do
            parse jackParser "" "class Foo {\n}"
                `shouldParse` JackClass "Foo" []

        it "1 static var" $ do
            parse jackParser ""
                "class Foo {\n\
                \    static int foo;\n\
                \}"
                `shouldParse`
                JackClass "Foo" [StaticDec (VarDec IntType "foo")]
