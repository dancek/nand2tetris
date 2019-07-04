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
                `shouldParse` JackClass "Foo" [] []

        it "1 static var" $ do
            parse jackParser ""
                "class Foo {\n\
                \    static int foo;\n\
                \}"
                `shouldParse`
                JackClass "Foo"
                    [StaticDec (VarDec IntType ["foo"])]
                    []

        it "multi-declaration" $ do
            parse jackParser ""
                "class Foo {\n\
                \    static int foo, bar, baz;\n\
                \}"
                `shouldParse`
                JackClass "Foo"
                    [StaticDec (VarDec IntType ["foo", "bar", "baz"])]
                    []

        it "fields, class types" $ do
            parse jackParser ""
                "class Foo {\n\
                \    field boolean bar;\n\
                \    field AnotherClass baz;\n\
                \}"
                `shouldParse`
                JackClass "Foo"
                    [ FieldDec (VarDec BooleanType ["bar"])
                    , FieldDec (VarDec (ClassType "AnotherClass") ["baz"])
                    ]
                    []

        it "empty subroutine" $ do
            parse jackParser ""
                "class Foo {\n\
                \    method void noop() {\n\
                \        return;\n\
                \    }\n\
                \}"
                `shouldParse`
                JackClass "Foo"
                    []
                    [ SubroutineDec VoidType "noop" []
                        (SubroutineBody [] [ReturnStatement])
                    ]
