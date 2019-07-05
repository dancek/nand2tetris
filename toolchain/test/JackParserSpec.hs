module JackParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

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
            expectParse jackClass "class Foo {\n}" $ JackClass "Foo" [] []

        it "1 static var" $
            expectParse jackClass
                "class Foo {\n\
                \    static int foo;\n\
                \}" $
                JackClass "Foo"
                    [StaticDec (VarDec IntType ["foo"])]
                    []

        it "multi-declaration" $
            expectParse jackClass
                "class Foo {\n\
                \    static int foo, bar, baz;\n\
                \}" $
                JackClass "Foo"
                    [StaticDec (VarDec IntType ["foo", "bar", "baz"])]
                    []

        it "fields, class types" $
            expectParse jackClass
                "class Foo {\n\
                \    field boolean bar;\n\
                \    field AnotherClass baz;\n\
                \}" $
                JackClass "Foo"
                    [ FieldDec (VarDec BooleanType ["bar"])
                    , FieldDec (VarDec (ClassType "AnotherClass") ["baz"])
                    ]
                    []

        it "empty subroutine" $
            expectParse jackClass
                "class Foo {\n\
                \    method void noop() {\n\
                \        return;\n\
                \    }\n\
                \}" $
                JackClass "Foo"
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
