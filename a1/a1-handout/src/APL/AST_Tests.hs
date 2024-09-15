module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      testCase "printExp CstInt" $
        printExp (CstInt 42) @?= "42",
      --
      testCase "printExp (CstBool True)" $
        printExp (CstBool True) @?= "True",
      --
      testCase "printExp (CstBool False)" $
        printExp (CstBool False) @?= "False",
      --
      testCase "printExp Add" $
        printExp (Add (CstInt 2) (CstInt 5)) @?= "(2 + 5)",
      --
      testCase "printExp Sub" $
        printExp (Sub (CstInt 2) (CstInt 5)) @?= "(2 - 5)",
      --
      testCase "printExp Mul" $
        printExp (Mul (CstInt 2) (CstInt 5)) @?= "(2 * 5)",
      --
      testCase "printExp Div" $
        printExp (Div (CstInt 2) (CstInt 5)) @?= "(2 / 5)",
      --
      testCase "printExp Pow" $
        printExp (Pow (CstInt 2) (CstInt 5)) @?= "(2 ** 5)",
      --
      testCase "printExp Eql" $
        printExp (Eql (CstInt 2) (CstInt 5)) @?= "(2 == 5)",
      --
      testCase "printExp If" $
        printExp (If (CstBool True) (CstInt 2) (CstInt 3)) @?= "(if True then 2 else 3)",
      --
      testCase "printExp Let" $
        printExp (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x")) @?= "(let x = (2 + 3) in x)",
      --
      testCase "printExp Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 1))) @?= "(\\x -> (x + 1))",
      --
      testCase "printExp Apply" $
        printExp (Let "f" (Lambda "a" (Var "a")) (Apply (Var "f") (Sub (CstInt 2) (CstInt 1)))) @?= "(let f = (\\a -> a) in (f(2 - 1)))",
      --
      testCase "printExp TryCatch" $
        printExp (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 0)) @?= "(try (1 / 0) catch 0)",
      --
      testCase "printExp Var" $
        printExp (Var "x") @?= "x"
    ]
