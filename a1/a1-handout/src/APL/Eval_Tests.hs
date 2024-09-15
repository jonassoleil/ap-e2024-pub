module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval, fact)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right
            (ValBool True),
      --
      testCase "Lambda" $
        eval
          envEmpty
          ( Let
              "x"
              (CstInt 2)
              (Lambda "y" (Add (Var "x") (Var "y")))
          )
          @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "Apply" $
        eval
          envEmpty
          ( Apply
              ( Let
                  "x"
                  (CstInt 2)
                  (Lambda "y" (Add (Var "x") (Var "y")))
              )
              (CstInt 3)
          )
          @?= Right (ValInt 5),
      --
      testCase "TryCatch 1" $
        eval
          envEmpty
          ( TryCatch
              (CstInt 0)
              (CstInt 1)
          )
          @?= Right (ValInt 0),
      --
      testCase "TryCatch 2" $
        eval
          envEmpty
          ( TryCatch
              (Var "Missing")
              (CstInt 1)
          )
          @?= Right (ValInt 1),
      --
      testCase "fact 5" $
        eval
          envEmpty
          ( Apply
              fact
              (CstInt 5)
          )
          @?= Right (ValInt 120),
                --
      testCase "TryCatch Catch Fails" $
        eval envEmpty (TryCatch (Div (CstInt 1) (CstInt 0)) (Pow (CstInt 2) (CstInt (-1))))
          @?= Left "Negative exponent",
      --
      testCase "Lambda body not evaluted without application" $
        eval
          envEmpty
          (Let "fun" (Lambda "x" (Div (CstInt 1) (CstInt 0))) (CstInt 42))
          @?= Right (ValInt 42),
      --
      testCase "Apply Wrong Type" $
        eval envEmpty (Apply (CstInt 1) (CstInt 2))
          @?= Left "Expected a function: Type error if not ValFun",
      --
      testCase "Lambda Capture" $
        eval
          envEmpty
          ( Let
              "x"
              (CstInt 1)
              ( Apply
                  (Lambda "y" (Add (Var "x") (Var "y")))
                  (CstInt 2)
              )
          )
          @?= Right (ValInt 3),
      --
      testCase "Lambda Multiple Capture" $
        eval
          envEmpty
          ( Let
              "a"
              (CstInt 1)
              ( Let
                  "b"
                  (CstInt 2)
                  ( Let
                      "c"
                      (CstInt 3)
                      ( Apply
                          (Lambda "x" (Add (Add (Var "a") (Var "b")) (Var "c")))
                          (CstInt 4)
                      )
                  )
              )
          )
          @?= Right (ValInt 6),
      --
      testCase "Apply \\f -> \\ g -> \\h -> f (g h)" $
        eval
          envEmpty
          ( Apply
              ( Apply
                  ( Apply
                      ( Lambda "f" (Lambda "g" (Lambda "h" (Apply (Var "f") (Apply (Var "g") (Var "h")))))
                      )
                      (Lambda "pOne" (Add (Var "pOne") (CstInt 1)))
                  )
                  (Lambda "tTwo" (Mul (Var "tTwo") (CstInt 2)))
              )
              (CstInt 10)
          )
          @?= Right (ValInt 21),
      --
      testCase "Apply Eval Order 1 - Func" $
        eval
          envEmpty
          (Apply (Add (CstInt 0) (CstInt 0)) (CstInt 1))
          @?= Left "Expected a function: Type error if not ValFun",
      --
      testCase "Apply Eval Order 2 - Arg" $
        eval
          envEmpty
          (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (Div (CstInt 1) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Apply Eval Order 3 - Body" $
        eval
          envEmpty
          (Apply (Lambda "x" (Div (CstInt 1) (Var "x"))) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "[Y-Combinator] 9! = 362880" $
        eval envEmpty (Apply fact (CstInt 9)) @?= Right (ValInt 362880)
    ]