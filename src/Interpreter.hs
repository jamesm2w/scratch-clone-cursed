--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone : Cursed Edition                               --
--------------------------------------------------------------------------------
-- I am so very sorry for what you are about to see.
-- Feel free to skip it, the actual code is identical to my normal solution.
-- This just uses lambdas, braces + semicolons to one-line everything
-- It passes functions around as parameters, and uses `fix` to create
-- recursive pointers to functions to allow recursion in the anonymous lambda
-- functions.
-- It also works. 
-- Testing the efficieny of the whole program, it's unsuprising that it also
-- exhibits O(n) time for memory read/write. Thus proving that whitespace
-- doesn't effect the running time of programs :)
--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Interpreter where
--------------------------------------------------------------------------------
import Language
import Control.Monad
import Data.Function
--------------------------------------------------------------------------------
type Memory = [(String, Int)]
data Err = DivByZeroError | NegativeExponentError | UninitialisedMemory String deriving (Eq, Show)
--------------------------------------------------------------------------------
interpret = (\execOl pr m->(case pr of{[]->Right m;(p:ps)->execOl p m>>=interpret ps}))$(\evalOl st m->(case st of{AssignStmt{..}->evalOl assignExpr m>>=(\r->return((\x v l->(x, v):filter((x/=).fst)l)assignVar r m));IfStmt{..}->evalOl ifCond m>>=(\r->if r==0 then case ifElseIf of{(c,b):cs->interpret [IfStmt c b cs ifElse] m;[]->interpret ifElse m}else interpret ifBody m);RepeatStmt{..}->evalOl repeatTimesExpr m>>=(\i->foldM(flip interpret)m(replicate i repeatBody))}))$fix(\evalOl ex m->(case ex of{ValE i->Right i;VarE x->(\n o->case lookup n o of{Nothing -> Left$UninitialisedMemory n;Just v->Right v})x m;BinOpE o l r->evalOl l m>>=(\vl -> evalOl r m>>=(\op a b->case op of{Add->pure$a+b;Sub->pure$a-b;Mul->pure$a*b;Div->if b==0 then Left DivByZeroError else Right$a`div`b;Pow->if b<0 then Left NegativeExponentError else Right$a^b;Equal->pure.fromEnum$a==b;Neq->pure.fromEnum$a/=b;LessThan->pure.fromEnum$a<b;LessOrEqual->pure.fromEnum$a<=b;GreaterThan->pure.fromEnum$a>b;GreaterOrEqual->pure.fromEnum$a>=b})o vl)}))
--------------------------------------------------------------------------------