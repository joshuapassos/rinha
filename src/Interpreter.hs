{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter where

import AST
import Control.Monad.State
import Data.Aeson.BetterErrors
import Data.Map (Map)
import qualified Data.Map as Map

type StateB = (Map String BaseType)

data BaseType
  = VInt Int
  | VStr String
  | VBool Bool
  | VFunction ([String], Term)
  deriving (Show)

evalAST :: AST -> StateT StateB IO BaseType
evalAST (File {name = name, expression = expression}) = do
  state <- get
  liftIO $ putStrLn ("Running " ++ name)
  lift $ evalStateT (evalTerm expression) state

evalInt :: TInt -> BaseType
evalInt (Int' {value = value}) = VInt value

evalBool :: TBool -> BaseType
evalBool (Bool' {value = value}) = VBool value

evalStr :: TStr -> BaseType
evalStr (Str' {value = value}) = VStr value

evalVar :: TVar -> String
evalVar (Var' {text = text}) = text

evalTerm :: Term -> StateT StateB IO BaseType
evalTerm (TInt s) = return (evalInt s)
evalTerm (TBool s) = return (evalBool s)
evalTerm (TStr s) = return (evalStr s)
evalTerm (Print s) = evalPrint s
evalTerm (Let (Let' {name = Parameter' {text = name}, value = value, next = next})) = do
  state <- get
  value' <- lift $ (evalStateT $ evalTerm value) state
  let state' = Map.insert name value' state
  modify (const state')
  lift $ (evalStateT $ evalTerm next) state'
evalTerm (Binary (Binary' {lhs = lhs, op = op, rhs = rhs})) = do
  l <- evalTerm lhs
  r <- evalTerm rhs
  return $ evalBinary op l r
evalTerm (TVar s) = do
  state <- get
  let s' = evalVar s
  let vall = Map.lookup s' state
  case vall of
    Just value -> return value
    Nothing -> error ("Var `" ++ s' ++ "` not defined")
evalTerm (Function (Function' {value = value, parameters = parameters})) = do
  return $ VFunction (getParameters parameters, value)
  where
    getParameters [] = []
    getParameters (Parameter' {text = text} : xs) = text : getParameters xs
evalTerm (Call (Call' {callee = TVar callee, arguments = arguments})) = do
  state <- get
  let s' = evalVar callee
  let vall = Map.lookup s' state
  case vall of
    Just (VFunction (parameters, value)) -> do
      let l = [(i,evalTerm j) | i <- parameters, j <- arguments]
      l' <- traverse sequence l
      let state' = Map.fromList l' `Map.union` state
      lift $ evalStateT (evalTerm value) state'
    _ -> error ("Var `" ++ s' ++ "` not defined")
evalTerm (If (If' {condition = condition, then' = then', else' = else'})) = do
  condition' <- evalTerm condition
  case condition' of
    VBool True -> evalTerm then'
    VBool False -> evalTerm else'
    _ -> error ("Invalid condition: `" ++ show condition' ++ "`")
evalTerm x = do
  lift $ error ("Not implemented term: " ++ show x)

evalBinary :: BinaryOP -> BaseType -> BaseType -> BaseType
evalBinary Add (VInt a) (VInt b) = VInt (a + b)
evalBinary Sub (VInt a) (VInt b) = VInt (a - b)
evalBinary Mul (VInt a) (VInt b) = VInt (a * b)
evalBinary Div (VInt a) (VInt b) = VInt (a `div` b)
evalBinary Rem (VInt a) (VInt b) = VInt (a `mod` b)
evalBinary Lt (VInt a) (VInt b) = VBool (a < b)
evalBinary Gt (VInt a) (VInt b) = VBool (a > b)
evalBinary Lte (VInt a) (VInt b) = VBool (a <= b)
evalBinary Gte (VInt a) (VInt b) = VBool (a >= b)
evalBinary Eq (VInt a) (VInt b) = VBool (a == b)
evalBinary Neq (VInt a) (VInt b) = VBool (a /= b)
evalBinary op _ _ = error ("Not implemented binary " ++ show op)

evalPrint :: Print -> StateT StateB IO BaseType
evalPrint Print' {value = value} = do
  state <- get
  r <- case value of
    TStr s -> return $ evalStr s
    TInt s -> return $ evalInt s
    TBool s -> return $ evalBool s
    TVar (Var' {text = text}) -> do
      let vall = Map.lookup text state
      case vall of
        Nothing -> error ("Var `" ++ text ++ "` not defined")
        Just value -> return value
    v -> evalTerm v
  lift $ printBasicTypes r
  return r
  where
    printBasicTypes (VInt s) = print s
    printBasicTypes (VBool s) = print s
    printBasicTypes (VStr s) = print s
    printBasicTypes _ = print "<Function>"

execute :: Either (ParseError String) AST -> IO ()
execute (Right s) = do
  _ <- evalStateT (evalAST s) Map.empty
  return ()
execute (Left err) = print (show err)