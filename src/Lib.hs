{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields, DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}

module Lib where

import AST

import Control.Monad.State
import Data.Aeson.BetterErrors
  ( Parse,
    ParseError,
    asIntegral,
    asString,
    key,
    parse,
    throwCustomError,
  )
import Data.Aeson.BetterErrors.Internal (asBool, eachInArray)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map

asAST :: Parse String AST.AST
asAST = do
  name <- key "name" asString
  expression <- key "expression" asTerm
  location <- key "location" asLocation
  return File {name = name, expression = expression, location = location}

asLocation :: Parse String Location
asLocation = do
  start <- key "start" asIntegral
  end <- key "end" asIntegral
  filename <- key "filename" asString
  return Location {start = start, end = end, filename = filename}

asTStr :: Parse String TStr
asTStr = do
  value <- key "value" asString
  location <- key "location" asLocation
  return Str' {value = value, location = location}

asTBool :: Parse String TBool
asTBool = do
  value <- key "value" asBool
  location <- key "location" asLocation
  return Bool' {value = value, location = location}

asTInt :: Parse String TInt
asTInt = do
  value <- key "value" asIntegral
  location <- key "location" asLocation
  return Int' {value = value, location = location}

asTVAr :: Parse String TVar
asTVAr = do
  text <- key "text" asString
  location <- key "location" asLocation
  return Var' {text = text, location = location}

asParameter :: Parse String Parameter
asParameter = do
  text <- key "text" asString
  location <- key "location" asLocation
  return Parameter' {text = text, location = location}

asFunction :: Parse String Function
asFunction = do
  value <- key "value" asTerm
  kind <- key "kind" asString
  parameters <- key "parameters" (eachInArray asParameter)
  location <- key "location" asLocation
  return Function' {value = value, kind = kind, parameters = parameters, location = location}

asPrint :: Parse String Print
asPrint = do
  kind <- key "kind" asString
  value <- key "value" asTerm
  location <- key "location" asLocation
  return Print' {kind = kind, value = value, location = location}

asCall :: Parse String Call
asCall = do
  kind <- key "kind" asString
  callee <- key "callee" asTerm
  arguments <- key "arguments" (eachInArray asTerm)
  location <- key "location" asLocation
  return Call' {kind = kind, callee = callee, arguments = arguments, location = location}

asTuple :: Parse String TTuple
asTuple = do
  kind <- key "kind" asString
  first <- key "first" asTerm
  second <- key "second" asTerm
  location <- key "location" asLocation
  return Tuple' {kind = kind, first = first, second = second, location = location}

asLet :: Parse String Let
asLet = do
  kind <- key "kind" asString
  name <- key "name" asParameter
  value <- key "value" asTerm
  next <- key "next" asTerm
  location <- key "location" asLocation
  return Let' {kind = kind, name = name, value = value, next = next, location = location}

asIf :: Parse String If
asIf = do
  kind <- key "kind" asString
  condition <- key "condition" asTerm
  then' <- key "then" asTerm
  else' <- key "otherwise" asTerm
  location <- key "location" asLocation
  return If' {kind = kind, condition = condition, then' = then', else' = else', location = location}

asFirst :: Parse String First
asFirst = do
  kind <- key "kind" asString
  value <- key "value" asTerm
  location <- key "location" asLocation
  return First' {kind = kind, value = value, location = location}

asSecond :: Parse String Second
asSecond = do
  kind <- key "kind" asString
  value <- key "value" asTerm
  location <- key "location" asLocation
  return Second' {kind = kind, value = value, location = location}

asBinary :: Parse String Binary
asBinary = do
  kind <- key "kind" asString
  lhs <- key "lhs" asTerm
  op <- do
    operation <- key "op" asString
    case operation of
      "Add" -> return Add
      "Sub" -> return Sub
      "Mul" -> return Mul
      "Div" -> return Div
      "Rem" -> return Rem
      "Eq" -> return Eq
      "Neq" -> return Neq
      "Lt" -> return Lt
      "Gt" -> return Gt
      "Lte" -> return Lte
      "Gte" -> return Gte
      "And" -> return And
      "Or" -> return Or
      _ -> throwCustomError operation
  rhs <- key "rhs" asTerm
  location <- key "location" asLocation
  return Binary' {kind = kind, lhs = lhs, op = op, rhs = rhs, location = location}

asTerm :: Parse String Term
asTerm = do
  kind' <- key "kind" asString
  case kind' of
    "Str" -> TStr <$> asTStr
    "Print" -> Print <$> asPrint
    "Int" -> TInt <$> asTInt
    "Bool" -> TBool <$> asTBool
    "Var" -> TVar <$> asTVAr
    "Function" -> Function <$> asFunction
    "Call" -> Call <$> asCall
    "Tuple" -> TTuple <$> asTuple
    "Let" -> Let <$> asLet
    "If" -> If <$> asIf
    "Binary" -> Binary <$> asBinary
    "First" -> First <$> asFirst
    "Second" -> Second <$> asSecond
    _ -> throwCustomError kind'

parseToAST :: String -> Either (ParseError String) AST
parseToAST str = parse asAST (BL.pack str)

type StateB = (Map String BaseType)

evalAST :: AST -> StateT StateB IO BaseType
evalAST (File {name = name, expression = expression }) = do
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

data BaseType =
  VInt Int
  | VStr String
  | VBool Bool
  | VFunction ([String], Term)
  | End
  deriving (Show)

getParameters :: [Parameter] -> [String]
getParameters [] = []
getParameters (Parameter' {text = text} :xs) = text : getParameters xs

evalTerm :: Term -> StateT StateB IO BaseType
evalTerm (TInt s) = return (evalInt s)
evalTerm (TBool s) = return (evalBool s)
evalTerm (TStr s) = return (evalStr s)

evalTerm (Print s) = do
  _ <- evalPrint s
  return End

evalTerm (Let (Let' { name = Parameter' {text = name}, value = value, next = next })) = do
  state <- get
  value' <- lift $ (evalStateT $ evalTerm value) state
  let state' = Map.insert name value' state
  modify (const state')
  _next' <- lift $ (evalStateT $ evalTerm next) state'
  return End

evalTerm (Binary (Binary' { lhs = lhs, op = op, rhs = rhs })) = do
  l <- evalTerm lhs
  r <- evalTerm rhs
  return $ evalBinary op l r

evalTerm (TVar s) = do
  state <- get
  let s' = evalVar s
  let vall = Map.lookup s' state
  case vall of
    Just value -> return value
    Nothing -> error ("Var `" ++ s' ++ "` not defined" )

evalTerm (Function (Function' { value = value, parameters = parameters })) = do return $ VFunction (getParameters parameters, value)

evalTerm (Call (Call' {callee = TVar callee, arguments = arguments})) = do
  state <- get
  let s' = evalVar callee
  let vall = Map.lookup s' state
  case vall of
    Just (VFunction (parameters, value)) -> do
      let l = zip parameters (map evalTerm arguments)
      l' <- mapM (\(k, v) -> do { v' <- v; return (k, v') }) l
      let state' = Map.fromList l' `Map.union`  state
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

evalPrint :: Print -> StateT StateB IO ()
evalPrint Print' { value = value } = do
  state <- get
  r <- case value of
    TStr s -> return $ evalStr s
    TInt s -> return $ evalInt s
    TBool s -> return $ evalBool s
    TVar (Var' { text = text }) -> do
      let vall = Map.lookup text state
      case vall of
        Nothing -> error ("Var `" ++ text ++ "` not defined" )
        Just value -> return value
    v -> evalTerm v
  lift $ printBasicTypes r


printBasicTypes :: BaseType -> IO ()
printBasicTypes (VInt s) = print s
printBasicTypes (VBool s) = print s
printBasicTypes (VStr s) = print s
printBasicTypes End = return ()
printBasicTypes _ = print "<Function>"

getLocation :: Term -> Location
getLocation (TStr (Str' {location = location})) = location
getLocation (Print (Print' {location = location})) = location
getLocation (TInt (Int' {location = location})) = location
getLocation (TBool (Bool' {location = location})) = location
getLocation (TVar (Var' {location = location})) = location
getLocation (Function (Function' {location = location})) = location
getLocation (Call (Call' {location = location})) = location
getLocation (TTuple (Tuple' {location = location})) = location
getLocation (Let (Let' {location = location})) = location
getLocation (Parameter (Parameter' {location = location})) = location
getLocation (If (If' {location = location})) = location
getLocation (Binary (Binary' {location = location})) = location
getLocation (First (First' {location = location})) = location
getLocation (Second (Second' {location = location})) = location

execute :: Either (ParseError String) AST -> IO ()
execute (Right s) = do
  _ <- evalStateT (evalAST s) Map.empty
  return ()
execute (Left err) = print (show err)