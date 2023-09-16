{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Lib where
import AST
import Data.Map (Map)
import Control.Monad.State

import Data.Aeson.BetterErrors
  ( Parse,
    ParseError,
    asIntegral,
    asString,
    key,
    parse,
    throwCustomError, keyMay,
  )
import Data.Aeson.BetterErrors.Internal (asBool, eachInArray)
import qualified Data.ByteString.Lazy.Char8 as BL


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

evalAST :: AST -> IO ()
evalAST (File {name = name, expression = expression, location = location}) = do
  print ("Running " ++ show name)

evalInt :: TInt -> Int
evalInt (Int' {value = value}) = value

evalBool :: TBool -> Bool
evalBool (Bool' {value = value}) = value

evalStr :: TStr -> String
evalStr (Str' {value = value}) = value

evalVar :: TVar -> String
evalVar (Var' {text = text}) = text


-- TODO: Use ST to functions and variables
evalTerm :: Term -> StateT (Map String String, Map String String) IO ()
-- evalTerm (TInt s) = do evalInt s
-- evalTerm (TBool s) = evalBool s
-- evalTerm (TStr s) = evalStr s
-- evalTerm (TVar s) = evalVar s
-- evalTerm (Print s) = evalPrint s
evalTerm x = do
  (s1,s2) <- get
  liftIO (print ("Not implemented" ++ show (getLocation x)))
-- evalPrint :: Print -> IO ()
-- evalPrint Print' {value = value}  = evalTerm value

getLocation :: Term -> Location
getLocation (TStr (Str' {location = location})) = location
getLocation (Print (Print' {location = location})) = location
getLocation (TInt ( Int' {location = location})) = location
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
execute (Right s) = evalAST s
execute (Left err) = print (show err)