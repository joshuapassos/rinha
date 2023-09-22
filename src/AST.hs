{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module AST where

data AST = File {name :: String, expression :: Term, location :: Location} deriving (Show)

data Location = Location {start :: Int, end :: Int, filename :: String} deriving (Show)

data TStr = Str' {value :: String, location :: Location} deriving (Show)

data TInt = Int' {value :: Int, location :: Location} deriving (Show)

data TVar = Var' {text :: String, location :: Location} deriving (Show)

data Function = Function' {value :: Term, parameters :: [Parameter], kind :: String, location :: Location} deriving (Show)

data Parameter = Parameter' {text :: String, location :: Location} deriving (Show)

data Call = Call' {kind :: String, callee :: Term, arguments :: [Term], location :: Location} deriving (Show)

data Let = Let' {kind :: String, name :: Parameter, value :: Term, next :: Term, location :: Location} deriving (Show)

data TBool = Bool' {value :: Bool, location :: Location} deriving (Show)

data TTuple = Tuple' {kind :: String, first :: Term, second :: Term, location :: Location} deriving (Show)

data If = If' {kind :: String, condition :: Term, then' :: Term, else' :: Term, location :: Location} deriving (Show)

data BinaryOP = Add | Sub | Mul | Div | Rem | Eq | Neq | Lt | Gt | Lte | Gte | And | Or deriving (Show)

data Binary = Binary' {kind :: String, lhs :: Term, op :: BinaryOP, rhs :: Term, location :: Location} deriving (Show)

data Term
  = TStr TStr
  | TInt TInt
  | TBool TBool
  | Print Print
  | TVar TVar
  | Function Function
  | Parameter Parameter
  | Call Call
  | Let Let
  | TTuple TTuple
  | If If
  | Binary Binary
  | First First
  | Second Second
  deriving (Show)

data First = First'{kind :: String, value :: Term, location :: Location} deriving (Show)

data Second = Second' {kind :: String, value :: Term, location :: Location} deriving (Show)

data Print = Print' {kind :: String, value :: Term, location :: Location} deriving (Show)