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

module Utils where
import AST


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
