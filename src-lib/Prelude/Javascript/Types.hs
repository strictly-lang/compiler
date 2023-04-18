{-# LANGUAGE InstanceSigs #-}

module Prelude.Javascript.Types where

import TypeChecker.Types

data JavascriptTypeHandler = JavascriptTypeHandler
  { destructure :: String -> Maybe JavascriptTypeHandler,
    call :: [JavascriptTypeHandler] -> Maybe JavascriptTypeHandler
  }

instance Show JavascriptTypeHandler where
  show a = "JavascriptTypeHandler"

instance TypeHandler JavascriptTypeHandler where
  destructure = Prelude.Javascript.Types.destructure
  call = Prelude.Javascript.Types.call
