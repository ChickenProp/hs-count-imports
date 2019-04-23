{-# LANGUAGE TypeFamilies #-}

module A where

foo = 5

(^&) = 6

data SomeType = SomeConstructor | (:++)

class SomeClass a where
  type SomeTF a :: *
  someMethod :: a -> (a, a)
