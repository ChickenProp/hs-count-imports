{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module A where

foo = 5

(^&) = 6

data x +++ y = (:+++) x y

data SomeType = SomeConstructor | (:++)

class SomeClass a where
  type SomeTF a :: *
  data (&+&) a :: *
  someMethod :: a -> (a, a)
  (&++) :: a -> (a, a, a)

-- brittany-disable-next-binding
pattern PattSyn = SomeConstructor
