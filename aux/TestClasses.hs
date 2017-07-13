{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies  #-}

module Main where


class Intable a where
 toInteger :: a -> Integer

class (Intable a, Ord c) => Intableable a b c | b -> a, b -> c where 
 toIntable :: b -> a

toIntable1 :: (Intableable a b c) => b -> Integer
toIntable1 = Main.toInteger . toIntable

instance Intable Integer where
 toInteger = id

instance Intableable Integer Integer Integer where
 toIntable = id


main = return () 
