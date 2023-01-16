{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data.Zipper
  ( Zipper
  , empty, fromList
  , cursor, index
  , left, right
  , prev, next
  ) where

data Zipper a = MkZipper ![a] ![a]

empty :: Zipper a
empty = MkZipper [] []

fromList :: [a] -> Zipper a
fromList xs = MkZipper [] xs

cursor :: Zipper a -> a
cursor (MkZipper _ (x:_)) = x

index :: Zipper a -> Int
index (MkZipper xs _) = length xs

next, prev :: Zipper a -> (a, Zipper a)
next (MkZipper xs (y:ys)) = (y, MkZipper (y:xs) ys)
prev (MkZipper (x:xs) ys) = (x, MkZipper xs (x:ys))

left, right :: Zipper a -> Zipper a
left (MkZipper (x:xs) ys) = MkZipper xs (x:ys)
right (MkZipper xs (x:ys)) = MkZipper (x:xs) ys