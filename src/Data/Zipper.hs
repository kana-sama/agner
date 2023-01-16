{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data.Zipper (Zipper, empty, fromList, cursor, index, left, right) where

data Zipper a = MkZipper ![a] ![a]

empty :: Zipper a
empty = MkZipper [] []

fromList :: [a] -> Zipper a
fromList xs = MkZipper [] xs

cursor :: Zipper a -> a
cursor (MkZipper _ (x:_)) = x

index :: Zipper a -> Int
index (MkZipper xs _) = length xs

left, right :: Zipper a -> Zipper a
left (MkZipper (x:xs) ys) = MkZipper xs (x:ys)
right (MkZipper xs (x:ys)) = MkZipper (x:xs) ys