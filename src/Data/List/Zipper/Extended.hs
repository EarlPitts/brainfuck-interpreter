module Data.List.Zipper.Extended where

import Data.List.Zipper

right' :: (Monoid a) => Zipper a -> Zipper a
right' z = if endp z then right $ insert mempty z else right z

replaceWith :: (Monoid a) => (a -> a) -> Zipper a -> Zipper a
replaceWith f z =
  if endp z
    then insert (f mempty) z
    else insert (f $ cursor z) (delete z)
