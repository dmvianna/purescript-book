module Data.Path
  ( Path()
  , allFiles
  , allFiles'
  , onlyFiles
  , maxFile
  , minFile
  , whereIs
  , root
  , ls
  , filename
  , isDirectory
  , size
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array ((:), concatMap, filter)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Monoid ((<>), mempty)

data Path
  = Directory String (Array Path)
  | File String Int

instance showPath :: Show Path where
  show = filename

root :: Path
root =
  Directory "/"
    [ Directory "/bin/"
        [ File "/bin/cp" 24800
        , File "/bin/ls" 34700
        , File "/bin/mv" 20200
        ]
    , Directory "/etc/"
        [ File "/etc/hosts" 300
        ]
    , Directory "/home/"
        [ Directory "/home/user/"
            [ File "/home/user/todo.txt" 1020
            , Directory "/home/user/code/"
                [ Directory "/home/user/code/js/"
                    [ File "/home/user/code/js/test.js" 40000
                    ]
                , Directory "/home/user/code/haskell/"
                    [ File "/home/user/code/haskell/test.hs" 5000
                    ]
                ]
            ]
        ]
    ]

filename :: Path -> String
filename (File name _) = name
filename (Directory name _) = name

isDirectory :: Path -> Boolean
isDirectory (Directory _ _) = true
isDirectory _ = false

ls :: Path -> Array Path
ls (Directory _ xs) = xs
ls _ = []

size :: Path -> Maybe Int
size (File _ bytes) = Just bytes
size _ = Nothing

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = (filter $ not isDirectory) <<< allFiles

compareSize :: (Int -> Int -> Boolean) -> Path -> Path -> Path
compareSize f a b =
  case [size a, size b] of
    [Just a', Just b'] -> if a' `f` b' then a else b
    [Just a', Nothing] -> a
    [Nothing, Just b'] -> b
    _ -> a

maxFile :: Path -> Path
maxFile ps = foldr (compareSize (>)) ps (allFiles ps)

minFile :: Path -> Path
minFile ps = foldr (compareSize (<)) ps (allFiles ps)

matchFile :: String -> Path -> First Path
matchFile s p@(File ps _) =
  if s == ps
  then First $ Just p
  else First Nothing
matchFile _ _ = First Nothing

--whereIs :: String -> Maybe Path
whereIs target = do
  fms <- do
    d <- do
      fs <- allFiles root
      guard $ isDirectory fs
      pure fs
    p <- ls d
    pure $ matchFile target p
  First m <- foldr (<>) mempty fms
  case m of
    Just d -> Just d
    Nothing -> Nothing
