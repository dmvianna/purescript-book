module Data.Path
  ( Path()
  , allFiles
  , allFiles'
  , onlyFiles
  , maxFile
  , minFile
  , root
  , ls
  , filename
  , isDirectory
  , size
  ) where

import Prelude
import Data.Array ((:), concatMap, filter)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))

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
