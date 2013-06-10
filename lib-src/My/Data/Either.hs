-- |The standard Data.Either module with some addenda
module My.Data.Either (module Data.Either, fromLeft, fromRight, fromEither, isLeft) where

import Data.Either

-- |Extracts a Left value
fromLeft :: Either c b -> c
-- |Extracts a Right value
fromRight :: Either a c -> c
-- |Extracts a value from an Either, with a default if it is a Left
fromEither :: c -> Either a c -> c

-- |Is it a Left ?
isLeft :: Either a b -> Bool

fromLeft = either id (error "fromLeft given a Right argument")
fromRight = either (error "fromRight given a Left argument") id
fromEither x = either (const x) id

isLeft = either (const True) (const False)
