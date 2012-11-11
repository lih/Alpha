module My.Data.Either (module Data.Either, fromLeft, fromRight, fromEither, isLeft) where

import Data.Either

fromLeft = either id (error "fromLeft given a Right argument")
fromRight = either (error "fromRight given a Left argument") id
fromEither x = either (const x) id

isLeft = either (const True) (const False)
