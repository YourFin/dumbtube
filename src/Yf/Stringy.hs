-- |

module Yf.Stringy (snakeCase, trainCase, camelCase, pascalCase) where

import Data.Char (toUpper, isUpper, toLower)
--import Data.String (IsString())

--class (IsString a, Eq a, Ord a, Semigroup a, Monoid a) => Stringy a where



-- | Snake casing, where the words are always lower case and separated by an
-- underscore.
snakeCase :: String -> String
snakeCase = symbCase '_'

-- | Train casing, where the words are always lower case and separated by
-- a hyphen

trainCase :: String -> String
trainCase = symbCase '-'

-- | Camel casing, where the words are separated by the first letter of each
-- word being a capital. However, the first letter of the field is never a
-- capital.
camelCase :: String -> String
camelCase = applyFirst toLower

-- | Pascal casing, where the words are separated by the first letter of each
-- word being a capital. The first letter of the field is always a capital.
pascalCase :: String -> String
pascalCase = applyFirst toUpper

-- | Generic casing for symbol separated names
symbCase :: Char -> (String -> String)
symbCase sym =  u . applyFirst toLower
  where u []                       = []
        u (x:xs) | isUpper x = sym : toLower x : u xs
                 | otherwise = x : u xs

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []     = []
applyFirst f [x]    = [f x]
applyFirst f (x:xs) = f x: xs
