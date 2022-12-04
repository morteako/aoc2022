module Letter (
    Letter,
    toLetter,
    isUpper,
    isLower,
    toUpper,
    toLower,
    letters,
    pattern ToUpper,
    pattern ToLower,
    pattern Let,
) where

import Data.Char qualified as Char

newtype Letter = Letter {getLetter :: Char}
    deriving newtype (Eq, Show)

toLetter :: Char -> Maybe Letter
toLetter x = if Char.isUpper x || Char.isLower x then Just (Letter x) else Nothing

over :: (Char -> Char) -> Letter -> Letter
over f (Letter x) = Letter $ f x

isUpper :: Letter -> Bool
isUpper = Char.isUpper . getLetter

isLower :: Letter -> Bool
isLower = Char.isLower . getLetter

toUpper :: Letter -> Letter
toUpper = over Char.toUpper

toLower :: Letter -> Letter
toLower = over Char.toLower

pattern ToUpper :: Letter -> Letter
pattern ToUpper x <- (toUpper -> x)
    where
        ToUpper x = toUpper x

pattern ToLower :: Letter -> Letter
pattern ToLower x <- (toUpper -> x)
    where
        ToLower x = toLower x

pattern Let :: Letter -> Char
pattern Let x <- (Letter -> x)

letters :: [Letter]
letters = [minBound .. maxBound]

instance Enum Letter where
    toEnum = Letter . Char.chr . asciiNumToLetterIndex . check
      where
        check :: Int -> Int
        check x = if x < 0 || x > 51 then (error $ "toEnum : Letter : 0-51 - " ++ show x) else x
        asciiNumToLetterIndex x = if x < numLetters then smallAOrd + x else largeAOrd + x - numLetters

    fromEnum :: Letter -> Int
    fromEnum = letterIndex . Char.ord . getLetter
      where
        letterIndex x = if x >= smallAOrd then x - smallAOrd else numLetters + x - largeAOrd

instance Ord Letter where
    Letter a <= Letter b = f a <= f b
      where
        f (Char.ord -> x) = if x >= smallAOrd then x - smallAOrd else x

instance Bounded Letter where
    minBound = Letter 'a'
    maxBound = Letter 'Z'

smallAOrd = 97

largeAOrd = 65

numLetters = 26
