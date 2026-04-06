module TypeclassTest where

-- Uses Num typeclass (desugars to $fNumInt dictionary)
double :: Int -> Int
double x = x + x

-- Uses Eq typeclass
isZero :: Int -> Bool
isZero x = x == 0

-- Uses Show typeclass (via print)
main :: IO ()
main = print (double 21)
