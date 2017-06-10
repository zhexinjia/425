data Comparison = Less | Equal | Greater
          deriving (Show, Eq)

compareInt :: Int -> Int -> Comparison
compareInt x y =
    if x < y then Less else if x > y then Greater else Equal

compareChar :: Char -> Char -> Comparison
compareChar x y =
    if x < y then Less else if x > y then Greater else Equal

class Comp a where
        (?=) :: a -> a -> Comparison

-- Integer comparison
instance Comp Int where
    (?=) x y = compareInt x y

-- Character comparison
instance Comp Char where
    (?=) x y = compareChar x y

-- Lists are compared element by element
instance Comp a => Comp [a] where
    (?=) [] [] = Equal
    (?=) (x:xs) [] = Greater
    (?=) [] (y:ys) = Less
    (?=) (x:xs) (y:ys) =
        if (x ?= y) /= Equal then x ?= y else xs ?= ys

-- Pairs are compared by first element, then by second element
instance (Comp a, Comp b) => Comp (a, b) where
    (?=) (x1, y1) (x2, y2) = if (x1 ?= x2) /= Equal then x1 ?= x2 else y1 ?= y2

f x y = let
        xx = (length x, x)
        yy = (length y, y)
        in ( xx ?= yy )