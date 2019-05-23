import Test.QuickCheck
import Control.Monad
import Control.Conditional
import Data.List

data Nil = Zero | One | Param1 | Param2
data Functie = Plus Functie Functie | Times Functie Functie |
               Min Functie Functie | Constant Nil

instance Show Nil where
  show Zero = "0"
  show One  = "1"

instance Show Functie where
  show (Plus a b) = "(" ++ show a ++ ")" ++ "+" ++ "(" ++ show b ++ ")"
  show (Times a b) = "(" ++ show a ++ ")" ++ "*" ++ "(" ++ show b ++ ")"
  show (Min a b) = "(" ++ show a ++ ")" ++ "-" ++ "(" ++ show b ++ ")"
  show (Constant Param1) = "a"
  show (Constant Param2) = "b"
  show (Constant Zero) = "0"
  show (Constant One) = "1"

instance Eq Nil where
  (==) Zero Zero = True
  (==) One One = True
  (==) Param1 Param1 = True
  (==) Param2 Param2 = True
  (==) _ _ = False

instance Eq Functie where
  (==) (Plus a b) (Plus c d) = (a == c && b == d) || (a == d && b == c)
  (==) (Times a b) (Times c d) = (a == c && b == d) || (a == d && b == c)
  (==) (Min a b) (Min c d) = (a == c && b == d)
  (==) (Constant a) (Constant b) = a == b
  (==) _ _ = False

prod :: [a] -> [a] -> (a -> a -> b) -> [b]
prod (x:xs) (y:ys) f = combine2 [x] [y] xs ys f
  where
    combine1 (l:ls) l2 (r:rs) [] f = [f l b| b <- l2] ++ combine1 (r:l:ls) l2 rs [] f
    combine1 (l:ls) l2 [] []     f = [f l b| b <- l2]
    combine1 (l:ls) l2 r1 (r:rs) f = [f l b| b <- l2] ++ combine2 (l:ls) (r:l2) r1 rs f
    combine2 l1 (l:ls) (r:rs) r2 f = [f a l| a <- l1] ++ combine1 (r:l1) (l:ls) rs r2 f
    combine2 l1 (l:ls) [] (r:rs) f = [f a l| a <- l1] ++ combine2 l1 (r:l:ls) [] rs f
    combine2 l1 (l:ls) [] []     f = [f a l| a <- l1]

nils :: [Nil]
nils = [Zero, One]

constants :: [Functie]
constants = [Constant Zero, Constant One, Constant Param1, Constant Param2]

myAbs :: Int -> Int
myAbs a = abs a

listFunctNames = [Plus, Times, Min]
listFunctNamesNoTimes = [Plus, Min]

makeFunctie :: Functie -> Functie -> [Functie]
makeFunctie _ (Constant Zero) = []
makeFunctie (Constant Zero) f2 = filter (not.simplifyable) [Min (Constant Zero) f2]
makeFunctie (Constant One) f2 = filter (not.simplifyable)
                                (map (\f -> f (Constant One) f2) (listFunctNamesNoTimes))
makeFunctie f1 (Constant One) = filter (not.simplifyable)
                                (map (\f -> f f1 (Constant One)) (listFunctNamesNoTimes))
makeFunctie f1 f2 = filter (not.simplifyable)
                    (map (\f -> f f1 f2) listFunctNames)

listcons :: [Functie]
listcons = [(Constant Param1), (Constant Param2), (Constant Zero), (Constant One)] ++
           concat (prod listcons listcons makeFunctie)

simplifyable :: Functie -> Bool
simplifyable (Plus a b) = simplifyable a || simplifyable b ||
                          (Constant Zero) == b || a == (Constant Zero)
simplifyable (Times a b) = simplifyable a || simplifyable b ||
                           a == (Constant Zero) || (Constant Zero) == b
simplifyable (Min a b) = simplifyable a || simplifyable b || a == b
simplifyable _ = False

evalNil :: Nil -> Int
evalNil Zero = 0
evalNil One  = 1

evalCons :: Functie -> (Int -> Int -> Int)
evalCons (Constant Zero) a b = 0
evalCons (Constant One) a b = 1
evalCons (Constant Param1) a b = a
evalCons (Constant Param2) a b = b
evalCons (Plus f1 f2) a b = evalCons f1 a b + evalCons f2 a b
evalCons (Times f1 f2) a b = evalCons f1 a b * evalCons f2 a b
evalCons (Min f1 f2) a b = evalCons f1 a b - evalCons f2 a b

data Funct = F ([Int] -> Int) Functie Int

getFold :: Funct -> ([Int] -> Int)
getFold (F fold _ _) = fold

getCons :: Funct -> Functie
getCons (F _ fun _) = fun

getNil :: Funct -> Int
getNil (F _ _ nil) = nil

data Res = R Result Functie Int

resGetResult :: Res -> Result
resGetResult (R r _ _) = r

resGetCons :: Res -> Functie
resGetCons (R _ f _) = f

resGetNil :: Res -> Int
resGetNil (R _ _ n) = n

evalFoldr :: Int -> Functie -> [Int] -> Int
evalFoldr b f l = foldr (evalCons f) b l

genfunctions :: Int -> [Funct]
genfunctions b = [F (evalFoldr b f) f b| f <- listcons]

genBasis :: [Int]
genBasis = [0..]

testfunctions :: [Funct] -> [IO Res]
testfunctions fs = map checkfunction fs

resultToBool :: Result -> Bool
resultToBool (Success _ _ _ _ _ _) = True
resultToBool _ = False

resToBool :: Res -> Bool
resToBool (R result _ _) = resultToBool result

prop :: ([Int] -> Int) -> [Int] -> Bool
prop f l = f l == foldr (\a b -> 4*a + b) 1 l

prop_basis :: Int -> Bool
prop_basis f = f == foldr (\a b -> 4*a + b) 1 []

checkfunction :: (Funct) -> IO Res
checkfunction (F f cons nil) = do
    result <- quickCheckWithResult (Args Nothing 100 10 100 False 0) (prop f)
    return (R result cons nil)

checkBasis :: Int -> IO Result
checkBasis a = do
    result <- quickCheckWithResult (Args Nothing 100 10 100 False 0) (prop_basis a)
    return result

main :: IO ()
main = do
    basis <- filterBasis genBasis
    f <- filterFunctions (genfunctions basis)
    putStrLn ("Uw functie is: foldr (\\a b -> \
    \" ++ show (getCons f) ++ ") " ++ show (getNil f))

filterFunctions :: [Funct] -> IO Funct
filterFunctions (funct:fs) =
  ifM (checkfunction funct >>= return.resToBool) (return funct) (filterFunctions fs)

filterBasis :: [Int] -> IO Int
filterBasis (a:as) =
  ifM (checkBasis a >>= return.resultToBool) (return a) (filterBasis as)

testFunct :: IO ()
testFunct = do
    l <- sequence (map return (take 100000 listcons))
    print "gedaan"

-- evalueer veelterm [a0, a1, a2]
evalPoly :: Int -> [Int] -> Int
evalPoly x = foldr (\a b -> a + x*b) 0
