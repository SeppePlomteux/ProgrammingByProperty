import Test.QuickCheck
import Control.Monad
import Control.Monad.Omega
import Control.Conditional

data Nil = Zero | One | Param1 | Param2
data Functie = Plus Functie Functie | Times Functie Functie |
               Min Functie Functie| Constant Nil

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

nils :: [Nil]
nils = [Zero, One]

listFunctNames :: [Functie -> Functie -> Functie]
listFunctNames = [Plus, Times]


listcons = [(Constant Param1), (Constant Param2), (Constant Zero), (Constant One)] ++
           [functie v1 v2 |v1 <- listcons, v2 <- listcons,
           functie <- listFunctNames]


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

--Pas deze functie aan om de gebruikte property te veranderen
prop :: ([Int] -> Int) -> [Int] -> Bool
prop f l = f l == foldr (\a b -> 9*a+b) 1 l
-- Pas deze functie aan om de gebruikte property voor het
-- basisgeval te veranderen
prop_basis :: Int -> Bool
prop_basis f = f == foldr (\a b -> 9*a+b) 1 []

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


filterBasis :: [Int] -> IO Int
filterBasis (a:as) =
  ifM (checkBasis a >>= return.resultToBool) (return a) (filterBasis as)

filterFunctions :: [Funct] -> IO Funct
filterFunctions (funct:fs) =
  ifM (checkfunction funct >>= return.resToBool) (return funct) (filterFunctions fs)

-- evalueer veelterm [a0, a1, a2]
evalPoly :: Int -> [Int] -> Int
evalPoly x = foldr (\a b -> a + x*b) 0
