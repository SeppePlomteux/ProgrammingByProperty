import Test.QuickCheck
import Control.Monad
import Control.Conditional

-- Difference lists

type Difflist a = ([a] -> [a])

emptyDifflist :: Difflist a
emptyDifflist = ([] ++)

makeDifflist :: a -> Difflist a
makeDifflist = (:)

convertToDifflist :: [a] -> Difflist a
convertToDifflist = (++)

concatDiff :: Difflist a -> Difflist a -> Difflist a
concatDiff = (.)

concatall :: [Difflist a] -> Difflist a
concatall = foldr (.) (id)

getList :: Difflist a -> [a]
getList l = l []

isEmptyDifflist :: Eq a => Difflist a -> Bool
isEmptyDifflist l = getList l == []

difflistHead :: Difflist a -> a
difflistHead l = (head.l) []

difflistTail :: Difflist a -> Difflist a
difflistTail = (tail.)

-- einde van difference lists

data Nil = Zero | One | Param1 | Param2
data Functie = Plus Functie Functie | Times Functie Functie |
               Min Functie Functie | Constant Nil

data Boom = Blad Functie | Knoop [Boom] | Leeg deriving (Eq, Show)

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

legeBoom :: Boom
legeBoom = Knoop []

searchSpace :: Boom
searchSpace = Knoop [a, b, c, d, e, f, g]
  where
    a = Blad (Constant Param1)
    b = Blad (Constant Param2)
    c = Blad (Constant Zero)
    d = Blad (Constant One)
    e = combine Plus searchSpace searchSpace
    f = combine Times searchSpace searchSpace
    g = combine Min searchSpace searchSpace

combine :: (Functie -> Functie -> Functie) -> Boom -> Boom -> Boom
combine f (Blad f1)   b2
  | simplifyable f1 = Leeg
  | otherwise = extend b2 (f f1)
combine f (Knoop bs1) b2 = Knoop (filter (Leeg /=) [combine f b1 b2 | b1 <- bs1])

extend :: Boom -> (Functie -> Functie) -> Boom
extend (Blad f1) f
  | simplifyable (f f1) = Leeg
  | otherwise = Blad (f f1)
extend (Knoop fs1) f = Knoop (filter (Leeg /=) [extend f1 f | f1 <- fs1])

dfs :: Boom -> [Functie]
dfs boom = getList (go boom)
  where
    go :: Boom -> Difflist Functie
    go (Blad funct) = makeDifflist funct
    go (Knoop bomen) = concatall [go boom | boom <- bomen]

bfs :: Boom -> [Functie]
bfs boom = getList (go boom emptyDifflist)
  where
    go :: Boom -> Difflist Boom -> Difflist Functie
    go (Blad funct)  queue = concatDiff (makeDifflist funct) (continue queue)
    go (Knoop bomen) queue = continue (concatDiff queue (convertToDifflist bomen))

    continue :: Difflist Boom -> Difflist Functie
    continue list
      | isEmptyDifflist list = emptyDifflist
      | otherwise = go (difflistHead list) (difflistTail list)

itDeepening :: Boom -> [Functie]
itDeepening boom = (getList.concatall.map (maxDepth (boom))) [1..]
  where
    maxDepth :: Boom -> Int -> Difflist Functie
    maxDepth _             0 = emptyDifflist
    maxDepth (Blad f1)     _ = makeDifflist f1
    maxDepth (Knoop bomen) d = concatall [maxDepth boom (d-1) | boom <- bomen]

myAbs :: Int -> Int
myAbs a = abs a

listcons :: [Functie]
listcons = itDeepening searchSpace

simplifyable :: Functie -> Bool
simplifyable (Plus a b) = simplifyable a || simplifyable b ||
                          (Constant Zero) == b || a == (Constant Zero)
simplifyable (Times a b) = simplifyable a || simplifyable b ||
                           a == (Constant Zero) || (Constant Zero) == b ||
                           a == (Constant One) || (b == Constant One)
simplifyable (Min a b) = simplifyable a || simplifyable b ||
                         a == b || b == (Constant Zero)
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

-- Pas deze functie aan om met een andere property te genereren
prop :: ([Int] -> Int) -> [Int] -> Bool
prop f l = f l == foldr (\a b -> 4*a + b) 1 l

-- Pas deze functie aan om de property van het besisgeval te veranderen
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
