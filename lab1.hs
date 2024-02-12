module Lab1 where
import Data.List
 -- part1 constructors for set theory
data TERM v = EmptySet                             -- Constructor for the empty set
            | SingletonSet (TERM v)                -- Constructor for a singleton set with a variable
            | UnionSet (TERM v) (TERM v)           -- Constructor for the union of two sets
            | IntersectionSet (TERM v) (TERM v)    -- Constructor for the intersection of two sets
            | VarSet v                             -- Constructor for a set variable
            | VN Integer
            deriving(Show)                         -- Constructor for a von neumann encoded natural number

data PRED v = Con (Bool)
            |Subset (TERM v) (TERM v)              -- Constructor for subset of two sets
            | ElementOf (TERM v) (TERM v)          -- Constructor for element of a set
            | Not (PRED v)                         -- Constructor for negation of a predicate
            | And (PRED v) (PRED v)                -- Constructor for conjunction of two predicates
            | Or (PRED v) (PRED v)                 -- Constructor for disjunction of two predicates
            | Implies (PRED v) (PRED v)
            | Eq (TERM v) (TERM v)
            deriving(Show)                         -- Constructor for implication of two predicates

instance Eq Set where
    (==) = checkSetEquality

-- checks if two sets are equal
checkSetEquality :: Set -> Set -> Bool
checkSetEquality a b = isSubset a b && isSubset b a



newtype Set = S [Set] -- Set is a list of sets





type Env var dom = [(var,dom)]    -- Environment is a list of pairs of variables and their corresponding domain

eval :: Eq v => Env v Set -> TERM v -> Set

-- TODO create an equality instance


eval _ EmptySet = S [] -- Evaluate the empty set to an empty set
eval env (SingletonSet t) = S [eval env t]
eval env (UnionSet t1 t2) = unionSets (eval env t1) (eval env t2) -- Evaluate the union of two sets
eval env (IntersectionSet t1 t2) = intersectSets (eval env t1) (eval env t2) -- Evaluate the intersection of two sets
eval env (VarSet x) = case lookup x env  of -- Evaluate a variable to its corresponding set
    Just set -> set
    Nothing -> error "Variable not found"
eval _ (VN n) = eval vonNeumannEnv (vonNeumann n) -- Evaluate a von neumann encoded natural number to its corresponding set   

vonNeumannEnv :: Env Integer Set
vonNeumannEnv = [(0, S [])]-- not needed(?)

-- gets the union or intersection of two sets
unionSets :: Set -> Set -> Set
unionSets (S set1) (S set2) = S $ union set1 set2

intersectSets :: Set -> Set -> Set
intersectSets (S set1) (S set2) = S [x | x <- set1, x `elem` set2]



check  :: Eq v => Env v Set -> PRED v -> Bool
check env (Subset t1 t2) = isSubset (eval env t1) (eval env t2)
check env (ElementOf t1 t2) = elementOf (eval env t1) (eval env t2)
check env (Not p) = not (check env p)
check env (And p1 p2) = check env p1 && check env p2
check env (Or p1 p2) = check env p1 || check env p2
check env (Implies p1 p2) = not (check env p1) || check env p2
check _ (Con b) = b
check env (Eq t1 t2) = eval env t1 == eval env t2




-- helpers for checking if its a subset or an element of a set
isSubset :: Set -> Set -> Bool
isSubset (S set1) (S set2) = all (`elem` set2) set1

elementOf :: Set -> Set -> Bool
elementOf x (S setX) = x `elem` setX

-- show function for sets
instance Show Set where
--  show :: Set -> String
    show (S []) = "{}"
    show (S [x]) = "{" ++ show x ++ "}"
    show (S (x:xs)) = "{" ++ show x ++ "," ++ show (S xs) ++ "}"

-- This von neumann function matches the one in the description better!
vonNeumann :: Integer -> TERM Integer
vonNeumann n
    | n == 0 = EmptySet -- Base case
    | otherwise = UnionSet (VN (n - 1)) (SingletonSet (VN (n - 1)))

-- claims 

envEval :: Env Integer Set
envEval  = [(1, S [S [], S [S []]])]

claim1 :: Integer -> Integer -> Bool
claim1 n1 n2 = check envEval (Implies  (Con (n1 <= n2) ) (Subset (VN n1) (VN n2)))


-- testers
-- set1 = eval [(1, S [S [], S [S []]])] (UnionSet (SingletonSet (SingletonSet EmptySet)) (SingletonSet EmptySet))
-- set2 = eval [(1, S [S [], S [S []]])] (UnionSet  (SingletonSet EmptySet) (SingletonSet (SingletonSet EmptySet)))

-- claim3 = set1 == set2



-- Abstractions
claim2 :: Integer -> Bool
claim2 n =  (eval vonNeumannEnv (VN n)== (createNumSet n))

-- createNumSet :: Integer -> TERM v

-- createNumSet 0 = EmptySet
createNumSet :: Integer -> Set
createNumSet n = set
    where set = S $ map (eval vonNeumannEnv . VN) [0..(n-1)]









