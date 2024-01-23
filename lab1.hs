module Lab1 where
import Data.List
 -- part1 constructors for set theory
data TERM v = EmptySet                             -- Constructor for the empty set
            | SingletonSet v                       -- Constructor for a singleton set with a variable
            | UnionSet (TERM v) (TERM v)           -- Constructor for the union of two sets
            | IntersectionSet (TERM v) (TERM v)    -- Constructor for the intersection of two sets
            | VarSet v                             -- Constructor for a set variable
            | VN Integer                           -- Constructor for a von neumann encoded natural number

data PRED v = Subset (TERM v) (TERM v)             -- Constructor for subset of two sets
            | ElementOf (TERM v) (TERM v)          -- Constructor for element of a set
            | Not (PRED v)                         -- Constructor for negation of a predicate
            | And (PRED v) (PRED v)                -- Constructor for conjunction of two predicates
            | Or (PRED v) (PRED v)                 -- Constructor for disjunction of two predicates
            | Implies (PRED v) (PRED v)            -- Constructor for implication of two predicates



newtype Set = S [Set] deriving Eq -- Set is a list of sets

type Env var dom = [(var,dom)]    -- Environment is a list of pairs of variables and their corresponding domain

eval :: Eq v => Env v Set -> TERM v -> Set
eval _ EmptySet = S [] -- Evaluate the empty set to an empty set
eval env (SingletonSet x) = case lookup x env of
    Just set -> set -- Evaluate a variable to its corresponding set
    Nothing -> S [] -- Default to an empty set if the variable is not in the environment
eval env (UnionSet t1 t2) = unionSets (eval env t1) (eval env t2) -- Evaluate the union of two sets
eval env (IntersectionSet t1 t2) = intersectSets (eval env t1) (eval env t2) -- Evaluate the intersection of two sets
eval env (VarSet x) = case lookup x env of -- Evaluate a variable to its corresponding set
    Just set -> set
    Nothing -> S []
eval env (VN n) = vnEnc n                 -- Evaluate a von neumann encoded natural number to its corresponding set   

unionSets :: Set -> Set -> Set
unionSets (S set1) (S set2) = S $ union set1 set2
--unionSets (S set1) (S set2) = S (set1 ++ set2) use union instead of concat?
intersectSets :: Set -> Set -> Set
intersectSets (S set1) (S set2) = S [x | x <- set1, x `elem` set2]


check  :: Eq v => Env v Set -> PRED v -> Bool
check env (Subset t1 t2) = isSubset (eval env t1) (eval env t2)
check env (ElementOf t1 t2) = elementOf (eval env t1) (eval env t2)
check env (Not p) = not (check env p)
check env (And p1 p2) = check env p1 && check env p2
check env (Or p1 p2) = check env p1 || check env p2
check env (Implies p1 p2) = not (check env p1) || check env p2



-- helpers for checking if its a subset or an element of a set
isSubset :: Set -> Set -> Bool
isSubset (S set1) (S set2) = all (`elem` set2) set1

elementOf :: Set -> Set -> Bool
elementOf x (S setX) = x `elem` setX

-- show function for sets
instance Show Set where
    show :: Set -> String
    show (S []) = "{}"
    show (S [x]) = "{" ++ show x ++ "}"
    show (S (x:xs)) = "{" ++ show x ++ "," ++ show (S xs) ++ "}"

--  von neumann encoding of natural numbers
vnEnc :: Integer -> Set
vnEnc n
    | n == 0 = S [] -- case when n is zero return the empty set
    | otherwise = unionSets (vnEnc (n - 1)) (S [vnEnc (n - 1)]) -- recursive case (tail rec?)

-- claims 
claim1 :: Set -> Set -> Bool
claim1 n1 n2 = undefined

claim2 :: Set -> Bool
claim2 = undefined










