module Lab1 where
 -- part1 constructors for set theory
data TERM v = EmptySet                             -- Constructor for the empty set
                        | SingletonSet v                       -- Constructor for a singleton set with a variable
                        | UnionSet (TERM v) (TERM v)                -- Constructor for the union of two sets
                        | IntersectionSet (TERM v) (TERM v)    -- Constructor for the intersection of two sets
                        | VarSet v                                               -- Constructor for a set variable
                        | VN Integer                                                 -- Constructor for a von neumann encoded natural number

data PRED v = Subset (TERM v) (TERM v)             -- Constructor for subset of two sets
                    | ElementOf (TERM v) (TERM v)          -- Constructor for element of a set
                    | Not (PRED v)                         -- Constructor for negation of a predicate
                    | And (PRED v) (PRED v)                -- Constructor for conjunction of two predicates
                    | Or (PRED v) (PRED v)                 -- Constructor for disjunction of two predicates
                    | Implies (PRED v) (PRED v)            -- Constructor for implication of two predicates



newtype Set = S [Set] deriving Eq
type Env var dom = [(var,dom)]

eval :: Eq v => Env v Set -> TERM v -> Set
eval _ EmptySet = S [] -- Evaluate the empty set to an empty set
eval env (SingletonSet x) = case lookup x env of
    Just set -> set -- Evaluate a variable to its corresponding set
    Nothing -> S [] -- Default to an empty set if the variable is not in the environment
eval env (UnionSet t1 t2) = unionSets (eval env t1) (eval env t2)
eval env (IntersectionSet t1 t2) = intersectSets (eval env t1) (eval env t2)
eval env (VarSet x) = case lookup x env of
    Just set -> set
    Nothing -> S []
eval _ (VN n) = vnEnc n

unionSets :: Set -> Set -> Set
unionSets (S set1) (S set2) = S (set1 ++ set2)

intersectSets :: Set -> Set -> Set
intersectSets (S set1) (S set2) = S [x | x <- set1, x `elem` set2]


check  :: Eq v => Env v Set -> PRED v -> Bool
check env (Subset t1 t2) =
    isSubset (eval env t1) (eval env t2)
check env (ElementOf t1 t2) =
    elementOf (eval env t1) (eval env t2)



isSubset :: Set -> Set -> Bool
isSubset (S set1) (S set2) = all (`elem` set2) set1

elementOf :: Set -> Set -> Bool
elementOf x (S setX) = x `elem` setX

-- show function for sets
instance Show Set where
    show (S []) = "{}"
    show (S [x]) = "{" ++ show x ++ "}"
    show (S (x:xs)) = "{" ++ show x ++ "," ++ show (S xs) ++ "}"

--  von neumann encoding of natural numbers
vnEnc :: Integer -> Set
vnEnc n
    | n == 0 = S []
    | otherwise = unionSets (vnEnc (n - 1)) (S [vnEnc (n - 1)])






    



