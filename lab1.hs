module Lab1 where
 -- part1 constructors for set theory
data TERM v = EmptySet                             -- Constructor for the empty set
                        | SingletonSet v                       -- Constructor for a singleton set with a variable
                        | UnionSet (TERM v) (TERM v)                -- Constructor for the union of two sets
                        | IntersectionSet (TERM v) (TERM v)    -- Constructor for the intersection of two sets
                        | VarSet v                                               -- Constructor for a set variable

data PRED v = Subset (TERM v) (TERM v)             -- Constructor for subset of two sets
                    | ElementOf (TERM v) (TERM v)          -- Constructor for element of a set
                    | Not (PRED v)                         -- Constructor for negation of a predicate
                    | And (PRED v) (PRED v)                -- Constructor for conjunction of two predicates
                    | Or (PRED v) (PRED v)                 -- Constructor for disjunction of two predicates
                    | Implies (PRED v) (PRED v)            -- Constructor for implication of two predicates

type Env var dom = [(var,dom)]

newtype Set = S [Set]
eval :: Eq v => Env v Set -> TERM v -> Set
eval env EmptySet = S []
eval env (SingletonSet x) = case lookup x env of
    Just set -> set -- Evaluate a variable to its corresponding set
    Nothing -> S [] -- Default to an empty set if the variable is not in the environment
eval env (UnionSet t1 t2) = unionSets (eval env t1) (eval env t2)
eval env (IntersectionSet t1 t2) = intersectSets (eval env t1) (eval env t2)
eval env (VarSet x) = case lookup x env of
    Just set -> set
    Nothing -> S []


check  :: Eq v => Env v Set -> PRED v -> Bool
check



