{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module STLC where

import Control.Monad.State
import Control.Applicative
import Data.List

{- variable identifiers -}
type Identifier = String -- I have no taste

{- Metavariable identifiers -}
type TI = Integer 
type CI = Integer

data Term = Var Identifier | Lam Identifier Term | App Term Term
            | ALam Identifier Type Term
  deriving Show

{- No separating out object-level types and contexts from meta-level -}

data Type = Con Identifier | 
            Type :-> Type | 
            TVar TI
  deriving Show

data Context = Ext Identifier Type Context | Empty | CVar CI
  deriving Show

data Constraint = EqCon Type Type |
                  CtxtIn (Identifier,Type) Context |
                  CtxtExt Context (Identifier,Type) Context |
                  TyDup Type Type Type |
                  CtxtDup Context Context Context
  deriving Show

{- given seems a better name for logic programming
   result on LHS, antecedents on RHS
-}
given = (<$)
infixl 3 `given`

infixl 8 `eqCon`

{- Sugar for three-place 'operators'

   Example usage: ctxt `ctxtExt` ctxtL & ctxtR
-}
f & p = f p
infixl 7 &

type Judgement = Context -> ConstraintGen Type

{- Type <<- (Judgement Context) generates an equality constraint 
     between LHS and result of RHS, implemented below
-}

{- Typing rules 

   Judgement-as-parameter is a context/hole for another typing rule
   Turning these into a typechecker is just a fold (see belocw)
-}

var :: Identifier -> Judgement
var i c = withTV (\ty -> ty `given` ctxtIn (i,ty) c)

lam :: Identifier -> Judgement -> Judgement
lam i jb c = withTV (\tf -> withTV (\tp -> withTV (\tr -> withCV (\cf -> 
               f tf tp tr cf))))
  where f tf tp tr cf = tf `given` cf `ctxtExt` (i, tp) & c *>
                                   tr <<- jb cf *>
                                   tf `eqCon` tp :-> tr

app :: Judgement -> Judgement -> Judgement
app jl jr c = withTV (\tf -> withTV (\tp -> withTV (\tr ->
                withCV (\cl -> withCV (\cr ->
                  f tf tp tr cl cr)))))
  where f tf tp tr cl cr = tr `given` c `ctxtDup` cl & cr *>
                                      tf <<- jl cl *>
                                      tp <<- jr cr *>
                                      tp :-> tr `eqCon` tf

alam :: Identifier -> Type -> Judgement -> Judgement
alam i ta jb c = withTV (\tap -> withTV (\taf ->
                  withTV (\tr -> withTV (\tf ->
                   withCV (\cf ->
                    f tap taf tr tf cf)))))
  where f tap taf tr tf cf = tf `given` ta `tyDup` tap & taf *>
                                        cf `ctxtExt` (i, tap) & c *>
                                        tr <<- jb cf *>
                                        tf `eqCon` taf :-> tr

{- Glue typing rules together to generate constraints from a Term -}

check :: Term -> Context -> ConstraintGen Type
check t c = (check' t) c -- Here be currying tricks!
check' (Var i) = var i
check' (Lam i t) = lam i (check' t)
check' (App l r) = app (check' l) (check' r)
check' (ALam i ty t) = alam i ty (check' t)

{- Constraint-generation applicative -}

data GenState = GS {constraints :: [Constraint],
                    nextTV :: TI,
                    nextCV :: TI}
  deriving Show

newtype ConstraintGen a = CG {unCG :: State GenState a}
  deriving (Functor, Applicative)

runConstraintGen c = runState (unCG c) (GS [] 0 0)

{- Locally bind metavariables -}
withTV f = CG (do gs@GS{nextTV = tv} <- get
                  put (gs {nextTV = tv + 1})
                  unCG $ f (TVar tv)
              )
withCV f = CG (do gs@GS{nextCV = cv} <- get
                  put (gs {nextCV = cv + 1})
                  unCG $ f (CVar cv)
              )

{- Adding constraints -}
addConstraint :: Constraint -> ConstraintGen ()
addConstraint c = 
  CG (modify (\gs@GS {constraints = cs} -> gs {constraints = c:cs}))

eqCon t t' = addConstraint $ EqCon t t'
ctxtIn (i, t) c = addConstraint $ CtxtIn (i, t) c
ctxtExt c' (i,t) c = addConstraint $ CtxtExt c' (i,t) c
tyDup i l r = addConstraint $ TyDup i l r
ctxtDup i l r = addConstraint $ CtxtDup i l r

{- type equality constraints for judgement output -}
(<<-) :: Type -> ConstraintGen Type -> ConstraintGen ()
t <<- j = CG (unCG j >>= (unCG . eqCon t))

{- Test terms -}

idTerm = Lam "x" (Var "x")
sTerm = Lam "x" $ Lam "y" $ Lam "z" $ 
          App (App (Var "x") (Var "z")) 
              (App (Var "y") (Var "z"))
wTerm = Lam "x" $ App (Var "x") (Var "x")

{- Solver

   Strategy: remove dups, then CtxtExts, then CtxtIns, then equalities

   Makes assumptions about the checker, eg: 
   dups always dup into metavariables
   polarised linearity of metavariables
-}

{- Solver state -}

data SolverState = Solve {dups :: [Constraint],
                          exts :: [Constraint],
                          ins :: [Constraint],
                          eqs :: [Constraint],
                          contexts :: [(CI, Context)],
                          types :: [(TI, Type)]
                         }
  deriving Show

emptySolverState = Solve [] [] [] [] [] []

constraintsToSolverState = foldl' insertCon emptySolverState
  where insertCon s d@TyDup{} = s {dups = d : dups s}
        insertCon s d@CtxtDup{} = s {dups = d : dups s}
        insertCon s e@CtxtExt{} = s {exts = e : exts s}
        insertCon s i@CtxtIn{} = s {ins = i : ins s}
        insertCon s e@EqCon{} = s {eqs = e : eqs s}

{- top level of solver -}

solveConstraints = solveState . constraintsToSolverState

solveState = eqStage . inStage . extStage . dupStage

{- Handle duplications by manually instantiating the things on the RHS to
   match the thing on the LHS - should be equivalent to resolving equality
   constraints immediately, though we don't have context equalities.
-}
dupStage s = foldl' dispatchDup (s {dups = []}) (dups s)
  where dispatchDup s (TyDup t (TVar tl) (TVar tr)) = 
          s {types = (tl, t) : (tr, t) : types s}
        dispatchDup s (CtxtDup c (CVar cl) (CVar cr)) = 
          s {contexts = (cl, c) : (cr, c) : contexts s}

{- Build all our contexts by solving all the CtxtExt constraints -}
extStage s = foldl' dispatchExt (s {exts = []}) (exts s)
  where dispatchExt s (CtxtExt (CVar cv) (i,t) c) = 
          s {contexts = (cv, Ext i t c) : (contexts s)}

{- Having found all the contexts, handle context lookups/CtxtIn -}
inStage s = foldl' dispatchIn (s {ins = []}) (ins s)
  where dispatchIn s (CtxtIn (i,t) c) = let t' = searchFor i c (contexts s)
                                         in s {eqs = EqCon t t': eqs s}
        searchFor i (Ext i' t c) cs | i == i' = t
                                    | otherwise = searchFor i c cs
        searchFor i Empty s = error ("Unbound variable " ++ i)
        searchFor i (CVar cv) cs = case lookup cv cs of
                                     Just c' -> searchFor i c' cs
                                     Nothing -> error "unknown CVar"

{- Finally, handle equality constraints one at a time, 
   potentially generating as we go
-}
eqStage s@Solve{eqs = []} = s
eqStage s@Solve{eqs = c:cs} = eqStage (dispatchEq (s{eqs = cs}) c)

eqStep s@Solve{eqs = c:cs} = dispatchEq (s{eqs = cs}) c

{- pseudo-unification tedium

   Always points larger TIs at smaller ones
   Doesn't prune chains, premature optimisation and all that
   "Recurses" along type structure by generating new equality constraints
-}

{- Dereference TVars as far as possible -}

dispatchEq s (EqCon (TVar a) b) = dispatchEq' s (unchain s a) b
dispatchEq s (EqCon a b) = dispatchEq' s a b

dispatchEq' s a (TVar b) = handleEq s a (unchain s b)
dispatchEq' s a b = handleEq s a b

{- Check for a top-level match and/or handle variable updates 

   Here's where we generate new constraints for :-> branches
-}

handleEq s (Con a) (Con b) | a == b = s
                           | otherwise = error "Constructor mismatch"
handleEq s (l :-> r) (l' :-> r') = s {eqs = EqCon l l' : EqCon r r' : eqs s}
handleEq s (TVar a) (TVar b) 
  | a == b = s
  | otherwise = s {types = (max a b, TVar (min a b)) : types s}
handleEq s (TVar v) t = updateTVar v t s
handleEq s t (TVar v) = updateTVar v t s

{- Helpers + occurs check -}

occurs :: TI -> Type -> SolverState -> Bool
occurs v Con{} _ = False
occurs v (l :-> r) s = occurs v l s || occurs v r s
occurs v (TVar v') s = case unchain s v' of
                         (TVar v'') -> v'' == v
                         t -> occurs v t s
updateTVar v t s | occurs v t s = error "Occurs check failure"
                 | otherwise = s {types = (v, t) : types s}

unchain s v = case lookup v (types s) of
                Nothing -> TVar v
                Just (TVar v') -> unchain s v'
                Just t -> t

{- Substitute out all the TVars we can, given a SolverState -}
                
extractType s (TVar v) = case unchain s v of
                           t@TVar{} -> t 
                           t -> extractType s t
extractType s (l :-> r) = (extractType s l) :-> (extractType s r)
extractType s c@Con{} = c

{- An actual, finished typechecker! -}

typecheck term = let (t, GS{constraints = cs}) = 
                       runConstraintGen (check term Empty)
                     solved = solveConstraints cs
                  in extractType solved t