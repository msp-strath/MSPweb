module SK where

open import Data.Unit
open import Data.Nat
open import Data.Maybe
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Function

{-



'##::::'##::'######::'########:::::'##:::::'#####::::::'##:::
 ###::'###:'##... ##: ##.... ##::'####::::'##.. ##:::'####:::
 ####'####: ##:::..:: ##:::: ##::.. ##:::'##:::: ##::.. ##:::
 ## ### ##:. ######:: ########::::: ##::: ##:::: ##:::: ##:::
 ##. #: ##::..... ##: ##.....:::::: ##::: ##:::: ##:::: ##:::
 ##:.:: ##:'##::: ##: ##::::::::::: ##:::. ##:: ##::::: ##:::
 ##:::: ##:. ######:: ##:::::::::'######::. #####::::'######:
..:::::..:::......:::..::::::::::......::::.....:::::......::

Dependently Typed Programming: Who knows what you are doing?

Fredrik Nordvall Forsberg, MSP
29 September 2021

Based in part on

Wouter Swierstra: A correct-by-construction conversion to combinators
https://webspace.science.uu.nl/~swier004/publications/2021-jfp-submission-2.pdf


-}

-- Lambda calculus

i : {A : Set} → A → A
i x = x

k : {A B : Set} → A → B → A
k = λ x → λ y → x

s : {A B C : Set} → (A → B → C) → (A → B) → (A → C)
s = λ f → λ g → λ x → f x (g x)

-- FACT: Every λ-term can be represented using (variables), s, k, and
-- application only. Concretely: Given a representation of a λ-term,
-- we can compute its "combinator" representation.

-- But: this is very fiddly. How can we, bears of very little brain,
-- get it right?

-- First we represent the types involved:
data Ty : Set where
  ι : Ty
  _⇒_ : (σ : Ty) → (τ : Ty) → Ty

infixr 5 _⇒_

-- And what they mean:
Val : Ty → Set
Val ι = ℕ
Val (σ ⇒ τ) = Val σ → Val τ


-- Next we need to represent a context for the types of variables
data Ctx : Set where
  [] : Ctx                -- "empty context"
  _-,_ : Ctx → Ty → Ctx   -- Γ -, σ is "Γ extended by x : σ"

infixl 5 _-,_

-- Evidence of a type occurring in a context
data _∈_ (σ : Ty) : Ctx → Set where
  here  : ∀ {Γ}           → σ ∈ Γ -, σ
  there : ∀ {Γ τ} → σ ∈ Γ → σ ∈ Γ -, τ

_ : ∀ {σ σ''} → σ ∈ [] -, σ -, σ -, σ''
_ = (there here)

infix 4 _∈_

-- Welltyped lambda terms
data Term : Ctx → Ty → Set where
  Var : ∀ {Γ σ}   → σ ∈ Γ                      → Term Γ σ
  App : ∀ {Γ σ τ} → Term Γ (σ ⇒ τ)  → Term Γ σ → Term Γ τ
  Lam : ∀ {Γ σ τ} → Term (Γ -, σ) τ            → Term Γ (σ ⇒ τ)

i' : ∀ {Γ σ} → Term Γ (σ ⇒ σ)
i' = Lam (Var here) -- λ x . x


-- The meaning of a term

Env : Ctx → Set
Env [] = ⊤
Env (Γ -, σ) = Env Γ × Val σ

lookup : ∀ {Γ σ} → σ ∈ Γ → Env Γ → Val σ
lookup here (ξ , x) = x
lookup (there i) (ξ , x) = lookup i ξ

⟦_⟧ : ∀ {Γ σ} → (t : Term Γ σ) → Env Γ → Val σ
⟦ Var i ⟧ ξ = lookup i ξ
⟦ App t t' ⟧ ξ = ⟦ t ⟧ ξ (⟦ t' ⟧ ξ)
⟦ Lam t ⟧ ξ = λ x → ⟦ t ⟧ (ξ , x)

-- Representing combinators

data Comb (Γ : Ctx) : (σ : Ty) → (f : Env Γ → Val σ) → Set where
  Var : ∀ {σ}   → (i : σ ∈ Γ) → Comb Γ σ (λ ξ → lookup i ξ)
  App : ∀ {σ τ f g} → Comb Γ (σ ⇒ τ) f → Comb Γ σ g → Comb Γ τ (λ ξ → f ξ (g ξ))
  I : ∀ {σ} → Comb Γ (σ ⇒ σ) (λ ξ → λ x → x)
  S : ∀ {σ τ τ'} → Comb Γ ((σ ⇒ τ ⇒ τ') ⇒ (σ ⇒ τ) ⇒ σ ⇒ τ') (λ ξ f g x → (f x) (g x))
  K : ∀ {σ τ} → Comb Γ (σ ⇒ τ ⇒ σ) (λ ξ x y → x)

lambda : ∀ {Γ σ τ f} →
         Comb (Γ -, σ) τ f →  Comb Γ (σ ⇒ τ) (λ ξ x → f (ξ , x))
lambda (Var here) = I
lambda (Var (there i)) = App K (Var i)
lambda (App t t') = App (App S (lambda t)) (lambda t')
lambda I = App K I
lambda S = App K S
lambda K = App K K


translate : ∀ {Γ σ} → (t : Term Γ σ) → Comb Γ σ ⟦ t ⟧
translate (Var i) = Var i
translate (App t t') = App (translate t) (translate t')
translate (Lam t) = lambda (translate t)

-- Examples

S' : ∀ {Γ σ τ τ'} → Comb Γ ((σ ⇒ τ ⇒ τ') ⇒ (σ ⇒ τ) ⇒ σ ⇒ τ') (λ ξ f g x → (f x) (g x))
S' = translate (Lam (Lam (Lam (App (App (Var (there (there here))) (Var here)) (App (Var (there here)) (Var here))))))

I' : ∀ {Γ σ} → Comb Γ (σ ⇒ σ) (λ ξ → λ x → x)
I' = translate i'

K' : ∀ {Γ σ τ} → Comb Γ (σ ⇒ τ ⇒ σ) (λ ξ x y → x)
K' = translate (Lam (Lam (Var (there here))))

module BonusNotCoveredInTalk where

  -- We add B and C combinators as variants of S which only copies x to one subterm
  data Comb' (Γ : Ctx) : (σ : Ty) → (f : Env Γ → Val σ) → Set where
    Var : ∀ {σ}   → (i : σ ∈ Γ) → Comb' Γ σ (λ ξ → lookup i ξ)
    App : ∀ {σ τ f g} → Comb' Γ (σ ⇒ τ) f → Comb' Γ σ g → Comb' Γ τ (λ ξ → f ξ (g ξ))
    I : ∀ {σ} → Comb' Γ (σ ⇒ σ) (λ ξ → λ x → x)
    S : ∀ {σ τ τ'} → Comb' Γ ((σ ⇒ τ ⇒ τ') ⇒ (σ ⇒ τ) ⇒ σ ⇒ τ') (λ ξ f g x → (f x) (g x))
    B : ∀ {σ τ τ'} → Comb' Γ (    (τ ⇒ τ') ⇒ (σ ⇒ τ) ⇒ σ ⇒ τ') (λ ξ f g x →  f    (g x))
    C : ∀ {σ τ τ'} → Comb' Γ ((σ ⇒ τ ⇒ τ') ⇒      τ  ⇒ σ ⇒ τ') (λ ξ f g x → (f x)  g)
    K : ∀ {σ τ} → Comb' Γ (σ ⇒ τ ⇒ σ) (λ ξ x y → x)

  -- We want to find out if a given combinator uses the most recent
  -- variable or not. If it does, we return the strengthened term in
  -- the tighter context.

  dropFst : ∀ {Γ σ} → Env (Γ -, σ) → Env Γ
  dropFst (ξ , x) = ξ

  strengthen : forall {Γ σ τ f} → Comb' (Γ -, σ) τ f
             → Maybe (Σ[ f' ∈ _ ] (f ≡ f' ∘′ dropFst × Comb' Γ τ f'))
  strengthen S = just (_ , (refl , S))
  strengthen K = just (_ , (refl , K))
  strengthen I = just (_ , (refl , I))
  strengthen B = just (_ , (refl , B))
  strengthen C = just (_ , (refl , C))
  strengthen (App t t') = do
    (_ , refl , t) <- strengthen t
    (_ , refl , t') <- strengthen t'
    just (_ , refl , App t t')
  strengthen (Var here) = nothing -- this is the only case where we
                                  -- fail directly: we are really using the variable
  strengthen (Var (there i)) = just (_ , refl , Var i)

  -- Now in the application case, we can give more optimised versions
  -- using B and C in case x is not used in some of the subterms
  lambda' : ∀ {Γ σ τ f} →
           Comb' (Γ -, σ) τ f →  Comb' Γ (σ ⇒ τ) (λ ξ x → f (ξ , x))
  lambda' (Var here) = I
  lambda' (Var (there i)) = App K (Var i)
  lambda' (App t t') with strengthen t | strengthen t'
  ... | just (_ , refl , t) | just (_ , refl , t') = App K (App t t')
  ... | just (_ , refl , t) | nothing = App (App B t)          (lambda' t')
  ... | nothing | just (_ , refl , t') = App (App C (lambda' t)) t'
  ... | nothing | nothing              = App (App S (lambda' t)) (lambda' t')
  lambda' I = App K I
  lambda' S = App K S
  lambda' B = App K B
  lambda' C = App K C
  lambda' K = App K K
