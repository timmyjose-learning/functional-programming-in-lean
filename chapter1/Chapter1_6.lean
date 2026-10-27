-- Polymorphism

structure PPoint (α : Type) where
  x : α
  y : α

def natOrigin : PPoint Nat := { x := 0, y := 0}
def floatOrigin : PPoint Float := { x := 0.0, y := 0.0 }

#check natOrigin
#check floatOrigin

def replaceX (α : Type) (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }

#check (replaceX)
#check (replaceX Nat)
#check (replaceX Float)

def natPoint := replaceX Nat natOrigin 11
#guard natPoint.x == 11

def floatPoint := replaceX Float floatOrigin 1.23
#guard floatPoint.x == 1.23

#eval replaceX Nat natOrigin 5

inductive Sign where
  | pos
  | neg

def posOrNegThree (sign : Sign) : match sign with | Sign.pos => Nat | Sign.neg => Int :=
  match sign with
    | Sign.pos => 3
    | Sign.neg => -3

#guard posOrNegThree Sign.pos == 3
#guard posOrNegThree Sign.neg == -3

-- Linked Lists

def primesUnder10 : List Nat := [2, 3, 5, 7]

-- inductive List (α : Type) where
--   | nil : List α
--   | cons : α -> List α -> List α

def explicitPrimesUnder10 : List Nat :=
  List.cons 2 (List.cons 3 (List.cons 5 (List.cons 7 List.nil)))

#guard primesUnder10 == explicitPrimesUnder10

def length (α : Type) (xs : List α) : Nat :=
  match xs with
     | .nil => .zero
     | .cons _ ys => .succ (length α ys)

#guard length Nat explicitPrimesUnder10 == 4

def sugaredLength (α : Type) (xs : List α) : Nat :=
  match xs with
    | [] => .zero
    | _ :: ys => .succ (sugaredLength α ys)

#guard sugaredLength Nat primesUnder10 == 4

-- implicit types

def implicitReplaceX {α : Type} (point: PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }

def natPoint2 := implicitReplaceX natOrigin 1
#guard natPoint2.x == 1 && natPoint2.y == 0

def floatPoint2 := implicitReplaceX floatOrigin 1.2
#guard floatPoint2.x == 1.2 && floatPoint2.y == 0.0

def implicitLength { α : Type} (xs : List α) : Nat :=
  match xs with
    | [] => .zero
    | _ :: ys => .succ (implicitLength ys)

#guard implicitLength primesUnder10 == 4

#check List.length
#check List.length (α := Int)

-- more built-in datatypes

-- inductive Option (α : Type) where
--   | none : Option α
--   | some (val : α) : Option α

-- def List.head? {α : Type} (xs : List α) : Option α :=
--   match xs with
--     | [] => .none
--     | y :: _ => .some y

#guard primesUnder10.head? == some 2
#guard primesUnder10.head! == 2

#eval [].head? (α := Int)
#eval ([] : List Nat).head?

-- structure Prod (α : Type) (β : Type) where
--   fst : α
--   snd : β

def fives : Prod String Int := { fst := "Hello", snd := 5 }
def alsoFives : String × Int := { fst := "Hello", snd := 5 }
def alsoAlsoFives : String × Int := ("Hello", 5)

#guard fives == alsoFives
#guard fives == alsoAlsoFives

def sevens : String × Int × Nat := ("VII", 7, 4 + 3)
-- right-associative
def alsoSevens : String × (Int × Nat) := ("VII", (7, 4 + 3))

#guard sevens == alsoSevens

-- inductive Sum (α : Type) (β : Type) : Type where
--   | inl : α -> Sum α β
--   | inr : β -> Sum α β

def PetName : Type := String ⊕ String

def animals : List PetName :=
  [.inl "Spot", .inr "Tiger", .inl "Fifi", .inl "Rex", .inr "Floof"]

def countDogs (pets : List PetName) : Nat :=
  match pets with
    | [] => 0
    | .inl _ :: morePets => 1 + countDogs morePets
    | _ :: morePets => countDogs morePets

def countCats (pets : List PetName) : Nat :=
  match pets with
    | [] => 0
    | .inr _ :: morePets => 1 + countCats morePets
    | _ :: morePets => countCats morePets

#guard countDogs animals == 3
#guard countCats animals == 2

-- inductive Unit : Type where
--   | unit : Unit

inductive ArithExpr (ann : Type) : Type where
  | int : ann -> Int -> ArithExpr ann
  | plus : ann -> ArithExpr ann -> ArithExpr ann -> ArithExpr ann
  | minus : ann -> ArithExpr ann -> ArithExpr ann -> ArithExpr ann
  | times : ann -> ArithExpr ann -> ArithExpr ann -> ArithExpr ann

def dummyExpr : ArithExpr Unit := .int () 12
#eval dummyExpr

-- inductive MyType : Type where
--   | ctor : (α : Type) -> α -> MyType

inductive MyType (α : Type) : Type where
  | ctor : α -> MyType α

  -- Important: recursive functions on multiple parameters should pattern match on each parameter separately
  -- so this does not work
  -- def sameLength {α : Type} (xs : List α) (ys : List β) : Bool :=
  --   match (xs, ys) with
  --     | ([], []) => true
  --     | (_ :: xs', _ :: ys') => sameLength xs' ys'
  --     | _ => false

  -- instead, pattern-match on each parameter separately, with nested pattern-matching
  def sameLength {α : Type} (xs : List α) (ys : List α) : Bool :=
    match xs with
      | [] => match ys with | [] => true | _ => false
      | _ :: xs' => match ys with | [] => false | _ :: ys' => sameLength xs' ys'

#guard sameLength ([] : List Int) ([] : List Int) == true
#guard sameLength [1] [1] == true
#guard sameLength [1] [2] == true
#guard sameLength [1] [1, 2] == false
#guard sameLength [1, 2, 3] [4, 5, 6] == true

-- deriving Repr
inductive WoodSplittingTool : Type where
  | axe
  | maul
  | froe
  deriving Repr

#eval WoodSplittingTool.froe

def allTools : List WoodSplittingTool := [
  .axe, .maul, .froe
]

#eval allTools

inductive Firewood : Type where
  | birch
  | pine
  | beech
  deriving Repr, BEq

def allFirewood : List Firewood := [
  Firewood.pine,
  Firewood.birch,
  Firewood.beech
]

#eval allFirewood


-- exercises

-- Write a functiont to return the last entry in a list. For an empty list, return an `Option`.

def List.last? {α : Type} (xs : List α) : Option α :=
  match xs with
    | [] => .none
    | [x] => .some x
    | _ :: ys => List.last? ys

#guard List.last? ([] : List Nat) == .none
#guard List.last? [1] == .some 1
#guard List.last? [1, 2] == .some 2
#guard List.last? [1, 2, 3, 4, 5] == .some 5

-- Write a function that returns the first entry in a list that satisfiesa predicate. If no match or empty list, return an `Option`.

def List.findFirst? {α : Type} (xs : List α) (pred : α -> Bool) : Option α :=
  match xs with
    | [] => .none
    | y :: ys => if pred y then y else List.findFirst? ys pred

def isEven (n : Nat) : Bool :=
  n % 2 == 0

#guard List.findFirst? ([] : List Nat) isEven == .none
#guard List.findFirst? [1] isEven == .none
#guard List.findFirst? [1, 3, 5, 7, 9, 11] isEven == .none
#guard List.findFirst? [1, 2] isEven == .some 2
#guard List.findFirst? [1, 2, 3, 4, 5] isEven == .some 2

-- Write a function that switches the two fields in a pair. So, `Prod α β` -> `Prod β α`
def Prod.switch {α β : Type} (pair : α × β) : β × α :=
  { fst := pair.snd, snd := pair.fst }

def p1 : String × Int := ("Hello", 11)
def p2 := p1.switch

#guard p2.fst == 11
#guard p2.snd == "Hello"

-- Rewrite the `PetName` example with a custom inductive type and compare it to the version that uses `Sum`

inductive AlsoPetName : Type where
  | dog : String → AlsoPetName
  | cat : String → AlsoPetName

def pets : List AlsoPetName := [ .dog "Rufus", .cat "Pickles", .dog "Poozer", .dog "Barky", .cat "Fluffball"]

def countAlsoDogs (pets : List AlsoPetName) : Nat :=
  match pets with
    | [] => 0
    | .dog _ :: ps => 1 + countAlsoDogs ps
    | .cat _ :: ps => countAlsoDogs ps

def countAlsoCats (pets : List AlsoPetName) : Nat :=
  match pets with
    | [] => 0
    | .cat _ :: ps => 1 + countAlsoCats ps
    | .dog _ :: ps => countAlsoCats ps

#guard countAlsoDogs pets == 3
#guard countAlsoCats pets == 2

-- Write a function `zip` that zips together two lists, capped at the length of the shorter list.

-- def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
--   match (xs, ys) with
--     | (_, []) => []
--     | ([], _) => []
--     | (x :: xs, y :: ys) => (x, y) :: zip xs ys
def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs with
    | [] => []
    | x :: xs' => match ys with | [] => [] | y :: ys' => (x, y) :: zip xs' ys'

#guard zip [1] ["hello"] == [(1, "hello")]
#guard zip ([] : List Int) ([] : List Int) == []
#guard zip ([] : List Int) ["hello"] == []
#guard zip [1] ([] : List String) == []
#guard zip [1, 2, 3, 4, 5] ["hello", "world", "again"]  == [(1, "hello"), (2, "world"), (3, "again")]


-- Write a polymorphic function take that returns the first n entries in a list, where n is a Nat.
-- If the list contains fewer than n entries, then the resulting list should be the entire input list.
-- #eval take 3 ["bolete", "oyster"] should yield ["bolete", "oyster"], and
-- #eval take 1 ["bolete", "oyster"] should yield ["bolete"].
def List.myTake {α : Type} (n : Nat) (xs : List α) : List α :=
  if List.length xs < n then
    xs
  else
    match n with
      | .zero => []
      | .succ k => match xs with | [] => [] | x :: xs' => x :: take k xs'

#guard List.myTake 3 ["bolete", "oyster"] == ["bolete", "oyster"]
#guard List.myTake 1 ["bolete", "oyster"] == ["bolete"]
#guard List.myTake 10 ["bolete", "oyster"] == ["bolete", "oyster"]

-- Using the analogy between types and arithmetic, write a function that distributes products over sums. In other words, it should have type α × (β ⊕ γ) → (α × β) ⊕ (α × γ).
def prodOverSum {α β γ : Type} (inp : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match inp with
    | (a, .inl b) => .inl (a, b)
    | (a, .inr c) => .inr (a, c)

def ps1 : String × (Nat ⊕ Bool) := ("Hello", .inl 42)
def ps2 : String × (Nat ⊕ Bool) := ("Hello", .inr true)

#guard prodOverSum ps1 == .inl ("Hello", 42)
#guard prodOverSum ps2 == .inr ("Hello", true)

-- Using the analogy between types and arithmetic, write a function that turns multiplication by two into a sum. In other words, it should have type Bool × α → α ⊕ α.
def multToSum {α : Type} (inp : Bool × α) : α ⊕ α :=
  match inp with
    | (false, x) => .inl x
    | (true, x) => .inr x

#eval multToSum (true, 42)
#eval multToSum (false, 42)
#eval multToSum (true, "hello")
#eval multToSum (false, "hello")

#guard multToSum (true, 10) == .inr 10
#guard multToSum (false, 10) == .inl 10
#guard multToSum (true, "Lean") == .inr "Lean"
#guard multToSum (false, "Lean") == .inl "Lean"

#guard multToSum (true, Firewood.pine) == .inr Firewood.pine
#guard multToSum (false, Firewood.pine) == .inl Firewood.pine
