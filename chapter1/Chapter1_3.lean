-- Functions and Definitions

def hello := "Hello"
def lean : String := "Lean"

#eval String.append hello (String.append ", " lean)

def add1 (n : Nat) : Nat := n + 1

#eval add1 7

def maximum (n : Nat) (k : Nat) : Nat :=
  if k > n then k else n

#eval maximum 11 10
#eval maximum 10 11

def spaceBetween (before : String) (after : String) : String :=
  String.append before (String.append " " after)

#eval spaceBetween hello lean

#check (add1)
#check (maximum)
#check (spaceBetween)

#check (maximum 3)
#check (spaceBetween hello)

-- Exercises

def joinStringsWith (sep : String) (before : String) (after : String) : String :=
  String.append before (String.append sep after)

#eval (joinStringsWith ", " hello lean) = "Hello, Lean"
#eval (joinStringsWith ", " "one" "and another") = "one, and another"

--- The type of `joinStringsWith ": " is `String -> String -> String`
#check (joinStringsWith ": ")

def volume (height : Nat) (width : Nat) (depth : Nat): Nat := height * width * depth

#eval (volume 10 20 30) = 6000

-- type aliases
def Str : Type := String

def aStr : Str := "Hola"

-- pitfalls to watch out for

def NaturalNumber : Type := Nat

-- failed to synthesize instance of type class
--   OfNat NaturalNumber 38
-- numerals are polymorphic in Lean, but the numeral `38` cannot be used in a context where the expected type is
--   NaturalNumber
-- due to the absence of the instance above

-- Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-- def thirtyEight : NaturalNumber := 38

-- this is okay though since we narrow it down to `Nat`
def thirtyEight : NaturalNumber := (38 : Nat)

-- `abbrev` always unfolds, so this works fine
abbrev N : Type := Nat

def fortyTwo : N := 42
