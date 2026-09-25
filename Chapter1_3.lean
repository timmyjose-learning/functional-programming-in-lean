-- Functions and Definitions

def name := "Hello"

def lean : String := "Lean"

#eval String.append name (String.append ", " lean)

def add1 (n : Nat) : Nat := n + 1

#eval add1 7

def maximum (n : Nat) (k : Nat) : Nat :=
  if n >= k then
    n
  else
    k

#eval maximum 7 11
#eval maximum 11 7

def spaceBetween (before : String) (after : String) : String :=
  String.append before (String.append " " after)

#eval spaceBetween name lean

#check maximum
#check (maximum)

-- exercises

def joinStringsWith (sep : String) (before : String) (after : String) : String :=
  String.append before (String.append sep after)

#eval joinStringsWith ", " "one" "and another"

#check joinStringsWith ":"

def volume (height : Nat) (width : Nat) (depth : Nat) : Nat :=
  height * width * depth

#eval volume 10 20 30

def Str : Type := String

def aStr : Str := "Hola"

def NaturalNumber : Type := Nat

-- def thirtyEight : NaturalNumber := 38

def thirtyEight : NaturalNumber := (38 : Nat)

#eval thirtyEight

-- always unfolded
abbrev N : Type := Nat

def fortyTwo : N := 42

#eval fortyTwo
