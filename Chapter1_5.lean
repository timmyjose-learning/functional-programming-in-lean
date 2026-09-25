-- datatypes and patterns

inductive MyBool where
  | false
  | true

#check MyBool.false
#eval MyBool.true

inductive MyNat where
  | zero
  | succ (n : MyNat) : MyNat

-- pattern matching

def isZero (n : Nat) : Bool :=
  match n with
    | .zero => true
    | .succ _ => false

#eval isZero 0
#eval isZero 1
#eval isZero (Nat.zero)
#eval isZero (Nat.succ (Nat.succ Nat.zero))

def pred (n : Nat) : Nat :=
  match n with
    | .zero => .zero
    | .succ k => k

#eval pred Nat.zero
#eval pred (Nat.succ (Nat.succ Nat.zero))

structure Point3D where
  x : Float
  y : Float
  z : Float

def point1 : Point3D := { x := 1.0, y := -2.0, z := 3.0 }

def depth (p : Point3D) : Float :=
  match p with
    | { x := _, y := _, z := z } => z

#eval depth point1

-- recursive functions

def even (n : Nat) : Bool :=
  match n with
    | .zero => true
    | .succ k => not (even k)

#eval even 0
#eval even 11
#eval even 12

-- def evenLoops (n : Nat) : Bool :=
--   match n with
--     | .zero => true
--     | .succ k => not (evenLoops n)

def plus (n : Nat) (m : Nat) : Nat :=
  match m with
    | .zero => n
    | .succ k => .succ (plus n k)

#eval plus 2 3 = 5
#eval plus 2 0 = 2

def minus (n : Nat) (m : Nat) : Nat :=
  match m with
    | .zero => n
    | .succ k => pred (minus n k)

#eval minus 0 1 = 0
#eval minus 2 1 = 1
#eval minus 1 1 = 0

def times (n : Nat) (m : Nat) : Nat :=
  match m with
    | .zero => .zero
    | .succ k => plus n (times n k)

#eval times 2 3 = 6
#eval times 2 1 = 2
#eval times 2 0 = 0
