-- DataTypes and Patterns

-- inductive Bool where
--   | false
--   | true

-- inductive Nat where
--   | zero
--   | succ (n : Nat)

-- Pattern Matching

def isZero (n : Nat) : Bool :=
  match n with
    | .zero => true
    | _ => false

#guard (isZero 0) == true
#guard (isZero 1) == false

def pred (n : Nat) : Nat :=
  match n with
    | .zero => .zero
    | .succ k => k

#guard (pred 0) == 0
#guard (pred 1) == 0
#guard (pred 123456) = 123455

structure Point3D where
  x : Float
  y : Float
  z : Float

def depth (p : Point3D) : Float :=
  match p with
    | { x := _h, y := _w, z := d } => d

def p1 : Point3D := { x := 1.0, y := 2.0 , z := 3.0 }

#guard (depth p1) == 3.0

def even (n : Nat) : Bool :=
  match n with
    | .zero => true
    | .succ k => not (even k)

#guard (even 0) == true
#guard (even 1) == false
#guard (even 10) == true

-- def evenLoops (n : Nat) : Bool :=
--   match n with
--     | .zero => true
--     | .succ k => not (evenLoops n)


def plus (n : Nat) (m : Nat) : Nat :=
  match m with
    | .zero => n
    | .succ k => .succ (plus n k)

#guard (plus 1 0) == 1
#guard (plus 1 2) == 3

def times (n : Nat) (m : Nat) : Nat :=
  match m with
    | .zero => .zero
    | .succ k => plus n (times n k)

#guard (times 1 0) == 0
#guard (times 0 1) == 0
#guard (times 1 2) == 2
#guard (times 2 1) == 2
#guard (times 2 3) == 6
#guard (times 3 2) == 6

def minus (n : Nat) (m : Nat) : Nat :=
  match m with
    | .zero => n
    | .succ k => pred (minus n k)

#guard (minus 1 0) == 1
#guard (minus 0 1) == 0
#guard (minus 2 3) == 0
#guard (minus 3 2) == 1
