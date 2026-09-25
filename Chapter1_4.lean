-- structures

#check 1.2
#check 0.0
#check -3.14159

#check 0
#check (0 : Float)

structure Point where
  x : Float
  y : Float

def origin : Point := { x := 0.0, y := 0.0 }

#eval origin
#eval origin.x
#eval origin.y

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

#eval addPoints { x := 1.5, y := 32 } { x := -8, y := 0.2 }

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt ((p1.x - p2.x)^2 + (p1.y - p2.y)^2)

#eval distance { x := 3.0, y := 4.0 } origin

structure Point3D where
  x : Float
  y : Float
  z : Float

def origin3D : Point3D := { x := 0, y := 0, z := 0 }

#eval origin3D

-- #check { x := 0, y := 0}

#check ({ x := 0, y := 0 } : Point)
#check { x := 0, y := 0 : Point }

def zeroX (p : Point) : Point :=
  { p with x := 0 }

#eval zeroX { x := 10.0, y := -12.3 }

def fourAndThree : Point := { x := 4, y := 3 }

#eval zeroX fourAndThree
#eval fourAndThree

-- constructors

#check Point.mk 1.2 (-3.0)

structure FloatPair where
  mkFloatPair ::
  first : Float
  second : Float

def pair1 : FloatPair := .mkFloatPair 1.0 2.0

#check pair1
#eval pair1

#check (FloatPair.mkFloatPair)
#check (FloatPair.first)
#check (FloatPair.second)

#eval "one string".append " and another"

def Point.modifyBoth (f : Float -> Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

#eval fourAndThree.modifyBoth Float.sqrt

-- exercises

structure RectangularPrism where
  height : Float
  width : Float
  depth : Float

def volume (r : RectangularPrism) : Float :=
  r.height * r.width * r.depth

#eval volume { height := 10, width := 20, depth := 30 : RectangularPrism }

structure Segment where
  start : Point
  stop : Point

def length (s : Segment) : Float :=
  distance s.start s.stop

#eval length { start := fourAndThree, stop := origin }
