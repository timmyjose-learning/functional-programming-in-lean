-- Structures

#check 1.2
#check -355.1212
#check 0.0

#check 0
#check (0 : Float)

structure Point where
  x : Float
  y : Float
  deriving instance BEq for Point

def Point.modifyBoth (f : Float -> Float) (p : Point) : Point :=
  { x := f p.x , y := f p.y }

#check (Point.mk)
#check (Point.x)
#check (Point.y)

def origin : Point := { x := 0.0, y := 0.0 }

#eval origin
#eval origin.x
#eval origin.y

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x , y := p1.y + p2.y }

def point1 : Point := { x := 3.0, y := 4.0 }
#eval addPoints origin point1

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt ((p1.x - p2.x)^ 2 + (p1.y - p2.y) ^ 2)

#guard (distance point1 origin) == 5.0

structure Point3D where
  x : Float
  y : Float
  z : Float

def origin3D : Point3D := { x := 0.0 , y := 0.0, z := 0.0 }

#check ({ x := 1.0 , y := 2.0 } : Point)

-- def zeroX (p : Point) : Point :=
--   { x := 0.0, y := p.y }

def zeroX (p : Point) : Point :=
  { p with x := 0.0 }

#eval zeroX point1
#guard zeroX point1 == { x := 0.0, y := 4.0 : Point }

def fourAndThree : Point := { x := 3.4, y := 4.3 }

#eval fourAndThree.modifyBoth Float.floor
#eval Point.modifyBoth Float.floor fourAndThree

-- Exercises

structure RectangularPrism  where
  height : Float
  width : Float
  depth : Float
  deriving instance BEq for RectangularPrism

def RectangularPrism.volume (prism : RectangularPrism) : Float :=
  prism.height * prism.width * prism.depth

structure Segment where
  start : Point
  finish : Point
  deriving instance BEq for Segment

def Segment.length (seg : Segment) : Float :=
  Float.sqrt ((seg.start.x - seg.finish.x)^ 2 + (seg.start.y - seg.finish.y) ^ 2)

def segment1 : Segment := { start := { x := 0.0, y := 0.0 }, finish := { x := 3.0, y := 4.0 }}

#guard segment1.length == 5.0
