
source("path.R")
source("R2mp.R")

## Errors
## Both direction and curl specified
try(knot(0, 0, dir.left=0, curl.left=1))
## Both tension and control point specified
try(knot(0, 0, cp.left.x=1, cp.left.y=1, tension.left=1))

## Printing
knot(0, 0) 
knot(0, 0, dir=0) 
knot(0, 0, dir.left=0) 
knot(0, 0, dir.left=0, dir.right=90) 
knot(0, 0, dir.right=90) 
knot(1:10, 0)
knot(1:10, 0, dir.left=0)
knot(1:10, 0, dir.left=1:10)
knot(1:2, 0, dir.left=c(1, NA), curl.left=c(NA, 1))
knot(0, 0, curl.left=1) 
knot(0, 0, curl.right=1) 
knot(0, 0, curl.left=1, curl.right=1) 
print(knot(0, 0, curl.left=1, curl.right=1), first=TRUE) 

## Building paths
knot(0, 0) + knot(1, 1)
## Only one explicit control point
knot(0, 0, cp.right.x=1, cp.right.y=1) + knot(1, 1)
## Two explicit control points
knot(0, 0, cp.right.x=1, cp.right.y=1) + knot(3, 0, cp.left.x=2, cp.left.y=1)
## Two tensions
knot(0, 0, tension.right=1) + knot(1, 1, tension.left=-4)
## Only one tension
knot(0, 0) + knot(1, 1, tension.left=-4)
## Specifying control points, tension, and direction separately from knots
knot(0, 0) + tension(-4) + knot(1, 1)
## Vectorised knot
knot(1:3, 0) + tension(-4) + knot(1, 1)
## Different operators
knot(0, 0) - knot(1, 1)
knot(0, 0) %+% knot(1, 1)
knot(0, 0) %-% knot(1, 1)
## More than two knots
knot(0, 0) + knot(1, 1) + knot(2, 2)
knot(0, 0) + knot(1, 1) + knot(2, 2) + knot(3, 3)
## Cycle
knot(0, 0) + knot(1, 1) + cycle()
##   vectorised connector 
knot(0, 0) + dir(1, 0) + knot(1, 1)
knot(0, 0) + dir(10) + knot(1, 1)
knot(0, 0) + dir(1:4*10) + knot(1, 1)
## Add path to path
p1 <- knot(0, 0) + knot(1, 1)
p2 <- knot(2, 2) + knot(3, 3)
p1 + p2

## MetaPost examples 
## p := (0,0)..(10,10)..(10,-5)..cycle;
knot(0, 0) + knot(10, 10) + knot(10, -5) + cycle()
## p := (0,0)..
##      (2,20)--
##      (10, 5)..controls (2,2) and (9,4.5)..
##      (3,10)..tension 3 and atleast 4 ..
##      (1,14){2,0} .. {0,1}(5,-4);
knot(0, 0) +
    knot(2, 20) -
    knot(10, 5) + cp(2, 2) + cp(9, 4.5) +
    knot(3, 10) + tension(3) + tension(-4) +
    knot(1, 14) + dir(2, 0) + dir(0, 1) +
    knot(5, -4)
