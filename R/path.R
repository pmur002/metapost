
## Specify a MetaPost path via R functions and data structures

## Individual knots
knot <- function(x, y,
                 dir=NA, dir.left=dir, dir.right=dir,
                 cp.left.x=NA, cp.right.x=NA,
                 cp.left.y=NA, cp.right.y=NA,
                 curl.left=NA, curl.right=NA,
                 tension.left=NA, tension.right=NA)
{
    if (any(!is.na(dir.left) &
            !is.na(curl.left)))
        stop("Invalid to specify both dir and curl at once")
    if (any(!is.na(dir.right) &
            !is.na(curl.right)))
        stop("Invalid to specify both dir and curl at once")
    if (any(!(is.na(cp.left.x) | is.na(cp.left.y)) &
            !is.na(tension.left)))
        stop("Invalid to specify both control point and tension at once")
    if (any(!(is.na(cp.right.x) | is.na(cp.right.y)) &
            !is.na(tension.right)))
        stop("Invalid to specify both control point and tension at once")
    k <- list(x=x, y=y,
              dir.left=dir.left, dir.right=dir.right,
              cp.left.x=cp.left.x, cp.right.x=cp.right.x,
              cp.left.y=cp.left.y, cp.right.y=cp.right.y,
              curl.left=curl.left, curl.right=curl.right,
              tension.left=tension.left, tension.right=tension.right)
    class(k) <- "knot"
    k
}

print.knot <- function(x, ...) {
    cat(as.character(x, ...), sep="\n")
}

## Knot connectors
## Explicit control points
cp <- function(x, y) {
    z <- c(x, y)
    class(z) <- c("controlPoint", "connector")
    z
}

## Directions
## Possibilities are:
##   numeric => angle
##   "dir"   => x/y vector => angle
dir <- function(x, y) {
    d <- 180*atan2(y, x)/pi
    class(d) <- c("direction", "connector")
    d
}

tension <- function(x) {
    x <- as.numeric(x)
    class(x) <- c("tension", "connector")
    x
}

## Matrix of knots
knots <- function(x, y,
                  dir.left=NA, dir.right=dir.left,
                  cp.left.x=NA, cp.right.x=NA,
                  cp.left.y=NA, cp.right.y=NA,
                  curl.left=NA, curl.right=NA,
                  tension.left=NA, tension.right=NA)
{
}

## Complete paths ...
path <- function(x, ..., cycle=FALSE) {
    UseMethod("path")
}

## ... made from individual knots
path.knot <- function(x, ..., cycle=FALSE) {
    knots <- list(x, ...)
    if (!all(sapply(knots, inherits, "knot")))
        stop("Path must contain only knots")
    class(knots) <- "path"
    knots
}

## ... made from a matrix (one row per knot)
path.matrix <- function(x, ..., cycle=FALSE) {
}

print.path <- function(x, ...) {
    cat(as.character(x, ...), sep="\n")
}

## Combining knots, paths, and connectors

## TODO:
## Disallow invalid combinations like knot(0,0) + cp(1,0) + tension(4) 
combine <- function(x, y) {
    UseMethod("combine")
}

addToIncompleteKnot <- function(x, knot) {
    UseMethod("addToIncompleteKnot")
}

addToIncompleteKnot.controlPoint <- function(x, knot) {
    if (!is.null(knot$cp2))
        stop("Two control points have already been specified")
    knot$cp2 <- x
    class(knot) <- c("incompleteKnot", "knot")
    knot
}

addToIncompleteKnot.tension <- function(x, knot) {
    if (!is.null(knot$t2))
        stop("Two tensions have already been specified")
    knot$t2 <- x
    class(knot) <- c("incompleteKnot", "knot")
    knot
}

addToIncompleteKnot.direction <- function(x, knot) {
    if (!is.null(knot$d2))
        stop("Two directions have already been specified")
    knot$d2 <- x
    class(knot) <- c("incompleteKnot", "knot")
    knot
}

addToIncompleteKnot.knot <- function(x, knot) {
    ## Resolve incomplete knot
    ## Control points
    if (!is.null(knot$cp1)) {
        if (is.null(knot$cp2)) {
            knot$cp2 <- knot$cp1
        }
        knot$cp.right.x <- knot$cp1[1]
        knot$cp.right.y <- knot$cp1[2]
        x$cp.left.x <- knot$cp2[1]
        x$cp.left.y <- knot$cp2[2]
    }
    ## Tension
    if (!is.null(knot$t1)) {
        if (is.null(knot$t2)) {
            knot$t2 <- knot$t1
        }
        knot$tension.right <- knot$t1
        x$tension.left <- knot$t2
    }
    ## Direction
    if (!is.null(knot$dir1)) {
        if (is.null(knot$dir2)) {
            knot$dir2 <- knot$dir1
        }
        knot$dir.right <- knot$dir1
        x$dir.left <- knot$dir2
    }
    path(knot, x)
}

addToIncompleteKnot.path <- function(x, knot) {
    ## Resolve incomplete knot
    ## Control points
    if (!is.null(knot$cp1)) {
        if (is.null(knot$cp2)) {
            knot$cp2 <- knot$cp1
        }
        knot$cp.right.x <- knot$cp1[1]
        knot$cp.right.y <- knot$cp1[2]
        x[[1]]$cp.left.x <- knot$cp2[1]
        x[[1]]$cp.left.y <- knot$cp2[2]
    }
    ## Tension
    if (!is.null(knot$t1)) {
        if (is.null(knot$t2)) {
            knot$t2 <- knot$t1
        }
        knot$tension.right <- knot$t1
        x[[1]]$tension.left <- knot$t2
    }
    ## Direction
    if (!is.null(knot$dir1)) {
        if (is.null(knot$dir2)) {
            knot$dir2 <- knot$dir1
        }
        knot$dir.right <- knot$dir1
        x[[1]]$dir.left <- knot$dir2
    }
    path <- c(list(knot), x)
    class(path) <- "path"
    path
}

combine.incompleteKnot <- function(x, y) {
    addToIncompleteKnot(y, x)
}

addToKnot <- function(x, knot) {
    UseMethod("addToKnot")
}

addToKnot.controlPoint <- function(x, knot) {
    knot$cp1 <- x
    class(knot) <- c("incompleteKnot", "knot")
    knot
}

addToKnot.tension <- function(x, knot) {
    knot$t1 <- x
    class(knot) <- c("incompleteKnot", "knot")
    knot
}

addToKnot.direction <- function(x, knot) {
    knot$d1 <- x
    class(knot) <- c("incompleteKnot", "knot")
    knot
}

addToKnot.knot <- function(x, knot) {
    path(knot, x)
}

addToKnot.path <- function(x, knot) {
    path <- c(list(knot), x)
    class(path) <- "path"
    path
}

combine.knot <- function(x, y) {
    addToKnot(y, x)
}

addToIncompletePath <- function(x, path) {
    UseMethod("addToIncompletePath")
}

addToIncompletePath.controlPoint <- function(x, path) {
    if (!is.null(path$cp2))
        stop("Two control points have already been specified")
    path$cp2 <- x
    class(path) <- c("incompletePath", "path")
    path
}

addToIncompletePath.tension <- function(x, path) {
    if (!is.null(path$t2))
        stop("Two tensions have already been specified")
    path$t2 <- x
    class(path) <- c("incompletePath", "path")
    path
}

addToIncompletePath.direction <- function(x, path) {
    if (!is.null(path$d2))
        stop("Two directions have already been specified")
    path$d2 <- x
    class(path) <- c("incompletePath", "path")
    path
}

addToIncompletePath.knot <- function(x, path) {
    ## Resolve incomplete path
    n <- length(path)
    ## Control points
    if (!is.null(path$cp1)) {
        if (is.null(path$cp2)) {
            path$cp2 <- path$cp1
        }
        path[[n]]$cp.right.x <- path$cp1[1]
        path[[n]]$cp.right.y <- path$cp1[2]
        x$cp.left.x <- path$cp2[1]
        x$cp.left.y <- path$cp2[2]
    }
    ## Tension
    if (!is.null(path$t1)) {
        if (is.null(path$t2)) {
            path$t2 <- path$t1
        }
        path[[n]]$tension.right <- path$t1
        x$tension.left <- path$t2
    }
    ## Direction
    if (!is.null(path$dir1)) {
        if (is.null(path$dir2)) {
            path$dir2 <- path$dir1
        }
        path[[n]]$dir.right <- path$dir1
        x$dir.left <- path$dir2
    }
    path <- c(path, list(x))
    class(path) <- "path"
    path
}

addToIncompletePath.path <- function(x, path) {
    ## Resolve incomplete path
    n <- length(path)
    ## Control points
    if (!is.null(path$cp1)) {
        if (is.null(path$cp2)) {
            path$cp2 <- path$cp1
        }
        path[[n]]$cp.right.x <- path$cp1[1]
        path[[n]]$cp.right.y <- path$cp1[2]
        x[[1]]$cp.left.x <- path$cp2[1]
        x[[1]]$cp.left.y <- path$cp2[2]
    }
    ## Tension
    if (!is.null(path$t1)) {
        if (is.null(path$t2)) {
            path$t2 <- path$t1
        }
        path[[n]]$tension.right <- path$t1
        x[[1]]$tension.left <- path$t2
    }
    ## Direction
    if (!is.null(path$dir1)) {
        if (is.null(path$dir2)) {
            path$dir2 <- path$dir1
        }
        path[[n]]$dir.right <- path$dir1
        x[[1]]$dir.left <- path$dir2
    }
    path <- c(path, x)
    class(path) <- "path"
    path
}

combine.incompletePath <- function(x, y) {
    addToIncompletePath(y, x)
}

addToPath <- function(x, path) {
    UseMethod("addToPath")
}

addToPath.controlPoint <- function(x, path) {
    path$cp1 <- x
    class(path) <- c("incompletePath", "path")
    path
}

addToPath.knot <- function(x, path) {
    path <- c(path, list(x))
    class(path) <- "path"
    path
}

addToPath.path <- function(x, path) {
    path <- c(path, x)
    class(path) <- "path"
    path    
}

combine.path <- function(x, y) {
    addToPath(y, x)
}

## Operations for building up paths
Ops.knot <- function(e1, e2) {
    if (.Generic != "+") {
        stop("Invalid operation on knots")
    }
    if (nargs() < 2) {
        stop("Unary operations not valid on knots")
    }
    if ((inherits(e1, "knot") && inherits(e2, "knot")) ||
        inherits(e1, "path") ||
        inherits(e1, "connecter") ||
        inherits(e2, "path") ||
        inherits(e2, "connector")) {
        combine(e1, e2)
    } else {
        stop("It is only valid to combine a knot with a path or a connector")
    }
}

Ops.path <- function(e1, e2) {
    if (.Generic != "+") {
        stop("Invalid operation on paths")
    }
    if (nargs() < 2) {
        stop("Unary operations not valid on paths")
    }
    if ((inherits(e1, "knot") && inherits(e2, "knot")) ||
        inherits(e1, "knot") ||
        inherits(e1, "connecter") ||
        inherits(e2, "knot") ||
        inherits(e2, "connector")) {
        combine(e1, e2)
    } else {
        stop("It is only valid to combine a knot with a path or a connector")
    }
}

