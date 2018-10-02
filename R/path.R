
## Specify a MetaPost path via R functions and data structures

## There is a superclass for knots, paths, and connectors
## (so can write a single Ops method)

## Individual knots
knot <- function(x, y,
                 units=getOption("metapost.units"),
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
    if (!is.unit(x))
        x <- unit(x, units)
    if (!is.unit(y))
        y <- unit(y, units)
    if (!is.unit(cp.left.x))
        cp.left.x <- unit(cp.left.x, units)
    if (!is.unit(cp.left.y))
        cp.left.y <- unit(cp.left.y, units)
    if (!is.unit(cp.right.x))
        cp.right.x <- unit(cp.right.x, units)
    if (!is.unit(cp.right.y))
        cp.right.y <- unit(cp.right.y, units)
    k <- list(x=x, y=y,
              dir.left=dir.left, dir.right=dir.right,
              cp.left.x=cp.left.x, cp.right.x=cp.right.x,
              cp.left.y=cp.left.y, cp.right.y=cp.right.y,
              curl.left=curl.left, curl.right=curl.right,
              tension.left=tension.left, tension.right=tension.right)
    class(k) <- c("knot", "mpobj")
    k
}

length.knot <- function(x) {
    max(sapply(x, length))
}

print.knot <- function(x, ...) {
    cat(as.character(x, ...), sep="\n")
}

## Special cycle knot
cycle <- function() {
    x <- knot(NA, NA)
    class(x) <- c("cycle", "knot", "mpobj")
    x
}

## Knot connectors
## Explicit control points
cp <- function(x, y, units=getOption("metapost.units")) {
    z <- list(x=x, y=y, units=units)
    class(z) <- c("controlPoint", "connector", "mpobj")
    z
}

cpx <- function(cp) {
    if (!is.unit(cp$x))
        unit(cp$x, cp$units)
    else 
        cp$x
}

cpy <- function(cp) {
    if (!is.unit(cp$y))
        unit(cp$y, cp$units)
    else 
        cp$y
}

## Directions
## Possibilities are:
##   one value => angle
##   two values => x/y vector => angle
dir <- function(x, y=NULL) {
    if (is.null(y)) {
        d <- as.numeric(x)
    } else {
        d <- 180*atan2(y, x)/pi
    }
    class(d) <- c("direction", "connector", "mpobj")
    d
}

## TODO
## limit to valid tension values
tension <- function(x) {
    x <- as.numeric(x)
    class(x) <- c("tension", "connector", "mpobj")
    x
}

## TODO
## limit to valid curl values
curl <- function(x) {
    x <- as.numeric(x)
    class(x) <- c("curl", "connector", "mpobj")
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
    p <- list(knots=knots)
    class(p) <- c("path", "mpobj")
    p
}

## ... made from a matrix (one row per knot)
path.matrix <- function(x, ..., cycle=FALSE) {
}

length.path <- function(x) {
    length(x$knots)
}

print.path <- function(x, ...) {
    cat(as.character(x, ...), sep="\n")
}

## Combining knots, paths, and connectors

## TODO:
## Disallow invalid combinations like knot(0,0) + cp(1,0) + tension(4)
## Disallow adding to cycle
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
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToIncompleteKnot.tension <- function(x, knot) {
    if (!is.null(knot$t2))
        stop("Two tensions have already been specified")
    knot$t2 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToIncompleteKnot.curl <- function(x, knot) {
    if (!is.null(knot$c2))
        stop("Two curls have already been specified")
    knot$c2 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToIncompleteKnot.direction <- function(x, knot) {
    if (!is.null(knot$d2))
        stop("Two directions have already been specified")
    knot$d2 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToIncompleteKnot.knot <- function(x, knot) {
    ## Resolve incomplete knot
    ## Control points
    if (!is.null(knot$cp1)) {
        if (is.null(knot$cp2)) {
            knot$cp2 <- knot$cp1
        }
        knot$cp.right.x <- cpx(knot$cp1)
        knot$cp.right.y <- cpy(knot$cp1)
        x$cp.left.x <- cpx(knot$cp2)
        x$cp.left.y <- cpy(knot$cp2)
        knot$cp1 <- NULL
        knot$cp2 <- NULL
    }
    ## Tension
    if (!is.null(knot$t1)) {
        if (is.null(knot$t2)) {
            knot$t2 <- knot$t1
        }
        knot$tension.right <- knot$t1
        x$tension.left <- knot$t2
        knot$t1 <- NULL
        knot$t2 <- NULL
    }
    ## Curl
    if (!is.null(knot$c1)) {
        if (is.null(knot$c2)) {
            knot$c2 <- knot$c1
        }
        knot$curl.right <- knot$c1
        x$curl.left <- knot$c2
        knot$c1 <- NULL
        knot$c2 <- NULL
    }
    ## Direction
    if (!is.null(knot$d1)) {
        if (is.null(knot$d2)) {
            knot$d2 <- knot$d1
        }
        knot$dir.right <- knot$d1
        x$dir.left <- knot$d2
        knot$d1 <- NULL
        knot$d2 <- NULL
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
        knot$cp.right.x <- cpx(knot$cp1)
        knot$cp.right.y <- cpy(knot$cp1)
        x$knots[[1]]$cp.left.x <- cpx(knot$cp2)
        x$knots[[1]]$cp.left.y <- cpy(knot$cp2)
        knot$cp1 <- NULL
        knot$cp2 <- NULL
    }
    ## Tension
    if (!is.null(knot$t1)) {
        if (is.null(knot$t2)) {
            knot$t2 <- knot$t1
        }
        knot$tension.right <- knot$t1
        x$knots[[1]]$tension.left <- knot$t2
        knot$t1 <- NULL
        knot$t2 <- NULL
    }
    ## Curl
    if (!is.null(knot$c1)) {
        if (is.null(knot$c2)) {
            knot$c2 <- knot$c1
        }
        knot$curl.right <- knot$c1
        x$knots[[1]]$curl.left <- knot$c2
        knot$c1 <- NULL
        knot$c2 <- NULL
    }
    ## Direction
    if (!is.null(knot$d1)) {
        if (is.null(knot$d2)) {
            knot$d2 <- knot$d1
        }
        knot$dir.right <- knot$d1
        x$knots[[1]]$dir.left <- knot$d2
        knot$d1 <- NULL
        knot$d2 <- NULL
    }
    do.call(path, c(list(knot), x))
}

combine.incompleteKnot <- function(x, y) {
    addToIncompleteKnot(y, x)
}

addToKnot <- function(x, knot) {
    UseMethod("addToKnot")
}

addToKnot.controlPoint <- function(x, knot) {
    knot$cp1 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToKnot.tension <- function(x, knot) {
    knot$t1 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToKnot.curl <- function(x, knot) {
    knot$c1 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToKnot.direction <- function(x, knot) {
    knot$d1 <- x
    class(knot) <- c("incompleteKnot", "knot", "mpobj")
    knot
}

addToKnot.knot <- function(x, knot) {
    path(knot, x)
}

addToKnot.path <- function(x, knot) {
    do.call(path, c(list(knot), x))
}

combine.knot <- function(x, y) {
    addToKnot(y, x)
}

addToIncompletePath <- function(x, p) {
    UseMethod("addToIncompletePath")
}

addToIncompletePath.controlPoint <- function(x, p) {
    if (!is.null(p$cp2))
        stop("Two control points have already been specified")
    p$cp2 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToIncompletePath.tension <- function(x, p) {
    if (!is.null(p$t2))
        stop("Two tensions have already been specified")
    p$t2 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToIncompletePath.curl <- function(x, p) {
    if (!is.null(p$c2))
        stop("Two curls have already been specified")
    p$c2 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToIncompletePath.direction <- function(x, p) {
    if (!is.null(p$d2))
        stop("Two directions have already been specified")
    p$d2 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToIncompletePath.knot <- function(x, p) {
    ## Resolve incomplete path
    n <- length(p)
    ## Control points
    if (!is.null(p$cp1)) {
        if (is.null(p$cp2)) {
            p$cp2 <- p$cp1
        }
        p$knots[[n]]$cp.right.x <- cpx(p$cp1)
        p$knots[[n]]$cp.right.y <- cpy(p$cp1)
        x$cp.left.x <- cpx(p$cp2)
        x$cp.left.y <- cpy(p$cp2)
    }
    ## Tension
    if (!is.null(p$t1)) {
        if (is.null(p$t2)) {
            p$t2 <- p$t1
        }
        p$knots[[n]]$tension.right <- p$t1
        x$tension.left <- p$t2
    }
    ## Curl
    if (!is.null(p$c1)) {
        if (is.null(p$c2)) {
            p$c2 <- p$c1
        }
        p$knots[[n]]$curl.right <- p$c1
        x$curl.left <- p$c2
    }
    ## Direction
    if (!is.null(p$d1)) {
        if (is.null(p$d2)) {
            p$d2 <- p$d1
        }
        p$knots[[n]]$dir.right <- p$d1
        x$dir.left <- p$d2
    }
    do.call(path, c(p$knots, list(x)))
}

addToIncompletePath.path <- function(x, p) {
    ## Resolve incomplete path
    n <- length(p)
    ## Control points
    if (!is.null(p$cp1)) {
        if (is.null(p$cp2)) {
            p$cp2 <- p$cp1
        }
        p$knots[[n]]$cp.right.x <- cpx(p$cp1)
        p$knots[[n]]$cp.right.y <- cpy(p$cp1)
        x$knots[[1]]$cp.left.x <- cpx(p$cp2)
        x$knots[[1]]$cp.left.y <- cpy(p$cp2)
    }
    ## Tension
    if (!is.null(p$t1)) {
        if (is.null(p$t2)) {
            p$t2 <- p$t1
        }
        p$knots[[n]]$tension.right <- p$t1
        x$knots[[1]]$tension.left <- p$t2
    }
    ## Curl
    if (!is.null(p$c1)) {
        if (is.null(p$c2)) {
            p$c2 <- p$c1
        }
        p$knots[[n]]$curl.right <- p$t1
        x$knots[[1]]$curl.left <- p$t2
    }
    ## Direction
    if (!is.null(p$d1)) {
        if (is.null(p$d2)) {
            p$d2 <- p$d1
        }
        p$knots[[n]]$dir.right <- p$d1
        x$knots[[1]]$dir.left <- p$d2
    }
    do.call(path, c(p$knots, x$knots))
}

combine.incompletePath <- function(x, y) {
    addToIncompletePath(y, x)
}

addToPath <- function(x, p) {
    UseMethod("addToPath")
}

addToPath.controlPoint <- function(x, p) {
    p$cp1 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToPath.tension <- function(x, p) {
    p$t1 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToPath.curl <- function(x, p) {
    p$c1 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToPath.direction <- function(x, p) {
    p$d1 <- x
    class(p) <- c("incompletePath", "path", "mpobj")
    p
}

addToPath.knot <- function(x, p) {
    do.call(path, c(p$knots, list(x)))
}

addToPath.path <- function(x, p) {
    do.call(path, c(p$knots, x$knots))
}

combine.path <- function(x, y) {
    addToPath(y, x)
}

## Operations for building up paths

## e1 + e2 is equivalent to e1..e2
## e1 - e2 is equivalent to e1--e2
## e1 %+% e2 is equivalent to e1...e2
## e1 %-% e2 is equivalent to e1---e2

## Page 129 of The MetaFont Book
## http://www.ctex.org/documents/shredder/src/mfbook.pdf
## says ...

## -- is an abbreviation for ‘{curl 1}..{curl 1}’
## ... is an abbreviation for ‘..tension atleast 1..’
## --- is an abbreviation for ‘..tension infinity..’

## TODO
## What about adding a connector to a path ?
## What about adding a path to a path ?
## Need a superclass above all of paths, knots, and connectors ?
Ops.mpobj <- function(e1, e2) {
    if (nargs() < 2) {
        stop("Unary operations not valid on knots")
    }
    if (!(.Generic %in% c("+", "-"))) {
        stop("Invalid operation on knots")
    }
    if (.Generic == "-") {
        if ((inherits(e1, "knot") || inherits(e1, "path")) &&
            (inherits(e2, "knot") || inherits(e2, "path"))) {
            e1 + curl(1) + curl(1) + e2
        } else {
            stop("It is only valid to use '-' between knots and paths")
        }
    } else {
        if (inherits(e1, "knot") || inherits(e1, "path")) {
            combine(e1, e2)
        } else {
            stop("It is only valid to combine a connector with a knot or path")
        }
    }
}

"%+%" <- function(e1, e2) {
    if ((inherits(e1, "knot") || inherits(e1, "path")) &&
        (inherits(e2, "knot") || inherits(e2, "path"))) {
        e1 + tension(-1) + e2
    } else {
        stop("It is only valid to use '%+%' between knots and paths")
    }
}

"%-%" <- function(e1, e2) {
    if ((inherits(e1, "knot") || inherits(e1, "path")) &&
        (inherits(e2, "knot") || inherits(e2, "path"))) {
        e1 + tension(Inf) + e2
    } else {
        stop("It is only valid to use '%+%' between knots and paths")
    }
}
