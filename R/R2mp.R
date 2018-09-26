
## Convert R objects to MetaPost path

## R path to MetaPost code
as.character.knot <- function(x, ..., first=FALSE, last=FALSE) {
    n <- max(length(x$x), length(x$y))
    x$dir.left <- rep(x$dir.left, length=n)
    x$curl.left <- rep(x$curl.left, length=n)
    dir.left <- rep("", n)
    if (!first) {
        ## knot() constructor has enforced that dir and curl not both spec'ed
        isnadir <- is.na(x$dir.left)
        isnacurl <- is.na(x$curl.left)
        if (!all(isnadir)) {
            dir.left[!isnadir] <- paste0("{dir ",
                                         x$dir.left[!isnadir],
                                         "}")
        }
        if (!all(isnacurl)) {
            dir.left[!isnacurl] <- paste0("{curl ",
                                          x$curl.left[!isnacurl],
                                          "}")
        }
    }
    x$dir.right <- rep(x$dir.right, length=n)
    x$curl.right <- rep(x$curl.right, length=n)
    dir.right <- ""
    if (!last) {
        ## knot() constructor has enforced that dir and curl not both spec'ed
        isnadir <- is.na(x$dir.right)
        isnacurl <- is.na(x$curl.right)
        if (!all(isnadir)) {
            dir.right[!isnadir] <- paste0("{dir ",
                                          x$dir.right[!isnadir],
                                          "}")
        }
        if (!all(isnacurl)) {
            dir.right[!isnacurl] <- paste0("{curl ",
                                           x$curl.right[!isnacurl],
                                           "}")
        }
    }
    paste0(dir.left, "(", x$x, ",", x$y, ")", dir.right)
}

## Construct connector string between two knots
connectorString <- function(x, y) {
    ## First knot
    isnatension.right <- is.na(x$tension.right)
    isnacp.right <- is.na(x$cp.right.x) | is.na(x$cp.right.y)
    tension.right <- ""
    if (!all(isnatension.right)) {
        tension <- x$tension.right[!isnatension.right]
        tension.right[!isnatension.right] <-
            paste0("tension ",
                   ifelse(tension < 0, "atleast ", ""),
                   abs(tension))
    }
    if (!all(isnacp.right)) {
        tension.right[!isnacp.right] <-
            paste0("controls (", x$cp.right.x[!isnacp.right],
                   ",", x$cp.right.y[!isnacp.right], ")")
    }
    ## Second knot
    ## knot() constructor has enforced that tension and cp not both spec'ed
    isnatension.left <- is.na(y$tension.left)
    isnacp.left <- is.na(y$cp.left.x) | is.na(y$cp.left.y)
    tension.left <- ifelse(nchar(tension.right) &
                           (!isnatension.left | !isnacp.left),
                           " and ",
                           ifelse(!nchar(tension.right) &
                                  isnatension.left & !isnacp.left,
                                  "controls ",
                                  ifelse(!nchar(tension.right) &
                                         isnacp.left & !isnatension.left,
                                         "tension ",
                                         "")))
    if (!all(isnatension.left)) {
        tension <- y$tension.left[!isnatension.left]
        tension.left[!isnatension.left] <-
            paste0(tension.left[!isnatension.left],
                   ifelse(tension < 0, "atleast ", ""),
                   abs(tension))
    }
    if (!all(isnacp.left)) {
        tension.left[!isnacp.left] <-
            paste0(tension.left[!isnacp.left],
                   "(", y$cp.left.x[!isnacp.left],
                   ",", y$cp.left.y[!isnacp.left], ")")
    }
    ifelse(nchar(tension.right) | nchar(tension.left),
           paste0("..", tension.right, tension.left, ".."),
           "..")
}

connectKnot <- function(x, y, last=FALSE) {
    paste0(connectorString(x, y), as.character(y, last=last))
}

as.character.path <- function(x, ...) {
    n <- length(x)
    if (n == 1) {
        as.character(x, first=TRUE, last=TRUE)
    } else if (n == 2) {
        paste0(as.character(x[[1]], first=TRUE),
               connectKnot(x[[1]], x[[2]], last=TRUE))
    } else {
        paste0(as.character(x[[1]], first=TRUE),
               Reduce(x[-c(1, n)], connectKnot),
               connectKnot(x[[n - 1]], x[[n]], last=TRUE))
    }
}

