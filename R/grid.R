
pathGrob <- function(controls, pathIndex=1) {
    BezierGrob(controls$x, controls$y,
               open=is.null(attr(controls, "cycle")),
               default.units="pt",
               name=paste0("path-", pathIndex))
}

makeContent.metapostgrob <- function(x) {
    wd <- getwd()
    setwd(tempdir())
    on.exit(setwd(wd))
    mpfile <- tempfile(fileext=".mp")
    logfile <- gsub(".mp$", ".log", mpfile)
    metapost(x$path, mpfile, x$digits)
    mpost(mpfile, tracing=TRUE)
    pathControls <- mptrace(logfile)
    paths <- mapply(pathGrob, pathControls, 1:length(pathControls),
                    SIMPLIFY=FALSE)
    setChildren(x, do.call(gList, paths))
}

metapostGrob <- function(x, ...) {
    UseMethod("metapostGrob")
}

## A solved path (scale already fixed)
metapostGrob.mpcontrols <- function(x,
                                    gp=gpar(),
                                    name=NULL, ...) {
    path <- pathGrob(x)
    gTree(children=do.call(gList, path),
          gp=gp, name=name, cl="mpsolvedgrob")    
}

## Several solved paths (scale already fixed)
metapostGrob.mpcontrolList <- function(x,
                                       gp=gpar(),
                                       name=NULL, ...) {
    paths <- mapply(pathGrob, x, 1:length(x), SIMPLIFY=FALSE)
    gTree(children=do.call(gList, paths),
          gp=gp, name=name, cl="mpsolvedgrob")    
}

## An unsolved path
metapostGrob.mppath <- function(x,
                                gp=gpar(),
                                name=NULL,
                                digits=2, ...) {
    gTree(path=x, gp=gp, name=name, digits=digits, cl="metapostgrob")
}

grid.metapost <- function(...) {
    grid.draw(metapostGrob(...))
}
