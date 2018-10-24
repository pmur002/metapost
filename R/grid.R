
pathGrobs <- function(controls, pathIndex) {
    BezierGrob(controls[,1], controls[,2],
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
    metapost(x$path, mpfile)
    mpost(mpfile, tracing=TRUE)
    pathControls <- mptrace(logfile)
    paths <- mapply(pathGrobs, pathControls, 1:length(pathControls),
                    SIMPLIFY=FALSE)
    setChildren(x, do.call(gList, paths))
}

metapostGrob <- function(x, ...) {
    UseMethod("metapostGrob")
}

## A solved path (scale already fixed)
metapostGrob.mpcontrols <- function(x,
                                    gp=gpar(),
                                    name=NULL) {
    paths <- mapply(pathGrobs, x, 1:length(x), SIMPLIFY=FALSE)
    gTree(children=do.call(gList, paths),
          gp=gp, name=name, cl="mpsolvedgrob")    
}

## An unsolved path
metapostGrob.mppath <- function(x,
                                gp=gpar(),
                                name=NULL) {
    gTree(path=x, gp=gp, name=name, cl="metapostgrob")
}

grid.metapost <- function(...) {
    grid.draw(metapostGrob(...))
}
