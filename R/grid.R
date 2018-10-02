
makeContent.metapostgrob <- function(x) {
    wd <- getwd()
    setwd(tempdir())
    on.exit(setwd(wd))
    mpfile <- tempfile(fileext=".mp")
    logfile <- gsub(".mp$", ".log", mpfile)
    metapost(x$path, mpfile)
    mpost(mpfile, tracing=TRUE)
    curves <- mptrace(logfile)
    setChildren(x, curves$children)
}

metapostGrob <- function(path,
                         gp=gpar(),
                         name=NULL) {
    gTree(path=path, gp=gp, name=name, cl="metapostgrob")
}

grid.metapost <- function(...) {
    grid.draw(metapostGrob(...))
}
