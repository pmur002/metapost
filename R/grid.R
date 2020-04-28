
pathGrob <- function(controls, pathIndex=1, units="bigpts") {
    BezierGrob(controls$x, controls$y,
               open=is.null(attr(controls, "cycle")),
               default.units=units,
               name=paste0("path-", pathIndex))
}

################################################################################
## grid.metapost()

makeContent.metapostgrob <- function(x) {
    wd <- getwd()
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
metapostGrob.mpfigure <- function(x,
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
                                digits=2,
                                ...) {
    gTree(path=x, gp=gp, name=name, digits=digits, 
          cl="metapostgrob")
}

grid.metapost <- function(...) {
    grid.draw(metapostGrob(...))
}

################################################################################
## grid.figure()

figureGrob <- function(figure, ...) {
    UseMethod("figureGrob")
}

figureVP <- function(pathControls, x, y, width, height, just,
                     expansion = 0.05,
                     xscale = NULL, yscale = NULL,
                     distort = FALSE, clip = "on") {
    
    if (is.null(xscale)) {
        xscale <- range(unlist(lapply(pathControls, function(p) p$x)))
    }
    if (is.null(yscale)) {
        yscale <- range(unlist(lapply(pathControls, function(p) p$y)))
    }
    xscale <- xscale + expansion * c(-1, 1) * diff(xscale)
    yscale <- yscale + expansion * c(-1, 1) * diff(yscale)

    # If distort=TRUE, having the two layers of viewports is
    # massively redundant, BUT I'm keeping it so that either
    # way there is the same viewport structure, which I think
    # is beneficial if anyone ever wants to make use of
    # these viewports (otherwise they would need to figure
    # out whether a picture grob has one or two viewports).
    vpStack(viewport(x, y, width, height, just=just,
                     name = "figure.shape", 
                     layout = grid.layout(1, 1,
                                          widths = abs(diff(xscale)),
                                          heights = abs(diff(yscale)),
                                          respect = ! distort)),
            viewport(name = "figure.scale",
                     layout.pos.col = 1,
                     xscale = xscale,
                     yscale = yscale,
                     clip = clip))
}

figureGTree <- function(figure,
                        x, y, width, height, just,
                        expansion, xscale, yscale, distort, clip) {
    paths <- mapply(pathGrob, figure, 1:length(figure), "native",
                    SIMPLIFY=FALSE)
    vp <- figureVP(figure, x, y, width, height, just,
                   expansion = expansion,
                   xscale = xscale, yscale = yscale,
                   distort = distort, clip = clip)
    gTree(children = do.call(gList, paths), vp = vp)
}

makeContent.figuregrob <- function(x) {
    wd <- getwd()
    mpfile <- tempfile(fileext=".mp")
    logfile <- gsub(".mp$", ".log", mpfile)
    metapost(x$path, mpfile, x$digits)
    mpost(mpfile, tracing=TRUE)
    pathControls <- mptrace(logfile)
    gt <- figureGTree(pathControls,
                      x$x, x$y, x$width, x$height, x$just,
                      x$expansion, x$xscale, x$yscale, x$distort, x$clip)
    setChildren(x, gList(gt))
}

## Several solved paths (scale already fixed)
figureGrob.mpfigure <- function(figure,
                                x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                                width = unit(1, "npc"), height = unit(1, "npc"),
                                just = "centre",
                                default.units = "npc",
                                expansion = 0, xscale = NULL, yscale = NULL,
                                distort = FALSE, clip = FALSE,
                                gp=gpar(),
                                name=NULL, ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(width))
        width <- unit(width, default.units)
    if (!is.unit(height))
        height <- unit(height, default.units)
    paths <- mapply(pathGrob, figure, 1:length(figure), "native",
                    SIMPLIFY=FALSE)
    gt <- figureGTree(figure, x, y, width, height, just,
                      expansion, xscale, yscale, distort, clip)
    gTree(children=gList(gt),
          gp=gp, name=name, cl="solvedfiguregrob")    
}

## An unsolved path
figureGrob.mppath <- function(figure,
                              x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                              width = unit(1, "npc"), height = unit(1, "npc"),
                              just = "centre", 
                              default.units = "npc",
                              expansion = 0, xscale = NULL, yscale = NULL,
                              distort = FALSE, clip = FALSE,
                              gp = gpar(),
                              name = NULL,
                              digits = 2,
                              ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(width))
        width <- unit(width, default.units)
    if (!is.unit(height))
        height <- unit(height, default.units)
    gTree(figure=figure, 
          x = x, y = y,
          width = width, height = height,
          just = just,
          default.units = default.units,
          expansion = expansion,
          xscale = xscale, yscale = yscale,
          distort = distort,
          gp=gp, name=name, digits=digits,
          cl="figuregrob")
}


grid.figure <- function(...) {
    grid.draw(figureGrob(...))
}
