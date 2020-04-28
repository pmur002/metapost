
## Convert MetaPost path to R objects

## Parse output from 'mpost -s tracingchoices=1'

## Some path lines run over onto second line
## Attempt to bring them back to single line
tidyPath <- function(path) {
    overrun <- !grepl("^([(]| [.][.])", path)
    if (any(overrun)) {
        overlines <- which(overrun)
        path[overlines - 1] <- paste0(path[overlines - 1], path[overlines])
        path <- path[-overlines]
    }
    path
}

parsePath <- function(path) {
    locStr <- gsub("^( [.][.])?[(]|,|[)][.][.]controls [(]|[)] and [(]|[)]$",
                   " ", tidyPath(path))
    cycle <- grepl("cycle", locStr[length(locStr)])
    if (cycle) {
        locStr <- locStr[-length(locStr)]
    }
    locs <- matrix(scan(textConnection(locStr)), byrow=TRUE, ncol=2)
    controls <- list(x=locs[,1], y=locs[,2])
    if (cycle) {
        attr(controls, "cycle") <- TRUE
    }
    class(controls) <- "mpcontrols"
    controls
}

mptrace <- function(logfile="fig.log", fig=1) {
    log <- readLines(logfile)
    if (!grepl("This is MetaPost", log[1]))
        stop("File does not appear to be a MetaPost log file")
    ## Figures must be labelled numerically
    ## (https://www.ntg.nl/doc/hobby/mpman.pdf pg. 7)
    ## BUT they do not have to be sequential OR in order
    if (length(fig) != 1) {
        stop("Must specify exactly one figure")
    }
    figures <- grep("[[][0-9]+[]]", log)
    nfig <- length(figures)
    figLabels <- gsub("^.*[[]|[]].*$", "", log[figures])
    if (is.numeric(fig)) {
        if (fig < 1 || fig > nfig) {
            stop("Invalid figure selection")
        } 
        figIndex <- fig
    } else {
        fig <- as.character(fig)
        if (!(fig %in% figLabels)) {
            stop("Invalid figure selection")
        }
        figIndex <- which(figLabels == fig)
    }
    ## Just look at the log content for the relevant figure
    logStart <- if (figIndex == 1) 1 else figures[figIndex - 1] + 1
    logEnd <- if (figIndex < nfig) figures[figIndex] - 1 else length(log)
    log <- log[logStart:logEnd]
    ## Now read paths 
    beforeStart <- grep("^Path .+ before choices", log)
    nBefore <- length(beforeStart)
    afterStart <- grep("^Path .+ after choices", log)
    nAfter <- length(afterStart)
    if (!nAfter)
        stop("Log file does not contain any tracing output")
    blankline <- "^$"
    ends <- grep(blankline, log)[1:(nBefore + nAfter)]
    starts <- c(beforeStart, afterStart)
    startOrder <- order(starts)
    types <- c(rep("ignore", nBefore), rep("paths", nAfter))
    info <- data.frame(start=starts[startOrder],
                       end=ends,
                       type=types[startOrder])
    paths <- mapply(function(s, e) {
                        log[(s + 1):(e - 1)]
                    },
                    info[info$type == "paths", 1], 
                    info[info$type == "paths", 2],
                    SIMPLIFY=FALSE)
    pathControls <- lapply(paths, parsePath)
    class(pathControls) <- "mpfigure"
    pathControls
}

mpbbox <- function(psfile) {
    ## Grab bounding box from PostScript output
    pscode <- readLines(psfile)
    bboxline <- grep("%%HiResBoundingBox", pscode)
    bboxVals <- strsplit(gsub("^.+? ", "", pscode[bboxline]), " ")[[1]]
    as.numeric(bboxVals)
}

mpvp <- function(psfile, ...) {
    bbox <- mpbbox(psfile)
    dx <- diff(bbox[c(1, 3)])
    dy <- diff(bbox[c(2, 4)])
    viewport(width=unit(dx, "bigpts"), height=unit(dy, "bigpts"), ...)
}    
                    
