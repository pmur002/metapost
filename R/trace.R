
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

mptrace <- function(logfile="fig.log") {
    log <- readLines(logfile)
    if (!grepl("This is MetaPost", log[1]))
        stop("File does not appear to be a MetaPost log file")
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
    class(pathControls) <- "mpcontrolList"
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
    viewport(width=unit(dx, "pt"), height=unit(dy, "pt"), ...)
}    
                    
