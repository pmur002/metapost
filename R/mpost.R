
## Call the mpost processor from R

mpost <- function(file) {
    cmd <- getOption("MP.cmd")
    if (is.null(cmd)) {
        cmd <- Sys.which("mpost")
        if (!nchar(cmd)) {
            stop("Unable to find 'mpost' program")
        }
    }
    template <- getOption("MP.outputtemplate")
    if (is.null(template)) {
        templateOpt <- ""
    } else {
        templateOpt <- paste0("-s 'outputtemplate=\"", template, "\"'")
    }
    format <- getOption("MP.outputformat")
    if (is.null(format)) {
        formatOpt <- ""
    } else {
        formatOpt <- paste0("-s 'outputformat=\"", format, "\"'")
    }
    tracing <- getOption("MP.tracingchoices")
    if (is.null(tracing)) {
        tracingOpt <- ""
    } else {
        tracingOpt <- paste0("-s tracingchoices=", tracing)
    }
    system2(cmd, args=paste(templateOpt, formatOpt, tracingOpt, file))
}

