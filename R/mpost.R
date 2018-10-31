
## Call the mpost processor from R

mpost <- function(file="fig.mp",
                  cmd=NULL, template=NULL, format=NULL, tracing=TRUE) {
    if (is.null(cmd)) {
        cmd <- Sys.which("mpost")
        if (!nchar(cmd)) {
            stop("Unable to find 'mpost' program")
        }
    }
    if (is.null(template)) {
        templateOpt <- ""
    } else {
        templateOpt <- paste0("-s 'outputtemplate=\"", template, "\"'")
    }
    if (is.null(format)) {
        formatOpt <- ""
    } else {
        formatOpt <- paste0("-s 'outputformat=\"", format, "\"'")
    }
    if (tracing) {
        tracingOpt <- "-s tracingchoices=1"
    } else {
        tracingOpt <- ""
    }
    system2(cmd, args=paste(templateOpt, formatOpt, tracingOpt, file))
}

