\name{mpost}
\alias{mpost}
\title{
  Run \command{mpost} on a MetaPost File
}
\description{
  Run \command{mpost} on a MetaPost file, possibly with additional options.
}
\usage{
mpost(file = "fig.mp",
      cmd = NULL, template = NULL, format = NULL, tracing = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    The name of a file containing MetaPost code.
  }
  \item{cmd}{
    The command to use to run \command{mpost}.  By default
    \code{Sys.which} is used to find a sensible value.
  }
  \item{template}{
    The naming template for the output files that are produced
    (see \command{mpost}'s \code{outputtemplate} option).
  }
  \item{format}{
    The output format
    (see \command{mpost}'s \code{outputformat} option).
  }
  \item{tracing}{
    A logical value indicating whether to generate a log
    file containing solved paths
    (see \command{mpost}'s \code{tracingchoices} option).    
  }
}
\details{
  By default, \code{tracing} is \code{TRUE}, which produces a log
  file that can be read into \R{} using \code{\link{mptrace}}.

  Both output and log files will be produced in the same directory
  as the \code{file}.
}
\value{
  Used for its side effect of producing output files and log files.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{metapost}},
  \code{\link{mptrace}} 
}
\examples{
\donttest{
oldunits <- options(metapost.units="in")
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1)
mpfile <- file.path(tempdir(), "fig.mp")
metapost(p, mpfile)
mpost(mpfile)
options(oldunits)
}
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
