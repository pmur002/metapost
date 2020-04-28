\name{grid.figure}
\alias{grid.figure}
\alias{figureGrob}
\alias{figureGrob.mppath}
\alias{figureGrob.mpfigure}
\title{
  Draw a MetaPost figure.
}
\description{
  Draw a MetaPost figure in \pkg{grid} graphics.
}
\usage{
\method{figureGrob}{mppath}(figure,
                            x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                            width = unit(1, "npc"), height = unit(1, "npc"),
                            just = "centre", 
                            default.units = "npc",
                            expansion = 0, xscale = NULL, yscale = NULL,
                            distort = FALSE, clip = FALSE,
                            gp = gpar(), name = NULL, digits=2, ...)
\method{figureGrob}{mpfigure}(figure,
                              x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                              width = unit(1, "npc"), height = unit(1, "npc"),
                              just = "centre",
                              default.units = "npc",
                              expansion = 0, xscale = NULL, yscale = NULL,
                              distort = FALSE, clip = FALSE,
                              gp = gpar(), name = NULL, ...)
grid.figure(...)
}
\arguments{
  \item{figure}{
    A MetaPost path, either unsolved (a description generated using
    \code{\link{knot}} etc), or solved (as produced by
    \code{\link{mptrace}}).
  }
  \item{x, y, width, height, just, default.units}{
    The location, size, and justification of the figure.
  }
  \item{expansion}{A factor to extend the scales of the figure
    (to create an empty margin around the figure).
  }
  \item{xscale, yscale}{Override the scales used to draw the figure;
    by default, these are chosen to encompass the figure (control points).
  }
  \item{distort}{Should the aspect ratio of the original figure be
    retained.}
  \item{clip}{Should drawing be clipped to the bounds of the figure
    (as defined by the scales).}
  \item{gp}{
    Graphical parameters (from a call to \code{gpar}).
  }
  \item{name}{
    A name for the grob that is created.
  }
  \item{digits}{
    The number of decimal places to use when writing floating
    point values in Figure code.
  }
  \item{\dots}{
    Arguments passed to \code{figureGrob}.
  }
}
\value{
  \code{figureGrob} creates a \code{"figuregrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{knot}},
  \code{\link{mptrace}}.
}
\examples{
\donttest{
oldunits <- options(figure.units="in")
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1) 
grid.figure(p)
options(oldunits)
}
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
