
library(metapost)
library(grid)
p <- knot(0, 0) + dir(45) + dir(0:9*-10) + knot(unit(6, "cm"), 0)
metapost(p, "fig.mp")
mpost("fig.mp", tracing=TRUE)

curves <- mptrace("fig.log")

library(grImport)
PostScriptTrace("fig.1")
fig <- readPicture("fig.1.xml")

pushViewport(viewport(y=.5, height=.5, just="bottom"))
grid.draw(curves)
upViewport()
pushViewport(viewport(y=0, height=.5, just="bottom"))
grid.picture(fig)
upViewport()
