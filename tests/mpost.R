
library(metapost)
p <- knot(0, 0) + dir(45) + dir(0:9*-10) + knot(6, 0)
metapost(p, "fig.mp")
mpost("fig.mp")


library(grImport)
PostScriptTrace("fig.1")
fig <- readPicture("fig.1.xml")
grid.picture(fig)
