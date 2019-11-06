
# Setup watermark drawing function ------------

.onLoad <- function(libname, pkgname) {
  
  drawDetails.watermark <<- function(x, rot = 45, ...){
    cex <- min(
      grid::convertX(unit(0.9,"npc"), "mm", valueOnly=TRUE) / grid::convertUnit(unit(1,"grobwidth", grid::textGrob(x$lab, rot=rot)), "mm",valueOnly=TRUE),
      grid::convertY(unit(0.9,"npc"), "mm", valueOnly=TRUE) / grid::convertUnit(unit(1,"grobheight", grid::textGrob(x$lab, rot=rot)), "mm",valueOnly=TRUE)
    )
    return(grid::grid.text(x$lab,  rot=rot, gp=grid::gpar(cex = cex, col="black", fontface = "bold", alpha = 0.05)))
  }
  
}