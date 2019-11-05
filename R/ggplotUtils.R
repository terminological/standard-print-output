
#' A standard X axis with hour from a sunday
#'
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+weekXaxis()
weekXaxis <- function() {
  return(geom_vline(xintercept = 0, colour='grey')+
           geom_vline(xintercept = 24, colour='grey')+
           geom_vline(xintercept = 48, colour='grey')+
           geom_vline(xintercept = 72, colour='grey')+
           geom_vline(xintercept = 96, colour='grey')+
           geom_vline(xintercept = 120, colour='grey')+
           geom_vline(xintercept = 144, colour='grey')+
           geom_vline(xintercept = 168, colour='grey')+
           xlab("hours from midnight sunday") +
           scale_x_continuous(breaks = seq(0, 24*7, 12))
  );
}

#' A standard X axis with hour from midnight
#'
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+dayXaxis()
dayXaxis <- function() {
  return(
    xlab("hours from midnight") +
    scale_x_continuous(breaks = seq(0, 24, 1))
  );
}

#' A standard X axis of months of year where data is a 'MM-DD' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+dayXaxis()
monthsXaxis <- function() {
  return(
    xlab("month of year")+
    scale_x_discrete(breaks =
        c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),
        labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}

#' A standard X axis of months of year where data is a 'MM' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+monthXaxis2()
monthsXaxis2 <- function(plot) {
  return(plot +
           xlab("month of year")+
           scale_x_discrete(breaks =
                c('01','02','03','04','05','06','07','08','09','10','11','12'),
                labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}



# TODO:

# theme(axis.text.x = element_text(angle=90,size=9))+

# remove text labels
# +rremove("x.text")+rremove("ylab")+rremove("xlab")
# remove guides
# guides(fill=FALSE, size=FALSE)+

# print sizes - width between 8.3 cms to 17.35 cms - BMJ
# nature: 89mm (single) 183mm (double) and max depth 247mm


#' A standard plot for publication
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param maxWidth maximum width in inches
#' @param maxHeight maximum height in inches
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @keywords plot
#' @import ggplot2
#' @export
#' @examples
#' saveFigure("the_filename",maxWidth=4,maxHeight=4,plot=ggplot())
saveFigure <- function(filename,maxWidth,maxHeight,aspectRatio=maxWidth/maxHeight,plot = last_plot()) {
  ggplot2::ggsave(paste0(filename,".pdf"), plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio));
  ggplot2::ggsave(paste0(filename,".png"), plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), dpi=300);
  ggplot2::ggsave(paste0(filename,".eps"), plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), dpi=300);
  embedFonts(paste0(filename,".pdf"));
}

#' A standard 6x8 inch plot size for a full page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveThesis()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveFullPageFigure("the_filename",ggplot())
saveFullPageFigure = function(filename, ...) {
  saveFigure(filename, plot=last_plot(),maxWidth=5.9, maxHeight=8, ...)
}

#' A standard max 6x4 inch plot size for a half page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveThesis()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveHalfPageFigure("the_filename",ggplot())
saveHalfPageFigure = function(filename, ...) {
  saveFigure(filename, plot=last_plot(),maxWidth=5.9, maxHeight=4, ...)
}

#' A standard max 6x3 plot size for a third page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveThesis()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveThirdPageFigure("the_filename",ggplot())
saveThirdPageFigure = function(filename, ...) {
  saveFigure(filename, plot=last_plot(),maxWidth=5.9, maxHeight=3, ...)
}

#' A standard max 3x3 plot size for a page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveThesis()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveThesisSixthPage("the_filename",ggplot())
saveSixthPageFigure = function(filename, ...) {
  saveFigure(filename, plot=last_plot(),maxWidth=3, maxHeight=3, ...)
}

#' A ggplot theme with standard print publication defaults
#' 
#' @keywords plot theme
#' @import ggplot2
#' @import extrafont
#' @export
#' @examples
#' theme_set(themeStandardOutput())
themeStandardOutput <- function(...) {
  if(!("package:extrafont") %in% search()) library(extrafont)
  return(
  theme_bw(base_size=10, base_family = "Arial", ...)+
    theme(
      plot.title=element_text(size=10,hjust=0.5),
      axis.title=element_text(size=10),
      axis.text=element_text(size=10),
      axis.text.x=element_text(angle = 30, hjust = 1)
    )
  )}

#' A ggplot theme modification for graphs that need to be narrower and taller than defaults
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+narrowAndTall()
narrowAndTall <- function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.x=element_text(angle = 60, hjust = 1)
  );
}
