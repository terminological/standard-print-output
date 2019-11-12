
#' A standard X axis with hour from a sunday
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_week_continuous()
scale_week_continuous <- function() {
  return(geom_vline(xintercept = 0, colour='grey')+
           geom_vline(xintercept = 24, colour='grey')+
           geom_vline(xintercept = 48, colour='grey')+
           geom_vline(xintercept = 72, colour='grey')+
           geom_vline(xintercept = 96, colour='grey')+
           geom_vline(xintercept = 120, colour='grey')+
           geom_vline(xintercept = 144, colour='grey')+
           geom_vline(xintercept = 168, colour='grey')+
           xlab("time of week") +
           scale_x_continuous(breaks = seq(0, 24*7, 12), labels = c(
             'Mon 00:00','Mon 12:00','Tue 00:00','Tue 12:00','Wed 00:00','Wed 12:00','Thu 00:00','Thu 12:00','Fri 00:00','Fri 12:00',
             'Sat 00:00','Sat 12:00','Sun 00:00','Sun 12:00','Mon 00:00'))+
           theme(axis.text.x=element_text(angle = 45, hjust = 1))
  )
}

#' A standard X axis with hour from midnight
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_hours_continuous()
scale_hours_continuous <- function() {
  return(
    xlab("hours from midnight") +
    scale_x_continuous(breaks = seq(0, 24, 1))
  );
}

#' A standard X axis of months of year where data is a 'MM-DD' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_day_of_year_discrete()
scale_day_of_year_discrete <- function() {
  return(
    xlab("day of year")+
    scale_x_discrete(breaks =
        c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),
        labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}

#' A standard X axis of months of year where data is a 'MM' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_month_discrete()
scale_month_discrete <- function() {
  return(
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


#' Saves standard plots for publication either as EPS, PNG with 300dpi or PDF
#' Allows specific maximum dimensions with an optional target aspect ratio.
#' If no aspect ratio supplied then plot will be shaped to occupy whole area
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
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveFigure(filename="the_filename",maxWidth=4,maxHeight=4)
saveFigure <- function(plot = last_plot(),filename,maxWidth,maxHeight,aspectRatio=maxWidth/maxHeight) {
  if (!capabilities()["cairo"] ) {
    stop("Needs cairo to work")
  }
  ggplot2::ggsave(
    normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
    plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), device = cairo_pdf);
  ggplot2::ggsave(
    normalizePath(paste0(filename,".png"),mustWork = FALSE), 
    plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), dpi=300, type = "cairo");
  ggplot2::ggsave(
    normalizePath(paste0(filename,".eps"),mustWork = FALSE), 
    plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), device = cairo_ps);
  embedFonts(normalizePath(paste0(filename,".pdf"),mustWork = FALSE));
  # embedFonts(paste0(filename,".eps"));
}

#' A standard 6x8 inch plot size for a full page
#' This may be combined with narrowAndTall() to maximise plotting area
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveFigure()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveFullPageFigure(filename="the_filename")
saveFullPageFigure = function(plot = last_plot(),filename, ...) {
  saveFigure(plot,filename,maxWidth=5.9, maxHeight=8, ...)
}

#' A standard max 6x4 inch plot size for a half page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveFigure()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveHalfPageFigure(filename="the_filename")
saveHalfPageFigure = function(plot = last_plot(),filename, ...) {
  saveFigure(plot,filename,maxWidth=5.9, maxHeight=4, ...)
}

#' A standard max 6x3 plot size for a third page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveFigure()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveThirdPageFigure(filename="the_filename")
saveThirdPageFigure = function(plot = last_plot(),filename, ...) {
  saveFigure(plot,filename,maxWidth=5.9, maxHeight=3, ...)
}

#' A standard max 3x3 plot size for a smaller plot
#' maybe combined with narrowAndTall()
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param ... passed to saveFigure()
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveSixthPageFigure(filename="the_filename")
saveSixthPageFigure = function(plot = last_plot(),filename, ...) {
  saveFigure(plot,filename,maxWidth=3, maxHeight=3, ...)
}

#' A ggplot theme with standard print publication defaults with a focus on 10pt Arial as the default font
#' 
#' @keywords plot theme
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' theme_set(defaultFigureLayout())
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
defaultFigureLayout <- function(...) {
  if(!("package:extrafont") %in% search()) {
    library(extrafont)
  }
  if(!("Arial" %in% fonts())) {
    stop("Arial is not installed")
  }
  return(
      theme_bw(base_size=10, base_family = "Arial", ...)+
      theme(
        plot.title=element_text(size=10,hjust=0.5),
        axis.title=element_text(size=10),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = 30, hjust = 1)
      )
  )
}

#' A ggplot theme modification for graphs that need to be narrower and taller than defaults
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point() + narrowAndTall()
narrowAndTall <- function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.x=element_text(angle = 60, hjust = 1)
  );
}

#' A ggplot annotation layer that puts a watermark on a graph
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @param show a flag to allow a global level of control
#' @param lab a label to annotate (e.g. DRAFT)
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point() + watermark()
watermark = function(show = TRUE, lab = "DRAFT") {
  if (show) {
    grb <- grid::grob(lab=lab, cl="watermark")
    return(annotation_custom(grb,xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf))
  } else {
    return(geom_blank())
  }
}

