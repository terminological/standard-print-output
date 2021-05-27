#' A map theme
#' 
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+mapTheme()
mapTheme = function() {
  return(theme(axis.text.x=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank()))
}


#' A standard X axis with hour from a sunday
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_week_continuous()
scale_week_continuous = function() {
  return(list(
	geom_vline(xintercept = 0, colour='grey'),
           geom_vline(xintercept = 24, colour='grey'),
           geom_vline(xintercept = 48, colour='grey'),
           geom_vline(xintercept = 72, colour='grey'),
           geom_vline(xintercept = 96, colour='grey'),
           geom_vline(xintercept = 120, colour='grey'),
           geom_vline(xintercept = 144, colour='grey'),
           geom_vline(xintercept = 168, colour='grey'),
           xlab("time of week"),
           scale_x_continuous(breaks = seq(0, 24*7, 12), labels = c(
             'Mon 00:00','Mon 12:00','Tue 00:00','Tue 12:00','Wed 00:00','Wed 12:00','Thu 00:00','Thu 12:00','Fri 00:00','Fri 12:00',
             'Sat 00:00','Sat 12:00','Sun 00:00','Sun 12:00','Mon 00:00')),
           theme(axis.text.x=element_text(angle = 45, hjust = 1))
  ))
}

#' A standard X axis with hour from midnight
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_hours_continuous()
scale_hours_continuous = function() {
  return(list(
    xlab("hours from midnight"),
    scale_x_continuous(breaks = seq(0, 24, 1))
  ));
}

#' A standard X axis of months of year where data is a 'MM-DD' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_day_of_year_discrete()
scale_day_of_year_discrete = function() {
  return(list(
    xlab("day of year"),
    scale_x_discrete(breaks =
        c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),
        labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  ));
}

#' A standard X axis of months of year where data is a 'MM' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' ggplot()+scale_month_discrete()
scale_month_discrete = function() {
  return(list(
           xlab("month of year"),
           scale_x_discrete(breaks =
                c('01','02','03','04','05','06','07','08','09','10','11','12'),
                labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  ));
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
saveFigure = function(plot,filename,maxWidth,maxHeight,aspectRatio=maxWidth/maxHeight) {
  if (!capabilities()["cairo"] ) {
    stop("Needs cairo to work")
  }
  dir = dirname(normalizePath(paste0(filename,".pdf"),mustWork = FALSE))
  if (!dir.exists(dir)) dir.create(dir,recursive = TRUE)
  ggplot2::ggsave(
    normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
    plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), device = cairo_pdf);
  ggplot2::ggsave(
    normalizePath(paste0(filename,".png"),mustWork = FALSE), 
    plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), dpi=300, type = "cairo");
  ggplot2::ggsave(
    normalizePath(paste0(filename,".eps"),mustWork = FALSE), 
    plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), device = cairo_ps);
  try(
    embedFonts(normalizePath(paste0(filename,".pdf"),mustWork = FALSE)),
    silent=TRUE
  );
  # embedFonts(paste0(filename,".eps"));
  if (isTRUE(getOption("knitr.in.progress"))) {
    hidefigs = getOption("spo.hidefigures",FALSE)
    if (hidefigs) {
      return(knitr::asis_output(paste0("INSERT FIGURE HERE: ",basename(filename),"\n\n")))
    } else {
      return(knitr::include_graphics(path = normalizePath(paste0(filename,".png"),mustWork = FALSE),auto_pdf = TRUE))
    }
  } else {
    return(plot)
  }
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
saveFullPageFigure = function(plot,filename, ...) {
  saveFigure(plot,filename,maxWidth=5.9, maxHeight=8, ...)
}

#' A standard 6x8 inch plot size for a full page in landscape format
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
#' saveFullPageLandscapeFigure(filename="the_filename")
saveFullPageFigureLandscape = function(plot,filename, ...) {
  saveFigure(plot,filename,maxWidth=8, maxHeight=5.9, ...)
  staplr::rotate_pdf(page_rotation = 270, 
                   input_filepath = normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
                   output_filepath = normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
                   overwrite = TRUE)
  magick::image_rotate(
    magick::image_read(
      normalizePath(paste0(filename,".png"),mustWork = FALSE)
    )
    ,270
  ) %>% magick::image_write(
    normalizePath(paste0(filename,".png"),mustWork = FALSE)
  )
  return(plot)
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
saveHalfPageFigure = function(plot,filename, ...) {
  return(saveFigure(plot,filename,maxWidth=5.9, maxHeight=4, ...))
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
saveThirdPageFigure = function(plot,filename, ...) {
  return(saveFigure(plot,filename,maxWidth=5.9, maxHeight=3, ...))
}

#' A standard max 6x6 plot size for a two third page
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
saveTwoThirdPageFigure = function(plot,filename, ...) {
  return(saveFigure(plot,filename,maxWidth=5.9, maxHeight=6, ...))
}

#' A standard max 6x2 plot size for a quarter page
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
#' saveQuarterPageFigure(filename="the_filename")
saveQuarterPageFigure = function(plot,filename, ...) {
  return(saveFigure(plot,filename,maxWidth=5.9, maxHeight=2, ...))
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
saveSixthPageFigure = function(plot,filename, ...) {
  return(saveFigure(plot,filename,maxWidth=3, maxHeight=3, ...))
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
saveSlideFigure = function(plot,filename, ...) {
  return(saveFigure(plot,filename,maxWidth=12, maxHeight=6, ...))
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
defaultFigureLayout = function(...) {
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
        plot.subtitle = element_text(size=8,hjust=0.5),
        axis.title=element_text(size=8),
        axis.text=element_text(size=8),
        axis.text.x=element_text(angle = 30, hjust = 1),
        strip.text = element_text(margin = margin(.05, 0, .05, 0, "cm"), size=8),
        strip.background = element_rect(fill = "#F8F8F8"),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.margin=margin(t = 0, unit='cm')
      )
  )
}

#' Set the ggplot default theme
#' 
#' @keywords plot theme
#' @export
setDefaults = function() {
  ggplot2::theme_set(defaultFigureLayout())
}

#' A ggplot theme with standard print publication defaults with a focus on 10pt Arial as the default font
#' 
#' @keywords plot theme
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' theme_set(defaultFigureLayout())
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
defaultMapLayout = function(...) {
  defaultFigureLayout()+mapTheme()
}

#' A ggplot theme modification for graphs that need to be narrower than defaults
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point() + narrower()
narrower = function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box="vertical"
  );
}

#' A ggplot theme modification for graphs that need to be narrower than defaults
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point() + narrower()
hideX = function() {
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
  );
}

#' A ggplot theme modification for graphs that need to be narrower than defaults
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point() + narrower()
hideY = function() {
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
  );
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
narrowAndTall = function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.x=element_text(angle = 60, hjust = 1),
    legend.box="vertical"
  );
}


smallLegend <- function(pointSize = 0.75, textSize = 8, spaceLegend = 0.2) {
  return(list(
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))),
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"),
          legend.box.margin = margin())
  ))
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
    grb = grid::grob(lab=lab, cl="watermark")
    return(annotation_custom(grb,xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf))
  } else {
    return(geom_blank())
  }
}

#' A fix geom_text sizes to be in the correct size for pdf export
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @param pts size of text in points
#' @examples
#' ...+geom_label_repel(size=labelInPoints(10))
labelInPoints = function(pts) {
  return (pts/ggplot2:::.pt/(96/72))
}

#' A label annotation in the corner of a plot
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @param text the text of the label (may be a vector)
#' @param pos one of NE, NW, SW, SE
#' @param pts size of text in points
#' @examples
#' ...+cornerAnnotation(c("line 1","line 2"),pos="SW",pts=10)
cornerAnnotation = function(text, pos = "NE", pts=8) {
  label4 = paste0(text,collapse = "\n")
  if (stringr::str_detect(pos,"N|n")) y_pos = Inf
  else y_pos = -Inf
  if (stringr::str_detect(pos,"E|e")) x_pos = Inf
  else x_pos = -Inf
  return(annotate(geom = "label", x = x_pos, y = y_pos, label = label4, fill=NA, label.size=0, label.padding = unit(2, "lines"),hjust="inward", vjust="inward",size=standardPrintOutput::labelInPoints(pts)))
}
  
#' A simple table as a ggplot patchwork object, no customisation allowed
#' 
#' @keywords graph layout
#' @import ggplot2
#' @export
#' @param df the dataframe with the table data
#' @param pts text size
#' @examples
#' ...+simpleFigureTable(tibble(x=c(1,2,3),y=c(5,4,3)),pts=10)
simpleFigureTable = function(df, pts=8, unwrapped = FALSE) {
  p = suppressWarnings(suppressMessages({
    ttheme = gridExtra::ttheme_minimal(
      base_size = pts, base_colour = "black", base_family = "Arial", 
      parse = FALSE, padding = unit(c(4, 1.5), "mm"), 
      core=list(fg_params=list(hjust=0, x=0.1)),
      colhead=list(fg_params=list(hjust=0, x=0.1))
    )
    g = gridExtra::tableGrob(d = df,rows = NULL,theme = ttheme)
    g <- gtable::gtable_add_grob(g,
                                 grobs = grid::segmentsGrob( # line across the bottom
                                   x0 = unit(0,"npc"),
                                   y0 = unit(0,"npc"),
                                   x1 = unit(1,"npc"),
                                   y1 = unit(0,"npc"),
                                   gp = grid::gpar(lwd = 2.0)),
                                 t = nrow(g), l = 1, r = ncol(g))
    g <- gtable::gtable_add_grob(g,
                                 grobs = grid::grobTree(
                                   grid::segmentsGrob( # line across the bottom
                                     x0 = unit(0,"npc"),
                                     y0 = unit(1,"npc"),
                                     x1 = unit(1,"npc"),
                                     y1 = unit(1,"npc"),
                                     gp = grid::gpar(lwd = 2.0)),
                                   grid::segmentsGrob( # line across the bottom
                                     x0 = unit(0,"npc"),
                                     y0 = unit(0,"npc"),
                                     x1 = unit(1,"npc"),
                                     y1 = unit(0,"npc"),
                                     gp = grid::gpar(lwd = 1.0))
                                 ),
                                 t = 1, l = 1, r = ncol(g))
    if(!unwrapped) return(patchwork::wrap_ggplot_grob(g))
    g
  }))
  return(p)
}
