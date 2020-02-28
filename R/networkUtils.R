#' Saves standard graphs for publication either as EPS, PNG with 300dpi or PDF
#' Allows specific maximum dimensions with an optional target aspect ratio.
#' If no aspect ratio supplied then plot will be shaped to occupy whole area
#'
#' @param filename base of target filename (excuding extension).
#' @param graph a DiagrammeR::graph object
#' @param maxWidth maximum width in inches
#' @param maxHeight maximum height in inches
#' @keywords plot
#' @export
#' @examples
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveFigure(filename="the_filename",maxWidth=4,maxHeight=4)
saveGraph = function(graph,filename,maxWidth,maxHeight,engine="dot") {
  
  dot = DiagrammeR::generate_dot(graph)
  
  widget = dot %>% DiagrammeR::grViz(engine = engine)
  widget$sizingPolicy = htmlwidgets::sizingPolicy(
    defaultWidth = maxWidth*72,
    defaultHeight = maxHeight*72,
    browser.fill = TRUE,
    padding = 10
  )
  htmlFile = normalizePath(paste0(filename,".html"),mustWork = FALSE)
  htmlwidgets::saveWidget(widget, file = htmlFile, libdir = 'html_support_files') # save html widget
  
  # webshot::webshot(
  #   url=paste0("file://",htmlFile),
  #   file=normalizePath(paste0(filename,".png"),mustWork = FALSE),
  #   vwidth=10,vheight=10) # png saves at 96 dpi
  # # webshot::resize(paste0(filename,'.png'),paste0(96/300*100,"%"))
  # 
  # dimensions = .dimensions(filename)
  # aspectRatio = dimensions$width / dimensions$height
  # scale = min(
  #   dimensions$width/maxWidth, 
  #   dimensions$height/maxHeight)/72 # pixels per inch default png printing
  # 
  # targetHeight = min(maxHeight,maxWidth/aspectRatio)
  # targetWidth = min(maxWidth,maxHeight*aspectRatio)
  
  webshot::webshot(
    url=paste0("file://",htmlFile),
  file=normalizePath(paste0(filename,".png"),mustWork = FALSE),
  vwidth=maxWidth*300-20,vheight=maxHeight*300-20)
  
  webshot::webshot(
    url=paste0("file://",htmlFile),
    file=normalizePath(paste0(filename,".pdf"),mustWork = FALSE),
    vwidth=maxWidth*72-20,vheight=maxHeight*72-20)
  
  staplr::select_pages(1,
    input_filepath = normalizePath(paste0(filename,".pdf"),mustWork = FALSE),
    output_filepath = normalizePath(paste0(filename,".pdf"),mustWork = FALSE),
    overwrite = TRUE)
  
  # svg = widget %>% DiagrammeRsvg::export_svg() %>% charToRaw() 
  # 
  # bitmap <- rsvg(svg)
  # defaultHeight = dim(bitmap)[1]/90 # * 1.25 # this y scale factor determined empirically as output is a bit squashed for some reason.
  # defaultWidth = dim(bitmap)[2]/90
  # aspectRatio = defaultWidth / defaultHeight
  # 
  # svg %>% rsvg::rsvg_pdf(
  #     file = normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
  #     width = min(maxWidth,maxHeight*aspectRatio,defaultWidth)*72, 
  #     height = min(maxHeight,maxWidth/aspectRatio,defaultHeight)*72
  #   )
  # 
  # svg %>% rsvg::rsvg_png(
  #     file = normalizePath(paste0(filename,".png"),mustWork = FALSE), 
  #     width = min(maxWidth,maxHeight*aspectRatio,defaultWidth)*300, 
  #     height = min(maxHeight,maxWidth/aspectRatio,defaultHeight)*300
  #   )
  # 
  # svg %>% rsvg::rsvg_svg(
  #     file = normalizePath(paste0(filename,".svg"),mustWork = FALSE), 
  #     width = min(maxWidth,maxHeight*aspectRatio,defaultWidth)*72, 
  #     height = min(maxHeight,maxWidth/aspectRatio,defaultHeight)*72
  #   )
  # 
  # svg %>% rsvg::rsvg_ps(
  #     file = normalizePath(paste0(filename,".ps"),mustWork = FALSE), 
  #     width = min(maxWidth,maxHeight*aspectRatio,defaultWidth)*72, 
  #     height = min(maxHeight,maxWidth/aspectRatio,defaultHeight)*72
  #   )
  
  try(
    embedFonts(normalizePath(paste0(filename,".pdf"),mustWork = FALSE)),
    silent=TRUE
  );
  # embedFonts(paste0(filename,".eps"));
  return(widget)
}

#' A standard 6x8 inch plot size for a full page
#' This may be combined with narrowAndTall() to maximise plotting area
#'
#' @param filename base of target filename (excuding extension).
#' @param graph a DiagrammeR::graph object
#' @param ... passed to saveFigure()
#' @keywords axes
#' @export
#' @examples
#' setwd(tempdir())
#' library(ggplot2)
#' library(standardPrintOutput)
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' saveFullPageFigure(filename="the_filename")
saveFullPageGraph = function(graph,filename, ...) {
  saveGraph(graph,filename,maxWidth=5.9, maxHeight=8, ...)
}

#' A standard 6x8 inch plot size for a full page in landscape format
#'
#' @param filename base of target filename (excuding extension).
#' @param graph a DiagrammeR::graph object
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
saveFullPageGraphLandscape = function(graph,filename, ...) {
  # return(
  #   graph %>%
  #     DiagrammeR::add_global_graph_attrs(
  #       attr = "rotate",
  #       value = 90,
  #       attr_type="graph"
  #     ) %>% 
  #     saveGraph(filename,maxWidth=8, maxHeight=5.9, ...)
  # )
  tmp = saveGraph(graph,filename,maxWidth=8, maxHeight=5.9, ...)
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
  return(tmp)
}

#' A standard max 6x4 inch plot size for a half page
#'
#' @param filename base of target filename (excuding extension).
#' @param graph a DiagrammeR::graph object
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
saveHalfPageGraph = function(graph,filename, ...) {
  return(saveGraph(graph,filename,maxWidth=5.9, maxHeight=4, ...))
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
saveThirdPageGraph = function(graph,filename, ...) {
  return(saveGraph(graph,filename,maxWidth=5.9, maxHeight=3, ...))
}

#' A standard max 3x3 plot size for a smaller plot
#' maybe combined with narrowAndTall()
#'
#' @param filename base of target filename (excuding extension).
#' @param graph a DiagrammeR::graph object
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
saveSixthPageGraph = function(graph,filename, ...) {
  return(saveGraph(graph,filename,maxWidth=3, maxHeight=3, ...))
}

#' A default dot theme with standard print publication defaults with a focus on 10pt Arial as the default font
#' 
#' @keywords DiagrammeR theme
#' @export
#' @examples
#' library(ggplot2)
#' library(standardPrintOutput)
#' theme_set(defaultFigureLayout())
#' ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
defaultGraphLayout = function(graph, ...) {
  if(!("package:extrafont") %in% search()) {
    library(extrafont)
  }
  if(!("Arial" %in% fonts())) {
    stop("Arial is not installed")
  }
  return(graph %>% 
    DiagrammeR::set_node_attrs(
      node_attr = "shape",
      values = "box"
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "fixedsize",
      values = FALSE
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "fontname",
      values = "Arial"
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "fontsize",
      values = 10.0 
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "fontcolor",
      values = "#000000" 
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "margin",
      values = "0.1,0.1"
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "width",
      values = 0
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "height",
      values = 0
    ) %>% DiagrammeR::set_node_attrs(
      node_attr = "color",
      values = "#000000"
    ) %>% DiagrammeR::add_global_graph_attrs(
      attr = "nodesep",
      value = 0.2,
      attr_type="graph"
    ) %>% DiagrammeR::add_global_graph_attrs(
      attr = "ranksep",
      value = 0.25,
      attr_type="graph"
    )
  )
}