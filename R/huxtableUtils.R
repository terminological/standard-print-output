
#' A tidy article theme for huxtables that works with google docs
#'
#' Once copied and pasted table needs to have leading spaces removed in google docs
#' @param ... a huxtable object
#' @keywords huxtable
#' @import huxtable
#' @export
#' @examples
#' hux <- huxtable(dataframe %>% select("col 1 title"=col1)) %>% defaultTableLayout()
defaultTableLayout = function(hux) {
  if(!("Arial" %in% extrafont::fonts())) {
    stop("Arial is not installed")
  }
  # return( theme_article(...) %>%
  #  set_wrap(TRUE) %>%
  #  set_all_padding(everywhere,everywhere,0) %>%
  #  set_valign(everywhere,everywhere,"top") )
  return( hux %>%
            set_font_size(everywhere,everywhere,10) %>%
            set_font(everywhere,everywhere,"Arial") %>%
            set_top_border(1, everywhere, 2) %>%
            set_bottom_border(1, everywhere, 1) %>%
            set_bottom_border(nrow(hux), everywhere, 1) %>%
            set_wrap(TRUE) %>%
            set_top_padding(everywhere,everywhere,"1pt") %>%
            set_bottom_padding(everywhere,everywhere,"0pt") %>%
            set_valign(everywhere,everywhere,"top") )
}

#' save labelled dataframe to html and pdf file silently
#'
#' @param labelledDataFrame e.g. dataframe %>% select("col 1 title"=col1)
#' @param filename file of desired output without extension.
#' @param pageWidth maximum width of the desired pdf output in inches (5.9)
#' @param defaultFontSize default size of font in points (10)
#' @param tableWidth width of the table in inches or NULL to fit contents automatically
#' @param colWidths a vector of relative column widths
#' @keywords huxtable webshot
#' @import huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' hux(iris) %>% saveTable("iris")
saveTable = function(labelledDataframe, filename, pageWidth=5.9, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  if (is_hux(labelledDataframe)) {
    tmp <- labelledDataframe
  } else {
    tmp <- mergeCells(labelledDataframe)
  }
  # if (!is.null(caption)) tmp <- tmp %>% set_caption(caption)
  if (!is.null(colWidths)) {
    tmp <- tmp %>% set_col_width(everywhere,value=colWidths)
  }
  if (!is.null(tableWidth)) {
    tmp <- tmp %>% set_width(tableWidth/pageWidth)
  } else {
    tmp <- tmp %>% set_width("auto")
  }
  
  write(
    paste0("<html><head><meta charset='UTF-8'></head><body>",
    stringr::str_remove(
      tmp %>% to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;")),
    "</body></html>"
    ),
    file=normalizePath(paste0(filename,".html"),mustWork = FALSE))
  
  attr(tmp,"font_size") <- ifelse(is.na(attr(tmp,"font_size")),defaultFontSize,attr(tmp,"font_size"))*2/3

  # tableWidth = tableWidth-(ncol(tmp)*0.1) # adjust for
  write(
    paste0("<html><head><meta charset='UTF-8'></head><body>",
    stringr::str_remove(
      tmp %>% to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;")),
    "</body></html>"
    ),
    file=normalizePath(paste0(filename,".tmp.html"),mustWork = FALSE))
  
  webshot::webshot(
    url=paste0("file://",normalizePath(paste0(filename,".html"),mustWork = FALSE)),
    file=normalizePath(paste0(filename,".png"),mustWork = FALSE),
    vwidth=pageWidth*96,vheight=10,zoom=300/96)
  # webshot::resize(paste0(filename,'.png'),paste0(96/300*100,"%"))
  
  webshot::webshot(
    url=paste0("file://",normalizePath(paste0(filename,".tmp.html"),mustWork = FALSE)),
    file=normalizePath(paste0(filename,".pdf"),mustWork = FALSE),
    vwidth=pageWidth*72,vheight=10)

  # fs::file_delete(normalizePath(paste0(filename,".tmp.html"),mustWork = FALSE))
  
}

#' save labelled dataframe to html and pdf file silently in rotated format for a landscape page
#'
#' @param labelledDataFrame e.g. dataframe %>% select("col 1 title"=col1)
#' @param filename file of desired output without extension.
#' @param pageLength maximum length of the page in inches (8)
#' @param defaultFontSize default size of font in points (10)
#' @param tableWidth width of the table in inches or NULL to fit contents automatically
#' @param colWidths a vector of relative column widths
#' @keywords huxtable webshot
#' @import huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' hux(iris) %>% saveTableLandscape("iris")
saveTableLandscape = function(labelledDataframe, filename, pageLength=8, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  saveTable(labelledDataFrame, filename, pageLength, defaultFontSize, tableWidth, colWidths)
  staplr::rotate_pdf(page_rotation = 270, 
                     input_filepath = normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
                     output_filepath = normalizePath(paste0(filename,".pdf"),mustWork = FALSE), 
                     overwrite = TRUE)
  magick::image_rotate(
      magick::image_read(
        normalizePath(paste0(filename,".png"),mustWork = FALSE)
        ),270) %>% magick::image_write(
          normalizePath(paste0(filename,".png"),mustWork = FALSE)
          )
}

#' prepare a huxtable with cells merged according to grouped colums
#'
#' @param labelledDataFrame e.g. dataframe %>% select("col 1 title"=col1)
#' @import huxtable
#' @import magrittr
#' @keywords huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' mergeCells(iris %>% group_by(Species,Petal.Width) %>% summarise(count = n()))
#' mergeCells(mtcars %>% rownames_to_column() %>% group_by(gear,carb))
#' mtcars %>% rownames_to_column() %>% group_by(gear,carb) %>% mergeCells() %>% saveTable("cars")
mergeCells = function(labelledDataFrame) {
  grps <- labelledDataFrame %>% groups()
  sel <- c(grps,as.list(colnames(labelledDataFrame)[!(colnames(labelledDataFrame) %in% grps)]))
  hux <- defaultTableLayout(huxtable(labelledDataFrame %>% arrange(!!!grps) %>% select(!!!sel),add_colnames = TRUE))
  tmpHux = hux
  for (colname in colnames(hux)) {
    if (colname %in% grps) {
      for (tt in unique(hux[[colname]][-1])) {

        colindex = grep(colname, colnames(hux))
        s = seq(1,length(hux[[colname]]))
        matches = (hux[[colname]] == tt)
        firsts = matches & !c(FALSE,(matches[seq_len(length(matches)-1)]))
        lasts = matches & !c(matches[-1],FALSE)
        # print(firsts)
        # print(lasts)
        for (i in seq(1,length(s[firsts]))) {
          l = s[firsts][i]
          r = s[lasts][i]
          lr = seq(l,r)
          tmpHux <- merge_cells(tmpHux, lr, colindex)
          tmpHux <- set_valign(tmpHux,lr,colindex,"middle")
          tmpHux <- tmpHux %>% set_top_border(l, every(n=1,from=colindex), 1) %>% set_bottom_border(l,colindex,1)
        }
      }
    }
  }
  return(tmpHux)
}

#' format a dataframe into a multi page table
#' best not to use a huxtable object with merged cells as thes will get merged.
#' Data frame needs to be ordered and grouped if it is to be split accross pages
#'
#' @param filename file of desired output without extension.
#' @param pageWidth maximum width of the desired pdf output in inches (5.9)
#' @param pageLength maximum width of the desired pdf output in inches (8)
#' @param defaultFontSize default size of font in points (10)
#' @param tableWidth width of the table in inches or NULL to fit contents automatically
#' @param colWidths a vector of relative column widths
#' @import huxtable
#' @keywords huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' mtcars %>% rownames_to_column() %>% arrange(gear,carb) %>% group_by(gear,carb) %>% saveMultiPage("carMultiTest",pageLength = 2)
saveMultiPage <- function(labelledDataFrame, filename, pageWidth=5.9, pageLength=8, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  # save the whole table
  saveTable(labelledDataFrame, filename, pageWidth, defaultFontSize, tableWidth, colWidths)
  height = .detectHeight(filename, pageWidth)
  pdfs = c(filename)
  if (height > pageLength) {
    estRowsPerPage = ((nrow(labelledDataFrame)+ceiling(pageLength / height)) * pageLength / height) - 1
    rowStart = 1
    count = 1
    while (rowStart < nrow(labelledDataFrame)) {
      tmpFilename = paste0(filename,sprintf("%03i",count))
      rowsPerPage = floor(estRowsPerPage)
      repeat {
        rowEnd = min(nrow(labelledDataFrame),rowStart+rowsPerPage)
        tmpDf = labelledDataFrame[c(rowStart:rowEnd),]
        
        saveTable(tmpDf, tmpFilename, pageWidth, defaultFontSize, tableWidth, colWidths)
        tmpHeight = .detectHeight(tmpFilename, pageWidth)
        if (tmpHeight<=pageLength) break
        rowsPerPage = floor(rowsPerPage*pageLength/tmpHeight)
      } 
      count = count + 1
      rowStart = rowStart+rowsPerPage
      pdfs = c(pdfs,tmpFilename)
    }
  }
  return(pdfs)
}

#' format a dataframe into a multi page table that is landscape in orientation
#'
#' @param filename file of desired output without extension.
#' @param pageWidth maximum width of the desired pdf output in inches (5.9)
#' @param pageLength maximum width of the desired pdf output in inches (8)
#' @param defaultFontSize default size of font in points (10)
#' @param tableWidth width of the table in inches or NULL to fit contents automatically
#' @param colWidths a vector of relative column widths
#' @import huxtable
#' @keywords huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' library(dplyr)
#' mtcars %>% rownames_to_column() %>% arrange(gear,carb) %>% group_by(gear,carb) %>% saveMultiPageLandscape("carMultiTest",pageWidth=2,pageLength = 5.9)
saveMultiPageLandscape <- function(labelledDataFrame, filename, pageWidth=5.9, pageLength=8, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  pdfs = saveMultiPage(labelledDataFrame, filename, pageLength, pageWidth, defaultFontSize, tableWidth, colWidths)
  for (filename in pdfs) {
    pdf = normalizePath(paste0(filename,".pdf"),mustWork = FALSE)
    png = normalizePath(paste0(filename,".png"),mustWork = FALSE)
    staplr::rotate_pdf(page_rotation = 270, input_filepath = pdf,output_filepath = pdf, overwrite = TRUE)
    magick::image_rotate(magick::image_read(png),270) %>% magick::image_write(png)
  }
}

.detectHeight <- function(filename, width) {
  img.n=png::readPNG(
    normalizePath(paste0(filename,".png"),mustWork = FALSE),
    info=TRUE)
  return(attr(img.n,"info")$dim[2]/attr(img.n,"info")$dim[1]*width)
}
