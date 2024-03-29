
#' A tidy article theme for huxtables that works with google docs
#'
#' Once copied and pasted table needs to have leading spaces removed in google docs
#' @param hux a huxtable object
#' @param defaultFontSize default size of font in points (10)
#' @keywords huxtable
#' @import huxtable
#' @export
#' @examples
#' hux = huxtable(dataframe %>% select("col 1 title"=col1)) %>% defaultTableLayout()
defaultTableLayout = function(hux, defaultFontSize=10) {
  if(!("Arial" %in% extrafont::fonts())) {
    stop("Arial is not installed")
  }
  # return( theme_article(...) %>%
  #  set_wrap(TRUE) %>%
  #  set_all_padding(everywhere,everywhere,0) %>%
  #  set_valign(everywhere,everywhere,"top") )
  return( hux %>%
            set_font_size(everywhere,everywhere,defaultFontSize) %>%
            set_font(everywhere,everywhere,"Arial") %>%
            set_top_border(1, everywhere, 2) %>%
            set_bottom_border(1, everywhere, 1) %>%
            set_bottom_border(nrow(hux), everywhere, 1) %>%
            set_wrap(TRUE) %>%
            set_top_padding(everywhere,everywhere,1) %>%
            set_bottom_padding(everywhere,everywhere,0) %>%
            set_valign(everywhere,everywhere,"top") )
}

#' save labelled dataframe to html and pdf file silently
#'
#' @param labelledDataFrame e.g. select(dataframe,"col 1 title"=col1)
#' @param filename file of desired output without extension.
#' @param pageWidth maximum width of the desired pdf output in inches (5.9)
#' @param defaultFontSize default size of font in points (10)
#' @param tableWidth width of the table see ?huxtable::set_width
#' @param colWidths a vector of relative column widths
#' @keywords huxtable webshot
#' @import huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' hux(iris) %>% saveTable("iris")
saveTable = function(labelledDataframe, filename = tempfile(), pageWidth=5.9, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  if (is_hux(labelledDataframe)) {
    tmp = labelledDataframe %>% collect()
  } else {
    tmp = mergeCells(labelledDataframe %>% collect(), defaultFontSize)
  }
  # if (!is.null(caption)) tmp = tmp %>% set_caption(caption)
  if (!is.null(colWidths)) {
    tmp = tmp %>% set_col_width(everywhere,value=colWidths/sum(colWidths))
  }
  if (!is.null(tableWidth)) {
    tmp = tmp %>% set_width(tableWidth)
  } else {
    if (isTRUE(getOption("knitr.in.progress"))) {
      fmt <- rmarkdown::default_output_format(knitr::current_input())$name
      if (fmt == "word_document"){
        # set width to 100% for word documents as auto doesn;t work 
        tmp = tmp %>% set_width(1)
      } else {
        # for latex etc the table will be a pdf of html table do this 
        # option relevant to html
        tmp = tmp %>% set_width("auto")
      }
    } else {
      # we are in RStudio most likely the width is irrelevant
      # TODO: tidy up required here.
      tmp = tmp %>% set_width("auto")
    }
  }
  
  tmp2 = tmp
  
  ## Do Png output
  
  attr(tmp,"font_size") = ifelse(is.na(attr(tmp,"font_size")),defaultFontSize,attr(tmp,"font_size"))
  
  write(
    paste0("<html><head><meta charset='UTF-8'></head><body>",
    stringr::str_remove(
      tmp %>% to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;")),
    "</body></html>"
    ),
    file=normalizePath(paste0(filename,".html"),mustWork = FALSE))
  
  webshot::webshot(
    url=paste0("file://",normalizePath(paste0(filename,".html"),mustWork = FALSE)),
    file=normalizePath(paste0(filename,".png"),mustWork = FALSE),
    vwidth=pageWidth*96,vheight=10,zoom=300/96)
  
  ## Do pdf output
  
  attr(tmp2,"font_size") = ifelse(is.na(attr(tmp2,"font_size")),defaultFontSize,attr(tmp2,"font_size"))*2/3
  
  write(
    paste0("<html><head><meta charset='UTF-8'></head><body>",
    stringr::str_remove(
      tmp2 %>% to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;")),
    "</body></html>"
    ),
    file=normalizePath(paste0(filename,".tmp.html"),mustWork = FALSE))
  
  webshot::webshot(
    url=paste0("file://",normalizePath(paste0(filename,".tmp.html"),mustWork = FALSE)),
    file=normalizePath(paste0(filename,".pdf"),mustWork = FALSE),
    vwidth=pageWidth*72,vheight=10)

  fs::file_delete(normalizePath(paste0(filename,".tmp.html"),mustWork = FALSE))
  
  if (isTRUE(getOption("knitr.in.progress"))) {
    fmt <- rmarkdown::default_output_format(knitr::current_input())$name
    # if (fmt == "pdf_document"){
    #   #...
    # }
    # 
    if (fmt == "word_document"){
      return(as_flextable(tmp))
    } else if (stringr::str_detect(fmt,"html")) {
      return(knitr::raw_html(tmp %>% to_html()))
    } else {
      return(knitr::include_graphics(path = normalizePath(paste0(filename,".png"),mustWork = FALSE),auto_pdf = TRUE))
    }
  } else {
    return(tmp)
  }
}

#' save labelled dataframe to html and pdf file silently in rotated format for a landscape page
#'
#' @param labelledDataFrame e.g. select(dataframe,"col 1 title"=col1)
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
saveTableLandscape = function(labelledDataframe, filename = tempfile(), pageLength=8, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  tmp = saveTable(labelledDataframe, filename, pageLength, defaultFontSize, tableWidth, colWidths)
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
  return(tmp)
}

#' prepare a huxtable with cells merged according to grouped colums
#'
#' @param labelledDataFrame e.g. select(dataframe,"col 1 title"=col1)
#' @param defaultFontSize the defaul font size
#' @import huxtable
#' @import magrittr
#' @keywords huxtable
#' @export
#' @examples
#' setwd(tempdir())
#' mergeCells(iris %>% group_by(Species,Petal.Width) %>% summarise(count = n()))
#' mergeCells(mtcars %>% rownames_to_column() %>% group_by(gear,carb))
#' mtcars %>% rownames_to_column() %>% group_by(gear,carb) %>% mergeCells() %>% saveTable("cars")
mergeCells = function(labelledDataFrame,defaultFontSize=10) {
  grps = labelledDataFrame %>% groups()
  cols = lapply(colnames(labelledDataFrame),as.symbol)
  sel = c(grps,cols[!cols %in% grps])
  hux = defaultTableLayout(huxtable(labelledDataFrame %>% arrange(!!!grps) %>% select(!!!sel) %>% collect(),add_colnames = TRUE),defaultFontSize)
  tmpHux = hux
  
  while (length(grps)>0) {
    mergeCol = grps %>% tail(1)
    for( mergeRows in (tmpHux %>% group_by(!!!grps) %>% group_data() %>% pull(.rows))) {
      #browser()
      colindex = grep(as_label(mergeCol[[1]]), colnames(hux))
      l = min(mergeRows)
      lr = c(min(mergeRows),max(mergeRows))
      tmpHux = huxtable::merge_cells(tmpHux,row=lr, col=colindex)
      tmpHux = set_valign(tmpHux,lr,colindex,"middle")
      tmpHux = tmpHux %>% set_top_border(l, final(ncol(tmpHux)-colindex+1), 1) #%>% set_bottom_border(l, colindex,1)
    }
    grps = grps %>% head(-1)
  }
  
  # for (colname in colnames(hux)) {
  #   if (colname %in% grps) {
  #     for (tt in unique(hux[[colname]][-1])) {
  # 
  #       colindex = grep(colname, colnames(hux))
  #       s = seq(1,length(hux[[colname]]))
  #       matches = (hux[[colname]] == tt)
  #       firsts = matches & !c(FALSE,(matches[seq_len(length(matches)-1)]))
  #       lasts = matches & !c(matches[-1],FALSE)
  #       # print(firsts)
  #       # print(lasts)
  #       for (i in seq(1,length(s[firsts]))) {
  #         l = s[firsts][i]
  #         r = s[lasts][i]
  #         lr = seq(l,r)
  #         tmpHux = merge_cells(tmpHux, lr, colindex)
  #         tmpHux = set_valign(tmpHux,lr,colindex,"middle")
  #         tmpHux = tmpHux %>% set_top_border(l, every(n=1,from=colindex), 1) %>% set_bottom_border(l,colindex,1)
  #       }
  #     }
  #   }
  # }
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
#' mtcars %>% rownames_to_column() %>% arrange(gear,carb) %>% group_by(gear,carb) %>% saveMultiPageTable("carMultiTest",pageLength = 2)
saveMultiPageTable = function(labelledDataFrame, filename = tempfile(), pageWidth=5.9, pageLength=8, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  # save the whole table
  tmp = saveTable(labelledDataFrame, filename, pageWidth, defaultFontSize, tableWidth, colWidths)
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
#' mtcars %>% rownames_to_column() %>% arrange(gear,carb) %>% group_by(gear,carb) %>% saveMultiPageTableLandscape("carMultiTest",pageWidth=2,pageLength = 5.9)
saveMultiPageTableLandscape = function(labelledDataFrame, filename = tempfile(), pageWidth=5.9, pageLength=8, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  pdfs = saveMultiPageTable(labelledDataFrame, filename, pageLength, pageWidth, defaultFontSize, tableWidth, colWidths)
  for (filename in pdfs) {
    pdf = normalizePath(paste0(filename,".pdf"),mustWork = FALSE)
    png = normalizePath(paste0(filename,".png"),mustWork = FALSE)
    staplr::rotate_pdf(page_rotation = 270, input_filepath = pdf,output_filepath = pdf, overwrite = TRUE)
    magick::image_rotate(magick::image_read(png),270) %>% magick::image_write(png)
  }
  return(pdfs)
}

.detectHeight = function(filename, width) {
  return(width/.aspectRatio(filename))
}

.aspectRatio = function(filename) {
  img.n=png::readPNG(
    normalizePath(paste0(filename,".png"),mustWork = FALSE),
    info=TRUE)
  return(attr(img.n,"info")$dim[1]/attr(img.n,"info")$dim[2]) # width/height
}

.dimensions = function(filename) {
  img.n=png::readPNG(
    normalizePath(paste0(filename,".png"),mustWork = FALSE),
    info=TRUE)
  return(list(
    width = attr(img.n,"info")$dim[1],
    height = attr(img.n,"info")$dim[2]
  ))
}

.detectWidth = function(filename, height) {
  return(height*.aspectRatio(filename))
}