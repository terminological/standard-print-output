
#' A tidy article theme for huxtables that works with google docs
#'
#' Once copied and pasted table needs to have leading spaces removed in google docs
#' @param ... a huxtable object
#' @keywords huxtable
#' @import huxtable
#' @export
#' @examples
#' huxtable(dataframe %>% select("col 1 title"=col1)) %>% defaultTableLayout()
defaultTableLayout = function(hux) {
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
            set_top_padding(everywhere,everywhere,"0pt") %>%
            set_bottom_padding(everywhere,everywhere,"0pt") %>%
            set_valign(everywhere,everywhere,"top") )
}

#' save labelled dataframe to html and tex file silently
#'
#' @param labelledDataFrame e.g. dataframe %>% select("col 1 title"=col1)
#' @param filename file of desired output without extension.
#' @param pageWidth width of the desired pdf output in inches (5.9)
#' @param defaultFontSize default size of font in points (10)
#' @param tableWidth width of the table in inches or NULL to fit contents automatically
#' @param colWidths a vector of relative column widths
#' @param caption a table caption
#' @keywords omop
#' @import huxtable
#' @import webshot
#' @export
#' @examples
#' hux(iris) %>% saveTable("iris")
saveTable = function(labelledDataframe, filename, pageWidth=5.9, defaultFontSize=10, tableWidth=NULL, colWidths = NULL) {
  if (is_hux(labelledDataframe)) {
    tmp <- labelledDataframe
  } else {
    tmp <- standard-R-output::mergeCells(labelledDataframe)
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
  attr(tmp,"font_size") <- ifelse(is.na(attr(tmp,"font_size")),defaultFontSize,attr(tmp,"font_size"))*3/4

  # tableWidth = tableWidth-(ncol(tmp)*0.1) # adjust for
  write(
    str_remove(
      tmp %>% to_html(),
      fixed("margin-bottom: 2em; margin-top: 2em;")
    ),
    file=paste0(filename,'.html'))

  webshot(
    url=paste0("file://",getwd(),"/",filename,".html"),
    file=paste0(filename,'.pdf'),vwidth=pageWidth*72,vheight=10)

  # write(
  #   tmp %>%
  #     set_width(paste0(as.integer(tableWidth*72.27),"pt")) %>%
  #     to_latex(tabular_only=TRUE),
  #   file = paste0(filename,'.tex'))
}

#' prepare a huxtable with cells merged according to grouped colums
#'
#' @param labelledDataFrame e.g. dataframe %>% select("col 1 title"=col1)
#' @import huxtable
#' @import magrittr
#' @keywords huxtable
#' @export
#' @examples
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
          tmpHux <- tmpHux %>% set_top_border(l, every(n=1,from=colindex), 1) %>% set_bottom_border(l,colindex,1)
        }
      }
    }
  }
  return(tmpHux)
}



