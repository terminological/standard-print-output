#' Publication friendly printing
#' 
#' The goal here is a figure, table and text snippet manager that allows us to 
#' 
#' Print inline in an rMarkdown document
#' Control figure sizes for final publication inline in markup
#' transparently produce png / pdf / eps for publication output
#' Allow automatically laid out tables in PDF output without latex agony
#' Allow inline tables in word output
#' Control document level defaults for tables and figures.
#' Manage captions and inline references to captions for tables and figures
#' Cache figures and tables and allow them to be reproduced at different sizes
#' 
#' @export
StandardPrint = R6::R6Class("StandardPrint", public = list(
))

#' Caching and dataset management
#' @export
OutputCache = R6::R6Class("OutputCache", 
  private = list(
    cache = list(),
    quiet = FALSE,
    wd = NULL
  ),
  
  public=list(
    
    initialize = function(wd = tempdir(), quiet = FALSE,  ...) {
      private$wd = path.expand(wd) %>% stringr::str_remove("/$") 
    },
    
    #' @description a pass through 2 level cache (memory / saved file / orElse function)
    cached = function(name, func, nocache = FALSE, ...) {
      dots = rlang::list2(...)
      funcId = as.character(openssl::md5(serialize(func, connection=NULL)))
      if(length(dots)>0) {
        dataId = as.character(openssl::md5(serialize(dots, connection = NULL)))
        outputId = paste0(name,"-",dataId,"-OUTPUT-",funcId)
        inputId = paste0(name,"-",dataId,"-INPUT")
        saveRDS(dots,paste0(private$wd,"/",inputId,".rda"))
      } else {
        outputId = paste0(name,"-OUTPUT-",funcId)
      }
      filename = paste0(wd,"/",outputId,".rda")
      if(nocache) {
        private$cache[[outputId]] <- NULL
        unlink(filename)
      }
      if (exists(outputId, where=private$cache)) {
        if(!private$quiet) message("using precalculated: ", outputId)
        return(private$cache[[outputId]]) 
      } else if(file.exists(filename)) {
        if(!private$quiet) message("using cached: ", outputId)
        map = readRDS(filename)
        private$cache[[outputId]]=map
        return(map)
      } else {
        if(!private$quiet) message("caching: ", outputId)
        map = rlang::exec("func", !!!dots)
        saveRDS(map,filename)
        private$cache[[outputId]]=map
        return(map)
      }
    }
  )
)

#' 
#' @export
StandardFigure = R6::R6Class("StandardFigure", public = list(
  
))


#' 
#' @export
StandardTable = R6::R6Class("StandardTable", public = list(
  
))



