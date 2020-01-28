#' 2 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param unit a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% mutate(display = twoDp(x))
#' df %>% mutate(display = twoDp(x,"km"))
twoDp = function(x,unit="") {
  paste0(sprintf("%.2f",x),unit)
}

#' 3 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param unit a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% mutate(display = threeDp(x))
#' df %>% mutate(display = threeDp(x,"mm"))
threeDp = function(x,unit="") {
  paste0(sprintf("%.3f",x),unit)
}

#' 4 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param unit a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% mutate(display = fourDp(x))
#' df %>% mutate(display = fourDp(x,"%"))
fourDp = function(x,unit="") {
  paste0(sprintf("%.4f",x),unit)
}

#' mean and confidence intervals using t-test
#'
#' @param x a numeric or vector of numerics
#' @param f a formatting function
#' @keywords format
#' @export
#' @examples
#' df %>% mutate(display = meanAndConfidence(x))
#' df %>% mutate(display = meanAndConfidence(x,fourDp,unit="cm"))
meanAndConfidence = function(x, f=twoDp, ...) {
  tmp = t.test(x)
  return(paste0(f(tmp$estimate[["mean of x"]],...)," (95% CI: ",f(tmp$conf.int[1],...)," \u2014 ",f(tmp$conf.int[2],...),")"))
}

#' confidence interval of mean using t-test
#'
#' @param x a numeric or vector of numerics
#' @param f a formatting function
#' @keywords format
#' @export
#' @examples
#' df %>% mutate(display = confidence(x))
confidence = function(x, f=twoDp, ...) {
  tmp = t.test(x)
  return(paste0(f(tmp$conf.int[1],...)," \u2014 ",f(tmp$conf.int[2],...)))
}


#' Truncate a dplyr summarisation using an "Other..." summary row
#' 
#' Sometimes when you have a lot of results you want to show a short version
#' of the results with an "Other..." category. This function performs a summarisation and allows you to specify 
#' the number of rows you want and something to sort the rows by. It will then give you
#' strictly the top n results (ignoring ties) and group all the other results into a single summary row 
#' entitled "Other..."
#' 
#' @param df - a grouped df - there must be some grouping for the summarise to return more than one row.
#' @param n the number of rows to keep - the result will have one more row than this
#' @param sortVar - the column containing the variable to sort by (syntax as in arrange(...))
#' @param label -  the name of the "Other..." row
#' @param ... - the arguments to the dplyr summarise(...)
#' @import dplyr
#' @export
summariseTopN = function(df, n, sortVar, label="Other...", ...) {
  grps = df %>% groups()
  sortVar = enquo(sortVar)
  
  if (length(grps) == 0) stop("data frame must be grouped")
  out = df %>% select(!!!grps) %>% distinct()
  
  grpsList = sapply(grps,as.character)
  otherGroup = data.frame(matrix(c(label,rep(NA,length(grpsList)-1)),1))
  colnames(otherGroup) <- grpsList
  
  out = df %>% group_by(!!!grps) %>% summarize(...) %>% arrange(!!sortVar) %>% head(n)
  others = df %>% anti_join(out, by=grpsList) %>% ungroup() %>% summarize(...)
  
  others = otherGroup %>% bind_cols(others)
  suppressWarnings(
    out = out %>% bind_rows(others)
  )
  
  return(out %>% group_by(!!!grps))
  
}

#' @export
summarizeTopN = summariseTopN