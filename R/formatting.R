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
  return(paste0(f(tmp$estimate[["mean of x"]],...)," (95% CI: ",f(tmp$conf.int[1],...),", ",f(tmp$conf.int[2],...),")"))
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
  return(paste0(f(tmp$conf.int[1],...)," — ",f(tmp$conf.int[2],...)))
}


#' Truncate a count
#' 
#' Truncate the results of a count using an "other..." category
#' 
#' @param df - a grouped df
#' @param n the number of rows to keep
#' @param countVar - the column containing the count, or in which the count is to be written
#' @param label -  the name of the "Other..." category
countWithOtherCategory = function(df, n, countVar, label="Other...") {
  grps = df %>% groups()
  if (length(grps) == 0) stop("data frame must be grouped")
  countVar = ensym(countVar)
  if (as.character(countVar) %in% colnames(df)) {
    # there is a count column. 
    df = df %>% group_by(!!!grps) %>% summarise(tmp_count = sum(!!countVar))
  } else {
    # there is no count column.
    df = df %>% group_by(!!!grps) %>% summarise(tmp_count = n())
  }
  total = df %>% ungroup() %>% summarise(n = sum(tmp_count)) %>% pull(n)
  df = df %>% arrange(desc(tmp_count)) %>% head(n)
  other = total -  (df %>% ungroup() %>% summarise(n = sum(tmp_count)) %>% pull(n))
  labels = c(sapply(grps,as.character))
  values = data.frame(matrix(c(label,rep(NA,length(labels)-1)),1))
  colnames(values) <- labels
  values = values %>% mutate(tmp_count = other)
  df = df %>% bind_rows(values) %>% rename(!!countVar := tmp_count)
  
  return(df)
}