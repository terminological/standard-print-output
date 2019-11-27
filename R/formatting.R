#' 2 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param unit a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% mutate(display = twoDp(x))
#' df %>% mutate(display = twoDp(x,"km"))
twoDp <- function(x,unit="") {
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
threeDp <- function(x,unit="") {
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
fourDp <- function(x,unit="") {
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
meanAndConfidence <- function(x, f=twoDp, ...) {
  tmp <- t.test(x)
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
confidence <- function(x, f=twoDp, ...) {
  tmp <- t.test(x)
  return(paste0(f(tmp$conf.int[1],...)," — ",f(tmp$conf.int[2],...)))
}
