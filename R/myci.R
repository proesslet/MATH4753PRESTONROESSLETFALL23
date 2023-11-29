#' A function to find the 95percent confidence interval of a single sample x
#'
#' @param x a sample
#' @importFrom stats t.test
#'
#' @return the 95 percent confidence interval
#' @export
#'
#' @examples
#' x=rnorm(30, mean=10, sd=12)
#' myci(x)
myci <- function(x){
  result <- t.test(x, conf.level = 0.95)

  # Print the confidence interval
  result$conf.int
}
