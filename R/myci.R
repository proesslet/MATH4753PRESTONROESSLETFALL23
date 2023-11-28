#' A function to find the 95% confidence interval of a single sample x
#'
#' @param x a sample
#'
#' @return the 95% confidence interval
#' @export
#'
#' @examples
#' myci(rnorm(15, mean = 250, sd = 25))
myci <- function(x){
  n1=length(x)
  spsq=((n1-1)*var(snapper))/(n1-1)
  t=qt(0.975,n1-1)
  ci=c()

  ci[1]=mean(x)-t*sqrt(spsq*(1/n1+1/n2))
  ci[2]=mean(x)+t*sqrt(spsq*(1/n1+1/n2))
  ci
}
