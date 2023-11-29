#' Histogram of sample means
#'
#' @param n - number
#' @param iter - iterations
#' @importFrom graphics hist
#' @importFrom stats runif
#'
#' @return
#' @export
#'
#' @examples
#' histo(n=10,iter=10000)
histo <- function(n,iter){
  y=runif(n*iter,0,5)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  samplemean=apply(data,2,mean)
  hist(samplemean)
}
