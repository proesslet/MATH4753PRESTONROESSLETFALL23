#' myBoot function
#'
#' @param iter - iterations
#' @param x - data
#' @param fun - mean
#' @param alpha - 0.05
#' @param ... - ...
#' @importFrom graphics hist
#' @importFrom graphics abline
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' myboot2(x=(MATH4753PRESTONROESSLETFALL23::fire)$DAMAGE)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,...){

  n=length(x)

  y=sample(x,n*iter,replace=TRUE)

  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,main="Histogram of Bootstrap sample statistics",...)

  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=1.5)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=1.5)

  text(pte,max(para$density)/2,round(pte,2),cex=1.5)

  # return(list(ci=ci, fun=fun, x=x, xstat=xstat))
}
