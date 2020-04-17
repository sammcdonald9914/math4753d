#' My Bootstrap Function
#'
#' Uses the bootstrap method to make point and interval estimates
#'
#' Creates a histogram showing a point estimate and confidence interval from sample
#'
#' @param iter number of resamples done with replacement from the original sample
#' @param x the original sample from the population
#' @param fun the function applied to each sample to get a statistic
#' @param alpha positive difference between 1 and the desired upper quartile of the confidence interval
#' @param cx amount by which R will scale text and symbols
#' @param ... Additional features for the histogram
#'
#' @return a histogram with a point estimate and confidence interval
#' @export
#'
#' @examples myboot2(x=rnorm(25,mean=25,sd=10),iter=10000,alpha=0.05)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  invisible(list(ci=ci,fun=fun,x=x))
}
