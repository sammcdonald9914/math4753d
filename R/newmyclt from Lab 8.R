#' My Central Limit Theorem function
#'
#' Plots a histogram of sample means
#'
#' Makes iter random samples of size n of values between 0 and 5 and plots a histogram of the sample means.
#'
#' @param n size of each sample
#' @param iter number of samples taken
#'
#' @return a histogram of the sample means
#' @export
#'
#' @examples new.myclt(n=10,iter=10000)
new.myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  mean=apply(data,2,mean) #C
  hist(mean)
}
