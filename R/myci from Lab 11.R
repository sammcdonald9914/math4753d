#' My confidence interval function for means
#'
#' Calculates the confidence interval for a population mean from a sample
#'
#' Can calculate the interval for any confidence level
#'
#' @param x a vector containing sample data
#' @param alpha positive difference between 1 and the desired confidence level
#'
#' @return a confidence interval for the population mean with the desired confidence level
#' @export
#'
#' @examples myci(x=rnorm(30,mean=10,sd=12),alpha=.05)
myci=function(x,alpha){
  mp=c(-1,1)
  mean(x)+mp*qt(1-alpha/2,length(x)-1)*sd(x)/sqrt(length(x))
}
