#' My sample function
#'
#' Shows frequencies of each factor in a sample
#'
#' Creates one barplot per iteration showing how often the factors from 1 to 10 appear in a sample of size n
#'
#' @param n size of the sample
#' @param iter number of samples taken
#' @param time amount of time R suspends its execution of R expressions
#'
#' @return a barplot of each factor's frequency in the sample
#' @export
#'
#' @examples mysample(n=1000, iter=10, time=1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    s=sample(1:10,n,replace=TRUE)
    sf=factor(s,levels=1:10)
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    Sys.sleep(time)
  }
}
