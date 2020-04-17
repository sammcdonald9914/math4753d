#' My function for plotting normal distributions
#'
#' Plots a normal distribution and shades an area
#'
#' Calculates the probability of the shaded area on the normal distribution
#'
#' @param mu mean of the distribution
#' @param sigma standard deviation of the distribution
#' @param a value for x
#'
#' @return a graph of the normal distribution and the probability of x<a
#' @export
#'
#' @examples myncurve(mu=10,sigma=5,a=6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xpoly=seq(mu-3*sigma,a, length=1000)
  ypoly=dnorm(xpoly, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma,xpoly,a), c(0,ypoly,0), col="Red")

  prob.a=pnorm(a, mean=mu, sd=sigma)
  prob.a=round(prob.a,4)
  prob.a

}
