#' My function of squares and cubes
#'
#' Produce list of squares and cubes
#'
#' This is a part of the introduction to package making
#'
#' @param x a vector
#'
#' @return a list of vectors
#' @export
#'
#' @examples myf(5)
myf=function(x){
  obj1=x^2
  obj2=x^3
  list(square=obj1,cube=obj2)
}
