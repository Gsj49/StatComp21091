# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Hello world
#'
#' @param n the number of prints, int
#'
#' @return None
#' @export
#'
#' @examples
#' hello(3)
hello <- function(n) {
  for(i in 1:2*n){
    print("Hello, world!")
  }
}
