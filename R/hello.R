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
