################################################################################
# This function estimates and returns a p-vale from a p-rep value reported in  # 
# a journal article.                                                           #
################################################################################


calcp <- function(x){
  x <- 1/x
  x <- x - 1
  x <- x^(3/2)
  x <- 1/x
  x <- x + 1
  x <- 1/x
  x
}
