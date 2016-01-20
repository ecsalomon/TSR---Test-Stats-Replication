################################################################################
# This function takes as input a vector and checks if the first element is NA  #
# If so, it returns a vector of 2 NAs. If not, it returns the original vector. #
################################################################################

doubleNA <- function(x) {
  if(is.na(x[1])) {
    c(NA, NA)
  } else {
    x
  }
}