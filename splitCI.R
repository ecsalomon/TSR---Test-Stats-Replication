################################################################################
# This function takes as input a string vector containing the lower and upper  #
# bounds of a confidence interval, separated by a comma. It returns a data     #
# frame containing two columns:                                                #
#      1. ciLow:   the lower bound of the condidence interval (numeric)        #
#      2. ciHigh:  the upper bound of the confidence interval (numeric)        #
################################################################################

splitCI <- function(ci) {
  source("doubleNA.R") # load function to create 2 NA elements for missing CI
  splitCI <- strsplit(ci, ",", TRUE) # split the CI at the comma
  splitCI <- lapply(splitCI, doubleNA) # double NAs where needed
  splitCI <- lapply(splitCI, gsub, pattern=" ", replacement="") # remove spaces
  # turn it into a matrix
  splitCI <- (matrix(unlist(splitCI), nrow = length(splitCI), byrow = TRUE))
  class(splitCI) <- "numeric" # coerce to numeric
  splitCI <- data.frame(splitCI) # convert to data frame
  names(splitCI) <- c("ciLow", "ciHigh") # name the columns
  return(splitCI)
}

