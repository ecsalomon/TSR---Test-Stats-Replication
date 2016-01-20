################################################################################
# This function reads in the Many Labs 1 summary (data available from          #
# https://osf.io/dmf62/ (Version 1)) and calculates 3 values for each effect   #
# in the data set:                                                             #
#      1. sigRep:  logical indicating whether the overall replication effect   #
#                  was significant                                             #
#      2. esDiff:  numberic indicating difference in ds between unweighted     #
#                  replication and original effect sizes (in Cohen's d units)  #
#      3. wesDiff: numberic indicating difference in ds between weighted       #
#                  replication and original effect sizes (in Cohen's d units)  #
#                                                                              #
# It then returns a data frame containing the names of the effects and the     #
# three new variables.                                                         #
#                                                                              #
# Required packages:                                                           #
#      xlsx: This package requires the Java 6 run time environment be          #
#            installed (for OS X:                                              #
#            https://support.apple.com/kb/DL1572?locale=en_US).                #
#            Attempting to load the package without installing the correct     #
#            Java version may cause the R session to crash.                    # 
################################################################################


inputML1results <- function() {
  # load packages and functions
  suppressPackageStartupMessages(require(xlsx))
  source("splitCI.R")
  
  # Read in the effect size data from the Many Labs 1 summary tables.
  df <- read.xlsx("Raw data/ML1/ML-_Summary_Statistics.xlsx",
                  sheetName = "Table 2",
                  rowIndex = c(3:18),
                  header = FALSE,
                  stringsAsFactors = FALSE)
  
  names(df) <- c("effect", "origES", "original95CI", "medRepES", "repES", 
                 "rep99CI", "wRepES", "wRep99CI", "propOppDir", "propSameDir", 
                 "propNS", "testStat","df1", "n", "p")
  
  # split original CIs intro two numeric columns
  origCI <- splitCI(df$original95CI)
  names(origCI) <- c("origCIlow", "origCIhi")
  
  # split replication CIs into two numeric columns
  repCI <- splitCI(df$rep99CI)
  names(repCI) <- c("repCIlow", "repCIhi")
  
  # split weighted replication CIs into two numeric columns
  wRepCI <- splitCI(df$wRep99CI)
  names(wRepCI) <- c("wRepCIlow", "wRepCIhi")
  
  # orginal ESs and final p-values are stored as factors; convert to numeric
  replaceNA <- function(x) {if (x == "na") {NA} else {as.numeric(x)}}
  df[, "origES"] <- sapply(df[, "origES"], replaceNA)
  df[, "p"] <- as.numeric(gsub("<", "", df[, "p"]))
  
  # split test statistics into two columns, one w type of test, one w stat
  testStats <- strsplit(df[, "testStat"], "=", TRUE) # split at =
  testStats <- matrix(unlist(testStats), nrow = length(testStats), byrow = TRUE)
  testStats[testStats[, 1] == "X^2", 1] <- "chi2" # change name of chi2 stat
  testStats <- data.frame(testStats) # convert to data frame
  names(testStats) <- c("statType", "keyStat") # name the columns
  testStats[, 2] <- as.numeric(as.character(testStats[, 2])) # coerce to numeric
  
  # create some columns to match with ML3 data frame
  df[, "ESstat"] <- "d" # all effect sizes are cohen's d
  df[, "df2"] <- NA # all effect sizes are cohen's d
  df[, "ML"] <- 1 # all effect sizes are cohen's d
  
  # create a new data frame that contains the columns we want to keep
  df2 <- cbind(df[, c("effect", "ESstat", "origES")], origCI,
               df[, c("medRepES", "repES")], repCI, df[, "wRepES"], wRepCI,
               df[, c("propOppDir", "propSameDir", "propNS")], testStats,
               df[, c("df1", "df2", "n", "p", "ML")])
  
  # fix pesky name
  colnames(df2)[10] <- "wRepES"
  
  return(df2)
}

