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


processML1 <- function() {
  # load package
  suppressPackageStartupMessages(require(xlsx))
  
  # Read in the effect size data from the Many Labs 1 summary tables.
  ml1 <- read.xlsx("Raw data/ML1/ML-_Summary_Statistics.xlsx",
                   sheetName = "Table 2",
                   rowIndex = c(3:18),
                   header = FALSE)
  
  names(ml1) <- c("effect", "originalES", "original95CI", "medRepES", "repES", 
                  "rep99CI", "wRepES", "wRep99CI", "propNegSig", "propPosSig", 
                  "propNS", "testStat","df","N","p")
  
  # create sigRep
  ml1$sigRep[ml1$p == "<.001"] <- TRUE
  ml1$sigRep[ml1$p != "<.001"] <- FALSE
  
  # orginal ESs are stored as a factor. convert to numeric
  for (i in 1:nrow(ml1)) {
    if (ml1$originalES[i] == "na"){
      ml1$numES[i] <- NA
    }
    else {
      ml1$numES[i] <- as.numeric(levels(ml1$originalES)[as.numeric(ml1$originalES[i])])
    }
  }
  
  colKeep <- c("effect", "repES", "wRepES", "testStat", "sigRep", "numES")
  
  # Four of the replication effects come from the same paper. Average their ESs  
  # to create a single unweighted and weighted replication ES for that paper
  ml1collapsed <- ml1[colKeep]
  ml1anchoring <- ml1collapsed[grep("^Anchoring.*", ml1collapsed$effect), ]
  ml1collapsed <- ml1collapsed[grep("^Anchoring.*", ml1collapsed$effect, 
                                      invert = TRUE), ]
  levels(ml1collapsed$effect) <- c(levels(ml1collapsed$effect), "Anchoring")
  
  anchoringRow <- list("Anchoring", 
                    mean(ml1anchoring$repES),
                    mean(ml1anchoring$wRepES),
                    NA,
                    mean(ml1anchoring$sigRep),
                    mean(ml1anchoring$numES))
  
  ml1collapsed <- rbind(anchoringRow, ml1collapsed)
  
  # create  esDiff and wesDiff
  ml1collapsed$esDiff <- ml1collapsed$repES - ml1collapsed$numES
  ml1collapsed$wesDiff <- ml1collapsed$wRepES - ml1collapsed$numES
  
  returncols <- c("effect", "sigRep", "esDiff", "wesDiff")
  
  return(ml1collapsed[returncols])
}

