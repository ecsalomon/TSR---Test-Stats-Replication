# import data from many labs 1 and 3
source("inputML1results.R")
source("inputML3results.R")
mlData <- rbind(inputML1results(), inputML3results(unix = TRUE))

# create sigRep
mlData$sigRep <- mlData[, "p"] < .05

# Not all of the original effects were converted to d because some of the
# designs/reports do not permit this conversion. They won't be able to be used
mlData$origD <- mlData$origES
# test stat & n taken from Cacioppo, J. T., Petty, R. E., & Morris, K. J. (1983)
mlData[mlData[, "ESstat"] != "d", ]$origD <- NA

# create  esDiff and wesDiff
mlData$esDiff <- mlData$repES - mlData$origD
mlData$wesDiff <- mlData$wRepES - mlData$origD

# Four of the replication effects come from the same paper (anchoring). Average
# their stats to create composite
anchoring <- mlData[grep("Anchoring", mlData$effect), ]
mlReduce <- mlData[-grep("Anchoring", mlData$effect), ]
mlReduce[nrow(mlReduce) + 1, ] <- c("Anchoring", "d",
                                    apply(anchoring[, c(3:15)], 2, mean), "t",
                                    apply(anchoring[, c(17:18)], 2, mean), NA, 
                                    apply(anchoring[, c(20:22)], 2, mean),
                                    TRUE,
                                    apply(anchoring[, c(24:26)], 2, mean))

# import article data
articleData <- read.csv("Raw\ data/metricsDataEntry/dataEntrySheet.csv",
                        fileEncoding="latin1")

# export text file for p-checker input
articleData$pchecker <- NA
articleData[articleData$statType != "F",]$pchecker <- with(articleData[articleData$statType != "F",],
                                                           paste(effect, ": ",
                                                                 statType, "(",
                                                                 df1, ") = ",
                                                                 statistic,
                                                                 sep = ""))
articleData[articleData$statType == "F",]$pchecker <- with(articleData[articleData$statType == "F",],
                                                           paste(effect, ": ",
                                                                 statType, "(",
                                                                 df1, ", ", df2,
                                                                 ") = ",
                                                                 statistic,
                                                                 sep = ""))
pcheckerInput <- articleData$pchecker[-grep("NA", articleData$pchecker)]
write.table(pcheckerInput, file = "processedData/pcheckerInput.txt",
            quote = FALSE, sep = "", eol = "\n", na = "NA", dec = ".",
            row.names = FALSE, col.names = FALSE)