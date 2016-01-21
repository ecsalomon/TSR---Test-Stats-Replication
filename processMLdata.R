################################################################################
# This file builds a data frame containing the values from Table 3 in the      #
# file "Many Labs 3 Manuscript Tables.pdf" (avail from https://osf.io/7zp9t/). #
# Original results are based on Version 3 of this file (2015-09-27 02:34 PM).  #
#                                                                              #
# Required packages: plyr, xlsx, reshape, ggplot2, aod, psych                  #
#                                                                              #
# ***WARNING***                                                                #
# This function relies on another function (inputML3results) that is designed  #
# to run on Unix-based systems, specifically OSX 10.10.5. Mac users will need  #
# to install the pdftotext program (available from the poppler package (run    #
# 'sudo port install poppler' at the Terminal). USERS OF OTHER SYSTEMS may     #
# experience errors. The data frame this function creates can be read in       #
# directly by setting unix to FALSE, if the file finaltable.txt was downloaded #
# from Github into the /Raw data/ML3/ folder.                                  #
################################################################################

#load required packages and functions
library(plyr)
library(reshape)
library(ggplot2)
library(aod)
library(psych)
source("inputML1results.R")
source("inputML3results.R")

# import data from many labs 1 and 3
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
mlReduce[, c(3:15, 17:18, 20:22, 24:26)] <- sapply(mlReduce[, c(3:15, 17:18,
                                                                20:22, 24:26)],
                                                   as.numeric)


# import article data
articleData <- read.csv("Raw\ data/metricsDataEntry/dataEntrySheet.csv",
                        fileEncoding="latin1")

# export text file for p-checker input
articleData$pchecker <- NA
articleData[articleData$statType != "F",]$pchecker <-
  with(articleData[articleData$statType != "F",], paste(effect, ": ", statType,
                                                        "(", df1, ") = ",
                                                        statistic, sep = ""))
articleData[articleData$statType == "F",]$pchecker <-
  with(articleData[articleData$statType == "F",], paste(effect, ": ", statType,
                                                        "(", df1, ", ", df2,
                                                        ") = ", statistic,
                                                        sep = ""))
articleData[articleData$statType == "chi2",]$pchecker <-
  with(articleData[articleData$statType == "chi2",], paste(effect, ": ",
                                                           statType, "(", df1,
                                                           ", ", N, ") = ",
                                                           statistic, sep = ""))
# a chi square value equal to N will cause p-checker to crash when it calculates
# effect sizes, so we won't include N for one of the chi square tests. This
# will only affect the correlation between N and Effect Size.
articleData$pchecker <- gsub(", 86", "", articleData$pchecker)
pcheckerInput <- articleData$pchecker[-grep("NA", articleData$pchecker)]
write.table(pcheckerInput, file = "processedData/pcheckerInput.txt",
            quote = FALSE, sep = "", eol = "\n", na = "NA", dec = ".",
            row.names = FALSE, col.names = FALSE)
# run the p-checker analyses using the output in the text file

# import p-checker data
pcurve <- read.csv("Raw\ data/pchecker/p_curve_results.csv")
rindex <- read.csv("Raw\ data/pchecker/rindex_results.csv")
tiva <- read.csv("Raw\ data/pchecker/tiva_results.csv")
effectSize <- read.csv("Raw\ data/pchecker/effect_size.csv")
articleData <- merge(articleData, effectSize, all = TRUE,
                     by.x = c("effect", "statistic", "df1"),
                     by.y = c("paper_id", "statistic", "df1"))

# create mean, median, max, and min N variables
nstats <- ddply(articleData[, c("effect", "N", "d")], .(effect), summarize,
                meanN = mean(N, na.rm = TRUE),
                medianN = median(N, na.rm = TRUE),
                maxN = max(N, na.rm = TRUE),
                minN = min(N, na.rm = TRUE),
                rangeN = max(N, na.rm = TRUE) - min(N, na.rm = TRUE),
                corESn = cor(d, N, use = "pairwise.complete.obs"))

# Schimmack (2014) defines the R-Index as: 
# Percentage of Significant Results – Median (Estimated Power)
# This is not the same value as returned by p-checker, so recalc the R-index
rindex$rIndex <- rindex$success_rate - rindex$median.obs.pow

# merge the data sets
final <- merge(mlReduce, pcurve, all = TRUE, by.x = "effect", by.y = "paper_id")
final <- merge(final, rindex, all = TRUE, by.x = "effect", by.y = "paper_id")
final <- merge(final, tiva, all = TRUE, by.x = "effect", by.y = "paper_id")
final <- merge(final, nstats, all = TRUE, by = "effect")

# convert sigRep to a binary outcome variable
final[final$sigRep == "TRUE", ]$sigRep <- 1
final[final$sigRep == "FALSE", ]$sigRep <- 0
final$sigRep <- as.numeric(final$sigRep)

# report correlations
corVars <- c("Z_evidence", "Z_lack", "r_index", "var.z", "corESn", "medianN")
cor(final[, c("esDiff", corVars)], use = "pairwise.complete.obs")


# run linear models with esDiff as the outcome variable
lm1 <- lm(esDiff ~ Z_evidence + r_index + var.z + corESn + medianN, final)
lm.pcurve1 <- lm(esDiff ~ Z_evidence, final)
lm.pcurve2 <- lm(esDiff ~ Z_lack, final)
lm.rindex <- lm(esDiff ~ r_index, final)
lm.tiva <- lm(esDiff ~ var.z, final)
lm.corESn <- lm(esDiff ~ corESn, final)
lm.medianN <- lm(esDiff ~ medianN, final)

# run logit models with sigRep as outcome
logit1 <- glm(sigRep ~ Z_evidence + r_index + var.z + corESn + medianN,
              final, family = "binomial")
logit.pcurve1 <- glm(sigRep ~ Z_evidence, final, family = "binomial")
logit.pcurve2 <- glm(sigRep ~ Z_lack, final, family = "binomial")
logit.rindex <- glm(sigRep ~ r_index, final, family = "binomial")
logit.tiva <- glm(sigRep ~ var.z, final, family = "binomial")
logit.corESn <- glm(sigRep ~ corESn, final, family = "binomial")
logit.medianN <- glm(sigRep ~ medianN, final, family = "binomial")


# make some pretty figures
# melt the data with esDiff as the outcome
long1 <- melt(final[, c("effect", "esDiff", corVars)],
              id.vars = c("effect", "esDiff"),
              variable_name = "metric")
levels(long1$metric) <- c("P-Curve Evidential Value",
                          "P-Curve Lacks Evidential Value",
                          "R-Index", "Variance of Z",
                          "Correlation between Effect Size and N",
                          "Median N")

diffPlot <- ggplot(long1[complete.cases(long1),], aes(x = value, y = esDiff)) +
  geom_point(aes(colour = effect)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap( ~ metric, scale = "free_x") +
  scale_y_continuous("Difference in Effect Size", limits = c(-1.28, .03),
                     breaks = c(-1.25, -1, -.75, -.5, -.25, 0))


# melt the data with sigRep as the outcome
long2 <- melt(final[, c("effect", "sigRep", corVars)],
              id.vars = c("effect", "sigRep"),
              variable_name = "metric")
levels(long2$metric) <- c("P-Curve Evidential Value",
                          "P-Curve Lacks Evidential Value",
                          "R-Index", "Variance of Z",
                          "Correlation between Effect Size and N",
                          "Median N")


probPlot <- ggplot(long2[complete.cases(long2),], aes(x = value, y = sigRep)) +
  geom_point(aes(colour = effect)) +
  geom_smooth(method="glm", family="binomial", se = FALSE, colour = "black") +
  facet_wrap( ~ metric, scale = "free_x") +
  scale_y_continuous("Predicted Probability of Successful Replication",
                     limits = c(0, 1))


