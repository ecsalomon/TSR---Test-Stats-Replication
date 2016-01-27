################################################################################
# This file builds a data frame containing data on effects and results from    #
# Many Labs 1 and 3 and conducts several analyses designed to answer the       #
# question of whether replication outcomes can be predicted on the basis of    #
# statistics that summarize the original articles reporting the replicated     #
# effects.                                                                     #
#                                                                              #
# Required packages: plyr, xlsx, reshape, ggplot2                              #
#                                                                              #
# ***WARNING***                                                                #
# This code relies on a function (inputML3results) that is designed to run on  #
# Unix-based systems, specifically OSX 10.10.5. Mac users will need to install #
# the pdftotext program (available from the poppler package (run 'sudo port    #
# install poppler' at the Terminal). USERS OF OTHER SYSTEMS may experience     #
# errors. The data frame this function creates can be read in directly by      #
# setting unix to FALSE, if the file finaltable.txt was downloaded  from       #
# Github into the /Raw data/ML3/ folder.                                       #
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
# designs/reports do not permit this conversion. They won't be able to be used.
# However, these effect sizes in other metrics are in the origES column. Create
# a new column containing only original ESs in Cohen's d.
mlData$origD <- mlData$origES
mlData[mlData[, "ESstat"] != "d", ]$origD <- NA

# create  difference between original and replication outcomes
mlData$esDiff <- mlData$repES - mlData$origD # diff from meta-analytic d
mlData$wesDiff <- mlData$wRepES - mlData$origD # diff from weighted avg d

# Four of the replication effects come from the same paper (anchoring). Average
# their stats to create a single composite
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

# import hand-coded article data, containing hypothesis-critical test info for
# each study in multi-study papers
articleData <- read.csv("Raw\ data/metricsDataEntry/dataEntrySheet.csv",
                        fileEncoding="latin1")

# export text file of test stats for p-checker input
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
# will only affect the correlation between N and Effect Size, none of the other
# metrics.
articleData$pchecker <- gsub(", 86", "", articleData$pchecker)
pcheckerInput <- articleData$pchecker[-grep("NA", articleData$pchecker)]
write.table(pcheckerInput, file = "processedData/pcheckerInput.txt",
            quote = FALSE, sep = "", eol = "\n", na = "NA", dec = ".",
            row.names = FALSE, col.names = FALSE)
################################################################################
# To fully reproduce these analyses, run the p-checker analyses using the      #
# output saved in processedData/pcheckerInput.txt and turn on "Group results   #
# by paper". The original analyses were done with p-checker version 0.40       #
# (http://shinyapps.org/apps/p-checker/) on 2016-1-20                          #
################################################################################

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

# merge the data sets
final <- merge(mlReduce, pcurve, all = TRUE, by.x = "effect", by.y = "paper_id")
final <- merge(final, rindex, all = TRUE, by.x = "effect", by.y = "paper_id")
final <- merge(final, tiva, all = TRUE, by.x = "effect", by.y = "paper_id")
final <- merge(final, nstats, all = TRUE, by = "effect")

# convert sigRep to a binary outcome variable
final[final$sigRep == "TRUE", ]$sigRep <- 1
final[final$sigRep == "FALSE", ]$sigRep <- 0
final$sigRep <- as.numeric(final$sigRep)

# take the absolute value of the effect size difference
final$absDiff <- abs(final$wesDiff)

# report correlations
corVars <- c("Z_evidence", "Z_lack", "r_index", "var.z", "corESn", "medianN")
absCorTable <- cor(final[, c("absDiff", corVars)], use = "pairwise.complete.obs")
write.csv(absCorTable, "processedData/absCorTable.csv")

for (i in 1:length(corVars)) {
  print(paste("Correlation Test for", corVars[i]))
  print(cor.test(final[, "wesDiff"], final[, corVars[i]]))
}

# run linear models with esDiff as the outcome variable
lm1 <- lm(wesDiff ~ Z_evidence + r_index + var.z + corESn + medianN, final)
lm.pcurve1 <- lm(wesDiff ~ Z_evidence, final)
lm.pcurve2 <- lm(wesDiff ~ Z_lack, final)
lm.rindex <- lm(wesDiff ~ r_index, final)
lm.tiva <- lm(wesDiff ~ var.z, final)
lm.corESn <- lm(wesDiff ~ corESn, final)
lm.medianN <- lm(wesDiff ~ medianN, final)



# run logit models with sigRep as outcome
logit1 <- glm(sigRep ~ Z_evidence + r_index + var.z + corESn + medianN,
              final, family = "binomial")
logit.pcurve1 <- glm(sigRep ~ Z_evidence, final, family = "binomial")
logit.pcurve2 <- glm(sigRep ~ Z_lack, final, family = "binomial")
logit.rindex <- glm(sigRep ~ r_index, final, family = "binomial")
logit.tiva <- glm(sigRep ~ var.z, final, family = "binomial")
logit.corESn <- glm(sigRep ~ corESn, final, family = "binomial")
logit.medianN <- glm(sigRep ~ medianN, final, family = "binomial")

# logit results
summary(logit.pcurve1)
summary(logit.pcurve2)
summary(logit.rindex)
summary(logit.tiva)
summary(logit.corESn)
summary(logit.medianN)

# odds ratios
exp(coef(logit.pcurve1))
exp(coef(logit.pcurve2))
exp(coef(logit.rindex))
exp(coef(logit.tiva))
exp(coef(logit.corESn))
exp(coef(logit.medianN))

# make some pretty figures
# melt the data with wesDiff as the outcome
long1 <- melt(final[, c("effect", "wesDiff", corVars)],
              id.vars = c("effect", "wesDiff"),
              variable_name = "metric")

# facet labels
levels(long1$metric) <- c("P-Curve:\nEvidential Value",
                          "P-Curve:\nLacks Evidential Value",
                          "R-Index", "TIVA:\nVariance of Z",
                          "Correlation Between\nEffect Size and N",
                          "N-Pact Factor:\nMedian N")

# set up color palette 
colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c",
            "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#dddd77", "#b15928",
            "#000000", "#969696", "#8dd3c7", "#01665e", "#8dd3c7")




# plot predicting diff in ES
diffPlot <- ggplot(long1[complete.cases(long1),],
                   aes(x = value, y = abs(wesDiff))) +
  geom_point(aes(colour = effect)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap( ~ metric, scale = "free_x") +
  scale_y_continuous("Absolute Value of Difference in Effect Size") +
  scale_x_continuous("") +
  labs(title = "Predicting Differences Between\nReplication and Original Effect Sizes (d)") +
  scale_colour_manual(values = colors,
                      guide = guide_legend(title = "Effect", ncol = 2)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "#666666"),
        plot.title = element_text( vjust=1),
        panel.margin = grid::unit(1, "lines"),
        legend.position = "bottom")

# save that plot!
ggsave(filename = "figure1.png", plot = diffPlot, width = 5.5,
       units = "in", dpi = 300)


# melt the data with sigRep as the outcome
long2 <- melt(final[, c("effect", "sigRep", corVars)],
              id.vars = c("effect", "sigRep"),
              variable_name = "metric")
# facet labels
levels(long2$metric) <- c("P-Curve:\nEvidential Value",
                          "P-Curve:\nLacks Evidential Value",
                          "R-Index", "TIVA:\nVariance of Z",
                          "Correlation between\nEffect Size and N",
                          "N-Pact Factor:\nMedian N")

# plot predicted probability of success
probPlot <- ggplot(long2[complete.cases(long2),], aes(x = value, y = sigRep)) +
  geom_point(aes(colour = effect), shape = 1,
             position = position_jitter(height = 0, width = .02)) +
  geom_smooth(method="glm", family="binomial", se = FALSE, colour = "black") +
  facet_wrap( ~ metric, scale = "free_x") +
  scale_y_continuous("", limits = c(0, 1))+
  scale_x_continuous("") +
  labs(title = "Predicted Probability of Replication Success") +
  scale_colour_manual(values = colors, 
                      guide = guide_legend(title = "Effect", ncol = 2)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "#666666"),
        plot.title = element_text( vjust=1),
        panel.margin = grid::unit(1, "lines"),
        legend.position = "bottom")

# save that plot!
ggsave(filename = "figure2.png", plot = probPlot, width = 5.5,
       units = "in", dpi = 300)


# plot predicting proportion of labs in same direction
long3 <- melt(final[, c("effect", "propSameDir", "sigRep", corVars)],
              id.vars = c("effect", "propSameDir", "sigRep"),
              variable_name = "metric")
long3$overall <- factor(long3$sigRep,
                        labels = c("Non-signficant", "Significant"))

# facet labels
levels(long3$metric) <- c("P-Curve:\nEvidential Value",
                          "P-Curve:\nLacks Evidential Value",
                          "R-Index", "TIVA:\nVariance of Z",
                          "Correlation Between\nEffect Size and N",
                          "N-Pact Factor:\nMedian N")


# plot predicting proportion of labs in same direction
propPlot <- ggplot(long3[complete.cases(long3),], aes(x = value, y = propSameDir)) +
  geom_point(aes(colour = overall), size = 6) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap( ~ metric, scale = "free_x") +
  scale_y_continuous("", limits = c(0, 1)) +
  scale_x_continuous("") +
  labs(title = "Fig 3. Proportion of Labs Returning Significant Result in Same Direction") +
  scale_colour_manual(values = colors,
                      guide = guide_legend(title = "Overall Result")) +
  theme_bw() +
  theme(strip.text = element_text(size = rel(2.4)),
        axis.text = element_text(size = rel(1.6), colour = "#666666"),
        axis.title.y = element_text(angle = 0),
        legend.text = element_text(size = rel(2)),
        plot.title = element_text(size = rel(3), vjust=3),
        panel.margin = grid::unit(1, "lines"),
        legend.title = element_text(size = rel(2)),
        legend.key.size = grid::unit(16, "mm"))

# save that plot!
ggsave(filename = "figure3.png", plot = propPlot, width = 25, height = 15,
       units = "in", dpi = 300)



# export data
write.csv(final, file = "processedData/final.csv") 

# data for before-and-after plot
beforeAfter <-melt(final[, c("effect", "origD", "repES", "sigRep", "Z_hack",
                             "wesDiff")],
                   id.vars = c("effect", "sigRep", "Z_hack", "esDiff"),
                   variable_name = "time")
write.csv(beforeAfter, file = "processedData/beforeAfter.csv")

#export long data
write.csv(long1, file = "processedData/longData.csv", row.names = FALSE) 

