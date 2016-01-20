source("inputML1results.R")
source("inputML3results.R")
mlData <- rbind(inputML1results(), inputML3results(unix = TRUE))

names(inputML1results())
names(inputML3results(unix = TRUE))


# create sigRep
mlData$sigRep <- mlData[, "p"] < .05

# create  esDiff and wesDiff
mlData$esDiff <- mlData$repES - mlData$origES
mlData$wesDiff <- mlData$wRepES - mlData$origES

# Four of the replication effects come from the same paper. Average their ESs  
# to create a single unweighted and weighted replication ES for that paper
