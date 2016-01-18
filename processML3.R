################################################################################
# This function builds the appropriate data set for Many Labs 3 from the       #
# values in "ManyLabs3 Tables.pdf" (available from https://osf.io/qpwf2/       #
# (Version 1)), consisting of the effect names and three new variables:        #
#      1. sigRep:  logical indicating whether the overall replication effect   #
#                  was significant                                             #
#      2. esDiff:  numeric indicating difference in ds between unweighted      #
#                  replication and original effect sizes (in Cohen's d units)  #
#      3. wesDiff: numeric indicating difference in ds between weighted        #
#                  replication and original effect sizes (in Cohen's d units)  #
#                                                                              #
# It then returns a data frame containing the names of the effects and the     #
# three new variables.                                                         #
################################################################################

processML3 <- function() {
  fn <- "Raw\\ data/ML3/ManyLabs3\\ Tables"
  
  # convert the many labs 3 results table to text
  system("pdftotext -layout -f 1 -l 2 Raw\\ data/ML3/ManyLabs3\\ Tables.pdf")
  
  # change missing values to R-readable NAs
  system(paste("sed -i.bak 's/[nN]\\/[aA]/NA/g' ", fn, ".txt", sep = ""))
  system(paste("sed -i.bak 's/ \\{6,8\\}NA/      NA NA/g' ", fn, ".txt",
               sep = ""))
  system(paste("sed -i.bak 's/various/    NA/g' ", fn, ".txt", sep = ""))
  
  
  # make sure key statistics are on a single line
  system(paste("sed -i.bak 's/t=//' ", fn, ".txt", sep = ""))
  system(paste("sed -i.bak 's/χ2 =//' ", fn, ".txt", sep = ""))
  system(paste("sed -i.bak 's/PSdep =//' ", fn, ".txt", sep = ""))
  system(paste("sed -i.bak 's/49.795/t = 49.795/' ", fn, ".txt", sep = ""))
  system(paste("sed -i.bak 's/21.90/chi2 = 21.90/' ", fn, ".txt", sep = ""))
  system(paste("sed -i.bak 's/.522/PSdep = .522/' ", fn, ".txt", sep = ""))
  
  
  # separate degrees of freedom into two columns; add NA if only 1 df value
  system(paste("sed -Ei.bak 's/(= [0-9]*.[0-9]+ +[0-9]+ +)/\\1 NA /g' ",
               fn, ".txt", sep = ""))
  
  # remove commas, angle brackets, and equals signs
  system(paste("sed -i.bak 's/[,<=]//g' ", fn, ".txt", sep = ""))
  
  # simplify Stroop effect name
  system(paste("sed -i.bak 's/Stroop Task/Stroop     /' ", fn, ".txt",
               sep = ""))
  
  
  # remove any lines that are just text and/or whitespace (no numbers)
  system(paste("grep '[0-9]' ", fn, ".txt > ", "Raw\\ data/ML3/finaltable.txt",
               sep = ""))
  
  # read in the data for the ten planned effects
  df <- read.table("Raw data/ML3/finaltable.txt", skip = 4, nrows = 10,
                   col.names = c("effect", "ESstat", "origES", "origCIlow",
                                 "origCIhi", "medRepES", "repES", "repCIlow",
                                 "repCIhi", "wtRepES", "wtRepCIlow",
                                 "wtRepCIhi", "propOppDir", "propSameDir",
                                 "propNS", "statType", "keyStat", "df1", "df2",
                                 "n", "p")
                   )
  
}