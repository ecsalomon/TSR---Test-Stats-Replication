################################################################################
# This function builds a data frame containing the values from Table 3 in the  #
# file "Many Labs 3 Manuscript Tables.pdf" (avail from https://osf.io/7zp9t/). #
# Original results are based on Version 3 of this file (2015-09-27 02:34 PM).  #
#                                                                              #
# ***WARNING***                                                                #
# This function is designed to run on Unix-based systems, specifically OSX     #
# 10.10.5. Mac users will need to install the pdftotext program (available     #
# from the poppler package (run 'sudo port install poppler' at the Terminal).  #
# USERS OF OTHER SYSTEMS may experience errors. The data frame this function   #
# creates can be read in directly by setting unix to FALSE, if the file        #
# finaltable.txt was downloaded from Github into the /Raw data/ML3/ folder.    #                                                        #
################################################################################

inputML3results <- function(unix = TRUE) {
  fn <- "Raw\\ data/ML3/Many\\ Labs\\ 3\\ Manuscript\\ Tables"
  
  
  # if user asks to rerun Unix commands, perform steps to convert PDF file
  # to a readable tabular data file, finaltable.txt
  if (unix == TRUE){
    # convert the many labs 3 results table to text
    system(paste("pdftotext -layout -f 1 -l 2 ", fn, ".pdf", sep = ""))
    
    # change missing values to R-readable NAs
    system(paste("sed -i.bak 's/[nN]\\/[aA]/NA/g' ", fn, ".txt", sep = ""))
    system(paste("sed -i.bak 's/ \\{5,8\\}NA/      NA NA/g' ", fn, ".txt",
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
  }
  
  
  # read in the data for the ten planned effects
  df <- read.table("Raw data/ML3/finaltable.txt", skip = 4, nrows = 10,
                   col.names = c("effect", "ESstat", "origES", "origCIlow",
                                 "origCIhi", "medRepES", "repES", "repCIlow",
                                 "repCIhi", "wRepES", "wRepCIlow", "wRepCIhi",
                                 "propOppDir", "propSameDir", "propNS",
                                 "statType", "keyStat", "df1", "df2", "n", "p"))
  
  # give more useful names for the effects
  df[,1] <- c("Stroop", "Metaphoric Restructuring", "Availability Heuristic",
              "Power and perspective-taking", "Weight embodiment",
              "Warmth perceptions", "Elaboration likelihood model",
              "Self-Esteem and subjective distance",
              "Credentials and prejudice", "Persistence and Conscientiousness")
  
  # add column indicating which project the data came from
  df[, "ML"] <- 3
  
  return(df)
  
}