################################################################################
# This function builds the appropriate data set for Many Labs 3 from the       #
# values in "ManyLabs3 Tables.pdf" (available from https://osf.io/qpwf2/       #
# (Version 1)), consisting of the effect names and three new variables:        #
#      1. sigRep:  logical indicating whether the overall replication effect   #
#                  was significant                                             #
#      2. esDiff:  numberic indicating difference in ds between unweighted     #
#                  replication and original effect sizes (in Cohen's d units)  #
#      3. wesDiff: numberic indicating difference in ds between weighted       #
#                  replication and original effect sizes (in Cohen's d units)  #
#                                                                              #
# It then returns a data frame containing the names of the effects and the     #
# three new variables.                                                         #
################################################################################

processML3 <- function() {
  effect <- c("Stroop", "Metaphoric Restructuring", "Availability Heuristic",
              "Persistence and Conscientiousness", "Power and Perspective",
              "Weight Embodiment", "Warmth Perceptions", 
              "Elaboration Likelihood", "Self-Esteem and Subjective Distance",
              "Credentials and Prejudice")
  
}