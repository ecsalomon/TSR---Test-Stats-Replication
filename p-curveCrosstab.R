################################################################################
# This performs some additional data coding and produces a new table based on  #
# categorizing the p-curve results as significant (p < .10 for a directional   #
# hypothesis test) or not and looking at the crosstab of this dichotimized     #
# variable with replication succes.                                            #
# The code assumes that the processMLdata.R (or processMLdata_blog.R) file has #
# been run to generate the final data set.                                     #
################################################################################

# recode p-curve metrics
final$evSig <- FALSE
final[!is.na(final$p_evidence) & final$p_evidence < .10, ]$evSig <- TRUE
final$levSig <- FALSE
final[!is.na(final$p_lack) & final$p_lack < .10, ]$levSig <- TRUE
final[is.na(final$p_lack), c("evSig", "levSig")] <- NA

table(final$sigRep, final$evSig)
table(final$sigRep, final$levSig)