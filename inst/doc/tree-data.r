################################################################
# name:tree-deviance
# func
require(rpart)
require(partykit) 

# load
civst_gend_sector  <- read.csv(textConnection(
    "civil_status gender activity_sector number_of_cases
         married   male         primary              50
         married   male       secondary              40
         married   male        tertiary               6
         married female         primary               0
         married female       secondary              14
         married female        tertiary              10
          single   male         primary               5
          single   male       secondary               5
          single   male        tertiary              12
          single female         primary              50
          single female       secondary              30
          single female        tertiary              18
divorced/widowed   male         primary               5
divorced/widowed   male       secondary               8
divorced/widowed   male        tertiary              10
divorced/widowed female         primary               6
divorced/widowed female       secondary               2
divorced/widowed female        tertiary               2
"),sep = "")
# save this for use later
dir.create("inst/extdata", recursive=T)
write.csv(civst_gend_sector, "inst/extdata/civst_gend_sector.csv", row.names = F)
# clean
str(civst_gend_sector)

# do
fit <- rpart(civil_status ~ gender + activity_sector,
             data = civst_gend_sector, weights = number_of_cases,
             control=rpart.control(minsplit=1))
# NB need minsplit to be adjusted for weights.
summary(fit)
  
# report
plot(fit, margin=.1)
text(fit, use.n = TRUE)
title("fit")

# nicer plots
png("images/fit1.png", 1000, 480)
plot(as.party(fit))
dev.off()
