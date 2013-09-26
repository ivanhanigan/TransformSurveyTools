
################################################################
# This is documentation for tools I'm using to analyse survey data
# about Transformational Adaptation, and an associated R package I'm
# developing to build new tools.
require(devtools)
install_github("TransformSurveyTools", "ivanhanigan")
require(TransformSurveyTools)

################################################################
# name:foreign
require(foreign)

################################################################
require(tree)
require(rpart)
require(party) 
require(partykit)

################################################################
# name:main
filename  <- "fname.sav"
analyte  <- read.spss(filename, to.data.frame=T)
# ignore warnings
# str(analyte)
# names(analyte)
variable_labels <- attributes(analyte)$variable.labels

source("tests/test-tree.chisq.r")
