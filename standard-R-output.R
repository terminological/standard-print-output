library(devtools)
library(roxygen2)

# create_package("standardPrintOutput")
document()
if("package:standardPrintOutput" %in% search()) detach("package:standardPrintOutput", unload=TRUE, character.only = TRUE)
devtools::install()
library(standardPrintOutput)