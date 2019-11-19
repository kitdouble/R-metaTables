# metaTables
Development of the metaTables package

To install:

`devtools::install_github("kitdouble/metaTables")`

`library(metaTables)`

## Vignettes

Create a new sub-groups table with all levels of a variable

`table1 <- subGroups(data = mydata,  grouping.factor = "EI_con", es = "yi", vi = "vi", study.id = "study_id", type = "cor")`

Add another set of sub-groups to an existing table

`table1 <- subGroups(append.table = table1, label = "Attachment Style", data = mydata, grouping.factor = "A_dimension", es = "yi", vi = "vi", study.id = "study_id", type = "cor")`


Make a frequency/proportion table

`propTable(data = mydata, variables = c("A_dimension", "EI_con"), k = T, study.id = "study_id", labels = c("Attachment", "EI"))`


Run Meta-regressions with a single moderator

`regTable(data = mydata, moderators = c"A_dimension", es = "yi", vi = "vi", study.id = "study_id", labels = c("Attachment", "EI"))`


Run separate meta-regressions for a series of moderators

`regTable(data = mydata, moderators = c("A_dimension", "EI_con"), es = "yi", vi = "vi", study.id = "study_id", labels = c("Attachment", "EI"))`


## To do

Alow for multiple groiping factors to be inputed into the `subGroups` function at once


Allow for multiple predictors in the meta-regressions for a single moderator

Allow for specified covariates in the meta-regression for sets of moderators
