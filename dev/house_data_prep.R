library(networksS17)
library(data.table)

# primary_DT <- gdata::read.xls(
#   "inst/extdata/House_primary_elections_(1956-2010)_data_(pettigrew).xlsx")
# setDT(primary_DT)
# primary_DT <- primary_DT[year <= 2008, ]
#
# primary_DT[, c("raceid", "redist", "law", "candnumber", "prez", "votep",
#   "type", "candvotes", "tvotes", "gender") := NULL]
#
# primary_DT <- primary_DT[inc == 0 & winner == 1, ]
# primary_DT <- primary_DT[fr != 9, ]
# primary_DT <- primary_DT[seat == 1 & party == 1 | seat == 0 & party == 0,]
#
# save(primary_DT, file = "inst/extdata/primary_DT.RData")
load("inst/extdata/primary_DT.RData")

load("inst/extdata/house_covariates.RData")
