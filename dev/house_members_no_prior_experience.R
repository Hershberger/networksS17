library(data.table)

# load LEP data for basis of variable
LEP_house_93_110 <- readstata13::read.dta13("inst/extdata/LEPData93to110Congresses.dta")
setDT(LEP_house_93_110)
LEP_house_93_110 <- LEP_house_93_110[is.na(icpsr) == FALSE, ]

# first measure is for those with state legislative experience
LEP_house_93_110[, prior_exp := 0]
LEP_house_93_110[state_leg == 1, prior_exp := 1]

# correct values
LEP_house_93_110[icpsr == 15063, thomas_name := "Smith, Robert"]
LEP_house_93_110[icpsr == 15245, state_leg := 1]
LEP_house_93_110[icpsr == 29350, thomas_name := "Buyer, Stephen"]
LEP_house_93_110[icpsr == 29514, thomas_name := "Chenoweth, Helen"]
LEP_house_93_110[icpsr == 15442, thomas_name := "Sawyer, Thomas"]
LEP_house_93_110[icpsr == 29576, thomas_name := "Davis, Thomas"]
LEP_house_93_110[icpsr == 14657, thomas_name := "Sensenbrenner, James"]
LEP_house_93_110[icpsr == 15019, state_leg := 0]
LEP_house_93_110[icpsr == 20343, thomas_name := "Ryan, Tim"]
LEP_house_93_110[icpsr == 20349, thomas_name := "Herseth, Stephanie"]

house_state_leg <- LEP_house_93_110[, .(icpsr, thomas_name, congress, state_leg)]

# # code to diagnose mistakes, correct above if necessary
# house_state_leg <- house_state_leg[, .N, .(icpsr, thomas_name, state_leg)]
# house_state_leg[duplicated(icpsr) == TRUE, ]


save(house_state_leg, file = "prelim/house_state_leg.RData")

# second is for those with state legislative or prior house experience
LEP_house_93_110[seniority == 1, freshman_congress := congress]
LEP_house_93_110[, min_congress := min(congress), .(icpsr)]
LEP_house_93_110 <- LEP_house_93_110[is.na(seniority) == FALSE, ]
LEP_house_93_110[min_congress == 93, min_congress := -seniority + 94]
LEP_house_93_110[, seniority := seniority - 1]
LEP_house_93_110$expected_congress <- LEP_house_93_110$min_congress + LEP_house_93_110$seniority
LEP_house_93_110[congress != expected_congress, prior_exp := 1]

house_prior_exp <- LEP_house_93_110[, .(icpsr, thomas_name, congress,
  state_leg, prior_exp)]

save(house_prior_exp, file = "prelim/house_prior_exp.RData")
