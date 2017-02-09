library(sna)
setwd("./QAP")

# Read in the cosponsorship data (108th Senate)
senlist <- dget("sennet.txt")
# Extract the adjacency matrix
SenNet <- senlist$net
# Read in vertex-level ideology data
dwnom <- read.csv("dwnom.csv")

# The covariate-creation operations that follow will be applied 
# to each column in dwnom--each of two network covariates

# Create Distance Matrix for homophily
ideoDist <- as.matrix(dist(dwnom[,1]))

# Create Sender covariate
# element ij is i's value
ideoSend <- matrix(dwnom[,1],nrow(dwnom),nrow(dwnom),byrow=F)

# Create Receiver covariate
# element ij is j's value
ideoRec <- matrix(dwnom[,1],nrow(dwnom),nrow(dwnom),byrow=T)

# Create Distance Matrix for homophily using the second dimension of DW-Nominate
ideoDist2 <- as.matrix(dist(dwnom[,2]))

# Create Sender covariate
ideoSend2 <- matrix(dwnom[,2],nrow(dwnom),nrow(dwnom),byrow=F)

# Create Receiver covariate
ideoRec2 <- matrix(dwnom[,2],nrow(dwnom),nrow(dwnom),byrow=T)

# Create the 'graph stack' of covariates
#
covariates <- list(ideoDist=ideoDist,ideoSend=ideoSend,ideoRec=ideoRec,
	ideoDist2=ideoDist2,ideoSend2=ideoSend2,ideoRec2=ideoRec2)

# Run QAP for contunious outcomes (netlogit is for dichotomous ties)
# First OLS (nullhyp="classical")
# Second QAP
ols <- netlm(SenNet,covariates,nullhyp="classical")

set.seed(5)
qap <- netlm(SenNet,covariates,nullhyp="qap",reps=100)

resultsOLS <- cbind(summary(ols)[[1]],summary(ols)[[9]],summary(ols)[[10]],summary(ols)[[11]])

resultsQAP <- cbind(summary(qap)[[1]],summary(qap)[[10]],summary(qap)[[11]],summary(qap)[[12]])

resultsALL <- cbind(summary(qap)[[1]],summary(ols)[[11]],summary(qap)[[12]])
