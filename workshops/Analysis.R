## =============================================================================
## Analysis of Krackhardt Advice Network
## =============================================================================

library(ergm) # Primary ERGM functionality
library(arm) # for invlogit() function only
library(xergm) # for interpret() function
setwd('~/Dropbox/writings/Networks Book/Data/KrackhardtManager/') #[SJC]
#setwd("~/Dropbox/Networks Book/Data/KrackhardtManager/") #[JM]
load('Krackhardt.RData')

## Covariate model without network structural parameters.
mod0 <- ergm(Net ~ edges
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             verbose=TRUE)
gof0 <- gof(mod0)
par(mfrow=c(2,2)); plot(gof0)
summary(mod0)

## Model with reciprocity and covariates.
set.seed(5)
mod1 <- ergm(Net ~ edges + mutual
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             control=control.ergm(
                 MCMC.samplesize=50000,
                 MCMC.burnin=100000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod1)
gof1 <- gof(mod1)
par(mfrow=c(2,2)); plot(gof1)
summary(mod1)

## Add out-stars
set.seed(10)
mod2 <- ergm(Net ~ edges + mutual + ostar(2:3)
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             control=control.ergm(
                 MCMC.samplesize=50000,
                 MCMC.burnin=100000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod2)
gof2 <- gof(mod2)
par(mfrow=c(2,2)); plot(gof2)
summary(mod2)

## Add transitivity
set.seed(15)
mod3 <- ergm(Net ~ edges + mutual + ostar(2:3) + transitive
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             control=control.ergm(
                 MCMC.samplesize=50000,
                 MCMC.burnin=100000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod3)
gof3 <- gof(mod3)
par(mfrow=c(2,2)); plot(gof3)
summary(mod3)

## Use GWESP
set.seed(20)
mod4 <- ergm(Net ~ edges + mutual + ostar(2:3) + gwesp(0, fixed=TRUE)
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             control=control.ergm(
                 MCMC.samplesize=50000,
                 MCMC.burnin=100000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod4)
gof4 <- gof(mod4)
par(mfrow=c(2,2)); plot(gof4)
summary(mod4)

## With GWEST, estimate decay parameter (curved exponential family)
set.seed(25)
mod5 <- ergm(Net ~ edges + mutual + ostar(2:3) + gwesp(0, fixed=FALSE)
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             control=control.ergm(
                 MCMC.samplesize=50000,
                 MCMC.burnin=100000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod5)
gof5 <- gof(mod5)
par(mfrow=c(2,2)); plot(gof5)
summary(mod5)


## The very simple model from Chapter 3
set.seed(25)
mod0 <- ergm(Net ~ edges + nodematch("Level"),
             control=control.ergm(
                 MCMC.samplesize=5000,
                 MCMC.burnin=10000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod0)
gof0 <- gof(mod0)
par(mfrow=c(2,2)); plot(gof0)
summary(mod0)

interpret(mod0, type="dyad", i=1, j=2)



## The theoretically more complete model from Chapter 3
set.seed(20)
mod1 <- ergm(Net ~ edges + mutual + ostar(2:3) + gwesp(0, fixed=TRUE)
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure") + absdiff("Tenure")
             + nodeicov("Age") + nodeocov("Age") + absdiff("Age"),
             control=control.ergm(
                 MCMC.samplesize=5000,
                 MCMC.burnin=10000,
                 MCMLE.maxit=10),
             verbose=TRUE)
mcmc.diagnostics(mod1)
gof1 <- gof(mod1)
par(mfrow=c(2,2)); plot(gof1)
gof1
summary(mod1)

dyads <- edgeprob(mod1, parallel = "multicore", ncpus = 2)

## Addapt bigger example from the xergm interpret

# extract coefficients and create null hypothesis vector
null <- coef(mod1)  # estimated coefs
null[2] <- 0       # set mutual term = 0

# sample 20 dyads and compute probability ratios
probabilities <- matrix(nrow = 9, ncol = 1)
# nrow = 9 because three probabilities + upper and lower CIs; ncol = 1 because only have one time point
d <- dim(as.matrix(Net))  # how many row and column nodes?
size <- d[1] * d[2]                 # size of the matrix
nw <- matrix(1:size, nrow = d[1], ncol = d[2])
nw <- nw[lower.tri(nw)]             # sample only from lower triangle b/c
samp <- sample(nw, 20)              # dyadic probabilities are symmetric
prob.est.00 <- numeric(0)
prob.est.01 <- numeric(0)
prob.est.11 <- numeric(0)
prob.null.00 <- numeric(0)
prob.null.01 <- numeric(0)
prob.null.11 <- numeric(0)
  for (k in 1:20) {
    i <- arrayInd(samp[k], d)[1, 1]   # recover 'i's and 'j's from sample
    j <- arrayInd(samp[k], d)[1, 2]
    # run interpretation function with estimated coefs and mutual = 0:
    int.est <- interpret(mod1, type = "dyad", i = i, j = j)
    int.null <- interpret(mod1, coefficients = null, type = "dyad", 
        i = i, j = j)
    prob.est.00 <- c(prob.est.00, int.est[[1]][1, 1])
    prob.est.11 <- c(prob.est.11, int.est[[1]][2, 2])
    mean.est.01 <- (int.est[[1]][1, 2] + int.est[[1]][2, 1]) / 2
    prob.est.01 <- c(prob.est.01, mean.est.01)
    prob.null.00 <- c(prob.null.00, int.null[[1]][1, 1])
    prob.null.11 <- c(prob.null.11, int.null[[1]][2, 2])
    mean.null.01 <- (int.null[[1]][1, 2] + int.null[[1]][2, 1]) / 2
    prob.null.01 <- c(prob.null.01, mean.null.01)
  }
  prob.ratio.00 <- prob.est.00 / prob.null.00  # ratio of est. and null hyp
  prob.ratio.01 <- prob.est.01 / prob.null.01
  prob.ratio.11 <- prob.est.11 / prob.null.11
  probabilities[1, t] <- mean(prob.ratio.00)   # mean estimated 00 tie prob
  probabilities[2, t] <- mean(prob.ratio.01)   # mean estimated 01 tie prob
  probabilities[3, t] <- mean(prob.ratio.11)   # mean estimated 11 tie prob
  ci.00 <- t.test(prob.ratio.00, conf.level = 0.99)$conf.int
  ci.01 <- t.test(prob.ratio.01, conf.level = 0.99)$conf.int
  ci.11 <- t.test(prob.ratio.11, conf.level = 0.99)$conf.int
  probabilities[4, t] <- ci.00[1]              # lower 00 conf. interval
  probabilities[5, t] <- ci.01[1]              # lower 01 conf. interval
  probabilities[6, t] <- ci.11[1]              # lower 11 conf. interval
  probabilities[7, t] <- ci.00[2]              # upper 00 conf. interval
  probabilities[8, t] <- ci.01[2]              # upper 01 conf. interval
  probabilities[9, t] <- ci.11[2]              # upper 11 conf. interval


# create barplots from probability ratios and CIs
require("gplots")
bp <- barplot2(probabilities[1:3, ], beside = TRUE, plot.ci = TRUE, 
    ci.l = probabilities[4:6, ], ci.u = probabilities[7:9, ], 
    col = c("tan", "tan2", "tan3"), ci.col = "grey40", 
    xlab = "Dyadic tie values", ylab = "Estimated Prob./Null Prob.")
mtext(1, at = bp, text = c("(0,0)", "(0,1)", "(1,1)"), line = 0, cex = 0.5)
