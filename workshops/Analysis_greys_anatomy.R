# Grey's Anatomy Hook-up data
# Gathered by Gary Weissman and Augmented by Benjamin Lind


# Load up
library(network)
library(ergm)

ga.mat <- as.matrix(read.table("inst/extdata/ga-mat.tsv", sep="\t", header=TRUE,
  row.names=1, quote="\""))
ga.atts <- read.table("inst/extdata/ga-atts.tsv", sep="\t", header=T, quote="\"",
  stringsAsFactors=F, strip.white=T, as.is=T)
hookups<-network(ga.mat, vertex.attr=ga.atts, vertex.attrnames=colnames(ga.atts),
                directed=F, hyper=F, loops=F, multiple=F, bipartite=F)


set.seed(5)
plot(hookups, vertex.col=c("blue","pink")[1+(get.vertex.attribute(hookups, "sex")=="F")],
          label=get.vertex.attribute(hookups, "name"), label.cex=.55)


m1 <- ergm(hookups ~ edges + absdiff("birthyear"), # people hookup within approximate age group
  control = control.ergm(MCMC.samplesize=50000,
  MCMC.burnin=100000,
  MCMLE.maxit=10)
)
g1 <- gof(m1)
# note that we have the general trend captured in this
# the general trend being captured is what we hope for in these things
par(mfrow=c(2,2)); plot(g1)

m1 <- ergm(hookups ~ edges +
  absdiff("birthyear") # people hookup within approximate age group
  + nodecov("birthyear"), # younger people are more likely to hookup
  control=control.ergm(
  MCMC.samplesize=50000,
  MCMC.burnin=100000,
  MCMLE.maxit=10)
)
# note that this isn't actually doing the MCMC stuff,
# thus the reported MCMC % is 0
summary(m1)



m1 <- ergm(hookups ~ edges
  + absdiff("birthyear") # people hookup within approximate age group
  + nodecov("birthyear") # younger people are more likely to hookup
  + nodemix("position"), # people are less likely to hookup within their working unit
  control=control.ergm(
  	MCMC.samplesize=50000,
   	MCMC.burnin=100000,
   	MCMLE.maxit=10)
)
summary(m1)

# this one does actual MCMC
# it's also degenerate
m1 <- ergm(hookups ~ edges
  + absdiff("birthyear") # people hookup within approximate age group
  + nodecov("birthyear") # younger people are more likely to hookup
  #+ nodemix("position") # people are less likely to hookup within their working unit
  + nodematch("race") # people prefer to hookup within their racial group
  + nodematch("sex") # people prefer to hookup with members of the opposite sex (negative effect)
  + nodefactor("sex") # one sex may prefer more hookups than the other
  + degree(1) # tendency for monogomy
  + degreepopularity # tendency for some people to be very hookup active
  #+ triangle # negative effect because bi-sex and homo-sex less common
  + cycle(4), # tendency for cliqueishness
  control=control.ergm(
   	MCMC.samplesize=500,
   	MCMC.burnin=100,
  	MCMLE.maxit=10)
)
summary(m1)







#-----------------------------------------------------------------------------
# These last two models are used in Chapter 4 of the book


set.seed(5)
m0 <- ergm(hookups ~ edges
             + absdiff("birthyear") # people hookup within approximate age group
             + nodecov("birthyear") # younger people are more likely to hookup
             + nodematch("position")#+ nodemix("position") # people are less likely to hookup within their working unit
             + nodematch("race") # people prefer to hookup within their racial group
             + nodematch("sex", diff=TRUE, keep=c(1)) # people prefer to hookup with members of the opposite sex (negative effect)
             + nodefactor("sex") # one sex may prefer more hookups than the other
             )
summary(m0)
g0 <- gof(m0)
par(mfrow=c(2,2)); plot(g0)


# this one also does real MCMC
# note that since it isn't degenerate it goes more quickly
set.seed(5)
m1 <- ergm(hookups ~ edges
  + absdiff("birthyear") # people hookup within approximate age group
  + nodecov("birthyear") # younger people are more likely to hookup
  + nodematch("position")#+ nodemix("position") # people are less likely to hookup within their working unit
  + nodematch("race") # people prefer to hookup within their racial group
  + nodematch("sex", diff=TRUE, keep=c(1)) # people prefer to hookup with members of the opposite sex (negative effect), throw out male-male becuaes that does not occur in the data
  #+ nodefactor("sex") # one sex may prefer more hookups than the other
  + degree(1)# tendency for monogamy
  #+ gwdegree(fixed=FALSE) # tendency for some people to be very hookup active
  #+ gwesp # negative effect because bi-sex and homo-sex less common
  + gwdsp(1, fixed=TRUE), # tendency for cliqueishness; also works well with gwdsp(1, fixed=TRUE)
  #+ cycle(4) # 4-cycle is an ex of an ex of an ex
  control=control.ergm(
   	MCMC.samplesize=5000,
   	MCMC.burnin=5000,
    MCMLE.maxit=10)
)
summary(m1)
g1 <- gof(m1)
par(mfrow=c(2,2)); plot(g1)
