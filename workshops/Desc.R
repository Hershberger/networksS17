## =============================================================================
## Descriptive statistics for Krackhardt (1987) manager data
## =============================================================================

# Load the network package
library(network)

# Set the working directory (note, yours will be differnt than mine)
setwd("~/Dropbox/writings/Networks Book/Data/KrackhardtManager/") #[SJC]

# load feature network
advice <- read.csv("Advice.csv", header=FALSE) 
# load "Reports To" covariate
reportsto <- read.csv("ReportsTo.csv", header=FALSE)
# load the vertex-level covariates 
attr <- read.csv("KrackhardtVLD.csv")
# create an object of class "network" from the adjacency matrix 
Net <- network(advice) 
# attach the vertex attribues to the vertices
set.vertex.attribute(Net, names(attr), attr) 
# attach the dyadic covariate as an attribute
set.network.attribute(Net, "reportsto", as.matrix(reportsto)) 

save(Net, file="Krackhardt.RData", compress="bzip2")


## Basic plot
pdf(file="Krackhardt.pdf", width=6, height=6)
set.seed(1)
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col="red",
     main="Krackhardt Manager Advice Network")
dev.off()

## Plot by department
pdf(file="Krackhardt-dept.pdf", width=6, height=6)
set.seed(1)
dept <- get.vertex.attribute(Net, "Department")
cols <- c("red", "green", "blue", "black", "orange")
col.dept <- cols[match(dept, unique(dept))]
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.dept, label=dept,
     main="Krackhardt Manager Advice Network: Department")
dev.off()

## Plot by Level
pdf(file="Krackhardt-level.pdf", width=6, height=6)
set.seed(1)
level <- get.vertex.attribute(Net, "Level")
cols <- c("black", "red", "blue")
col.level <- cols[match(level, unique(level))]
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.level, label=level,
     main="Krackhardt Manager Advice Network: Level")
dev.off()

## Plot by tenure
pdf(file="Krackhardt-tenure.pdf", width=6, height=6)
set.seed(1)
Pal <- colorRampPalette(c("blue", "red"))
tenure <- get.vertex.attribute(Net, "Tenure")
col.tenure <- Pal(10)[as.numeric(cut(tenure, breaks = 10))]
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.tenure, label=tenure,
     main="Krackhardt Manager Advice Network: Tenure")
dev.off()

## Plot by age
pdf(file="Krackhardt-age.pdf", width=6, height=6)
set.seed(1)
Pal <- colorRampPalette(c("blue", "red"))
age <- get.vertex.attribute(Net, "Age")
col.age <- Pal(10)[as.numeric(cut(age, breaks = 10))]
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.tenure, label=age,
     main="Krackhardt Manager Advice Network: Age")
dev.off()

# Figure for book
# Set up a 1x3 plot
par(mfrow=c(1,3))
# Set the random seed
set.seed(1)
# define the color palette
Pal <- colorRampPalette(c("blue", "red"))
# create a temporary object housing the vertex attribute
tenure <- get.vertex.attribute(Net, "Tenure")
# assign different colors based on the vertex attribute
col.tenure <- Pal(10)[as.numeric(cut(tenure, breaks = 10))]
# plot the network, including vertex color options
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.tenure, label=tenure,
     main="Tenure")
# repeat the above steps for age and department
set.seed(1)
Pal <- colorRampPalette(c("blue", "red"))
age <- get.vertex.attribute(Net, "Age")
col.age <- Pal(10)[as.numeric(cut(age, breaks = 10))]
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.tenure, label=age,
     main="Age")
set.seed(1)
dept <- get.vertex.attribute(Net, "Department")
cols <- c("red", "green", "blue", "black", "orange")
col.dept <- cols[match(dept, unique(dept))]
plot(Net, edge.col="gray", label.col="black", vertex.cex=1.5,
     vertex.col=col.dept, label=dept,
     main="Department")
