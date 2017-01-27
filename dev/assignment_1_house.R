library(network)
library(sna)
library(data.table)
sponsors_93 <- as.matrix(read.delim("inst/extdata/093_housematrix.txt",
  header = FALSE, sep = ","))
house_93 <- read.delim("inst/extdata/93_house.txt", header = FALSE, sep = ",")

sponsors_94 <- as.matrix(read.delim("inst/extdata/094_housematrix.txt",
  header = FALSE, sep = ","))
house_94 <- read.delim("inst/extdata/94_house.txt", header = FALSE, sep = ",")

sponsors_95 <- as.matrix(read.delim("inst/extdata/095_housematrix.txt",
  header = FALSE, sep = ","))
house_95 <- read.delim("inst/extdata/95_house.txt", header = FALSE, sep = ",")

sponsors_96 <- as.matrix(read.delim("inst/extdata/096_housematrix.txt",
  header = FALSE, sep = ","))
house_96 <- read.delim("inst/extdata/96_house.txt", header = FALSE, sep = ",")

sponsors_97 <- as.matrix(read.delim("inst/extdata/097_housematrix.txt",
  header = FALSE, sep = ","))
house_97 <- read.delim("inst/extdata/97_house.txt", header = FALSE, sep = ",")

sponsors_98 <- as.matrix(read.delim("inst/extdata/098_housematrix.txt",
  header = FALSE, sep = ","))
house_98 <- read.delim("inst/extdata/98_house.txt", header = FALSE, sep = ",")

sponsors_99 <-  as.matrix(read.delim("inst/extdata/099_housematrix.txt",
  header = FALSE, sep = ","))
house_99 <- read.delim("inst/extdata/99_house.txt", header = FALSE, sep = ",")

sponsors_100 <- as.matrix(read.delim("inst/extdata/100_housematrix.txt",
  header = FALSE, sep = ","))
house_100 <- read.delim("inst/extdata/100_house.txt", header = FALSE, sep = ",")

sponsors_101 <- as.matrix(read.delim("inst/extdata/101_housematrix.txt",
  header = FALSE, sep = ","))
house_101 <- read.delim("inst/extdata/101_house.txt", header = FALSE, sep = ",")

sponsors_102 <- as.matrix(read.delim("inst/extdata/102_housematrix.txt",
  header = FALSE, sep = ","))
house_102 <- read.delim("inst/extdata/102_house.txt", header = FALSE, sep = ",")

sponsors_103 <- as.matrix(read.delim("inst/extdata/103_housematrix.txt",
  header = FALSE, sep = ","))
house_103 <- read.delim("inst/extdata/103_house.txt", header = FALSE, sep = ",")

sponsors_104 <- as.matrix(read.delim("inst/extdata/104_housematrix.txt",
  header = FALSE, sep = ","))
house_104 <- read.delim("inst/extdata/104_house.txt", header = FALSE, sep = ",")

sponsors_105 <- as.matrix(read.delim("inst/extdata/105_housematrix.txt",
  header = FALSE, sep = ","))
house_105 <- read.delim("inst/extdata/105_house.txt", header = FALSE, sep = ",")

sponsors_106 <- as.matrix(read.delim("inst/extdata/106_housematrix.txt",
  header = FALSE, sep = ","))
house_106 <- read.delim("inst/extdata/106_house.txt", header = FALSE, sep = ",")

sponsors_107 <- as.matrix(read.delim("inst/extdata/107_housematrix.txt",
  header = FALSE, sep = ","))
house_107 <- read.delim("inst/extdata/107_house.txt", header = FALSE, sep = ",")

sponsors_108 <- as.matrix(read.delim("inst/extdata/108_housematrix.txt",
  header = FALSE, sep = ","))
house_108 <- read.delim("inst/extdata/108_house.txt", header = FALSE, sep = ",")

sponsors_109 <- as.matrix(read.delim("inst/extdata/109_housematrix.txt",
  header = FALSE, sep = ","))
house_109 <- read.delim("inst/extdata/109_house.txt", header = FALSE, sep = ",")

sponsors_110 <- as.matrix(read.delim("inst/extdata/110_housematrix.txt",
  header = FALSE, sep = ","))
house_110 <- read.delim("inst/extdata/110_house.txt", header = FALSE, sep = ",")

# get cosponsorship info from fowler sponsor matrix
get_strong_ties <- function(sponsor_matrix, member_names) {
  # prep data for making matrix
  sponsor_matrix[sponsor_matrix == 2] <- 1
  sponsor_matrix[sponsor_matrix == 3] <- 1
  sponsor_matrix[sponsor_matrix == 5] <- 0
  rownames(sponsor_matrix) <- member_names[, 1]
  cs_matrix <- sponsor_matrix %*% t(sponsor_matrix)
  diag(cs_matrix) <- 0
  tie_mean <- mean(cs_matrix)
  tie_sd <- sd(cs_matrix)
  cs_matrix[cs_matrix  < (tie_mean + tie_sd)] <- 0
  st_net <- network::network(cs_matrix)
  st_net
}

get_centrality_table <- function(network_object) {
  dc <- degree(network_object)
  ndc <- dc / max(dc)
  ec <- evcent(network_object)
  i_net <- igraph::graph.adjacency(network_object[,])
  i_net <- as.list(igraph::closeness(i_net))
  i_net <- t(t(i_net))
  ct <- data.table(mc = rownames(i_net), closeness_centrality = i_net,
    degree_centrality = dc, normalized_degree_centrality = ndc,
    eigenvector_centrality = ec)
  ct
}

network_summary_by_congress <- function(congress_number, chamber) {
  if (chamber == "senate") {
    net <- get(paste0("sen_net", congress_number))
  } else if (chamber == "house") {
    net <- get(paste0("hou_net", congress_number))
  }
  cat("*** working on", chamber, congress_number, "\n")
  deg <- degree(net)
  i_net <- igraph::graph.adjacency(net[,])
  net_data <- data.table(Congress = congress_number,
    Density = igraph::graph.density(i_net),
    Transitivity = igraph::transitivity(i_net, type = "global"),
    Isolates = length(deg[deg == 0]))
  net_data
}

hou_net93 <- get_strong_ties(sponsors_93, house_93)
hou_net94 <- get_strong_ties(sponsors_94, house_94)
hou_net95 <- get_strong_ties(sponsors_95, house_95)
hou_net96 <- get_strong_ties(sponsors_96, house_96)
hou_net97 <- get_strong_ties(sponsors_97, house_97)
hou_net98 <- get_strong_ties(sponsors_98, house_98)
hou_net99 <- get_strong_ties(sponsors_99, house_99)
hou_net100 <- get_strong_ties(sponsors_100, house_100)
hou_net101 <- get_strong_ties(sponsors_101, house_101)
hou_net102 <- get_strong_ties(sponsors_102, house_102)
hou_net103 <- get_strong_ties(sponsors_103, house_103)
hou_net104 <- get_strong_ties(sponsors_104, house_104)
hou_net105 <- get_strong_ties(sponsors_105, house_105)
hou_net106 <- get_strong_ties(sponsors_106, house_106)
hou_net107 <- get_strong_ties(sponsors_107, house_107)
hou_net108 <- get_strong_ties(sponsors_108, house_108)
hou_net109 <- get_strong_ties(sponsors_109, house_109)
hou_net110 <- get_strong_ties(sponsors_110, house_110)

house_summary <- rbindlist(lapply(93:110, network_summary_by_congress,
  chamber = "house"))
setDT(house_summary)
xtable::xtable(house_summary)

house_table_104 <- get_centrality_table(hou_net104)
xtable::xtable(house_table_104, digits = 6)
stargazer::stargazer(house_table_104, summary = TRUE)

hou104_graph <- igraph::graph.adjacency(hou_net104[,])
house_degree_104 <- igraph::degree(hou104_graph, mode = "all")
house_degree_distribution_104 <- igraph::degree.distribution(hou104_graph,
  cumulative = TRUE, mode = "all")


plot(house_degree_distribution_104, log = "xy", type="l", lwd = 1,
  col = "darkorange2", xlab = "Degree",
  ylab = "Cumulative Distribution Function",
  main = "Degree Distribution House 104")
