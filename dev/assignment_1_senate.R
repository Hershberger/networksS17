library(network)
library(sna)
library(data.table)
sponsors_93 <- as.matrix(read.delim("inst/extdata/093_senmatrix.txt",
  header = FALSE, sep = ","))
senators_93 <- read.delim("inst/extdata/93_senators.txt", header = FALSE, sep = ",")

sponsors_94 <- as.matrix(read.delim("inst/extdata/094_senmatrix.txt",
  header = FALSE, sep = ","))
senators_94 <- read.delim("inst/extdata/94_senators.txt", header = FALSE, sep = ",")

sponsors_95 <- as.matrix(read.delim("inst/extdata/095_senmatrix.txt",
  header = FALSE, sep = ","))
senators_95 <- read.delim("inst/extdata/95_senators.txt", header = FALSE, sep = ",")

sponsors_96 <- as.matrix(read.delim("inst/extdata/096_senmatrix.txt",
  header = FALSE, sep = ","))
senators_96 <- read.delim("inst/extdata/96_senators.txt", header = FALSE, sep = ",")

sponsors_97 <- as.matrix(read.delim("inst/extdata/097_senmatrix.txt",
  header = FALSE, sep = ","))
senators_97 <- read.delim("inst/extdata/97_senators.txt", header = FALSE, sep = ",")

sponsors_98 <- as.matrix(read.delim("inst/extdata/098_senmatrix.txt",
  header = FALSE, sep = ","))
senators_98 <- read.delim("inst/extdata/98_senators.txt", header = FALSE, sep = ",")

sponsors_99 <-  as.matrix(read.delim("inst/extdata/099_senmatrix.txt",
  header = FALSE, sep = ","))
senators_99 <- read.delim("inst/extdata/99_senators.txt", header = FALSE, sep = ",")

sponsors_100 <- as.matrix(read.delim("inst/extdata/100_senmatrix.txt",
  header = FALSE, sep = ","))
senators_100 <- read.delim("inst/extdata/100_senators.txt", header = FALSE, sep = ",")

sponsors_101 <- as.matrix(read.delim("inst/extdata/101_senmatrix.txt",
  header = FALSE, sep = ","))
senators_101 <- read.delim("inst/extdata/101_senators.txt", header = FALSE, sep = ",")

sponsors_102 <- as.matrix(read.delim("inst/extdata/102_senmatrix.txt",
  header = FALSE, sep = ","))
senators_102 <- read.delim("inst/extdata/102_senators.txt", header = FALSE, sep = ",")

sponsors_103 <- as.matrix(read.delim("inst/extdata/103_senmatrix.txt",
  header = FALSE, sep = ","))
senators_103 <- read.delim("inst/extdata/103_senators.txt", header = FALSE, sep = ",")

sponsors_104 <- as.matrix(read.delim("inst/extdata/104_senmatrix.txt",
  header = FALSE, sep = ","))
senators_104 <- read.delim("inst/extdata/104_senators.txt", header = FALSE, sep = ",")

sponsors_105 <- as.matrix(read.delim("inst/extdata/105_senmatrix.txt",
  header = FALSE, sep = ","))
senators_105 <- read.delim("inst/extdata/105_senators.txt", header = FALSE, sep = ",")

sponsors_106 <- as.matrix(read.delim("inst/extdata/106_senmatrix.txt",
  header = FALSE, sep = ","))
senators_106 <- read.delim("inst/extdata/106_senators.txt", header = FALSE, sep = ",")

sponsors_107 <- as.matrix(read.delim("inst/extdata/107_senmatrix.txt",
  header = FALSE, sep = ","))
senators_107 <- read.delim("inst/extdata/107_senators.txt", header = FALSE, sep = ",")

sponsors_108 <- as.matrix(read.delim("inst/extdata/108_senmatrix.txt",
  header = FALSE, sep = ","))
senators_108 <- read.delim("inst/extdata/108_senators.txt", header = FALSE, sep = ",")

sponsors_109 <- as.matrix(read.delim("inst/extdata/109_senmatrix.txt",
  header = FALSE, sep = ","))
senators_109 <- read.delim("inst/extdata/109_senators.txt", header = FALSE, sep = ",")

sponsors_110 <- as.matrix(read.delim("inst/extdata/110_senmatrix.txt",
  header = FALSE, sep = ","))
senators_110 <- read.delim("inst/extdata/110_senators.txt", header = FALSE, sep = ",")

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

sen_net93 <- get_strong_ties(sponsors_93, senators_93)
sen_net94 <- get_strong_ties(sponsors_94, senators_94)
sen_net95 <- get_strong_ties(sponsors_95, senators_95)
sen_net96 <- get_strong_ties(sponsors_96, senators_96)
sen_net97 <- get_strong_ties(sponsors_97, senators_97)
sen_net98 <- get_strong_ties(sponsors_98, senators_98)
sen_net99 <- get_strong_ties(sponsors_99, senators_99)
sen_net100 <- get_strong_ties(sponsors_100, senators_100)
sen_net101 <- get_strong_ties(sponsors_101, senators_101)
sen_net102 <- get_strong_ties(sponsors_102, senators_102)
sen_net103 <- get_strong_ties(sponsors_103, senators_103)
sen_net104 <- get_strong_ties(sponsors_104, senators_104)
sen_net105 <- get_strong_ties(sponsors_105, senators_105)
sen_net106 <- get_strong_ties(sponsors_106, senators_106)
sen_net107 <- get_strong_ties(sponsors_107, senators_107)
sen_net108 <- get_strong_ties(sponsors_108, senators_108)
sen_net109 <- get_strong_ties(sponsors_109, senators_109)
sen_net110 <- get_strong_ties(sponsors_110, senators_110)

senate_summary <- rbindlist(lapply(93:110, network_summary_by_congress,
  chamber = "senate"))
setDT(senate_summary)
xtable::xtable(senate_summary)

senate_table_104 <- get_centrality_table(sen_net104)
xtable::xtable(senate_table_104, digits = 5)
stargazer::stargazer(senate_table_104, summary = TRUE)

sen104_graph <- igraph::graph.adjacency(sen_net104[,])
semate_degree_104 <- igraph::degree(sen104_graph, mode = "all")
senate_degree_distribution_104 <- igraph::degree.distribution(hou104_graph,
  cumulative = TRUE, mode = "all")


plot(senate_degree_distribution_104, log = "xy", type="l", lwd = 1,
  col = "darkorange2", xlab = "Degree",
  ylab = "Cumulative Distribution Function",
  main = "Degree Distribution Senate 104")
