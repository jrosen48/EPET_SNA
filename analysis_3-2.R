# analysis.R

#################################
# 1. setting up, loading data
#################################

library(dplyr)
library(igraph)
library(googlesheets)
library(GGally) # make sure to install with install.packages("GGally") if not intalled
library(ggplot2) # same 

#setwd("~/Google Drive/EPET SN Survey")
sheet <- gs_title("EPET_Connections_DEIDENTIFIED")
df <- sheet %>% gs_read(ws = "EPET_Connections_Coded")
table(df$advisor1)
df_ss <- select(df, student, mode, year, person1:person10) # also advisor1:hope3?

#################################
# 2. plotting
#################################

for_graph <- tidyr::gather(df_ss, connection_number, connection_student, person1:person10, -student, -mode, na.rm = T)
to_plot <- select(for_graph, student, connection_student, mode, year)
sum(table(complete.cases(to_plot)))
to_plot <- to_plot[complete.cases(to_plot), ]
table(complete.cases(to_plot))
to_plot <- to_plot[to_plot$connection_student %in% to_plot$student, ]
table(complete.cases(to_plot))

# Models

vertex_df <- dplyr::distinct(dplyr::select(to_plot, student, mode, year))
vertex_df$year[vertex_df$year > 6] <- 6
g <- igraph::graph_from_data_frame(to_plot[, c(1:2)], vertices = vertex_df, directed = T)
V(g)$mode <- ifelse(V(g)$mode != "1", "Face-to-Face", "Hybrid")

ggnet2(g,
       size = "year",
       color = "mode", 
       palette = "Set2")

ggsave("epet_connections_sociogram.png", width = 8, height = 8)
