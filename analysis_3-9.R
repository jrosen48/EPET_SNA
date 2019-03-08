#################################
# 1. setting up, loading data
#################################
library(tidyverse) # tidyverse includes ggplot2 dplyr tidyr readr takes a dataframe and outputs a dataframe
library(igraph)
library(googlesheets)
library(GGally) # make sure to install with install.packages("GGally") if not intalled
#setwd("~/Google Drive/EPET SN Survey") don't have to set the directory when you use projects
sheet <- read_csv("EPET_Connections_DEIDENTIFIED.csv")
# df <- sheet %>% gs_read(ws = "EPET_Connections_Coded")
table(df$advisor1)
df_ss <- select(df, student, mode, year, person1:person10) # also advisor1:hope3?
#################################
# 2. plotting
#################################
for_graph <- gather(df_ss, connection_number, connection_student, person1:person10, -student, -mode, na.rm = T) # creates edge list
to_plot <- select(for_graph, -connection_number)
to_plot <- select(to_plot, student, connection_student, mode, year)
to_plot <- to_plot[complete.cases(to_plot), ]
to_plot <- to_plot[to_plot$connection_student %in% to_plot$student, ] #if connection student is also student then keep it
#graph from data takes the first two columns as the edge column 
vertex_df <- dplyr::distinct(dplyr::select(to_plot, student, mode, year)) #we only want the unique combinations of student, mode & year
vertex_df$year[vertex_df$year > 6] <- 6
g <- igraph::graph_from_data_frame(to_plot[, c(1:2)], vertices = vertex_df, directed = T)
V(g)$mode <- ifelse(V(g)$mode != "1", "Face-to-Face", "Hybrid")

g1 <- intergraph::asNetwork(g)
network::is.directed(g1)
ggnet2(g1,
       size = "year",
       color = "mode", 
       palette = "Set2",
       directed = T,
       arrow.size = 5,
       arrow.gap = .03)

#################################
# 3. models
#################################

library(lme4) # will have to install

x <- igraph::as_adjacency_matrix(g)
y <- as.matrix(x)
nam <- row.names(y)
y <- as_tibble(y)
y$sender <- nam
z <- gather(y, to, edge, -sender)
fin <- rename(z, from = sender)
fin$from <- as.integer(fin$from)
df_ss <- rename(df_ss, from = student)
df_ss_ss <- select(df_ss, from, mode, year)

to_model <- left_join(fin, df_ss_ss, by = "from")
to_model <- rename(to_model, from_mode = mode, from_year = year)
to_model$from_mode <- as.factor(to_model$from_mode)

df_ss_ss <- rename(df_ss_ss, to = from)
to_model$to <- as.integer(to_model$to)
to_model <- left_join(to_model, df_ss_ss, by = "to")
to_model <- rename(to_model, to_mode = mode, to_year = year)

m1 <- glmer(edge ~ from_mode + from_year + to_mode + to_year + 
                (1|from) + (1|to), 
            data = to_model, 
            family = binomial(link = "logit"),
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))
            )
summary(m1)
