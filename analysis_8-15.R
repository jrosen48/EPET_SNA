# analysis.R

#################################
# 1. setting up, loading data
#################################

library(dplyr)
library(igraph)
library(googlesheets)
library(GGally) # make sure to install with install.packages("GGally") if not intalled

#setwd("~/Google Drive/EPET SN Survey")
sheet <- gs_title("EPET_Connections_DEIDENTIFIED")
df <- sheet %>% gs_read(ws = "EPET_Connections_Coded")
table(df$advisor1)
df_ss <- select(df, student, mode, year, person1:person10) # also advisor1:hope3?

#################################
# 2. processing
#################################

for_graph <- tidyr::gather(df_ss, connection_number, connection_student, person1:person10, -student, -mode, na.rm = T)

to_plot <- select(for_graph, student, connection_student, mode, year)

to_plot <- to_plot[to_plot$connection_student %in% to_plot$student, ]

to_plot <- to_plot[complete.cases(to_plot), ]
to_plot
g <- igraph::graph_from_data_frame(to_plot)

#non_sender_names <- unique(to_plot$connection_student[!(to_plot$connection_student %in% to_plot$student)])

vertex_df <- dplyr::distinct(select(to_plot, student, mode, year))

#to_add <- data.frame(student = non_sender_names, mode = rep(NA, 23), year = rep(NA, 23))

#vertex_df_all <- bind_rows(vertex_df, to_add)

g <- igraph::graph_from_data_frame(to_plot[, c(1:2)], vertices = vertex_df, directed = T)
g

ggnet2(g,
       size = "year",
       color = "mode")

edgematrix <- as.matrix(data.frame(node1 = as.character(for_graph$student), node2 = as.character(for_graph$connection_student)))
g <- graph.edgelist(edgematrix)
g <- as.directed(g)

for_graph$connection_number

weights <- substr(for_graph$connection_number, 7, 8)
weights <- car:::recode(weights, "1 = 10; 2 = 9; 3 = 8; 4 = 7; 5 = 6; 6 = 5; 7 = 4; 8 = 3; 9 = 2; 10 = 1")
E(g)$weight <- (weights ^ 2) / 10

g.simple <- g
V(g.simple)$label <- NA
l <- layout.auto(g.simple)    
V(g.simple)$size = degree(g.simple) / 2

modality_hybrid <- df$student[df$mode == 1]
modality_f2f <- df$student[df$mode == 2]

hybrid_bool <- tolower(V(g.simple)$name) %in% modality_hybrid
f2f_bool <- tolower(V(g.simple)$name) %in% modality_f2f

V(g.simple)$color[hybrid_bool] <- "blue"
V(g.simple)$color[f2f_bool] <- "red"

library(dplyr)

code_the_var <- function(df, var, id){
    out <- vector()
    for(i in 1:length(unique(var))){
        out
    }

    
}

# year1 <- df$student[df$year == 1]
# year2 <- df$student[df$year == 2]
# year3 <- df$student[df$year == 3]
# year4 <- df$student[df$year == 4]
# year5 <- df$student[df$year == 5]
# year6plus <- df$student[df$year >= 6]
# 
# y1_bool <- tolower(V(g.simple)$name) %in% year1
# y2_bool <- tolower(V(g.simple)$name) %in% year2
# y3_bool <- tolower(V(g.simple)$name) %in% year3
# y4_bool <- tolower(V(g.simple)$name) %in% year4
# y5_bool <- tolower(V(g.simple)$name) %in% year5
# y6_bool <- tolower(V(g.simple)$name) %in% year6plus
# 
# V(g.simple)$color[y1_bool] <- "white"
# V(g.simple)$color[y2_bool] <- "dark gray"
# V(g.simple)$color[y3_bool] <- "light blue"
# V(g.simple)$color[y4_bool] <- "dark blue"
# V(g.simple)$color[y5_bool] <- "black"
# V(g.simple)$color[y6_bool] <- "purple"

g.simple_ss <- as.undirected(g.simple)
x <- fastgreedy.community(g.simple_ss)
membership(x)
plot(x, g.simple)

plot(g.simple, layout = l,
     vertex.label.color = "black",
     edge.arrow.size = .25,
     edge.width = E(g.simple)$weight / 3,
     vertex.label.font = 4)