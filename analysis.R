# analysis.R

#################################
# 0. notes
#################################

# cleaning data - non-IDs, any other issues
# how to deal with folks who did not complete survey but were mentioned - permute?

#################################
# 1. setting up, loading data
#################################

library(dplyr)
library(igraph)

setwd("~/Google Drive/EPET SN Survey")

data <- read.csv("survey-data.csv", header = T, skip = 1, stringsAsFactors = F)

# install.packages("googlesheets)
library(googlesheets)

#gs_ls()

sheet <- gs_title("EPET_Connections_DEIDENTIFIED")

df <- sheet %>% gs_read(ws = "EPET_Connections_Coded")

str(df)

"https://docs.google.com/spreadsheets/d/1SNDkOqgyCUpTsB4eEvuoeid8Tnjx5M74QdToPReSU6E/pub?output=csv"

fix_strings <- function(x) {
    x[x == "faculty" | x == "advisor" | x == "group" | x == "research groups " | x == ""] <- NA
    x
}

data[] <- lapply(data, fix_strings)

attributes(data)

data_ss <- dplyr::select(data, 
                         student = Student,
                         person_1 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.1,
                         person_2 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.2,
                         person_3 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.3,
                         person_4 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.4,
                         person_5 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.5,
                         person_6 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.6,
                         person_7 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.7,
                         person_8 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.8,
                         person_9 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.9,
                         person_10 = Who.in.the.EPET.program.has.most.frequently.helped.you..Rank.them..with.the.most.beneficial.on.t....Person.10,
                         modality = dplyr::contains("hybrid"),
                         advisor = dplyr::contains("advisor"),
                         year = What.year.are.you.in.the.program.)

#################################
# 2. processing
#################################

str(data_ss)

to_plot <- tidyr::gather(data_ss, var1, var2, person_1:person_10, -modality, -advisor, -year, na.rm = T)

str(to_plot)

View(to_plot)

n_helpers <- 
    to_plot %>% 
        group_by(student) %>% 
        summarize(n = n())

sd(n_helpers$n, na.rm = T)

# View(n_helpers)
hist(n_helpers$n)

edgematrix <- as.matrix(data.frame(node1 = as.character(to_plot$student), node2 = as.character(to_plot$var2)))
g <- graph.edgelist(edgematrix)
g <- as.directed(g)
weights <- as.numeric(sapply(stringr::str_split(to_plot$var1, "_"), function(x) x[[2]]))
weights <- car:::recode(weights, "1 = 10; 2 = 9; 3 = 8; 4 = 7; 5 = 6; 6 = 5; 7 = 4; 8 = 3; 9 = 2; 10 = 1")
weights <- (weights ^ 2) / 10

E(g)$weight <- weights

g.simple <- simplify(g, remove.loops = T)
V(g.simple)$label <- NA
l <- layout.auto(g.simple)    
V(g.simple)$size = sqrt(degree(g.simple) * 2)

sort(names(V(g.simple)))

modality_hybrid <- data_ss$student[data_ss$modality == 1]
modality_f2f <- data_ss$student[data_ss$modality == 2]

hybrid_bool <- tolower(V(g.simple)$name) %in% modality_hybrid
f2f_bool <- tolower(V(g.simple)$name) %in% modality_f2f

V(g.simple)$color[hybrid_bool] <- "blue"
V(g.simple)$color[f2f_bool] <- "red"

# year1 <- data_ss$student[data_ss$year == 1]
# year2 <- data_ss$student[data_ss$year == 2]
# year3 <- data_ss$student[data_ss$year == 3]
# year4 <- data_ss$student[data_ss$year == 4]
# year5 <- data_ss$student[data_ss$year == 5]
# 
# y1_bool <- tolower(V(g.simple)$name) %in% year1
# y2_bool <- tolower(V(g.simple)$name) %in% year2
# y3_bool <- tolower(V(g.simple)$name) %in% year3
# y4_bool <- tolower(V(g.simple)$name) %in% year4
# y5_bool <- tolower(V(g.simple)$name) %in% year5
# 
# V(g.simple)$color[y1_bool] <- "white"
# V(g.simple)$color[y2_bool] <- "dark gray"
# V(g.simple)$color[y3_bool] <- "light blue"
# V(g.simple)$color[y4_bool] <- "dark blue"
# V(g.simple)$color[y5_bool] <- "black"

plot(g.simple, layout = l,
     vertex.label.color = "black",
     edge.arrow.size = .25,
     edge.width = E(g.simple)$weight / 3,
     vertex.label.font = 4)

#################################
# 3. setting up, loading data
#################################
