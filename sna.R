df1 <- read.csv("~/documents/data.txt", sep="")
df2 <- read.csv("~/documents/data1.txt", sep="")
df3 <- read.csv("~/documents/data2.txt", sep = "")

# View(TestData)
# View(TestData1)
# View(TestData2)

# need to install with install.packages("dplyr") if not already installed
library(dplyr)

# Merging df and df1
names(df2) <- c("nominator", "yvar1")
df <- left_join(df1, df2, by = "nominator")

# Calculating indegree and merging indegree with df
tempdf <- data.frame(table(df$nominee))
names(tempdf) <- c("nominator", "indegree")
tempdf$nominator <- as.numeric(tempdf$nominator)
df <- left_join(df, tempdf, by = "nominator")

# Calculating exposure
df$exposure <- df$relate * df$yvar1 * df$indegree

# Merging df and df2
df <- left_join(df, df3, by = "nominator")

# Calculating mean exposure
output1 <-
     df %>%
     group_by(nominator) %>%
     summarize(exposure_mean = mean(exposure) / n())

df <- left_join(df, output1, by = "nominator")
duplicated(df)
df_ss <- df[duplicated(df$nominator), ]
str(df_ss)
model1 <- lm(yvar2 ~ yvar1 + exposure_mean, data = df)
summary(model1)

# # With sum 
# 
# output <-
#     MergedDataNew %>%
#     group_by(nominator) %>%
#     summarize(exposure_sum = sum(exposure) / n())
# 
# MergedDataModel1 <- left_join(MergedDataFinal, output, by = "nominator")
# 
# model2 <- lm(yvar2 ~ yvar1 + exposure_sum, data = MergedDataModel1)
# summary(model1)
# # plot(model1)