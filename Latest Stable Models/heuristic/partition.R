setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())

load('data/toys.RData')

###################
### Calculation ###
###################
require(caret)
set.seed(8888)
toys_dat <- data.frame(toys)
index <- createFolds(toys_dat$Duration, k = 900, list = T)
save(index, file='Latest Stable Models/heuristic/900_Folds.RData')

toy_length <-c()
for (i in 1:900){
    toy_length <- c(toy_length, length(index[[i]]))
}
table(toy_length)
candidate <- which(toy_length == 11113)

toy_duration <-c()
for (i in 1:900){
    toy_duration <- c(toy_duration, sum(toys[index[[i]],3]))
}
range(toy_duration)
plot(density(toy_duration))
plot(toy_duration, type='l')

plot(toys[index[[900]],3])
