setwd('/Users/ivan/Work_directory/FICO/')
gc(); rm(list=ls())

toy <- read.csv('data/toys_rev1.csv')
elf <- data.frame(ElfId=c(1:900), Productivity = rep(1.0,900), Available = rep(1, 900))
sample <- read.csv('data/sampleSubmission_rev1.csv')
a <- 1.02
b <- 0.9
h <- 60
n <- 1
m <- 0
p <- elf[1,2]*(a^n)*(b^m)

# S=tf∗log(1+nw)
time1 <- strftime("2024/12/17 17:2", "%Y-%m-%d %H:%M:%OS")
time2 <- strftime("2024/12/17 18:2", "%Y-%m-%d %H:%M:%OS")
difftime(time1, time2, units = "mins")
S <- tf * log(1+900)

### Toys Conditions ###
# 1. Work on toy only start after it comes in.
# 2. Once work on toy starts, it must continue until the toy is complete
# 3. it must be performed by only one elf. 
# 4. an elf cannot start work one day, stop and resume the next morning. 

### Working Conditions ###
# 1. Work hour: 2014 1 1 9 0, 7 days a week, 9 - 19 (10 hours per day)
# 2. Overtime work must be compensated: 14:00-19:33 => 9:33-19:00

### Elf Conditions ###
# 1. 900 elves total
# 2. productivity rating: 0.25 - 4.0 (start from 1.0)
# 3. build time = standard time / rate = 120 / 1.25 = 96
# 4. rating are held constant during the building and updated once the toy is completed
# 5. rating are calculated on standard time of a toy
# 6. working hour, rating p=p′∗(1.02)n
# 7. after work hour, rating p=p′∗(0.9)m (can be decimal)
# 8. the productivity is updated in a single step once work is over as: p=p′∗(1.02)n∗(0.9)m