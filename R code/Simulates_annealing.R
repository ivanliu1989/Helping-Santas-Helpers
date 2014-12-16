# Use SA to separately optimise each elf's workflow.
# A reasonable starting distribution of toys
# Recalculate the elf's total time(cost function) on each change
# e.g.
# from arbitrary sequence of toy data, calculate end time for processing them in strict order

# Step 1. Read data and calculate CP value
# Step 2. Calculate x0 and f(x0) using SPT heuristic
# Step 3. Store x0 as best schedule xbest and f(x0) as best objective function value fbest
# Step 4. Store the activity list of x0 as current list and f(x0) as fcurrent
# Step 5. Read the SA parameters: N0, h, T0max, alpha, S and C
# for C Chains Do
#     T=T0max
#     Ns=N0
#     for S steps Do
#         Ns=Ns(1+h.s)
#         for Np neighbours Do
#             Generate a neighbour x1 of the current solution xcurrent
#             Calculate f(x1)
#             Calculate delta=f(x1)-f(x)
#             if delta<0 then store x1=scurrent and f(x1)=fcurrent
#                 if f(x1)<fbest then store x1=xbest and f(x1)=fbest
#                 if fbest=CP value then exit the procedure
#             else if P=e^((-delta)/T)>xrandom then store x1 and f(x1) as currents
#         endDo N
#         Calculate T=alpha*T
#     endDo S
# endDo C
# Step 6. Explore the Neighbourhood of the best solution


#############
### Setup ###
#############
setwd('/Users/ivan/Work_directory/FICO/Helping-Santas-Helpers/')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/FICO/Helping-Santas-Helpers')
setwd('H:/Machine_Learning/FICO/Helping-Santas-Helpers')
gc(); rm(list=ls())
source('R code/Functions.R')
load('data/toys.RData')

###################
### Calculation ###
###################
require(caret)
toys_dat <- data.frame(toys)
index <- createDataPartition(toys_dat$Duration, p = 1/900, list = F)
myToys <- data.matrix(toys_dat[index,])
schedule <- c(1:nrow(myToys))
NUM_ELVES <- 1
myelves <- create_elves(NUM_ELVES)

### f(x) ###
solution_Elf <- function(myToys, myelves, schedule){
    # cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    myToys <- myToys[schedule,]
    for(current_toy in 1:nrow(myToys)){
        
        c_toy_id <- myToys[current_toy,'ToyId']
        c_toy_arrival <- myToys[current_toy, 'Arrival_time'] 
        c_toy_duration <- myToys[current_toy,'Duration']
        
        c_elf_id <- myelves[, 'elf_id']
        c_elf_start_time <- myelves[, 'next_available_time']
        c_elf_rating <- myelves[, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
        
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        
        myelves[, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
#         if(current_toy %% 10000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
#                                            format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    # cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes[nrow(outcomes), 3]+outcomes[nrow(outcomes), 4])
}

### parameters ###
C <- 50 # multiple cooling chain
N0 <- runif(C)*nrow(myToys) # initial point
h <- 5 # used to modulate the step length.
alpha <- .2 # limited to a proportion of ~20% of fx0 in the first step
S <- 5 # current value times, step width
x0 <- schedule; fx0 <- solution_Elf(myToys, myelves, x0)
xbest <- x0; fbest <- fx0
xcurrent <- x0; fcurrent <- fx0
T0max <- 0.8*fx0 # initial temperature value

### main loop ###
cat(paste('Initial Score:',fbest))
for (c in 1:C){ # multiple cooling chain
    Ns <- xbest[N0[c]]
    cat(paste('\nChain:',c, '; Initial point:', Ns))
    for (s in 1:S){ 
        cat(paste('\n - Step:',s))
        Np <- (1+h+s/10) # different initial solution
        for (np in max((Ns-Np),1):min((Ns+Np),nrow(myToys))){ # Np = initail point range
            x1 <- xcurrent
            x1[np] <- xcurrent[Ns] # initial point <> range point
            x1[Ns] <- xcurrent[np]
            fx1 <- solution_Elf(myToys, myelves, x1) # x1, fx1 - updated schedule and time
            delta=fx1-fcurrent # difference
            if(delta<0){
                xcurrent <- x1; fcurrent <- fx1 # select better one between fx1, fcurrent and save it into fx1
            }
            if (fcurrent<fbest){
                xbest <- xcurrent; fbest <- fcurrent
                cat(paste('\n -- Point:',np,'; Score:',fbest))
            }
        }
    }
}

### main loop ###
for (c in 1:C){ # multiple cooling chain
    temperature <- T0max
    Ns <- xbest[N0[c]]
    cat(paste('\nChain:',c, '; Initial point:', Ns))
    for (s in 1:S){ 
        cat(paste('\n - Step:',s))
        Np <- (1+h+s/10) # different initial solution
        for (np in max((Ns-Np),1):min((Ns+Np),nrow(myToys))){ # Np = initail point range
            x1 <- xcurrent
            x1[np] <- xcurrent[Ns] # initial point <> range point
            x1[Ns] <- xcurrent[np]
            fx1 <- solution_Elf(myToys, myelves, x1) # x1, fx1 - updated schedule and time
            delta <- fx1-fcurrent # difference
            cat(paste('\n -- Delta:',delta))
            if(delta<0){
                xcurrent <- x1; fcurrent <- fx1 # select better one between fx1, fcurrent and save it into fx1
            }else{
                proba <- runif(1)
                test <- exp(-delta/temperature)
                if(proba<test){
                    xcurrent <- x1; fcurrent <- fx1
                }
            }
            if (fcurrent<fbest){
                xbest <- xcurrent; fbest <- fcurrent
                cat(paste('\n -- Point:',np,'; Score:',fbest))
            }
        }
        temperature <- alpha * temperature
        cat(paste('\n -- Temperature:',temperature))
    }
}
# C:50 | h:5 | S:5 => 50*5*2*5 => 2500