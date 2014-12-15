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
solution_sortedElf <- function(myToys, myelves){
    cat(format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))
    outcomes <- matrix(0, nrow = nrow(myToys), ncol = 5, 
                       dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))
    
    for(current_toy in 1:nrow(myToys)){
        
        # train_elf <- 1.0; overwork_elf <- 3.9
        myelves[myelves[,'current_rating']>overwork_elf,'score'] <- 3
        myelves[myelves[,'current_rating']<=overwork_elf,'score'] <- 2
        myelves[myelves[,'current_rating']<=train_elf,'score'] <- 1
        
        c_toy_id <- assign_toy(myelves, myToys)
        c_toy_arrival <- myToys[c_toy_id, 'Arrival_time'] 
        c_toy_duration <- myToys[c_toy_id,'Duration']
        c_toy_size <- myToys[c_toy_id,'Size']
        
        next_elf <- assign_elf(myelves, c_toy_duration, c_toy_size)
        
        c_elf_id <- myelves[next_elf, 'elf_id']
        c_elf_start_time <- myelves[next_elf, 'next_available_time']
        c_elf_rating <- myelves[next_elf, 'current_rating']
        
        if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
        
        work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
        
        myelves[next_elf, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
        
        myelves[next_elf, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)
        
        outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)
        
        myToys <- myToys[-c_toy_id, ]
        
        if(current_toy %% 100000 == 0) cat('\nCompleted', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes',
                                           format(Sys.time(),format = '%Y-%m-%d %H:%M:%S'))    
    }
    cat('\nCompleted 10 mil toys at', convert_to_chardate(c_elf_start_time)) 
    return(outcomes)
}