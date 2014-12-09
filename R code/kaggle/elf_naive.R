#  This is specific for naive approach
assign_elf <- function(elves) {
  assigned_elf <-as.integer(elves[which.min(elves[,'next_available_time']),'elf_id'][1])
  return(assigned_elf)
}


toys <- foo.bar # Toys data.frame.  Assumes ToyId and Duration are integer, ArrivalTime is character (NOT factor)

toys[,'Arrival_time'] <- convert_to_minute(toys[,'Arrival_time'])

toys <- data.matrix(toys)

elves <- create_elves(num_elves, seed_rating, seed_time)

outcomes <- matrix(0, nrow = nrow(toys), ncol = 5, 
                   dimnames = list(NULL, c('ToyId', 'ElfId', 'StartTime', 'Duration', 'current_rating')))

system.time({
for(current_toy in 1:nrow(toys)) {
  
  c_toy_id <- toys[current_toy,'ToyId']
  c_toy_arrival <- toys[current_toy, 'Arrival_time'] 
  c_toy_duration <- toys[current_toy,'Duration']
  
  next_elf <- assign_elf(elves)
  
  c_elf_id <- elves[next_elf, 'elf_id']
  c_elf_start_time <- elves[next_elf, 'next_available_time']
  c_elf_rating <- elves[next_elf, 'current_rating']
  
  if(c_elf_start_time < c_toy_arrival) c_elf_start_time <- c_toy_arrival    
  
  work_duration <- as.integer(ceiling(c_toy_duration/c_elf_rating))
    
  elves[next_elf, 'next_available_time'] <- update_next_available_minute(c_elf_start_time, work_duration)
  
  elves[next_elf, 'current_rating'] <- update_productivity(c_elf_start_time, work_duration, c_elf_rating)

  outcomes[current_toy,] <- c(c_toy_id, c_elf_id, c_elf_start_time, work_duration, c_elf_rating)

  if(current_toy %% 100000 == 0) cat('Completed', current_toy/1000000, 'mil toys, makespan', c_elf_start_time, 'minutes \n')
  
}
})

rm(toys, c_toy_id, c_toy_arrival, c_toy_duration, c_elf_id, c_elf_start_time, c_elf_rating, next_elf, work_duration)

submission <- data.frame(ToyId = as.integer(outcomes[,1]), 
                         ElfId = as.integer(outcomes[,2]), 
                         StartTime = convert_to_chardate(outcomes[,3]), 
                         Duration = as.integer(outcomes[,4]), stringsAsFactors = FALSE)

write.csv(submission, 'toys_submission', row.names = FALSE)

