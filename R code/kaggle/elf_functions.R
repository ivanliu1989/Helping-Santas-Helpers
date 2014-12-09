### Elf table should start with the following:  elf_id, elf_rating = 1.0, next_available_time = 540 (540 minutes into the new year 9am jan1)

rating_increase <- 1.02
rating_decrease <- 0.90
num_elves <- 900
seed_rating <- 1.0
seed_time <-  540

create_elves <- function(num_elves, seed_rating, seed_time) {
  col_names <- c('elf_id', 'current_rating', 'next_available_time')
  elf_mat <- matrix(0, nrow = num_elves, ncol = length(col_names), dimnames = list(NULL,col_names))
  elf_mat[,'elf_id'] <- seq_len(num_elves)
  elf_mat[,'current_rating'] <- seed_rating
  elf_mat[,'next_available_time'] <- seed_time
  return(elf_mat)
}

update_next_available_minute <- function(start_minute, work_duration) {
  sanctioned_breakdown <- get_sanctioned_breakdown(start_minute, work_duration)
  sanc_time <- sanctioned_breakdown[1]
  unsanc_time <- sanctioned_breakdown[2]
  end_minute <- start_minute + work_duration
  ## In theory i can start a toy during unsanctioned time if no rest is needed.  
  ## Unfortunately, Evaluator does not like this.  keep as is.
  if(unsanc_time == 0) {
    if(is_sanctioned_time(end_minute)) {
      next_available_time <- end_minute
    } else {
      next_available_time <- next_sanctioned_minute(end_minute)
    }
  } else {
    next_available_time <- apply_resting_period(end_minute, unsanc_time)
  }
  return(next_available_time)
}


update_productivity <- function(start_minute, work_duration, current_rating) {
  sanctioned_breakdown <- get_sanctioned_breakdown(start_minute, work_duration)
  sanc_time <- sanctioned_breakdown[1]
  unsanc_time <- sanctioned_breakdown[2]
  new_rating <- max(c(0.25, min(c(4.0, current_rating * (rating_increase ** (sanc_time/60)) * (rating_decrease ** (unsanc_time/60))))))
  return(new_rating)
}

