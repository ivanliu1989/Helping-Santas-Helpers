######################
### Init Parameter ###
######################
toy_init <- function(input){
    reference_start_time <- strftime("2015/1/1 0:0", "%Y-%m-%d %H:%M:%OS")  # set when elf starts working on toy
    id = input$ToyId
    arrival_minute <- convert_to_minute(as.character(input$Arrival_time))
    duration <- as.integer(input$Duration)
    completed_minute <- 0  
    return(data.frame(reference_start_time=reference_start_time, id=id, 
                      arrival_minute=arrival_minute, duration=duration, completed_minute=completed_minute))
}
# input <- toys_rev2[888,]
# toy_task <- toy_init(input)


##################################
### 开始时间是否在到达时间之后 ###
##################################
outside_toy_start_period <- function(arrival_minute, start_minute){
    # Checks that work on toy does not start outside of the allowed starting period.
    # :param hrs: Hours class
    # :param start_minute: minute the work is scheduled to start
    # :return: True of outside of allowed starting period, False otherwise    
    return (start_minute < arrival_minute)
}
# outside_toy_start_period(toy_task$arrival_minute, 10000)


##################################
### 是否完成根据工作时间和效率 ###
##################################
is_complete <- function(duration, completed_minute, start_minute, elf_duration, rating){
    # Determines if the toy is completed given duration of work and elf's productivity rating
    # param start_minute: minute work started
    # param elf_duration: duration of work in minutes
    # param rating: elf's productivity rating
    # return: Boolean
    if (duration/rating <= elf_duration){
        completed_minute <- start_minute + as.integer(ceiling(duration/rating))
        return(completed_minute)
    }else{
        return(FALSE)
    } 
}
# is_complete(toy_task$duration, toy_task$completed_minute, 100000, 10, 1.2)
