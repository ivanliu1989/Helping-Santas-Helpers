######################
### Init Parameter ###
######################
elf_init <- function(input){
#     Each Elf starts with a rating of 1.0 and are available at 09:00 on Jan 1.
    id <- input$elfid
    rating <- 1.0
    next_available_time <- 540  # Santa's Workshop opens Jan 1, 2014 9:00 (= 540 minutes)
    rating_increase <- 1.02
    rating_decrease <- 0.90
    return(data.frame(id=id, rating=rating, next_available_time=next_available_time,
                      rating_increase=rating_increase, rating_decrease=rating_decrease))
}
elf_list <- elf_init(elf)

##############################
### 更新效率和下次工作时间 ###
##############################
update_elf <- function(elf_list, hrs, toy, start_minute, duration){
#     Updates the elf's productivity rating and next available time based on last toy completed.
#     :param hrs: Hours object for bookkeeping
#     :param toy: Toy object for the toy the elf just finished
#     :param start_minute: minute work started
#     :param duration: duration of work, in minutes
#     :return: void
    update_next_available_minute(elf_list, hrs, start_minute, duration)
    update_productivity(hrs, start_minute, int(elf_list, math.ceil(toy.duration / self.rating)))
}


##########################
### 更新下次可工作时间 ###
##########################
update_next_available_minute <- function(elf_list, hrs, start_minute, duration){
    
    
}

def update_next_available_minute(self, hrs, start_minute, duration):
    """ Apply the resting time constraint and determine the next minute when the elf can work next.
        Here, elf can only start work during sanctioned times
        :param start_minute: time work started on last toy
        :param duration: duration of work on last toy
        :return: void
        """
    sanctioned, unsanctioned = hrs.get_sanctioned_breakdown(start_minute, duration)
    
    # enforce resting time based on the end_minute and the unsanctioned minutes that
    # need to be accounted for.
    end_minute = start_minute + duration
    if unsanctioned == 0:
        if hrs.is_sanctioned_time(end_minute):
        self.next_available_time = end_minute
    else:
        self.next_available_time = hrs.next_sanctioned_minute(end_minute)
    else:
        self.next_available_time = hrs.apply_resting_period(end_minute, unsanctioned)

####################
### 更新工作效率 ###
####################
update_productivity <- function(elf_list, hrs, start_minute, toy_required_minutes){
    
    
}

def update_productivity(self, hrs, start_minute, toy_required_minutes):
    """ Update the elf's productivity rating based on the number of minutes the toy required that were
        worked during sanctioned and unsanctioned times.
        max(0.5,
            min(2.0, previous_rating * (self.rating_increase ** sanctioned_hours) *
            (self.rating_decrease ** unsanctioned_hours)))
        :param hrs: hours object
        :param start_minute: minute work started
        :param toy_required_minutes: minutes required to build the toy (may be different from minutes elf worked)
        :return: void
        """
    # number of required minutes to build toy worked by elf, broken up by sanctioned and unsanctioned minutes
    sanctioned, unsanctioned = hrs.get_sanctioned_breakdown(start_minute, toy_required_minutes)
    self.rating = max(0.25,
                      min(4.0, self.rating * (self.rating_increase ** (sanctioned/60.0)) *
                              (self.rating_decrease ** (unsanctioned/60.0))))

