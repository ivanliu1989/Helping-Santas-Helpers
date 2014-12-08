Helping-Santas-Helpers
======================

Every year Santa has to satisfy a grueling toy-production schedule. Toy orders arrive all year long and the Elfin Workers Union is stronger than ever. Let's help Santa develop a job scheduling algorithm that meets his toy target while keeping his elves healthy and happy.


##### Toys Constrains
num = 10,000,000 <br>
id | arrive time | build time 
 - arrive time <= start time
 - finish time = start time + build time
 - by only one elf and cannot stop
 - all toys must be finished
1. **outside_toy_start_period()**: True of outside of allowed starting period, False otherwise. 
2. **is_complete()**: True of the toy is completed given duration of work and elf's productivity rating, False otherwise. 


##### Working Constrains
start = January 1, 2014 at 9:00 am <br>
sanctioned = 9:00 to 19:00 (10 hours) * 7 <br>
unsanctioned = 19:00 to 9:00 (14 hours) * 7 <br>
 - avg work hours per day <= 10
 - sanctioned <=> unsanctioned
1. **convert_to_minute**: Converts the arrival time string to minutes since the reference start time. integer (minutes since arrival time)
2. **is_sanctioned_time**: Return boolean True or False if a given time (in minutes) is a sanctioned working day minute.
3. **get_sanctioned_breakdown**: 24-hr contribute fixed quantities of sanctioned and unsanctioned time.
4. **next_sanctioned_minute**: Given a minute, finds the next sanctioned minute.
5. **apply_resting_period**: Enforces the rest period and returns the minute when the elf is next available for work. Rest period is only applied to sanctioned work hours.


##### Elves Constrains
num = 900 <br>
id | productivity rate
 - productivity rate > 0.25 and < 4.0 and = 1.0
 - real build time = build time / productivity rate
 - P = p * 1.02^n * 0.9^m (P: new productivity rate | p: old productivity rate | n: sanctioned hours | m: unsanctioned hours)
1. **update_elf**: Updates the elf's productivity rating and next available time based on last toy completed.
2. **update_next_available_minute**: Apply the resting time constraint and determine the next minute when the elf can work next.
3. **update_productivity**: Update the elf's productivity rating based on the number of minutes of sanctioned and unsanctioned times.


##### Solution


##### Evaluation


##### results
simple solution: Score - 1875730155.06 | Time - 2681.2335 <br>
sorted solution: Score - 1862020962.7 | Time - 2584.91999984


