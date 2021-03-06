Helping-Santas-Helpers
======================

Every year Santa has to satisfy a grueling toy-production schedule. Toy orders arrive all year long and the Elfin Workers Union is stronger than ever. Let's help Santa develop a job scheduling algorithm that meets his toy target while keeping his elves healthy and happy.

##### ThinkPad (1:225) | Ultrabook (676:900) | Allienware (451:675) | iMac (226:450) 

#### Toys Constrains
num = 10,000,000 <br>
id | arrive time | build time 
 - arrive time <= start time
 - finish time = start time + build time
 - by only one elf and cannot stop
 - all toys must be finished
1. **outside_toy_start_period(self, start_minute)**: True of outside of allowed starting period, False otherwise. <br>
2. **is_complete(self, start_minute, elf_duration, rating)**: True of the toy is completed given duration of work and elf's productivity rating, False otherwise. <br>


#### Working Constrains
start = January 1, 2014 at 9:00 am <br>
sanctioned = 9:00 to 19:00 (10 hours) * 7 <br>
unsanctioned = 19:00 to 9:00 (14 hours) * 7 <br>
 - avg work hours per day <= 10
 - sanctioned <=> unsanctioned
1. **convert_to_minute(arrival)**: Converts the arrival time string to minutes since the reference start time. integer (minutes since arrival time)<br>
2. **is_sanctioned_time(self, minute)**: Return boolean True or False if a given time (in minutes) is a sanctioned working day minute.<br>
3. **get_sanctioned_breakdown(self, start_minute, duration)**: 24-hr contribute fixed quantities of sanctioned and unsanctioned time.<br>
4. **next_sanctioned_minute(self, minute)**: Given a minute, finds the next sanctioned minute.<br>
5. **apply_resting_period(self, start, num_unsanctioned)**: Enforces the rest period and returns the minute when the elf is next available for work. Rest period is only applied to sanctioned work hours.<br>


#### Elves Constrains
num = 900 <br>
id | productivity rate
 - productivity rate > 0.25 and < 4.0 and = 1.0
 - real build time = build time / productivity rate
 - P = p * 1.02^n * 0.9^m (P: new productivity rate | p: old productivity rate | n: sanctioned hours | m: unsanctioned hours)
1. **update_elf(self, hrs, toy, start_minute, duration)**: Updates the elf's productivity rating and next available time based on last toy completed.<br>
2. **update_next_available_minute(self, hrs, start_minute, duration)**: Apply the resting time constraint and determine the next minute when the elf can work next.<br>
3. **update_productivity(self, hrs, start_minute, toy_required_minutes)**: Update the elf's productivity rating based on the number of minutes of sanctioned and unsanctioned times.<br>


#### Solution
NUM_ELVES = 900 <br>
myelves = create_elves(NUM_ELVES)<br>
1. **create_elves(NUM_ELVES)**: Elves are stored in a sorted list using heapq to maintain their order by next available time.<br>
2. **assign_elf_to_toy(input_time, current_elf, current_toy, hrs)**: Assigns the next elf to the toy. Computes elf's updated rating, rest period (if any), and next available time.<br>
3. **solution_firstAvailableElf(toy_file, soln_file, myelves)**: Creates a simple solution where the next available elf is assigned a toy. Elves do not start work outside of sanctioned hours.<br>
4. **sort_toys()**: sort toys by finish time<br>

#### Evaluation
NUM_TOYS = 10000000 <br>
NUM_ELVES = 900<br>
1. **read_toys(toy_file, num_toys)**: Reads the toy file and returns a dictionary of Toys. Toy file format: ToyId, Arrival_time, Duration<br>
2. **score_submission(sub_file, myToys, hrs, NUM_ELVES)**: Score submission, constraint checking. Returns the time (in minutes) when final present is complete.<br>

#### results
simple solution: Score - 1875730155.06 | Time - 2681.2335 <br>
sorted solution: Score - 1862020962.7 | Time - 2584.91999984<br>
fixed sansationed solution: Score - 1862020962.7 | Time - 3485.48336697<br>
800 elves solution: Score - 2058850705.9 | Time - 2532.48336697<br>
850 elves solution: Score - 1955181153.14 | Time - 3694.09899998<br>

#### Improvements
1. Sort by productivity 
2. Allocate the proper productivity to build time
3. Cost Function = elf_build_time * (1 + log(1+n)) * max(1, elf_available / toy_arrival)

#### R - results - Raw Data
1. Naive Solution: 2538-03-09 09:17 | 2538-03-13 17:12 | 1875730155.06 
2. First Available and Highest Rate: 2538-03-10 10:55
3. Cost Function (build * time): 2546-10-24 17:41 | 1906561661 
4. Classification : 2538-01-31 17:13 | 1875328483 | 1875371106.87

#### R - results - Sorted Data
1. Naive Benchmark Solution: 2534-05-14 09:33 | 2532-08-27 18:52 | 1856032068.34
2. Classification : 2532-01-22 09:07 | 1853761875 | 1853964496.7 (1.2)
3. Regulate Time + Classified : 2532-10-10 13:16 | 1856330395

#### Leaderboard
2369-12-27 14:32

#### Strategies
1. Punch-Rest-Train. Where a long shift (the punch) is taken to build a multiday item, followed by the mandatory rest, and at most 14 days of re-training.
2. Diminishing shift extensions. This exploits the fact that there are multiple solutions to x* log 1.02 + y * log 0.9 = 0.
3. Next step: add work type and elf type into model. eg: only allocate 4.0 elf to 40 hour works
4. Rebuild work schedule

http://optlab-server.sce.carleton.ca/POAnimations/Default.aspx
http://adv-r.had.co.nz/Rcpp.html
