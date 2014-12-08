Helping-Santas-Helpers
======================

Every year Santa has to satisfy a grueling toy-production schedule. Toy orders arrive all year long and the Elfin Workers Union is stronger than ever. Let's help Santa develop a job scheduling algorithm that meets his toy target while keeping his elves healthy and happy.

##### Toys Constrains
num = 10,000,000
id | arrive time | build time
 - arrive time <= start time
 - finish time = start time + build time
 - by only one elf and cannot stop
 - all toys must be finished
1. outside_toy_start_period(): True of outside of allowed starting period, False otherwise. 
2. is_complete(): True of the toy is completed given duration of work and elf's productivity rating, False otherwise. 


##### Working Constrains
start = January 1, 2014 at 9:00 am
sanctioned = 9:00 to 19:00 (10 hours) * 7
unsanctioned = 19:00 to 9:00 (14 hours) * 7
 - avg work hours per day <= 10
 - sanctioned <=> unsanctioned

##### Elves Constrains
num = 900
id | productivity rate
 - productivity rate > 0.25 and < 4.0 and = 1.0
 - real build time = build time / productivity rate
 - P = p * 1.02^n * 0.9^m (P: new productivity rate | p: old productivity rate | n: sanctioned hours | m: unsanctioned hours)

##### results
simple solution: Score - 1875730155.06 | Time - 2681.2335
sorted solution: Score - 1862020962.7 | Time - 2584.91999984


