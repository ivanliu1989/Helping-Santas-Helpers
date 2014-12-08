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

##### Working Constrains
start = January 1, 2014 at 9:00 am
sanctioned = 9:00 to 19:00 (10 hours) * 7
unsanctioned = 19:00 to 9:00 (14 hours) * 7
 - avg work hours per day <= 10
 - sanctioned <=> unsanctioned

##### Elves Constrains

simple solution: Score - 1875730155.06 | Time - 2681.2335
sorted solution: Score - 1862020962.7 | Time - 2584.91999984


