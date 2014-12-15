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
    