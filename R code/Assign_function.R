# 1. train all elves to 4.0
# 2. assign earliest exhausation and large toys to 4.0 elves
# 3. retrain >0.5 elves
# 4. assign 4.0 to exhausation toys

# 寻找能量E（s）最低的状态s
# 
# s := s0; e := E (s)                           // 設定目前狀態為s0，其能量E (s0)
# k := 0                                       // 評估次數k
# while k < kmax and e > emax                  // 若還有時間（評估次數k還不到kmax）且結果還不夠好（能量e不夠低）則：
# sn := neighbour (s)                         //   隨機選取一鄰近狀態sn
# en := E (sn)                                //   sn的能量為E (sn)
# if random() < P(e, en, temp(k/kmax)) then  //   決定是否移至鄰近狀態sn
# s := sn; e := en                         //     移至鄰近狀態sn
# k := k + 1                                 //   評估完成，次數k加一
# return s                                     // 回傳狀態s

# a1 <- function(mins_rated = 600){
#     d <- test_dat[,'Duration'] * (1-test_dat[,'finished'])
#     idx <- which(d <= mins_rated)
#     current_toy <- idx[which.max(test_dat[idx,'Duration'])]
#     return(current_toy)
# }
# 
# dt2 <- function(mins_rated = 600){
#     d <- toys_dt %>% 
#         filter(Duration <= mins_rated & finished==0) %>%
#         top_n(n=1, wt=Duration)
#     current_toy <- d$ToyId[1]
#     return(current_toy)
# }

# all toys arrival time => next 9.00 
# if(elf_rate == 4.0){
#     exhausation
#     
# }else{
#     earliest
# }

assign_toy <- function(myelves, myToys) {
    if(sum(myelves[,'score'] == 3) > 0){
        assigned_toy <-as.integer(myToys[myToys[,'Size']==3,'ToyId'][1])
        return(assigned_toy)
    }else if(sum(myelves[,'score'] == 2) > 0){
        assigned_toy <-as.integer(myToys[myToys[,'Size']==2,'ToyId'][1])
        return(assigned_toy)
    }else{
        assigned_toy <-as.integer(myToys[myToys[,'Size']==1,'ToyId'][1])
        return(assigned_toy)
    }
}

assign_elf <- function(myelves, di, c_toy_size) {
    myelves <- myelves[myelves[,'score']==c_toy_size,]
    assigned_elf <-as.integer(myelves[order(myelves[,'next_available_time']) ,'elf_id'][1])
    return(assigned_elf)
}
