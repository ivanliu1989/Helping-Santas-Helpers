//First
if((c_elf_rating == 4.0) & (toy_row(1) < myToys_1.nrow())){
    ind = 1;
    c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(toy_row(1),1));
    act_duration = ceil(myToys_1(toy_row(1),2)/c_elf_rating);
    sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration);
    double a = updateProductivity(c_elf_start_time_2,(int)act_duration, c_elf_rating)/c_elf_rating;
    
    if((a<=0.95) & (toy_row(0) < myToys_0.nrow())){
        c_elf_start_time = std::max(c_elf_start_time, (int)myToys_0(toy_row(0),1));
        act_duration = ceil(myToys_0(toy_row(0),2)/c_elf_rating);
        sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
        
        while(sanc/act_duration < 0.98){
            c_elf_start_time = 840 + sanc + c_elf_start_time;
            act_duration = ceil(myToys_0(toy_row(0),2)/c_elf_rating);
            sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
        }
        c_toy_id = myToys_0(toy_row(0),0);
        c_toy_arrival = myToys_0(toy_row(0),1);
        c_toy_duration = myToys_0(toy_row(0),2);
        toy_row(0) += 1;
        
    }else{
        c_toy_id = myToys_1(toy_row(1),0);
        c_toy_arrival = myToys_1(toy_row(1),1);
        c_toy_duration = myToys_1(toy_row(1),2);
        toy_row(1) += 1;
    }
}
    
// Second
if((c_elf_rating >= 3.28) & (toy_row(17) < myToys_17.nrow())){
    c_elf_start_time = std::max((int)c_elf_start_time, (int)myToys_17(toy_row(17),1));
    act_duration = ceil(myToys_17(toy_row(17),2)/c_elf_rating);
    sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
    rate_duration = sanc/act_duration;
    
    while(rate_duration<=0.03){
        c_elf_start_time = c_elf_start_time + 840 + sanc;
        sanc = getSanctionedBreakdown(c_elf_start_time, act_duration);
        rate_duration = sanc/act_duration;
    }
    if((sanc/act_duration<=0.95) & (toy_row(0) < myToys_0.nrow())){
        c_toy_id = myToys_0(toy_row(0),0);
        c_toy_arrival = myToys_0(toy_row(0),1);
        c_toy_duration = myToys_0(toy_row(0),2);
        toy_row(0) += 1;
    }else{
        c_toy_id = myToys_17(toy_row(17),0);
        c_toy_arrival = myToys_17(toy_row(17),1);
        c_toy_duration = myToys_17(toy_row(17),2);
        toy_row(17) += 1;
    }
}

//New1
if((c_elf_rating >= 3.95) & (toy_row(1) < myToys_1.nrow())){
    
    int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(toy_row(1),1)); //toy1 start time 
    int act_duration = ceil(myToys_1(toy_row(1),2)/c_elf_rating); //toy1 actual work time 
    int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
    double sanc_rate = (double)sanc/act_duration;
    
    while(sanc_rate<=0.05){
        c_elf_start_time_2 = c_elf_start_time_2 + 840 + sanc;
        sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration);
        sanc_rate = (double)sanc/act_duration;
    }
    if((sanc_rate<=0.83) & (toy_row(0) < myToys_0.nrow())){
        c_elf_start_time = c_elf_start_time_2;
        c_toy_id = myToys_0(toy_row(0),0);
        c_toy_arrival = myToys_0(toy_row(0),1);
        c_toy_duration = myToys_0(toy_row(0),2);
        toy_row(0) += 1;
    }else{
        c_elf_start_time = c_elf_start_time_2;
        c_toy_id = myToys_1(toy_row(1),0);
        c_toy_arrival = myToys_1(toy_row(1),1);
        c_toy_duration = myToys_1(toy_row(1),2);
        toy_row(1) += 1;
    }
}

//New
if((c_elf_rating == 4.0) & (toy_row(1) < myToys_1.nrow())){
    
    int c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(toy_row(1),1)); //toy1 start time 
    int act_duration = ceil(myToys_1(toy_row(1),2)/c_elf_rating); //toy1 actual work time 
    int sanc = getSanctionedBreakdown(c_elf_start_time_2, act_duration); //toy1 sanctional time
    double sanc_rate = (double)sanc/act_duration;
    //Rcpp::Rcout << '\n' << sanc_rate;
    if(sanc_rate <= 0.83){
        
        int c_elf_start_time_3 = std::max(c_elf_start_time, (int)myToys_0(toy_row(0),1)); //trival start time 
        int act_duration_2 = ceil(myToys_0(toy_row(0),2)/c_elf_rating); //trival actual work time 
        int sanc_2 = getSanctionedBreakdown(c_elf_start_time_3, act_duration); //trival sanctional time
        double sanc_rate_2 = (double)sanc_2/act_duration_2;
        
        if((toy_row(0) < myToys_0.nrow()) && (sanc_rate_2 >= 0.95)){
            c_toy_id = myToys_0(toy_row(0),0);
            c_toy_arrival = myToys_0(toy_row(0),1);
            c_toy_duration = myToys_0(toy_row(0),2);
            toy_row(0) += 1;
        }else{
            c_elf_start_time = 840 + sanc + c_elf_start_time_2;
            c_toy_id = myToys_1(toy_row(1),0);
            c_toy_arrival = myToys_1(toy_row(1),1);
            c_toy_duration = myToys_1(toy_row(1),2);
            toy_row(1) += 1;   
        }
    }else{
        c_toy_id = myToys_1(toy_row(1),0);
        c_toy_arrival = myToys_1(toy_row(1),1);
        c_toy_duration = myToys_1(toy_row(1),2);
        toy_row(1) += 1; 
    }
}