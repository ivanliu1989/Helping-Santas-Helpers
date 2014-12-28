int index_row, c_elf_start_time_2, act_duration_2, act_duration, c_elf_start_time_3, sanc_2;
            double index_rate, sanc_rate_2;
            
            for(int i = 0; i < myToys_1.nrow(); i++){
                if(myToys_1(i,3)!=1){
                    c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(i,1)); 
                    act_duration = ceil(myToys_1(i,2)/c_elf_rating); 
                    index_rate = updateProductivity(c_elf_start_time_2, act_duration, c_elf_rating);
                    index_row = i;
                }
            }
        
            for(int j = index_row + 1; j < myToys_1.nrow(); j++){
                if(myToys_1(j,3)!=1){
                    c_elf_start_time_2 = std::max(c_elf_start_time, (int)myToys_1(j,1)); 
                    act_duration = ceil(myToys_1(j,2)/c_elf_rating); 
                    if(index_rate < updateProductivity(c_elf_start_time_2, act_duration, c_elf_rating)){
                        index_rate = updateProductivity(c_elf_start_time_2, act_duration, c_elf_rating);
                        index_row = j;
                    }
                }
            }
            if(index_rate < c_elf_rating){
                c_elf_start_time_3 = std::max(c_elf_start_time, (int)myToys_0(toy_row(0),1)); //trival start time 
                act_duration_2 = ceil(myToys_0(toy_row(0),2)/c_elf_rating); //trival actual work time 
                sanc_2 = getSanctionedBreakdown(c_elf_start_time_3, act_duration_2); //trival sanctional time
                sanc_rate_2 = (double)sanc_2/act_duration_2;
                            
                if((toy_row(0) < myToys_0.nrow()) && (sanc_rate_2 >= 0.95)){
                    c_toy_id = myToys_0(toy_row(0),0);
                    c_toy_arrival = myToys_0(toy_row(0),1);
                    c_toy_duration = myToys_0(toy_row(0),2);
                    toy_row(0) += 1;
                }else{
                    c_elf_start_time = 840 + sanc_2 + c_elf_start_time_2;
                                
                    delay_sum += sanc_2;
                                
                    c_toy_id = myToys_1(index_row,0);
                    c_toy_arrival = myToys_1(index_row,1);
                    c_toy_duration = myToys_1(index_row,2);
                    toy_row(1) += 1; 
                    myToys_1(index_row,3) = 1;
                }
            }else{
                c_toy_id = myToys_1(index_row,0);
                c_toy_arrival = myToys_1(index_row,1);
                c_toy_duration = myToys_1(index_row,2);
                toy_row(1) += 1;
                myToys_1(index_row,3) = 1;
            }