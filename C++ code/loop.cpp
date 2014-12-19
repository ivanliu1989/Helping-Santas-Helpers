    if(p<=0.5){
        partition_1 <- max(((np-1)/num)*toy_row + 1, 1) 
        partition_2 <- min((np/num)*toy_row, toy_row) 
        rep_range <- as.integer(partition_1:partition_2)
        
        x1[rep_range] <- sample(x1[rep_range])
    }else{
        partition_1 <- max((Ns-Np),1):min((Ns+Np),toy_row) ## New
        partition_2 <- max((Nd-Np),1):min((Nd+Np),toy_row)
        regulate_rng <- min(length(partition_1),length(partition_2))
        partition_1 <- partition_1[1:regulate_rng]
        partition_2 <- partition_2[1:regulate_rng]
        x1 <- xbest
        ori_partition <- sample(x1[partition_1]) ## New
        des_partition <- sample(x1[partition_2])
        x1[partition_1] <- des_partition
        x1[partition_2] <- ori_partition
    }   


x1 <- xbest    
NumericVector assignX1(NumericVector x1, double p, double np, int toy_row, int num, double Ns, double Np){
    if(p<=0.5){
        int partition_1, partition_2, length;
        NumericVector rep_range;
        partition_1 = ceil(std::max(((np-1)/num)*toy_row + 1, 1));
        partition_2 = ceil(std::min((np/num)*toy_row, toy_row));
        length = partition_2-partition_1;
        for(int i =0; i<=length; ++i){
            rep_range()
        }; 
     
    }else{

    }
}
