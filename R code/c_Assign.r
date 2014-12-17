require(Rcpp)
cppFunction('double updateProductivity(int start_minute, int work_duration, double current_rating){
            NumericVector out(2);
            int S = 0;
            int U = 0;
            int full_days = work_duration / (60*24);
            S = full_days * (10*60);
            U = full_days * (14*60);
            int remainder = start_minute + full_days * (60*24);
            for (int i = remainder; i < (start_minute+work_duration); ++i) {
            bool isSanctionedTime = ((i - 540) % (60*24)) < 600;
            if (isSanctionedTime) S += 1;
            else U += 1;
            }
            double new_rating = std::max(0.25, std::min(4.0, current_rating * pow(1.02, S / 60.0) * pow(0.90, U / 60.0)));
            return new_rating;
}')

assign_elf <- function(myelves, c_toy_size) {
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 2
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 1
    if(sum(myelves[,'score']==c_toy_size)<1) c_toy_size <- 3
    myelves <- myelves[which(myelves[,'score']==c_toy_size),]
    if(length(myelves)==4){
        assigned_elf <- as.integer(myelves['elf_id'])
    }else{
        assigned_elf <-as.integer(myelves[which.min(myelves[,'next_available_time']) ,'elf_id'][1])
    }
    return(assigned_elf)
}

int assign_elf(int myelves[], int c_toy_size){
    return myelves[1][1]
}

cppFunction('int assign_elf(int myelves[], int c_toy_size){
    return myelves[1];
}')