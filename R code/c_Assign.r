assign_toy <- function(myelves, myToys) {
    
    while (trigger != 1){
        if(c_toy_size %in% myelves[,'Size']){
            myelves <- myelves[which(myelves[,'Size']== c_toy_size),]
            trigger <- 1
        }else{
            
        }
    }
    
    
}

### New structure ###
current_toy <- 1
while(current_toy < 10000000){
    
    assign_elf()
    assign_toy()
    current_toy <- current_toy + 1
}