library(readr)
library("ape", lib.loc="~/R/win-library/3.3")

para_num<-"14"
map_num<-"50"
start_wd<-(paste("C:/Users/Madeline/Desktop/Weis lab/EEB498/", paste("para_set", para_num, sep="_"), paste("model_run_1"), sep="/"))
setwd(start_wd)
offspring_map <- read_csv(paste(getwd(), paste("offspring_map", map_num, sep="_"), sep="/"))
    
    offspring_matrix_FL<-as.matrix(dist(cbind(offspring_map$X_pos[], offspring_map$Y_pos)))
    offspring_matrix_FL_inv<-1/offspring_matrix_FL
    diag(offspring_matrix_FL_inv)<-0
    Moran.I(offspring_map$FLday, offspring_matrix_FL_inv)
    
    offspring_matrix_A<-as.matrix(dist(cbind(offspring_map$X_pos, offspring_map$Y_pos)))
    offspring_matrix_A_inv<-1/offspring_matrix_A
    diag(offspring_matrix_A_inv)<-0
    Moran.I(offspring_map$mapA, offspring_matrix_A_inv)
    
    offspring_matrix_B<-as.matrix(dist(cbind(offspring_map$X_pos, offspring_map$Y_pos)))
    offspring_matrix_B_inv<-1/offspring_matrix_B
    diag(offspring_matrix_B_inv)<-0
    Moran.I(offspring_map$mapB, offspring_matrix_B_inv)
    
    offspring_matrix_C<-as.matrix(dist(cbind(offspring_map$X_pos, offspring_map$Y_pos)))
    offspring_matrix_C_inv<-1/offspring_matrix
    diag(offspring_matrix_C_inv)<-0
    Moran.I(offspring_map$mapC, offspring_matrix_C_inv)

