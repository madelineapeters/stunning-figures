library(readr)

para_num<-"17"
FLmean<-100
pop_size<-2500
start_wd<-(paste("C:/Users/Madeline/Desktop/Weis lab/EEB498/", paste("para_set", para_num, sep="_"), paste("model_run_1"), sep="/"))
setwd(start_wd)

Fst_tab<-matrix(nrow=11, ncol=4)

for (i in c(1, 50, 100, 150, 200, 250)) {
  
  offspring_map <-  read.csv(paste(getwd(), paste("offspring_map_", i, sep=""), sep="/"))
  FLday_matrix<-matrix(nrow= sqrt(pop_size), ncol= sqrt(pop_size))
  
  for (m in 1:50) {
    for (p in 1:50) {
      FLday_matrix[m,p]<-offspring_map$FLday[((p-1)*50)+m]
    }
  }
############################################################################

  A_matrix<-matrix(nrow= sqrt(pop_size), ncol= sqrt(pop_size))
  
  for (m in 1:50) {
    for (p in 1:50) {
      A_matrix[m,p]<-offspring_map$mapA[((p-1)*50)+m]
    }
  }
  ##############################################################################
  
  B_matrix<-matrix(nrow= sqrt(pop_size), ncol= sqrt(pop_size))
  
  for (m in 1:50) {
    for (p in 1:50) {
      B_matrix[m,p]<-offspring_map$mapB[((p-1)*50)+m]
    }
  }
  ##############################################################################
  
  C_matrix<-matrix(nrow= sqrt(pop_size), ncol= sqrt(pop_size))
  
  for (m in 1:50) {
    for (p in 1:50) {
      C_matrix[m,p]<-offspring_map$mapC[((p-1)*50)+m]
    }
  }
  ##############################################################################
  
  firstqrt<-FLday_matrix[1:25,26:50]
  secondqrt<-FLday_matrix[1:25,1:25]
  thirdqrt<-FLday_matrix[26:50,1:25]
  fourthqrt<-FLday_matrix[26:50,26:50]
  
  mean(firstqrt)
  mean(secondqrt)
  mean(thirdqrt)
  mean(fourthqrt)
  
  (mean(FLday_matrix)-mean(firstqrt))/(mean(FLday_matrix))
  (mean(FLday_matrix)-mean(secondqrt))/(mean(FLday_matrix))
  
  #############################################################################
  
  firstqrt_A<-A_matrix[1:25,26:50]
  secondqrt_A<-A_matrix[1:25,1:25]
  thirdqrt_A<-A_matrix[26:50,1:25]
  fourthqrt_A<-A_matrix[26:50,26:50]
  
  p_total_A<-(sum(A_matrix)/(2*pop_size))
  H_total_A<-2*p_total_A*(1-p_total_A)
  
  p_first_A<-(sum(firstqrt_A)/(pop_size*0.5))
  H_first_A<-2*p_first_A*(1-p_first_A)
  
  p_second_A<-(sum(secondqrt_A)/(pop_size*0.5))
  H_second_A<-2*p_second_A*(1-p_second_A)
  
  p_third_A<-(sum(thirdqrt_A)/(pop_size*0.5))
  H_third_A<-2*p_third_A*(1-p_third_A)
  
  p_fourth_A<-(sum(fourthqrt_A)/(pop_size*0.5))
  H_fourth_A<-2*p_fourth_A*(1-p_fourth_A)
  
  Fst_A<-(H_total_A - mean(H_first_A,H_second_A,H_third_A,H_fourth_A))/(H_total_A)
  if (i==1){
    Fst_tab[1,2]<-Fst_A
    } else {
      Fst_tab[(i/50)+1,2]<-Fst_A
    }
  
  ################################################################################
  
  firstqrt_B<-B_matrix[1:25,26:50]
  secondqrt_B<-B_matrix[1:25,1:25]
  thirdqrt_B<-B_matrix[26:50,1:25]
  fourthqrt_B<-B_matrix[26:50,26:50]
  
  p_total_B<-(sum(B_matrix)/(2*pop_size))
  H_total_B<-2*p_total_B*(1-p_total_B)
  
  p_first_B<-(sum(firstqrt_B)/(pop_size*0.5))
  H_first_B<-2*p_first_B*(1-p_first_B)
  
  p_second_B<-(sum(secondqrt_B)/(pop_size*0.5))
  H_second_B<-2*p_second_B*(1-p_second_B)
  
  p_third_B<-(sum(thirdqrt_B)/(pop_size*0.5))
  H_third_B<-2*p_third_B*(1-p_third_B)
  
  p_fourth_B<-(sum(fourthqrt_B)/(pop_size*0.5))
  H_fourth_B<-2*p_fourth_B*(1-p_fourth_B)
  
  Fst_B<-(H_total_B - mean(H_first_B,H_second_B,H_third_B,H_fourth_B))/(H_total_B)
  if (i==1){
    Fst_tab[1,3]<-Fst_B
    } else {
      Fst_tab[(i/50)+1,3]<-Fst_B
    }
  
  ################################################################################
  
  firstqrt_C<-C_matrix[1:25,26:50]
  secondqrt_C<-C_matrix[1:25,1:25]
  thirdqrt_C<-C_matrix[26:50,1:25]
  fourthqrt_C<-C_matrix[26:50,26:50]
  
  p_total_C<-(sum(C_matrix)/(2*pop_size))
  H_total_C<-2*p_total_C*(1-p_total_C)
  
  p_first_C<-(sum(firstqrt_C)/(pop_size*0.5))
  H_first_C<-2*p_first_C*(1-p_first_C)
  
  p_second_C<-(sum(secondqrt_C)/(pop_size*0.5))
  H_second_C<-2*p_second_C*(1-p_second_C)
  
  p_third_C<-(sum(thirdqrt_C)/(pop_size*0.5))
  H_third_C<-2*p_third_C*(1-p_third_C)
  
  p_fourth_C<-(sum(fourthqrt_C)/(pop_size*0.5))
  H_fourth_C<-2*p_fourth_C*(1-p_fourth_C)
  
  Fst_C<-(H_total_C - mean(H_first_C,H_second_C,H_third_C,H_fourth_C))/(H_total_C)
  if (i==1){
    Fst_tab[1,4]<-Fst_C} else {
      Fst_tab[(i/50)+1,4]<-Fst_C
    }
  
  ################################################################################
  
  firstqrt_FL<-FLday_matrix[1:25,26:50]
  secondqrt_FL<-FLday_matrix[1:25,1:25]
  thirdqrt_FL<-FLday_matrix[26:50,1:25]
  fourthqrt_FL<-FLday_matrix[26:50,26:50]
  
  p_total_FL<-(sum(FLday_matrix)/(2*pop_size))
  H_total_FL<-2*p_total_FL*(1-p_total_FL)
  
  p_first_FL<-(sum(firstqrt_FL)/(pop_size*0.5))
  H_first_FL<-2*p_first_FL*(1-p_first_FL)
  
  p_second_FL<-(sum(secondqrt_FL)/(pop_size*0.5))
  H_second_FL<-2*p_second_FL*(1-p_second_FL)
  
  p_third_FL<-(sum(thirdqrt_FL)/(pop_size*0.5))
  H_third_FL<-2*p_third_FL*(1-p_third_FL)
  
  p_fourth_FL<-(sum(fourthqrt_FL)/(pop_size*0.5))
  H_fourth_FL<-2*p_fourth_FL*(1-p_fourth_FL)
  
  Fst_FL<-(H_total_FL - mean(H_first_FL,H_second_FL,H_third_FL,H_fourth_FL))/(H_total_FL)
  if (i==1){
    Fst_tab[1,1]<-Fst_FL} else {
      Fst_tab[(i/50)+1,1]<-Fst_FL
    }
}

#######################################################################################

write.csv(Fst_tab,
          paste("Fst_over_generations_",para_num, ".csv", sep=""), row.names=F)

#######################################################################################

distance_matrix<-matrix(nrow=pop_size, ncol=pop_size)

for (m in 1:pop_size) {
  for (p in 1:pop_size) {
    
    distance_matrix[m,p]<-sqrt((offspring_map$X_pos[m]-offspring_map$X_pos[p])^2 + (offspring_map$Y_pos[m] - offspring_map$Y_pos[p])^2)	
  }}

FL_matrix<-matrix(nrow=pop_size, ncol=pop_size)

for (m in 1:pop_size) {
  for (p in 1:pop_size) {
    
    FL_matrix[m,p]<-abs((offspring_map$FLday[m]-offspring_map$FLday[p])/FLmean)
  }}

mantel.test(FL_matrix, distance_matrix, nperm=100, alternative="two.sided")

##############################################################################
