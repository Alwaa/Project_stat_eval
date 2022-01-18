load("armdata.RData")


##
test_ind <- armdata[[7]][[1]][1:10]
arr <- array( unlist(test_ind ) , c(100,3,10) )
ind_1_mean <- apply( arr, 1:2 , mean )

test_ind2 <- armdata[[7]][[2]][1:10]
arr2 <- array( unlist(test_ind2 ) , c(100,3,10) )
ind_2_mean <- apply( arr2, 1:2 , mean )

#rank willcox between z-values
wilcox.test(ind_1_mean[,3], ind_2_mean[,3])
t.test(ind_1_mean[,3], ind_2_mean[,3])


##
#Same but for same person different set of trials
##
ra <- sample(10)

test_ind <- armdata[[7]][[1]][ra[1:5]]
arr <- array( unlist(test_ind ) , c(100,3,10) )
ind_1_mean <- apply( arr, 1:2 , mean )

test_ind2 <- armdata[[7]][[1]][ra[6:10]]
arr2 <- array( unlist(test_ind2 ) , c(100,3,10) )
ind_2_mean <- apply( arr2, 1:2 , mean )

#rank willcox between z-values
wilcox.test(ind_1_mean[,3], ind_2_mean[,3])
t.test(ind_1_mean[,3], ind_2_mean[,3])

##
#Final rn trough with select values
##
reps_n <- 40

ex_sample <- sample(1:16,reps_n,replace = T)

p_pals_diff <- rep(NA,reps_n)
for (i in 1:reps_n){
  per_sample <- sample(1:10,2,replace = F)
  
  test_ind <- armdata[[ex_sample[i]]][[per_sample[1]]][1:10]
  arr <- array( unlist(test_ind ) , c(100,3,10) )
  ind_1_mean <- apply( arr, 1:2 , mean )
  
  test_ind2 <- armdata[[ex_sample[i]]][[per_sample[2]]][1:10]
  arr2 <- array( unlist(test_ind2 ) , c(100,3,10) )
  ind_2_mean <- apply( arr2, 1:2 , mean )
  
  #rank willcox between z-values
  p_pals_diff[i] <- wilcox.test(ind_1_mean[,3], ind_2_mean[,3])$p.value
}


plot(sort(p_pals_diff))


per_combs <- combn(10,2)
p_vals_diff <- rep(NA,length(per_combs[1,])*16)
for (ex in 1:16){
  for (i in 1:length(per_combs[1,])){
    pa <- per_combs[1,i]
    pb <- per_combs[2,i]
    
    test_ind <- armdata[[ex]][[pa]][1:10]
    arr <- array( unlist(test_ind ) , c(100,3,10) )
    ind_1_mean <- apply( arr, 1:2, mean, na.rm=TRUE)
    
    test_ind2 <- armdata[[ex]][[pb]][1:10]
    arr2 <- array( unlist(test_ind2 ) , c(100,3,10) )
    ind_2_mean <- apply( arr2, 1:2, mean, na.rm=TRUE)
    
    inxs <- i + 45*(ex-1)
    #rank willcox between z-values
    p_vals_diff[inxs] <- wilcox.test(ind_1_mean[,3], ind_2_mean[,3])$p.value
  }
}

plot(sort(na.omit(p_vals_diff)), xlab = "", ylab = "p-value")
abline(0, 1/length(na.omit(p_vals_diff)), lty = 2)
mean(p_vals_diff < 0.05, na.rm = T)

mean(p.adjust(p_vals_diff, method = "BH") < 0.05, na.rm = T)


