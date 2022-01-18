
load("armdata.RData")

library(rgl)
start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl3 <- cylinder3d(cbind(30, 0, seq(0, 20, length = 10)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = "darkgreen")
shade3d(addNormals(subdivision3d(target_cyl)), col = "darkgreen")
shade3d(addNormals(subdivision3d(cyl1)), col = "pink")
shade3d(addNormals(subdivision3d(cyl2)), col = "pink", alpha = 0.5)
shade3d(addNormals(subdivision3d(cyl3)), col = "lightblue")
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")
#lines3d(armdata[[7]][[1]][[1]])
for (i in 1:10) {
  lines3d(armdata[[7]][[1]][[i]],col = 2)
  lines3d(armdata[[7]][[2]][[i]],col = 3)
  lines3d(armdata[[7]][[3]][[i]],col = 4)
}
#rgl.viewpoint( theta = 0, phi = 15, fov = 60, zoom = 0, interactive = TRUE )
#um <- par3d()$userMatrix
#rgl.viewpoint(userMatrix = um)

#Different setup (1,3)
setup_n <- 1
d <- 15
h <- 20 #(20 cm (S), 27.5 cm (M), 35 cm (T)

start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl3 <- cylinder3d(cbind(d, 0, seq(0, h, length = 10)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = "darkgreen")
shade3d(addNormals(subdivision3d(target_cyl)), col = "darkgreen")
shade3d(addNormals(subdivision3d(cyl1)), col = "pink")
shade3d(addNormals(subdivision3d(cyl2)), col = "pink", alpha = 0.5)
shade3d(addNormals(subdivision3d(cyl3)), col = "lightblue")
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")
#lines3d(armdata[[7]][[1]][[1]])
for (i in 1:10) {
  lines3d(armdata[[setup_n]][[1]][[i]],col = 2)
  lines3d(armdata[[setup_n]][[2]][[i]],col = 3)
  lines3d(armdata[[setup_n]][[3]][[i]],col = 4)
}
#rgl.viewpoint(userMatrix = um)


test_ind_1 <- armdata[[7]][[1]][[1]]

test_ind <- armdata[[7]][[1]]

arr <- array( unlist(test_ind ) , c(100,3,10) )

ind_1_mean <- apply( arr, 1:2 , mean )

errors_1_1 <- rep(NA,10)
errors_1_2 <- rep(NA,10)
errors_1_3 <- rep(NA,10)

for (i in 1:10) {
  errors_1_1[i] <- sum((ind_1_mean - test_ind[[i]])**2)
  errors_1_2[i] <- sum((ind_1_mean - armdata[[7]][[2]][[i]])**2)
  errors_1_3[i] <- sum((ind_1_mean - armdata[[7]][[3]][[i]])**2)
}

boxplot(errors_1_1,errors_1_2,errors_1_3)
#Training on all
test_ind <- armdata[[7]][[1]]

arr <- array( unlist(test_ind ) , c(100,3,10) )

ind_1_mean <- apply( arr, 1:2 , mean )


errors_v1_all <- matrix(, nrow = 10, ncol = 10)
for (per in 1:10) {
  for (i in 1:10){
    errors_v1_all[,per][i] <- mean((ind_1_mean - armdata[[7]][[per]][[i]])**2, na.rm = T)
  }
}

boxplot(errors_v1_all)

#Training on five
test_ind <- armdata[[7]][[1]][1:5]

arr <- array( unlist(test_ind ) , c(100,3,10) )

ind_1_mean <- apply( arr, 1:2 , mean )


errors_v1 <- matrix(, nrow = 10, ncol = 10)
for (per in 1:10) {
  for (i in 6:10){
    errors_v1[,per][i] <- mean((ind_1_mean - armdata[[7]][[per]][[i]])**2, na.rm = T)
  }
}

boxplot(errors_v1[,1:10], main = "Mean square distance from curve")


df_errors_v1 <- as.data.frame(errors_v1_all)

diff_v1 <- c(errors_v1_all)

person <- gl(10,10,labels = c( sprintf("Per %d",seq(1:10))))
trial <- rep(c( sprintf("Trial %d",seq(1:10))),10)

ex1data <- data.frame(Differense = diff_v1,
                       Trial = as.factor(trial),
                       Person = as.factor(person))

#Not appropriate? here, but person is very significant
L1 <- lm(Differense ~ Person + Trial, data = ex1data)
anova(L1)



L2 <- lm(Differense ~ Person + Trial, data = ex1data[11:100,])
anova(L2)


Large <- c(1,4,7,10,13)
Medium <- c(2,5,8,11,14)
Small <- c(3,6,9,12,15)

#Z axis boxplot per size
y <- armdata[[7]][[1]][[1]][,3]
y_L <- array(numeric(),c(5,10,10))
y_M <- array(numeric(),c(5,10,10))
y_S <- array(numeric(),c(5,10,10)) 
for (size in Large){
  for (per in 1:10) {
    for (t in 1:10){
      y_S[(size%/%3)+1,per,t] <- min(armdata[[size]][[per]][[t]][,2])
      y_M[(size%/%3)+1,per,t]  <- min(armdata[[size + 1]][[per]][[t]][,2])
      y_L[(size%/%3)+1,per,t]  <- min(armdata[[size + 2]][[per]][[t]][,2])
    }
  }
}

boxplot(y_L,y_M,y_S)

##
#Y axis boxplot per size
##

y <- armdata[[7]][[1]][[1]][,3]
y_L <- array(numeric(),c(5,10,10))
y_M <- array(numeric(),c(5,10,10))
y_S <- array(numeric(),c(5,10,10))
y_S2 <- array(numeric(),c(5,10,10))
for (size in Large){
  for (per in 1:10) {
    for (t in 1:10){
      y_S[(size%/%3)+1,per,t] <- max(armdata[[size]][[per]][[t]][,3])
      y_M[(size%/%3)+1,per,t]  <- max(armdata[[size + 1]][[per]][[t]][,3])
      y_L[(size%/%3)+1,per,t]  <- max(armdata[[size + 2]][[per]][[t]][,3])
    }
  }
}
#Maximum z-values for different obstacle sizes
boxplot(y_L,y_M,y_S, names = c("T","M","S"))


#Centroid testing

#Training on five
test_ind <- armdata[[7]][[1]][1:5]

arr <- array( unlist(test_ind ) , c(100,3,10) )

ind_1_mean <- apply( arr, 1:2 , mean )
cent_1_mean <- apply( ind_1_mean,2, mean )

errors_v1_cent <- matrix(, nrow = 10, ncol = 10)
for (per in 1:10) {
  for (i in 6:10){
    loop_cent <- apply( armdata[[7]][[per]][[i]],2, mean )
    errors_v1_cent[,per][i] <- dist(rbind(cent_1_mean,loop_cent))
    #errors_v1_cent[,per][i] <- sqrt(sum((cent_1_mean - loop_cent) ^ 2))
  }
}

boxplot(errors_v1_cent[6:10,1:10])

#Checking x only variation
errors_v1_cent <- matrix(, nrow = 10, ncol = 10)
for (per in 1:10) {
  for (i in 6:10){
    ax <- 1
    loop_cent <- apply( armdata[[7]][[per]][[i]],2, mean )
    errors_v1_cent[,per][i] <- dist(rbind(cent_1_mean[ax],loop_cent[ax]))
    #errors_v1_cent[,per][i] <- sqrt(sum((cent_1_mean - loop_cent) ^ 2))
  }
}

boxplot(errors_v1_cent[6:10,1:10])

#1,13

test_ind <- armdata[[1]][[1]][1:5]

arr <- array( unlist(test_ind ) , c(100,3,10) )

ind_1_mean <- apply( arr, 1:2 , mean )
cent_1_mean <- apply( ind_1_mean,2, mean )

errors_v1_cent <- matrix(, nrow = 10, ncol = 10)
for (per in 1:10) {
  for (i in 6:10){
    loop_cent <- apply( armdata[[1]][[per]][[i]],2, mean )
    errors_v1_cent[,per][i] <- dist(rbind(cent_1_mean,loop_cent))
    #errors_v1_cent[,per][i] <- sqrt(sum((cent_1_mean - loop_cent) ^ 2))
  }
}
c(cent_1_mean[1],loop_cent[1])
boxplot(errors_v1_cent[6:10,1:10])

#1,13

test_ind <- armdata[[13]][[1]][1:5]

arr <- array( unlist(test_ind ) , c(100,3,10) )

ind_1_mean <- apply( arr, 1:2 , mean )
cent_1_mean <- apply( ind_1_mean,2, mean )

errors_v1_cent <- matrix(, nrow = 10, ncol = 10)
for (per in 1:10) {
  for (i in 6:10){
    loop_cent <- apply( armdata[[13]][[per]][[i]],2, mean )
    errors_v1_cent[,per][i] <- dist(rbind(cent_1_mean,loop_cent))
    #errors_v1_cent[,per][i] <- sqrt(sum((cent_1_mean - loop_cent) ^ 2))
  }
}
c(cent_1_mean[1],loop_cent[1])
boxplot(errors_v1_cent[6:10,1:10])

#missing data
#armdata[[7]][[9]] #2 first point is missing

arr <- array( unlist(armdata ) , c(16,10,10,100,3) )
arr2 <- array( unlist(armdata ) , c(100,3,10,10,16) )
apply(arr2,1:2,mean) #

which(is.na(armdata[[7]][[9]][[2]]))

#array( unlist(test_ind ) , c(100,3,10) )

is.integer0 <- function(x){is.integer(x) && length(x) == 0L}
for (ex in 1:16){
  for (per in 1:10){
    for (tr in 1:10){
      ts <- which(is.na(armdata[[ex]][[per]][[tr]]))
      if (!is.integer0(ts)){
        print(sprintf("EX: %s, PERS: %s, TRIAL: %s.", ex, per, tr))
        print(ts)
      }
    }
  }
}


#normality of max-y

y <- armdata[[7]][[1]][[1]][,3]
y_L <- array(numeric(),c(5,10,10))
z_L <- array(numeric(),c(5,10,10))
y_M <- array(numeric(),c(5,10,10))
z_M <- array(numeric(),c(5,10,10))
y_S <- array(numeric(),c(5,10,10))
z_S <- array(numeric(),c(5,10,10))
for (size in Large){
  for (per in 1:10) {
    for (t in 1:10){
      y_S[(size%/%3)+1,per,t] <- max(armdata[[size]][[per]][[t]][,3])
      z_S[(size%/%3)+1,per,t] <- max(abs(armdata[[size]][[per]][[t]][,2]))
      y_M[(size%/%3)+1,per,t]  <- max(armdata[[size + 1]][[per]][[t]][,3])
      z_M[(size%/%3)+1,per,t] <- max(abs(armdata[[size + 1]][[per]][[t]][,2]))
      y_L[(size%/%3)+1,per,t]  <- max(armdata[[size + 2]][[per]][[t]][,3])
      z_L[(size%/%3)+1,per,t] <- max(abs(armdata[[size + 2]][[per]][[t]][,2]))
    }
  }
}

plot(z_S,y_S)
boxplot(y_L,y_M,y_S, names = c("Large","Medium","Small"), main = "Maximum z-values of trajectories")
boxplot(z_L,z_M,z_S, names = c("L","M","S"))

qqnorm(y_S)
qqline(y_S)

qqnorm(z_S)
qqline(z_S)

hist(y_L)




##
#QQ of all data raw at a given time
##
ex <-7
per <-1
tr <-1

#flattened matrix
pkts <- 45
pktslist <- c(1,5,30,50,70,95)
for (pkts in pktslist){
  xs <- array(,c(16,10,10))
  ys <- array(,c(16,10,10))
  zs <- array(,c(16,10,10))
  for (ex in 1:16){
    for (per in 1:10){
      for (tr in 1:10){
        dat <- c(armdata[[ex]][[per]][[tr]])
        xs[ex,per,tr] <- dat[pkts]
        ys[ex,per,tr] <- dat[pkts+100]
        zs[ex,per,tr] <- dat[pkts+200]
      }
    }
  }
  par(mfrow=c(3,1))
  qqnorm(c(xs),main = sprintf("Normal Q-Q x of t=%s", pkts))
  qqline(c(xs), col = 2,lwd=1,lty=2)
  qqnorm(c(ys),main = sprintf("Normal Q-Q y of t=%s", pkts))
  qqline(c(ys), col = 2,lwd=1,lty=2)
  qqnorm(c(zs),main = sprintf("Normal Q-Q z of t=%s", pkts))
  qqline(c(zs), col = 2,lwd=1,lty=2)
  
  par(mfrow=c(1,1))
}

##
#y-values histogram
##
zs <- array(,c(100,10,10,16))
for (ex in 1:16){
  for (per in 1:10){
    for (tr in 1:10){
      dat <- armdata[[ex]][[per]][[tr]][,2]
      zs[,tr,per,ex] <- dat
    }
  }
}
#main = "Histogram of y-values",
hist(zs, xlab = "y-value" , probability = T,main = "")

tot <- 16*10*10*100*3
exp_fakt <- 10*10*100*3
per_fakt <- 10*100*3
trl_fakt <- 100*3
flattened_arm <- rep(NA, tot)
Experiment <- rep(NA, tot)
Person <- rep(NA, tot)
Trial <- rep(NA, tot)
Axis <- rep(NA, tot)
Time_idx <- rep(NA, tot)
for (ex in 1:16){
  for (per in 1:10){
    for (tr in 1:10){
      data_loop <- armdata[[ex]][[per]][[tr]]
      idx <- ((ex-1)*exp_fakt + (per-1)*per_fakt + (tr-1)*trl_fakt) + 1
      
      flattened_arm[idx:(idx + 299)] <- c(data_loop[,1], data_loop[,2], data_loop[,3])
      Axis[idx:(idx + 299)] <- gl(3,100,labels = c("x","y","z"))
      Time_idx[idx:(idx + 299)] <- rep(seq(1,100),3)
      Experiment[idx:(idx + 299)] <- rep(ex,300)
      Person[idx:(idx + 299)] <- rep(per,300)
      Trial[idx:(idx + 299)] <- rep(tr,300)
    }
  }
}

armframe <- data.frame(value = flattened_arm,
                       Experiment = as.factor(Experiment),
                       Person = as.factor(Person),
                       Trial = as.factor(Trial),
                       Axis = as.factor(Axis),
                       Time = as.factor(Time_idx))

L1 <- lm(value ~ Experiment + Person + Trial + Axis + Time, data = armframe)
anova(L1)
L2 <- lm(value ~ Experiment + Person + Trial + Time, data = armframe)
anova(L2)
L3 <- lm(value ~ Experiment + Person + Time, data = armframe)
anova(L3)
armframe_z <- armframe[(armframe$Axis == 3),]
armframe_MID <- armframe[(armframe$Time == 50 & armframe$Axis == 3),]

#LZall <- lm(value ~ Experiment + Person + Trial + Time, data = armframe_z)

LX1 <- lm(value ~ Experiment + Person + Trial, data = armframe_MID)
anova(LX1)
plot(aov(LX1),1)
plot(aov(LX1),2)


LX1_1 <- lm(value ~ Experiment * Person + Trial, data = armframe_MID)
library(car)
leveneTest(value ~ Person * Experiment, data = armframe_MID)
anova(LX1_1)
anova(LX1_1,LX1)

LS_1 <- lm(value ~ Experiment + Person, data = armframe_MID)
anova(LS_1)
LS_2 <- lm(value ~ Experiment * Person, data = armframe_MID)
anova(LS_1,LS_2)

qqnorm(residuals(LX1))
abline(0, sd(residuals(LX1)))
plot(fitted(LX1), residuals(LX1))

LX2 <- lm(value ~ Experiment * Person, data = armframe_MID)
anova(LX2)
drop1(LX2, test = "F")





par(mfrow=c(1,1))

prime <- rep(NA,100)
for (i in 1:100){
  poop_list <- armframe[(armframe$Time == i & armframe$Axis == 3),]$value
  prime[i] <- var(poop_list, na.rm = T)
}
plot(prime, ylab = "Varience in z-value", xlab = "Index number")

prime2 <- rep(NA,100)
for (i in 1:100){
  poop_list2 <- armframe[(armframe$Time == i & armframe$Axis == 2),]$value
  prime2[i] <- var(poop_list2, na.rm = T)
}
plot(prime2, ylab = "Varience in y-value", xlab = "Index number")

prime3 <- rep(NA,100)
for (i in 1:100){
  poop_list3 <- armframe[(armframe$Time == i & armframe$Axis == 1),]$value
  prime3[i] <- var(poop_list3, na.rm = T)
}
plot(prime3, ylab = "Varience in x-value", xlab = "Index number")


##
#QQ-plots for selected values
##
par(mfrow=c(1,1))
for (i in c(1,3)){
  for (j in c(4,16)) {
    that <- armframe_MID[(armframe_MID$Person == i & armframe_MID$Experiment == j),]$value
    qqnorm(that, main = sprintf("QQ plot Per. %s Exp. %s", i,j ) ) 
    qqline(that, col = 2,lwd=1,lty=2)
  }
}
