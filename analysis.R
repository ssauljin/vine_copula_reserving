#### Data Preparation ####

library(ChainLadder)

# Paid triangle of Insurance North American Workers' Compensation
losstri1 <- as.triangle(as.matrix(read.csv("table1.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable1 <- as.data.frame(losstri1, na.rm=TRUE)
losstable1$AY <- as.numeric(losstable1$AY)
tcy1 <- subset(losstable1, AY+DL==11)[-c(1,10),3]
lcy1 <- subset(losstable1, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri1))[-(10:11),]
colnames(dd) <- 2:10
atatri1 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable1 <- as.data.frame(atatri1, na.rm=TRUE)
atatable1$AY <- as.numeric(atatable1$AY)
atatable1 <- subset(atatable1, AY+DL<=10)
rm(atatri1, losstable1)

# Paid triangle of Insurance North American General Liability
losstri2 <- as.triangle(as.matrix(read.csv("table2.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable2 <- as.data.frame(losstri2, na.rm=TRUE)
losstable2$AY <- as.numeric(losstable2$AY)
tcy2 <- subset(losstable2, AY+DL==11)[-c(1,10),3]
lcy2 <- subset(losstable2, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri2))[-(10:11),]
colnames(dd) <- 2:10
atatri2 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable2 <- as.data.frame(atatri2, na.rm=TRUE)
atatable2$AY <- as.numeric(atatable2$AY)
atatable2 <- subset(atatable2, AY+DL<=10)
rm(atatri2, losstable2)

# Paid triangle of Insurance North American Other Casualty
losstri3 <- as.triangle(as.matrix(read.csv("table3.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable3 <- as.data.frame(losstri3, na.rm=TRUE)
losstable3$AY <- as.numeric(losstable3$AY)
tcy3 <- subset(losstable3, AY+DL==11)[-c(1,10),3]
lcy3 <- subset(losstable3, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri3))[-(10:11),]
colnames(dd) <- 2:10
atatri3 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable3 <- as.data.frame(atatri3, na.rm=TRUE)
atatable3$AY <- as.numeric(atatable3$AY)
atatable3 <- subset(atatable3, AY+DL<=10)
rm(atatri3, losstable3)

# Paid triangle of Insurance North American Non-Casualty
losstri4 <- as.triangle(as.matrix(read.csv("table4.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable4 <- as.data.frame(losstri4, na.rm=TRUE)
losstable4$AY <- as.numeric(losstable4$AY)
tcy4 <- subset(losstable4, AY+DL==11)[-c(1,10),3]
lcy4 <- subset(losstable4, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri4))[-(10:11),]
colnames(dd) <- 2:10
atatri4 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable4 <- as.data.frame(atatri4, na.rm=TRUE)
atatable4$AY <- as.numeric(atatable4$AY)
atatable4 <- subset(atatable4, AY+DL<=10)
rm(atatri4, losstable4)

# Paid triangle of Insurance Overseas General Casualty
losstri5 <- as.triangle(as.matrix(read.csv("table5.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable5 <- as.data.frame(losstri5, na.rm=TRUE)
losstable5$AY <- as.numeric(losstable5$AY)
tcy5 <- subset(losstable5, AY+DL==11)[-c(1,10),3]
lcy5 <- subset(losstable5, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri5))[-(10:11),]
colnames(dd) <- 2:10
atatri5 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable5 <- as.data.frame(atatri5, na.rm=TRUE)
atatable5$AY <- as.numeric(atatable5$AY)
atatable5 <- subset(atatable5, AY+DL<=10)
rm(atatri5, losstable5)

# Paid triangle of Insurance Overseas General Non-Casualty
losstri6 <- as.triangle(as.matrix(read.csv("table6.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable6 <- as.data.frame(losstri6, na.rm=TRUE)
losstable6$AY <- as.numeric(losstable6$AY)
tcy6 <- subset(losstable6, AY+DL==11)[-c(1,10),3]
lcy6 <- subset(losstable6, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri6))[-(10:11),]
colnames(dd) <- 2:10
atatri6 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable6 <- as.data.frame(atatri6, na.rm=TRUE)
atatable6$AY <- as.numeric(atatable6$AY)
atatable6 <- subset(atatable6, AY+DL<=10)
rm(atatri6, losstable6)

# Paid triangle of Insurance Overseas General Personal Accident
losstri7 <- as.triangle(as.matrix(read.csv("table7.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable7 <- as.data.frame(losstri7, na.rm=TRUE)
losstable7$AY <- as.numeric(losstable7$AY)
tcy7 <- subset(losstable7, AY+DL==11)[-c(1,10),3]
lcy7 <- subset(losstable7, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri7))[-(10:11),]
colnames(dd) <- 2:10
atatri7 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable7 <- as.data.frame(atatri7, na.rm=TRUE)
atatable7$AY <- as.numeric(atatable7$AY)
atatable7 <- subset(atatable7, AY+DL<=10)
rm(atatri7, losstable7)

# Paid triangle of Global Reinsurance Property
losstri8 <- as.triangle(as.matrix(read.csv("table8.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable8 <- as.data.frame(losstri8, na.rm=TRUE)
losstable8$AY <- as.numeric(losstable8$AY)
tcy8 <- subset(losstable8, AY+DL==11)[-c(1,10),3]
lcy8 <- subset(losstable8, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri8))[-(10:11),]
colnames(dd) <- 2:10
atatri8 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable8 <- as.data.frame(atatri8, na.rm=TRUE)
atatable8$AY <- as.numeric(atatable8$AY)
atatable8 <- subset(atatable8, AY+DL<=10)
rm(atatri8, losstable8)

# Paid triangle of Global Reinsurance Non-Property
losstri9 <- as.triangle(as.matrix(read.csv("table9.csv",header=FALSE)), origin = "AY", dev= "DL")
losstable9 <- as.data.frame(losstri9, na.rm=TRUE)
losstable9$AY <- as.numeric(losstable9$AY)
tcy9 <- subset(losstable9, AY+DL==11)[-c(1,10),3]
lcy9 <- subset(losstable9, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstri9))[-(10:11),]
colnames(dd) <- 2:10
atatri9 <- as.triangle(dd, origin = "AY", dev= "DL")
atatable9 <- as.data.frame(atatri9, na.rm=TRUE)
atatable9$AY <- as.numeric(atatable9$AY)
atatable9 <- subset(atatable9, AY+DL<=10)
rm(atatri9, losstable9)

# Aggregation of paid triangles for silo approach
losstriS <- losstri1 + losstri2 + losstri3 + losstri4 + losstri5 + losstri6 + losstri7 + losstri8 + losstri9
losstableS <- as.data.frame(losstriS, na.rm=TRUE)
losstableS$AY <- as.numeric(losstableS$AY)
tcyS <- subset(losstableS, AY+DL==11)[-c(1,10),3]
lcyS <- subset(losstableS, AY+DL==10)[-c(9,10),3]
dd <- as.matrix(ata(losstriS))[-(10:11),]
colnames(dd) <- 2:10
atatriS <- as.triangle(dd, origin = "AY", dev= "DL")
atatableS <- as.data.frame(atatriS, na.rm=TRUE)
atatableS$AY <- as.numeric(atatableS$AY)
atatableS <- subset(atatableS, AY+DL<=10)
rm(atatriS, losstableS, losstriS, losstri1, losstri2, losstri3, losstri4, losstri5, losstri6, losstri7, losstri8, losstri9)

atatable1$DL <- factor(atatable1$DL)
atatable1$AY <- factor(atatable1$AY)
lnm1 <- glm(value ~-1+DL, data=atatable1, family= gaussian(link="log"))

atatable2$DL <- factor(atatable2$DL)
atatable2$AY <- factor(atatable2$AY)
lnm2 <- glm(value ~-1+DL, data=atatable2, family= gaussian(link="log"))

atatable3$DL <- factor(atatable3$DL)
atatable3$AY <- factor(atatable3$AY)
lnm3 <- glm(value ~-1+DL, data=atatable3, family= gaussian(link="log"))

atatable4$DL <- factor(atatable4$DL)
atatable4$AY <- factor(atatable4$AY)
lnm4 <- glm(value ~-1+DL, data=atatable4, family= gaussian(link="log"))

atatable5$DL <- factor(atatable5$DL)
atatable5$AY <- factor(atatable5$AY)
lnm5 <- glm(value ~-1+DL, data=atatable5, family= gaussian(link="log"))

atatable6$DL <- factor(atatable6$DL)
atatable6$AY <- factor(atatable6$AY)
lnm6 <- glm(value ~-1+DL, data=atatable6, family= gaussian(link="log"))

atatable7$DL <- factor(atatable7$DL)
atatable7$AY <- factor(atatable7$AY)
lnm7 <- glm(value ~-1+DL, data=atatable7, family= gaussian(link="log"))

atatable8$DL <- factor(atatable8$DL)
atatable8$AY <- factor(atatable8$AY)
lnm8 <- glm(value ~-1+DL, data=atatable8, family= gaussian(link="log"))

atatable9$DL <- factor(atatable9$DL)
atatable9$AY <- factor(atatable9$AY)
lnm9 <- glm(value ~-1+DL, data=atatable9, family= gaussian(link="log"))

atatableS$DL <- factor(atatableS$DL)
atatableS$AY <- factor(atatableS$AY)
lnmS <- glm(value ~-1+DL, data=atatableS, family= gaussian(link="log"))


library(rstan)
# Input data for RStan fits
dataList1 = list(
  d = atatable1$value,
  N = nrow(atatable1),
  K = ncol(model.matrix(lnm1)),
  x = model.matrix(lnm1)
)

dataList2 = list(
  d = atatable2$value,
  N = nrow(atatable2),
  K = ncol(model.matrix(lnm2)),
  x = model.matrix(lnm2)
)

dataList3 = list(
  d = atatable3$value,
  N = nrow(atatable3),
  K = ncol(model.matrix(lnm3)),
  x = model.matrix(lnm3)
)

dataList4 = list(
  d = atatable4$value,
  N = nrow(atatable4),
  K = ncol(model.matrix(lnm4)),
  x = model.matrix(lnm4)
)

dataList5 = list(
  d = atatable5$value,
  N = nrow(atatable5),
  K = ncol(model.matrix(lnm5)),
  x = model.matrix(lnm5)
)

dataList6 = list(
  d = atatable6$value,
  N = nrow(atatable6),
  K = ncol(model.matrix(lnm6)),
  x = model.matrix(lnm6)
)

dataList7 = list(
  d = atatable7$value,
  N = nrow(atatable7),
  K = ncol(model.matrix(lnm7)),
  x = model.matrix(lnm7)
)

dataList8 = list(
  d = atatable8$value,
  N = nrow(atatable8),
  K = ncol(model.matrix(lnm8)),
  x = model.matrix(lnm8)
)

dataList9 = list(
  d = atatable9$value,
  N = nrow(atatable9),
  K = ncol(model.matrix(lnm9)),
  x = model.matrix(lnm9)
)

dataListS = list(
  d = atatableS$value,
  N = nrow(atatableS),
  K = ncol(model.matrix(lnmS)),
  x = model.matrix(lnmS)
)

#### Constrained Marginal Models ####

# Code for fitting constrained lognormal model
E_LNC.code <-"
 data{
   int N;
   int K;
   matrix[N, K] x;   // predictor matrix
   vector[N] d;
   }
 parameters{
   vector<lower=0>[K] zeta;
   real < lower=0> sigma;
 }
 transformed parameters{
   vector<lower=0>[K] eta;
   vector[N] mu;
   vector[N] logf;
   eta = sum(zeta) - cumulative_sum(zeta) + zeta[1];
   mu = x*eta;
   for (i in 1:N){
     logf[i] = normal_lpdf(log(d[i])|mu[i], sigma);
    }
  }
 model{
   for (i in 1:N){
     target += logf[i];
      }
   }
"
# Fitting constrained lognormal model for nine triangles and silo 
system.time(
  LNC1.stanfit <- stan(model_code=E_LNC.code, data=dataList1, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC1.sim <- extract(LNC1.stanfit, permuted=T)

system.time(
  LNC2.stanfit <- stan(model_code=E_LNC.code, data=dataList2, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC2.sim <- extract(LNC2.stanfit, permuted=T)

system.time(
  LNC3.stanfit <- stan(model_code=E_LNC.code, data=dataList3, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC3.sim <- extract(LNC3.stanfit, permuted=T)

system.time(
  LNC4.stanfit <- stan(model_code=E_LNC.code, data=dataList4, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC4.sim <- extract(LNC4.stanfit, permuted=T)

system.time(
  LNC5.stanfit <- stan(model_code=E_LNC.code, data=dataList5, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC5.sim <- extract(LNC5.stanfit, permuted=T)

system.time(
  LNC6.stanfit <- stan(model_code=E_LNC.code, data=dataList6, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC6.sim <- extract(LNC6.stanfit, permuted=T)

system.time(
  LNC7.stanfit <- stan(model_code=E_LNC.code, data=dataList7, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC7.sim <- extract(LNC7.stanfit, permuted=T)

system.time(
  LNC8.stanfit <- stan(model_code=E_LNC.code, data=dataList8, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC8.sim <- extract(LNC8.stanfit, permuted=T)

system.time(
  LNC9.stanfit <- stan(model_code=E_LNC.code, data=dataList9, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNC9.sim <- extract(LNC9.stanfit, permuted=T)

system.time(
  LNCS.stanfit <- stan(model_code=E_LNC.code, data=dataListS, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNCS.sim <- extract(LNCS.stanfit, permuted=T)


# Computing DICs and LPMLs of constrained lognormal model for nine triangles
loglihood_Gaussian <- function(d, x, eta, sigma) {
  mu <- as.matrix(x) %*% eta
  f  <- dnorm(log(d),mu, sigma)
  
  target <- sum(log(f))
  return(target) }

cpo_Gaussian <- function(eta, sigma, d, x) {
  mu <- as.vector(as.matrix(x) %*% eta)
  f <- dnorm(log(d),mu, sigma)
  target <- length(sigma)/ sum( exp(-log(f)))
  return(target) }


dd1 <- rep(NA,nrow(atatable1))
for (j in 1:nrow(atatable1)) {
  dd1[j] <- cpo_Gaussian(eta=LNC1.sim$eta[j,], sigma=LNC1.sim$sigma[j], d= atatable1$value,
                         x = model.matrix(lnm1))  }
LNC1_LPML <- sum(log(dd1))

LNC1_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC1.sim$eta), sigma=mean(LNC1.sim$sigma), d= atatable1$value,
                                    x = model.matrix(lnm1) )
LNC1_DIC <- 2*LNC1_meanlogli -4*mean(LNC1.sim$logf)*36

dd2 <- rep(NA,nrow(atatable2))
for (j in 1:nrow(atatable2)) {
  dd2[j] <- cpo_Gaussian(eta=LNC2.sim$eta[j,], sigma=LNC2.sim$sigma[j], d= atatable2$value,
                         x = model.matrix(lnm2))  }
LNC2_LPML <- sum(log(dd2))

LNC2_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC2.sim$eta), sigma=mean(LNC2.sim$sigma), d= atatable2$value,
                                    x = model.matrix(lnm2) )
LNC2_DIC <- 2*LNC2_meanlogli -4*mean(LNC2.sim$logf)*36



dd3 <- rep(NA,nrow(atatable3))
for (j in 1:nrow(atatable3)) {
  dd3[j] <- cpo_Gaussian(eta=LNC3.sim$eta[j,], sigma=LNC3.sim$sigma[j], d= atatable3$value,
                         x = model.matrix(lnm3))  }
LNC3_LPML <- sum(log(dd3))

LNC3_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC3.sim$eta), sigma=mean(LNC3.sim$sigma), d= atatable3$value,
                                    x = model.matrix(lnm3) )
LNC3_DIC <- 2*LNC3_meanlogli -4*mean(LNC3.sim$logf)*36



dd4 <- rep(NA,nrow(atatable4))
for (j in 1:nrow(atatable4)) {
  dd4[j] <- cpo_Gaussian(eta=LNC4.sim$eta[j,], sigma=LNC4.sim$sigma[j], d= atatable4$value,
                         x = model.matrix(lnm4))  }
LNC4_LPML <- sum(log(dd4))

LNC4_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC4.sim$eta), sigma=mean(LNC4.sim$sigma), d= atatable4$value,
                                    x = model.matrix(lnm4) )
LNC4_DIC <- 2*LNC4_meanlogli -4*mean(LNC4.sim$logf)*36

dd5 <- rep(NA,nrow(atatable5))
for (j in 1:nrow(atatable5)) {
  dd5[j] <- cpo_Gaussian(eta=LNC5.sim$eta[j,], sigma=LNC5.sim$sigma[j], d= atatable5$value,
                         x = model.matrix(lnm5))  }
LNC5_LPML <- sum(log(dd5))

LNC5_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC5.sim$eta), sigma=mean(LNC5.sim$sigma), d= atatable5$value,
                                    x = model.matrix(lnm5) )
LNC5_DIC <- 2*LNC5_meanlogli -4*mean(LNC5.sim$logf)*36

dd6 <- rep(NA,nrow(atatable6))
for (j in 1:nrow(atatable6)) {
  dd6[j] <- cpo_Gaussian(eta=LNC6.sim$eta[j,], sigma=LNC6.sim$sigma[j], d= atatable6$value,
                         x = model.matrix(lnm6))  }
LNC6_LPML <- sum(log(dd6))

LNC6_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC6.sim$eta), sigma=mean(LNC6.sim$sigma), d= atatable6$value,
                                    x = model.matrix(lnm6) )
LNC6_DIC <- 2*LNC6_meanlogli -4*mean(LNC6.sim$logf)*36

dd7 <- rep(NA,nrow(atatable7))
for (j in 1:nrow(atatable7)) {
  dd7[j] <- cpo_Gaussian(eta=LNC7.sim$eta[j,], sigma=LNC7.sim$sigma[j], d= atatable7$value,
                         x = model.matrix(lnm7))  }
LNC7_LPML <- sum(log(dd7))

LNC7_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC7.sim$eta), sigma=mean(LNC7.sim$sigma), d= atatable7$value,
                                    x = model.matrix(lnm7) )
LNC7_DIC <- 2*LNC7_meanlogli -4*mean(LNC7.sim$logf)*36

dd8 <- rep(NA,nrow(atatable8))
for (j in 1:nrow(atatable8)) {
  dd8[j] <- cpo_Gaussian(eta=LNC8.sim$eta[j,], sigma=LNC8.sim$sigma[j], d= atatable8$value,
                         x = model.matrix(lnm8))  }
LNC8_LPML <- sum(log(dd8))

LNC8_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC8.sim$eta), sigma=mean(LNC8.sim$sigma), d= atatable8$value,
                                    x = model.matrix(lnm8) )
LNC8_DIC <- 2*LNC8_meanlogli -4*mean(LNC8.sim$logf)*36

dd9 <- rep(NA,nrow(atatable9))
for (j in 1:nrow(atatable9)) {
  dd9[j] <- cpo_Gaussian(eta=LNC9.sim$eta[j,], sigma=LNC9.sim$sigma[j], d= atatable9$value,
                         x = model.matrix(lnm9))  }
LNC9_LPML <- sum(log(dd9))

LNC9_meanlogli <- loglihood_Gaussian(eta=colMeans(LNC9.sim$eta), sigma=mean(LNC9.sim$sigma), d= atatable9$value,
                                    x = model.matrix(lnm9) )
LNC9_DIC <- 2*LNC9_meanlogli -4*mean(LNC9.sim$logf)*36

ddS <- rep(NA,nrow(atatableS))
for (j in 1:nrow(atatableS)) {
  ddS[j] <- cpo_Gaussian(eta=LNCS.sim$eta[j,], sigma=LNCS.sim$sigma[j], d= atatableS$value,
                         x = model.matrix(lnmS))  }
LNCS_LPML <- sum(log(ddS))

LNCS_meanlogli <- loglihood_Gaussian(eta=colMeans(LNCS.sim$eta), sigma=mean(LNCS.sim$sigma), d= atatableS$value,
                                    x = model.matrix(lnmS) )
LNCS_DIC <- 2*LNCS_meanlogli -4*mean(LNCS.sim$logf)*36

# Code for fitting constrained gamma model 
E_GamC.code <-"
 data{
   int N;
   int K;
   matrix[N, K] x;   // predictor matrix
   vector[N] d;
   }
 parameters{
   vector<lower=0>[K] zeta;
   real < lower=0> phi;
 }
 transformed parameters{
   vector<lower=0>[K] eta;
   vector[N] mu;
   vector[N] logf;
   eta = sum(zeta) - cumulative_sum(zeta) + zeta[1];
   mu = exp(x*eta);
   for (i in 1:N){
     logf[i] = gamma_lpdf( d[i] | phi, phi/mu[i]);
    }
  }
 model{
   for (i in 1:N){
     target += logf[i];
      }
   }
"

# Fitting constrained gamma model for nine triangles and silo 
system.time(
  GamC1.stanfit <- stan(model_code=E_GamC.code, data=dataList1, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC1.sim <- extract(GamC1.stanfit, permuted=T)

system.time(
  GamC2.stanfit <- stan(model_code=E_GamC.code, data=dataList2, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC2.sim <- extract(GamC2.stanfit, permuted=T)

system.time(
  GamC3.stanfit <- stan(model_code=E_GamC.code, data=dataList3, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC3.sim <- extract(GamC3.stanfit, permuted=T)

system.time(
  GamC4.stanfit <- stan(model_code=E_GamC.code, data=dataList4, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC4.sim <- extract(GamC4.stanfit, permuted=T)

system.time(
  GamC5.stanfit <- stan(model_code=E_GamC.code, data=dataList5, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC5.sim <- extract(GamC5.stanfit, permuted=T)

system.time(
  GamC6.stanfit <- stan(model_code=E_GamC.code, data=dataList6, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC6.sim <- extract(GamC6.stanfit, permuted=T)

system.time(
  GamC7.stanfit <- stan(model_code=E_GamC.code, data=dataList7, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC7.sim <- extract(GamC7.stanfit, permuted=T)

system.time(
  GamC8.stanfit <- stan(model_code=E_GamC.code, data=dataList8, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC8.sim <- extract(GamC8.stanfit, permuted=T)

system.time(
  GamC9.stanfit <- stan(model_code=E_GamC.code, data=dataList9, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamC9.sim <- extract(GamC9.stanfit, permuted=T)

system.time(
  GamCS.stanfit <- stan(model_code=E_GamC.code, data=dataListS, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamCS.sim <- extract(GamCS.stanfit, permuted=T)

# Computing DICs and LPMLs of constrained gamma model for nine triangles

loglihood_GamCma <- function(d, x, eta, phi) {
  mu <- exp(as.matrix(x) %*% eta)
  f  <- dgamma(d,shape=phi,scale= mu/phi)
  target <- sum(log(f))
  return(target) }

cpo_GamCma <- function(eta, phi, d, x) {
  mu <- as.vector(exp(as.matrix(x) %*% eta))
  f <- dgamma(d, shape=phi, scale= mu/phi)
  target <- length(phi)/ sum( exp(-log(f)))
  return(target) }

dd1 <- rep(NA,nrow(atatable1))
for (j in 1:nrow(atatable1)) {
  dd1[j] <- cpo_GamCma(eta=GamC1.sim$eta[j,], phi=GamC1.sim$phi[j], d= atatable1$value,
                      x = model.matrix(lnm1))  }
GamC1_LPML <- sum(log(dd1))

GamC1_meanlogli <- loglihood_GamCma(eta=colMeans(GamC1.sim$eta), phi=mean(GamC1.sim$phi), d= atatable1$value,
                                  x = model.matrix(lnm1) )
GamC1_DIC <- 2*GamC1_meanlogli -4*mean(GamC1.sim$logf)*36

dd2 <- rep(NA,nrow(atatable2))
for (j in 1:nrow(atatable2)) {
  dd2[j] <- cpo_GamCma(eta=GamC2.sim$eta[j,], phi=GamC2.sim$phi[j], d= atatable2$value,
                      x = model.matrix(lnm2))  }
GamC2_LPML <- sum(log(dd2))

GamC2_meanlogli <- loglihood_GamCma(eta=colMeans(GamC2.sim$eta), phi=mean(GamC2.sim$phi), d= atatable2$value,
                                  x = model.matrix(lnm2) )
GamC2_DIC <- 2*GamC2_meanlogli -4*mean(GamC2.sim$logf)*36

dd3 <- rep(NA,nrow(atatable3))
for (j in 1:nrow(atatable3)) {
  dd3[j] <- cpo_GamCma(eta=GamC3.sim$eta[j,], phi=GamC3.sim$phi[j], d= atatable3$value,
                         x = model.matrix(lnm3))  }
GamC3_LPML <- sum(log(dd3))

GamC3_meanlogli <- loglihood_GamCma(eta=colMeans(GamC3.sim$eta), phi=mean(GamC3.sim$phi), d= atatable3$value,
                                    x = model.matrix(lnm3) )
GamC3_DIC <- 2*GamC3_meanlogli -4*mean(GamC3.sim$logf)*36

dd4 <- rep(NA,nrow(atatable4))
for (j in 1:nrow(atatable4)) {
  dd4[j] <- cpo_GamCma(eta=GamC4.sim$eta[j,], phi=GamC4.sim$phi[j], d= atatable4$value,
                      x = model.matrix(lnm4))  }
GamC4_LPML <- sum(log(dd4))

GamC4_meanlogli <- loglihood_GamCma(eta=colMeans(GamC4.sim$eta), phi=mean(GamC4.sim$phi), d= atatable4$value,
                                  x = model.matrix(lnm4) )
GamC4_DIC <- 2*GamC4_meanlogli -4*mean(GamC4.sim$logf)*36

dd5 <- rep(NA,nrow(atatable5))
for (j in 1:nrow(atatable5)) {
  dd5[j] <- cpo_GamCma(eta=GamC5.sim$eta[j,], phi=GamC5.sim$phi[j], d= atatable5$value,
                      x = model.matrix(lnm5))  }
GamC5_LPML <- sum(log(dd5))

GamC5_meanlogli <- loglihood_GamCma(eta=colMeans(GamC5.sim$eta), phi=mean(GamC5.sim$phi), d= atatable5$value,
                                  x = model.matrix(lnm5) )
GamC5_DIC <- 2*GamC5_meanlogli -4*mean(GamC5.sim$logf)*36

dd6 <- rep(NA,nrow(atatable6))
for (j in 1:nrow(atatable6)) {
  dd6[j] <- cpo_GamCma(eta=GamC6.sim$eta[j,], phi=GamC6.sim$phi[j], d= atatable6$value,
                      x = model.matrix(lnm6))  }
GamC6_LPML <- sum(log(dd6))

GamC6_meanlogli <- loglihood_GamCma(eta=colMeans(GamC6.sim$eta), phi=mean(GamC6.sim$phi), d= atatable6$value,
                                  x = model.matrix(lnm6) )
GamC6_DIC <- 2*GamC6_meanlogli -4*mean(GamC6.sim$logf)*36

dd7 <- rep(NA,nrow(atatable7))
for (j in 1:nrow(atatable7)) {
  dd7[j] <- cpo_GamCma(eta=GamC7.sim$eta[j,], phi=GamC7.sim$phi[j], d= atatable7$value,
                      x = model.matrix(lnm7))  }
GamC7_LPML <- sum(log(dd7))

GamC7_meanlogli <- loglihood_GamCma(eta=colMeans(GamC7.sim$eta), phi=mean(GamC7.sim$phi), d= atatable7$value,
                                  x = model.matrix(lnm7) )
GamC7_DIC <- 2*GamC7_meanlogli -4*mean(GamC7.sim$logf)*36

dd8 <- rep(NA,nrow(atatable8))
for (j in 1:nrow(atatable8)) {
  dd8[j] <- cpo_GamCma(eta=GamC8.sim$eta[j,], phi=GamC8.sim$phi[j], d= atatable8$value,
                      x = model.matrix(lnm8))  }
GamC8_LPML <- sum(log(dd8))

GamC8_meanlogli <- loglihood_GamCma(eta=colMeans(GamC8.sim$eta), phi=mean(GamC8.sim$phi), d= atatable8$value,
                                  x = model.matrix(lnm8) )
GamC8_DIC <- 2*GamC8_meanlogli -4*mean(GamC8.sim$logf)*36

dd9 <- rep(NA,nrow(atatable9))
for (j in 1:nrow(atatable9)) {
  dd9[j] <- cpo_GamCma(eta=GamC9.sim$eta[j,], phi=GamC9.sim$phi[j], d= atatable9$value,
                      x = model.matrix(lnm9))  }
GamC9_LPML <- sum(log(dd9))

GamC9_meanlogli <- loglihood_GamCma(eta=colMeans(GamC9.sim$eta), phi=mean(GamC9.sim$phi), d= atatable9$value,
                                  x = model.matrix(lnm9) )
GamC9_DIC <- 2*GamC9_meanlogli -4*mean(GamC9.sim$logf)*36

ddS <- rep(NA,nrow(atatableS))
for (j in 1:nrow(atatableS)) {
  ddS[j] <- cpo_GamCma(eta=GamCS.sim$eta[j,], phi=GamCS.sim$phi[j], d= atatableS$value,
                      x = model.matrix(lnmS))  }
GamCS_LPML <- sum(log(ddS))

GamCS_meanlogli <- loglihood_GamCma(eta=colMeans(GamCS.sim$eta), phi=mean(GamCS.sim$phi), d= atatableS$value,
                                  x = model.matrix(lnmS) )
GamCS_DIC <- 2*GamCS_meanlogli -4*mean(GamCS.sim$logf)*36


#### Unconstrained Marginal Models ####

# Code for fitting unconstrained gamma model and silo 
E_LNU.code <-"
 data{
   int N;
   int K;
   matrix[N, K] x;   // predictor matrix
   vector[N] d;
   }
 parameters{
   vector<lower=0>[K] eta;
   real < lower=0> sigma;
 }
 transformed parameters{
   vector[N] mu;
   vector[N] logf;
   mu = x*eta;
   for (i in 1:N){
     logf[i] = normal_lpdf(log(d[i])|mu[i], sigma);
    }
  }
 model{
   for (i in 1:N){
     target += logf[i];
      }
   }
"

# Fitting unconstrained lognormal model for nine triangles and silo
system.time(
  LNU1.stanfit <- stan(model_code=E_LNU.code, data=dataList1, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU1.sim <- extract(LNU1.stanfit, permuted=T)

system.time(
  LNU2.stanfit <- stan(model_code=E_LNU.code, data=dataList2, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU2.sim <- extract(LNU2.stanfit, permuted=T)

system.time(
  LNU3.stanfit <- stan(model_code=E_LNU.code, data=dataList3, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU3.sim <- extract(LNU3.stanfit, permuted=T)

system.time(
  LNU4.stanfit <- stan(model_code=E_LNU.code, data=dataList4, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU4.sim <- extract(LNU4.stanfit, permuted=T)

system.time(
  LNU5.stanfit <- stan(model_code=E_LNU.code, data=dataList5, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU5.sim <- extract(LNU5.stanfit, permuted=T)

system.time(
  LNU6.stanfit <- stan(model_code=E_LNU.code, data=dataList6, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU6.sim <- extract(LNU6.stanfit, permuted=T)

system.time(
  LNU7.stanfit <- stan(model_code=E_LNU.code, data=dataList7, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU7.sim <- extract(LNU7.stanfit, permuted=T)

system.time(
  LNU8.stanfit <- stan(model_code=E_LNU.code, data=dataList8, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU8.sim <- extract(LNU8.stanfit, permuted=T)

system.time(
  LNU9.stanfit <- stan(model_code=E_LNU.code, data=dataList9, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNU9.sim <- extract(LNU9.stanfit, permuted=T)

system.time(
  LNUS.stanfit <- stan(model_code=E_LNU.code, data=dataListS, iter=1000, chains=4,seed=500, pars = c("eta","sigma","logf"))
)
LNUS.sim <- extract(LNUS.stanfit, permuted=T)

# Computing DICs and LPMLs of unconstrained lognormal model for nine triangles
loglihood_Gaussian <- function(d, x, eta, sigma) {
  mu <- as.matrix(x) %*% eta
  f  <- dnorm(log(d),mu, sigma)
  
  target <- sum(log(f))
  return(target) }

cpo_Gaussian <- function(eta, sigma, d, x) {
  mu <- as.vector(as.matrix(x) %*% eta)
  f <- dnorm(log(d),mu, sigma)
  target <- length(sigma)/ sum( exp(-log(f)))
  return(target) }


dd1 <- rep(NA,nrow(atatable1))
for (j in 1:nrow(atatable1)) {
  dd1[j] <- cpo_Gaussian(eta=LNU1.sim$eta[j,], sigma=LNU1.sim$sigma[j], d= atatable1$value,
                         x = model.matrix(lnm1))  }
LNU1_LPML <- sum(log(dd1))

LNU1_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU1.sim$eta), sigma=mean(LNU1.sim$sigma), d= atatable1$value,
                                     x = model.matrix(lnm1) )
LNU1_DIC <- 2*LNU1_meanlogli -4*mean(LNU1.sim$logf)*36

dd2 <- rep(NA,nrow(atatable2))
for (j in 1:nrow(atatable2)) {
  dd2[j] <- cpo_Gaussian(eta=LNU2.sim$eta[j,], sigma=LNU2.sim$sigma[j], d= atatable2$value,
                         x = model.matrix(lnm2))  }
LNU2_LPML <- sum(log(dd2))

LNU2_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU2.sim$eta), sigma=mean(LNU2.sim$sigma), d= atatable2$value,
                                     x = model.matrix(lnm2) )
LNU2_DIC <- 2*LNU2_meanlogli -4*mean(LNU2.sim$logf)*36



dd3 <- rep(NA,nrow(atatable3))
for (j in 1:nrow(atatable3)) {
  dd3[j] <- cpo_Gaussian(eta=LNU3.sim$eta[j,], sigma=LNU3.sim$sigma[j], d= atatable3$value,
                         x = model.matrix(lnm3))  }
LNU3_LPML <- sum(log(dd3))

LNU3_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU3.sim$eta), sigma=mean(LNU3.sim$sigma), d= atatable3$value,
                                     x = model.matrix(lnm3) )
LNU3_DIC <- 2*LNU3_meanlogli -4*mean(LNU3.sim$logf)*36



dd4 <- rep(NA,nrow(atatable4))
for (j in 1:nrow(atatable4)) {
  dd4[j] <- cpo_Gaussian(eta=LNU4.sim$eta[j,], sigma=LNU4.sim$sigma[j], d= atatable4$value,
                         x = model.matrix(lnm4))  }
LNU4_LPML <- sum(log(dd4))

LNU4_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU4.sim$eta), sigma=mean(LNU4.sim$sigma), d= atatable4$value,
                                     x = model.matrix(lnm4) )
LNU4_DIC <- 2*LNU4_meanlogli -4*mean(LNU4.sim$logf)*36

dd5 <- rep(NA,nrow(atatable5))
for (j in 1:nrow(atatable5)) {
  dd5[j] <- cpo_Gaussian(eta=LNU5.sim$eta[j,], sigma=LNU5.sim$sigma[j], d= atatable5$value,
                         x = model.matrix(lnm5))  }
LNU5_LPML <- sum(log(dd5))

LNU5_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU5.sim$eta), sigma=mean(LNU5.sim$sigma), d= atatable5$value,
                                     x = model.matrix(lnm5) )
LNU5_DIC <- 2*LNU5_meanlogli -4*mean(LNU5.sim$logf)*36

dd6 <- rep(NA,nrow(atatable6))
for (j in 1:nrow(atatable6)) {
  dd6[j] <- cpo_Gaussian(eta=LNU6.sim$eta[j,], sigma=LNU6.sim$sigma[j], d= atatable6$value,
                         x = model.matrix(lnm6))  }
LNU6_LPML <- sum(log(dd6))

LNU6_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU6.sim$eta), sigma=mean(LNU6.sim$sigma), d= atatable6$value,
                                     x = model.matrix(lnm6) )
LNU6_DIC <- 2*LNU6_meanlogli -4*mean(LNU6.sim$logf)*36

dd7 <- rep(NA,nrow(atatable7))
for (j in 1:nrow(atatable7)) {
  dd7[j] <- cpo_Gaussian(eta=LNU7.sim$eta[j,], sigma=LNU7.sim$sigma[j], d= atatable7$value,
                         x = model.matrix(lnm7))  }
LNU7_LPML <- sum(log(dd7))

LNU7_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU7.sim$eta), sigma=mean(LNU7.sim$sigma), d= atatable7$value,
                                     x = model.matrix(lnm7) )
LNU7_DIC <- 2*LNU7_meanlogli -4*mean(LNU7.sim$logf)*36

dd8 <- rep(NA,nrow(atatable8))
for (j in 1:nrow(atatable8)) {
  dd8[j] <- cpo_Gaussian(eta=LNU8.sim$eta[j,], sigma=LNU8.sim$sigma[j], d= atatable8$value,
                         x = model.matrix(lnm8))  }
LNU8_LPML <- sum(log(dd8))

LNU8_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU8.sim$eta), sigma=mean(LNU8.sim$sigma), d= atatable8$value,
                                     x = model.matrix(lnm8) )
LNU8_DIC <- 2*LNU8_meanlogli -4*mean(LNU8.sim$logf)*36

dd9 <- rep(NA,nrow(atatable9))
for (j in 1:nrow(atatable9)) {
  dd9[j] <- cpo_Gaussian(eta=LNU9.sim$eta[j,], sigma=LNU9.sim$sigma[j], d= atatable9$value,
                         x = model.matrix(lnm9))  }
LNU9_LPML <- sum(log(dd9))

LNU9_meanlogli <- loglihood_Gaussian(eta=colMeans(LNU9.sim$eta), sigma=mean(LNU9.sim$sigma), d= atatable9$value,
                                     x = model.matrix(lnm9) )
LNU9_DIC <- 2*LNU9_meanlogli -4*mean(LNU9.sim$logf)*36

ddS <- rep(NA,nrow(atatableS))
for (j in 1:nrow(atatableS)) {
  ddS[j] <- cpo_Gaussian(eta=LNUS.sim$eta[j,], sigma=LNUS.sim$sigma[j], d= atatableS$value,
                         x = model.matrix(lnmS))  }
LNUS_LPML <- sum(log(ddS))

LNUS_meanlogli <- loglihood_Gaussian(eta=colMeans(LNUS.sim$eta), sigma=mean(LNUS.sim$sigma), d= atatableS$value,
                                     x = model.matrix(lnmS) )
LNUS_DIC <- 2*LNUS_meanlogli -4*mean(LNUS.sim$logf)*36

# Code for fitting unconstrained gamma model 
E_GamU.code <-"
 data{
   int N;
   int K;
   matrix[N, K] x;   // predictor matrix
   vector[N] d;
   }
 parameters{
   vector<lower=0>[K] eta;
   real < lower=0> phi;
 }
 transformed parameters{
   vector[N] mu;
   vector[N] logf;
   mu = exp(x*eta);
   for (i in 1:N){
     logf[i] = gamma_lpdf( d[i] | phi, phi/mu[i]);
    }
  }
 model{
   for (i in 1:N){
     target += logf[i];
      }
   }
"

# Fitting unconstrained gamma model for nine triangles and silo
system.time(
  GamU1.stanfit <- stan(model_code=E_GamU.code, data=dataList1, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU1.sim <- extract(GamU1.stanfit, permuted=T)

system.time(
  GamU2.stanfit <- stan(model_code=E_GamU.code, data=dataList2, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU2.sim <- extract(GamU2.stanfit, permuted=T)

system.time(
  GamU3.stanfit <- stan(model_code=E_GamU.code, data=dataList3, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU3.sim <- extract(GamU3.stanfit, permuted=T)

system.time(
  GamU4.stanfit <- stan(model_code=E_GamU.code, data=dataList4, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU4.sim <- extract(GamU4.stanfit, permuted=T)

system.time(
  GamU5.stanfit <- stan(model_code=E_GamU.code, data=dataList5, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU5.sim <- extract(GamU5.stanfit, permuted=T)

system.time(
  GamU6.stanfit <- stan(model_code=E_GamU.code, data=dataList6, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU6.sim <- extract(GamU6.stanfit, permuted=T)

system.time(
  GamU7.stanfit <- stan(model_code=E_GamU.code, data=dataList7, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU7.sim <- extract(GamU7.stanfit, permuted=T)

system.time(
  GamU8.stanfit <- stan(model_code=E_GamU.code, data=dataList8, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU8.sim <- extract(GamU8.stanfit, permuted=T)

system.time(
  GamU9.stanfit <- stan(model_code=E_GamU.code, data=dataList9, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamU9.sim <- extract(GamU9.stanfit, permuted=T)

system.time(
  GamUS.stanfit <- stan(model_code=E_GamU.code, data=dataListS, iter=1000, chains=4,seed=500, pars = c("eta","phi","logf"))
)
GamUS.sim <- extract(GamUS.stanfit, permuted=T)

# Computing DICs and LPMLs of unconstrained gamma model for nine triangles
loglihood_GamUma <- function(d, x, eta, phi) {
  mu <- exp(as.matrix(x) %*% eta)
  f  <- dgamma(d,shape=phi,scale= mu/phi)
  target <- sum(log(f))
  return(target) }

cpo_GamUma <- function(eta, phi, d, x) {
  mu <- as.vector(exp(as.matrix(x) %*% eta))
  f <- dgamma(d, shape=phi, scale= mu/phi)
  target <- length(phi)/ sum( exp(-log(f)))
  return(target) }

dd1 <- rep(NA,nrow(atatable1))
for (j in 1:nrow(atatable1)) {
  dd1[j] <- cpo_GamUma(eta=GamU1.sim$eta[j,], phi=GamU1.sim$phi[j], d= atatable1$value,
                       x = model.matrix(lnm1))  }
GamU1_LPML <- sum(log(dd1))

GamU1_meanlogli <- loglihood_GamUma(eta=colMeans(GamU1.sim$eta), phi=mean(GamU1.sim$phi), d= atatable1$value,
                                    x = model.matrix(lnm1) )
GamU1_DIC <- 2*GamU1_meanlogli -4*mean(GamU1.sim$logf)*36

dd2 <- rep(NA,nrow(atatable2))
for (j in 1:nrow(atatable2)) {
  dd2[j] <- cpo_GamUma(eta=GamU2.sim$eta[j,], phi=GamU2.sim$phi[j], d= atatable2$value,
                       x = model.matrix(lnm2))  }
GamU2_LPML <- sum(log(dd2))

GamU2_meanlogli <- loglihood_GamUma(eta=colMeans(GamU2.sim$eta), phi=mean(GamU2.sim$phi), d= atatable2$value,
                                    x = model.matrix(lnm2) )
GamU2_DIC <- 2*GamU2_meanlogli -4*mean(GamU2.sim$logf)*36

dd3 <- rep(NA,nrow(atatable3))
for (j in 1:nrow(atatable3)) {
  dd3[j] <- cpo_GamUma(eta=GamU3.sim$eta[j,], phi=GamU3.sim$phi[j], d= atatable3$value,
                       x = model.matrix(lnm3))  }
GamU3_LPML <- sum(log(dd3))

GamU3_meanlogli <- loglihood_GamUma(eta=colMeans(GamU3.sim$eta), phi=mean(GamU3.sim$phi), d= atatable3$value,
                                    x = model.matrix(lnm3) )
GamU3_DIC <- 2*GamU3_meanlogli -4*mean(GamU3.sim$logf)*36

dd4 <- rep(NA,nrow(atatable4))
for (j in 1:nrow(atatable4)) {
  dd4[j] <- cpo_GamUma(eta=GamU4.sim$eta[j,], phi=GamU4.sim$phi[j], d= atatable4$value,
                       x = model.matrix(lnm4))  }
GamU4_LPML <- sum(log(dd4))

GamU4_meanlogli <- loglihood_GamUma(eta=colMeans(GamU4.sim$eta), phi=mean(GamU4.sim$phi), d= atatable4$value,
                                    x = model.matrix(lnm4) )
GamU4_DIC <- 2*GamU4_meanlogli -4*mean(GamU4.sim$logf)*36

dd5 <- rep(NA,nrow(atatable5))
for (j in 1:nrow(atatable5)) {
  dd5[j] <- cpo_GamUma(eta=GamU5.sim$eta[j,], phi=GamU5.sim$phi[j], d= atatable5$value,
                       x = model.matrix(lnm5))  }
GamU5_LPML <- sum(log(dd5))

GamU5_meanlogli <- loglihood_GamUma(eta=colMeans(GamU5.sim$eta), phi=mean(GamU5.sim$phi), d= atatable5$value,
                                    x = model.matrix(lnm5) )
GamU5_DIC <- 2*GamU5_meanlogli -4*mean(GamU5.sim$logf)*36

dd6 <- rep(NA,nrow(atatable6))
for (j in 1:nrow(atatable6)) {
  dd6[j] <- cpo_GamUma(eta=GamU6.sim$eta[j,], phi=GamU6.sim$phi[j], d= atatable6$value,
                       x = model.matrix(lnm6))  }
GamU6_LPML <- sum(log(dd6))

GamU6_meanlogli <- loglihood_GamUma(eta=colMeans(GamU6.sim$eta), phi=mean(GamU6.sim$phi), d= atatable6$value,
                                    x = model.matrix(lnm6) )
GamU6_DIC <- 2*GamU6_meanlogli -4*mean(GamU6.sim$logf)*36

dd7 <- rep(NA,nrow(atatable7))
for (j in 1:nrow(atatable7)) {
  dd7[j] <- cpo_GamUma(eta=GamU7.sim$eta[j,], phi=GamU7.sim$phi[j], d= atatable7$value,
                       x = model.matrix(lnm7))  }
GamU7_LPML <- sum(log(dd7))

GamU7_meanlogli <- loglihood_GamUma(eta=colMeans(GamU7.sim$eta), phi=mean(GamU7.sim$phi), d= atatable7$value,
                                    x = model.matrix(lnm7) )
GamU7_DIC <- 2*GamU7_meanlogli -4*mean(GamU7.sim$logf)*36

dd8 <- rep(NA,nrow(atatable8))
for (j in 1:nrow(atatable8)) {
  dd8[j] <- cpo_GamUma(eta=GamU8.sim$eta[j,], phi=GamU8.sim$phi[j], d= atatable8$value,
                       x = model.matrix(lnm8))  }
GamU8_LPML <- sum(log(dd8))

GamU8_meanlogli <- loglihood_GamUma(eta=colMeans(GamU8.sim$eta), phi=mean(GamU8.sim$phi), d= atatable8$value,
                                    x = model.matrix(lnm8) )
GamU8_DIC <- 2*GamU8_meanlogli -4*mean(GamU8.sim$logf)*36

dd9 <- rep(NA,nrow(atatable9))
for (j in 1:nrow(atatable9)) {
  dd9[j] <- cpo_GamUma(eta=GamU9.sim$eta[j,], phi=GamU9.sim$phi[j], d= atatable9$value,
                       x = model.matrix(lnm9))  }
GamU9_LPML <- sum(log(dd9))

GamU9_meanlogli <- loglihood_GamUma(eta=colMeans(GamU9.sim$eta), phi=mean(GamU9.sim$phi), d= atatable9$value,
                                    x = model.matrix(lnm9) )
GamU9_DIC <- 2*GamU9_meanlogli -4*mean(GamU9.sim$logf)*36

ddS <- rep(NA,nrow(atatableS))
for (j in 1:nrow(atatableS)) {
  ddS[j] <- cpo_GamUma(eta=GamUS.sim$eta[j,], phi=GamUS.sim$phi[j], d= atatableS$value,
                       x = model.matrix(lnmS))  }
GamUS_LPML <- sum(log(ddS))

GamUS_meanlogli <- loglihood_GamUma(eta=colMeans(GamUS.sim$eta), phi=mean(GamUS.sim$phi), d= atatableS$value,
                                    x = model.matrix(lnmS) )
GamUS_DIC <- 2*GamUS_meanlogli -4*mean(GamUS.sim$logf)*36

#### Marginal Model Selection ####

# Table 1: DIC and LPML for marginal components
marginal_table <- rbind( c(LNU1_DIC , LNU2_DIC , LNU3_DIC , LNU4_DIC , LNU5_DIC , LNU6_DIC , LNU7_DIC,  LNU8_DIC,  LNU9_DIC),
                         c(GamU1_DIC, GamU2_DIC, GamU3_DIC, GamU4_DIC, GamU5_DIC, GamU6_DIC, GamU7_DIC, GamU8_DIC, GamU9_DIC),
                         c(LNC1_DIC , LNC2_DIC , LNC3_DIC , LNC4_DIC , LNC5_DIC , LNC6_DIC , LNC7_DIC,  LNC8_DIC,  LNC9_DIC),
                         c(GamC1_DIC, GamC2_DIC, GamC3_DIC, GamC4_DIC, GamC5_DIC, GamC6_DIC, GamC7_DIC, GamC8_DIC, GamC9_DIC),
                         c(LNU1_LPML, LNU2_LPML, LNU3_LPML, LNU4_LPML, LNU5_LPML , LNU6_LPML , LNU7_LPML,  LNU8_LPML,  LNU9_LPML),
                         c(GamU1_LPML, GamU2_LPML, GamU3_LPML, GamU4_LPML, GamU5_LPML, GamU6_LPML, GamU7_LPML, GamU8_LPML, GamU9_LPML),
                         c(LNC1_LPML, LNC2_LPML, LNC3_LPML, LNC4_LPML, LNC5_LPML , LNC6_LPML , LNC7_LPML,  LNC8_LPML,  LNC9_LPML),
                         c(GamC1_LPML, GamC2_LPML, GamC3_LPML, GamC4_LPML, GamC5_LPML, GamC6_LPML, GamC7_LPML, GamC8_LPML, GamC9_LPML))

colnames(marginal_table) <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9")
rownames(marginal_table) <- NULL
marginal_table <- round(marginal_table, 1)

# Table 2: Estimated regression coefficients for marginal models
LNCest     <- cbind(
  summary(LNC1.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC2.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC3.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC4.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC5.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC6.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC7.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC8.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNC9.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)])

LNCest <- round(LNCest, 2)
rownames(LNCest) <- c("$\\eta_2$", "$\\eta_3$", "$\\eta_4$", "$\\eta_5$",
                      "$\\eta_6$", "$\\eta_7$", "$\\eta_8$", "$\\eta_9$")
colnames(LNCest) <- c("Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                      "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                      "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%")

LNUest     <- cbind(
  summary(LNU1.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU2.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU3.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU4.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU5.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU6.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU7.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU8.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(LNU9.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)])

LNUest <- round(LNUest, 2)
rownames(LNUest) <- c("$\\eta_2$", "$\\eta_3$", "$\\eta_4$", "$\\eta_5$",
                      "$\\eta_6$", "$\\eta_7$", "$\\eta_8$", "$\\eta_9$")
colnames(LNUest) <- c("Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                      "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                      "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%")

GamCest     <- cbind(
  summary(GamC1.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC2.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC3.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC4.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC5.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC6.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC7.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC8.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamC9.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)])

GamCest <- round(GamCest, 2)
rownames(GamCest) <- c("$\\eta_2$", "$\\eta_3$", "$\\eta_4$", "$\\eta_5$",
                       "$\\eta_6$", "$\\eta_7$", "$\\eta_8$", "$\\eta_9$")
colnames(GamCest) <- c("Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                       "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                       "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%")

GamUest     <- cbind(
  summary(GamU1.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU2.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU3.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU4.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU5.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU6.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU7.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU8.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)],
  summary(GamU9.stanfit, pars = "eta", probs = c(0.05, 0.95))$summary[,c(1,4,5)])

GamUest <- round(GamUest, 2)
rownames(GamUest) <- c("$\\eta_2$", "$\\eta_3$", "$\\eta_4$", "$\\eta_5$",
                       "$\\eta_6$", "$\\eta_7$", "$\\eta_8$", "$\\eta_9$")
colnames(GamUest) <- c("Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                       "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%",
                       "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%", "Mean", "5\\%", "95\\%")

#Table 8: Rhat values for marginal models
rhattable <- round(cbind(summary(LNU1.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU2.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU3.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU4.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU5.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU6.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU7.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU8.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNU9.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU1.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU2.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU3.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU4.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU5.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU6.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU7.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU8.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamU9.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC1.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC2.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC3.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC4.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC5.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC6.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC7.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC8.stanfit, pars = c("eta"))$summary[,10],
                         summary(LNC9.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC1.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC2.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC3.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC4.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC5.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC6.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC7.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC8.stanfit, pars = c("eta"))$summary[,10],
                         summary(GamC9.stanfit, pars = c("eta"))$summary[,10]),3)

rownames(rhattable) <- c("$\\eta_2$", "$\\eta_3$", "$\\eta_4$", "$\\eta_5$",
                         "$\\eta_6$", "$\\eta_7$", "$\\eta_8$", "$\\eta_9$")
colnames(rhattable) <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9",
                         "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9",
                         "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9",
                         "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9")

library(ggplot2)
library(gridExtra)

# Fig. 5: Randomly chosen traceplots for marginal models
p1 <- traceplot(LNU5.stanfit, pars=c("eta","sigma")) + ggtitle("Unconstrained Lognormal Model for T5") +
  theme(plot.title = element_text(hjust = 0.5, size=15)) + scale_x_continuous(breaks = c(500,750,1000))
p2 <- traceplot(GamU8.stanfit,pars=c("eta","phi"))   + ggtitle("Unconstrained Gamma Model for T8") +
  theme(plot.title = element_text(hjust = 0.5, size=15)) + scale_x_continuous(breaks = c(500,750,1000))
p3 <- traceplot(LNC1.stanfit, pars=c("eta","sigma")) + ggtitle("Constrained Lognormal Model for T1") +
  theme(plot.title = element_text(hjust = 0.5, size=15)) + scale_x_continuous(breaks = c(500,750,1000))
p4 <- traceplot(GamC4.stanfit,pars=c("eta","phi"))   + ggtitle("Constrained Gamma Model for T4")+
  theme(plot.title = element_text(hjust = 0.5, size=15)) + scale_x_continuous(breaks = c(500,750,1000))
grid.arrange(p1,  p2, p3, p4,  nrow = 2)

#### Vine Structure Selection ####
library("VineCopula")
library("CDVine")

U1_C <- pnorm((log(atatable1$value)- model.matrix(lnm1) %*% colMeans(LNC1.sim$eta) )
               / mean(LNC1.sim$sigma))
U2_C <- pnorm((log(atatable2$value)- model.matrix(lnm2) %*% colMeans(LNC2.sim$eta) )
            / mean(LNC2.sim$sigma))
U3_C <- pnorm((log(atatable3$value)- model.matrix(lnm3) %*% colMeans(LNC3.sim$eta) )
            / mean(LNC3.sim$sigma))
U4_C <- pnorm((log(atatable4$value)- model.matrix(lnm4) %*% colMeans(LNC4.sim$eta) )
            / mean(LNC4.sim$sigma))
U5_C <- pnorm((log(atatable5$value)- model.matrix(lnm5) %*% colMeans(LNC5.sim$eta) )
            / mean(LNC5.sim$sigma))
U6_C <- pnorm((log(atatable6$value)- model.matrix(lnm6) %*% colMeans(LNC6.sim$eta) )
            / mean(LNC6.sim$sigma))
U7_C <- pnorm((log(atatable7$value)- model.matrix(lnm7) %*% colMeans(LNC7.sim$eta) )
            / mean(LNC7.sim$sigma))
U8_C <- pnorm((log(atatable8$value)- model.matrix(lnm8) %*% colMeans(LNC8.sim$eta) )
            / mean(LNC8.sim$sigma))
U9_C <- pnorm((log(atatable9$value)- model.matrix(lnm9) %*% colMeans(LNC9.sim$eta) )
            / mean(LNC9.sim$sigma))

Bayespm_U_C <- as.data.frame(cbind(U1_C, U2_C, U3_C, U4_C, U5_C, U6_C, U7_C, U8_C, U9_C))
colnames(Bayespm_U_C) <- c("T1","T2","T3", "T4", "T5", "T6", "T7", "T8", "T9")

system.time(
  Bayespm_strs_C <- RVineStructureSelect(data=Bayespm_U_C, familyset=c(1,3,4,5), type="RVine", selectioncrit = "BIC",
                                       indeptest = TRUE, level=0.05)
)

# Fig. 2: R-Vine structure for Tree 1
plot(Bayespm_strs_C, edge.labels = "family-tau", tree = c(1,2,3))

# Fig. 3: Normalized contour plots for Trees 1, 2, and 3
contour(Bayespm_strs_C, tree = c(1,2,3), cex.nums = 2)

# Table 3: Specification of vine structure
copstructure <- summary(Bayespm_strs_C)[summary(Bayespm_strs_C)$cop != "I",c(1,2,4,5,7)]
colnames(copstructure) <- c("Tree", "Edge", "Family", "$\\phi$", "$\\tau$")
rownames(copstructure) <- NULL

copstructure

#### Prediction ####

# Table 4: Summary of unpaid claims prediction with the fitted models    
  copula_predC <- (exp(colMeans(LNC1.sim$eta)+0.5*mean(LNC1.sim$sigma)^2)-1)*lcy1+
  (exp(colMeans(LNC2.sim$eta)+0.5*mean(LNC2.sim$sigma)^2)-1)*lcy2+
  (exp(colMeans(LNC3.sim$eta)+0.5*mean(LNC3.sim$sigma)^2)-1)*lcy3+
  (exp(colMeans(LNC4.sim$eta)+0.5*mean(LNC4.sim$sigma)^2)-1)*lcy4+
  (exp(colMeans(LNC5.sim$eta)+0.5*mean(LNC5.sim$sigma)^2)-1)*lcy5+
  (exp(colMeans(LNC6.sim$eta)+0.5*mean(LNC6.sim$sigma)^2)-1)*lcy6+
  (exp(colMeans(LNC7.sim$eta)+0.5*mean(LNC7.sim$sigma)^2)-1)*lcy7+
  (exp(colMeans(LNC8.sim$eta)+0.5*mean(LNC8.sim$sigma)^2)-1)*lcy8+
  (exp(colMeans(LNC9.sim$eta)+0.5*mean(LNC9.sim$sigma)^2)-1)*lcy9

  silo_predC <- (exp(colMeans(LNCS.sim$eta)+0.5*mean(LNCS.sim$sigma)^2)-1)*lcyS

  copula_predU <- (exp(colMeans(LNU1.sim$eta)+0.5*mean(LNU1.sim$sigma)^2)-1)*lcy1+
  (exp(colMeans(LNU2.sim$eta)+0.5*mean(LNU2.sim$sigma)^2)-1)*lcy2+
  (exp(colMeans(LNU3.sim$eta)+0.5*mean(LNU3.sim$sigma)^2)-1)*lcy3+
  (exp(colMeans(LNU4.sim$eta)+0.5*mean(LNU4.sim$sigma)^2)-1)*lcy4+
  (exp(colMeans(LNU5.sim$eta)+0.5*mean(LNU5.sim$sigma)^2)-1)*lcy5+
  (exp(colMeans(LNU6.sim$eta)+0.5*mean(LNU6.sim$sigma)^2)-1)*lcy6+
  (exp(colMeans(LNU7.sim$eta)+0.5*mean(LNU7.sim$sigma)^2)-1)*lcy7+
  (exp(colMeans(LNU8.sim$eta)+0.5*mean(LNU8.sim$sigma)^2)-1)*lcy8+
  (exp(colMeans(LNU9.sim$eta)+0.5*mean(LNU9.sim$sigma)^2)-1)*lcy9
  
  silo_predU <- (exp(colMeans(LNUS.sim$eta)+0.5*mean(LNUS.sim$sigma)^2)-1)*lcyS
  
  actual <- tcyS - lcyS


predtable <-  cbind( copula_predU, silo_predU, copula_predC, silo_predC, actual)
predtable <- rbind( predtable, colSums(predtable))  

# Table 5: Summary of validation performance measures of the fitted models

copulaU_RMSE <- sqrt(mean((copula_predU - actual)^2))
siloU_RMSE   <- sqrt(mean((silo_predU   - actual)^2))
copulaC_RMSE <- sqrt(mean((copula_predC - actual)^2))
siloC_RMSE   <- sqrt(mean((silo_predC   - actual)^2))

copulaU_MAE <- mean(abs(copula_predU - actual))
siloU_MAE   <- mean(abs(silo_predU   - actual))
copulaC_MAE <- mean(abs(copula_predC - actual))
siloC_MAE   <- mean(abs(silo_predC   - actual))

valtable <- rbind( c(copulaU_RMSE, siloU_RMSE, copulaC_RMSE, siloC_RMSE),
                   c(copulaU_MAE , siloU_MAE , copulaC_MAE , siloC_MAE))


## Predictive distribution of L by simulation
set.seed(100)
copulaC_sim <- qnorm(RVineSim(16000, Bayespm_strs_C))
indepC_sim  <- qnorm(matrix(runif(144000),16000,9))
siloC_sim   <- qnorm(runif(16000))

copulaC_upc_T1 <- rowSums(exp(matrix(as.vector(LNC1.sim$eta) + rep(LNC1.sim$sigma,8)*
                  copulaC_sim[,1], nrow=2000)) %*% diag(lcy1)) - sum(lcy1)
copulaC_upc_T2 <- rowSums(exp(matrix(as.vector(LNC2.sim$eta) + rep(LNC2.sim$sigma,8)*
                  copulaC_sim[,2], nrow=2000)) %*% diag(lcy2)) - sum(lcy2)
copulaC_upc_T3 <- rowSums(exp(matrix(as.vector(LNC3.sim$eta) + rep(LNC3.sim$sigma,8)*
                  copulaC_sim[,3], nrow=2000)) %*% diag(lcy3)) - sum(lcy3)
copulaC_upc_T4 <- rowSums(exp(matrix(as.vector(LNC4.sim$eta) + rep(LNC4.sim$sigma,8)*
                  copulaC_sim[,4], nrow=2000)) %*% diag(lcy4)) - sum(lcy4)
copulaC_upc_T5 <- rowSums(exp(matrix(as.vector(LNC5.sim$eta) + rep(LNC5.sim$sigma,8)*
                  copulaC_sim[,5], nrow=2000)) %*% diag(lcy5)) - sum(lcy5)
copulaC_upc_T6 <- rowSums(exp(matrix(as.vector(LNC6.sim$eta) + rep(LNC6.sim$sigma,8)*
                  copulaC_sim[,6], nrow=2000)) %*% diag(lcy6)) - sum(lcy6)
copulaC_upc_T7 <- rowSums(exp(matrix(as.vector(LNC7.sim$eta) + rep(LNC7.sim$sigma,8)*
                  copulaC_sim[,7], nrow=2000)) %*% diag(lcy7)) - sum(lcy7)
copulaC_upc_T8 <- rowSums(exp(matrix(as.vector(LNC8.sim$eta) + rep(LNC8.sim$sigma,8)*
                  copulaC_sim[,8], nrow=2000)) %*% diag(lcy8)) - sum(lcy8)
copulaC_upc_T9 <- rowSums(exp(matrix(as.vector(LNC9.sim$eta) + rep(LNC9.sim$sigma,8)*
                  copulaC_sim[,9], nrow=2000)) %*% diag(lcy9)) - sum(lcy9)
copulaC_upc_total <- copulaC_upc_T1 + copulaC_upc_T2 + copulaC_upc_T3 + copulaC_upc_T4 +
    copulaC_upc_T5 + copulaC_upc_T6 + copulaC_upc_T7 + copulaC_upc_T8 + copulaC_upc_T9



indepC_upc_T1 <- rowSums(exp(matrix(as.vector(LNC1.sim$eta) + rep(LNC1.sim$sigma,8)*
                                       indepC_sim[,1], nrow=2000)) %*% diag(lcy1)) - sum(lcy1)
indepC_upc_T2 <- rowSums(exp(matrix(as.vector(LNC2.sim$eta) + rep(LNC2.sim$sigma,8)*
                                       indepC_sim[,2], nrow=2000)) %*% diag(lcy2)) - sum(lcy2)
indepC_upc_T3 <- rowSums(exp(matrix(as.vector(LNC3.sim$eta) + rep(LNC3.sim$sigma,8)*
                                       indepC_sim[,3], nrow=2000)) %*% diag(lcy3)) - sum(lcy3)
indepC_upc_T4 <- rowSums(exp(matrix(as.vector(LNC4.sim$eta) + rep(LNC4.sim$sigma,8)*
                                       indepC_sim[,4], nrow=2000)) %*% diag(lcy4)) - sum(lcy4)
indepC_upc_T5 <- rowSums(exp(matrix(as.vector(LNC5.sim$eta) + rep(LNC5.sim$sigma,8)*
                                       indepC_sim[,5], nrow=2000)) %*% diag(lcy5)) - sum(lcy5)
indepC_upc_T6 <- rowSums(exp(matrix(as.vector(LNC6.sim$eta) + rep(LNC6.sim$sigma,8)*
                                       indepC_sim[,6], nrow=2000)) %*% diag(lcy6)) - sum(lcy6)
indepC_upc_T7 <- rowSums(exp(matrix(as.vector(LNC7.sim$eta) + rep(LNC7.sim$sigma,8)*
                                       indepC_sim[,7], nrow=2000)) %*% diag(lcy7)) - sum(lcy7)
indepC_upc_T8 <- rowSums(exp(matrix(as.vector(LNC8.sim$eta) + rep(LNC8.sim$sigma,8)*
                                       indepC_sim[,8], nrow=2000)) %*% diag(lcy8)) - sum(lcy8)
indepC_upc_T9 <- rowSums(exp(matrix(as.vector(LNC9.sim$eta) + rep(LNC9.sim$sigma,8)*
                                       indepC_sim[,9], nrow=2000)) %*% diag(lcy9)) - sum(lcy9)
indepC_upc_total <- indepC_upc_T1 + indepC_upc_T2 + indepC_upc_T3 + indepC_upc_T4 +
    indepC_upc_T5 + indepC_upc_T6 + indepC_upc_T7 + indepC_upc_T8 + indepC_upc_T9


siloC_upc_total <- rowSums(exp(matrix(as.vector(LNCS.sim$eta) + rep(LNCS.sim$sigma,8)*
                  siloC_sim, nrow=2000)) %*% diag(lcyS)) - sum(lcyS)

# Fig. 4: Predictive density of aggregate unpaid claims for constrained models
plot(density(indepC_upc_total, bw="SJ", adjust=0.5),
     xlab="Aggregate Unpaid Claims", main="Kernel density", lty=2)
lines(density(copulaC_upc_total, bw="SJ", adjust=0.5),col="blue")
lines(density(siloC_upc_total, bw="SJ", adjust=0.5),col="red", lty=2)
legend("topright", c("Independent","Copula","Silo")
       , lwd=2, lty=c(2, 1,2), col=c("black","blue","red"))

## Estimation of Hellinger distances
library(statip)
hdist <-  matrix(c(hellinger(indepC_upc_total/1000000, copulaC_upc_total/1000000),
                   hellinger(indepC_upc_total/1000000,   siloC_upc_total/1000000))*100,ncol=2)
colnames(hdist) <- c("Copula", "Silo")


# Table 6: Estimated risk margins for the unpaid claims
risktable <- rbind(
  c(quantile(indepC_upc_T1, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T1[order(indepC_upc_T1)],200)),
    mean(tail(indepC_upc_T1[order(indepC_upc_T1)],100)),
    mean(tail(indepC_upc_T1[order(indepC_upc_T1)],20))),
  c(quantile(indepC_upc_T2, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T2[order(indepC_upc_T2)],200)),
    mean(tail(indepC_upc_T2[order(indepC_upc_T2)],100)),
    mean(tail(indepC_upc_T2[order(indepC_upc_T2)],20))),
  c(quantile(indepC_upc_T3, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T3[order(indepC_upc_T3)],200)),
    mean(tail(indepC_upc_T3[order(indepC_upc_T3)],100)),
    mean(tail(indepC_upc_T3[order(indepC_upc_T3)],20))),
  c(quantile(indepC_upc_T4, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T4[order(indepC_upc_T4)],200)),
    mean(tail(indepC_upc_T4[order(indepC_upc_T4)],100)),
    mean(tail(indepC_upc_T4[order(indepC_upc_T4)],20))),
  c(quantile(indepC_upc_T5, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T5[order(indepC_upc_T5)],200)),
    mean(tail(indepC_upc_T5[order(indepC_upc_T5)],100)),
    mean(tail(indepC_upc_T5[order(indepC_upc_T5)],20))),
  c(quantile(indepC_upc_T6, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T6[order(indepC_upc_T6)],200)),
    mean(tail(indepC_upc_T6[order(indepC_upc_T6)],100)),
    mean(tail(indepC_upc_T6[order(indepC_upc_T6)],20))),
  c(quantile(indepC_upc_T7, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T7[order(indepC_upc_T7)],200)),
    mean(tail(indepC_upc_T7[order(indepC_upc_T7)],100)),
    mean(tail(indepC_upc_T7[order(indepC_upc_T7)],20))),
  c(quantile(indepC_upc_T8, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T8[order(indepC_upc_T8)],200)),
    mean(tail(indepC_upc_T8[order(indepC_upc_T8)],100)),
    mean(tail(indepC_upc_T8[order(indepC_upc_T8)],20))),
  c(quantile(indepC_upc_T9, probs = c(0.9, 0.95, 0.99)),
    mean(tail(indepC_upc_T9[order(indepC_upc_T9)],200)),
    mean(tail(indepC_upc_T9[order(indepC_upc_T9)],100)),
    mean(tail(indepC_upc_T9[order(indepC_upc_T9)],20))))

risktable <- rbind(risktable, colSums(risktable),
           c(quantile(indepC_upc_total, probs = c(0.9, 0.95, 0.99)),
           mean(tail(indepC_upc_total[order(indepC_upc_total)],200)),
           mean(tail(indepC_upc_total[order(indepC_upc_total)],100)),
           mean(tail(indepC_upc_total[order(indepC_upc_total)],20))),
           c(quantile(copulaC_upc_total, probs = c(0.9, 0.95, 0.99)),
             mean(tail(copulaC_upc_total[order(copulaC_upc_total)],200)),
             mean(tail(copulaC_upc_total[order(copulaC_upc_total)],100)),
             mean(tail(copulaC_upc_total[order(copulaC_upc_total)],20))),
           c(quantile(siloC_upc_total, probs = c(0.9, 0.95, 0.99)),
             mean(tail(siloC_upc_total[order(siloC_upc_total)],200)),
             mean(tail(siloC_upc_total[order(siloC_upc_total)],100)),
             mean(tail(siloC_upc_total[order(siloC_upc_total)],20))))

colnames(risktable) <- c("90\\%", "95\\%", "99\\%", "90\\%", "95\\%", "99\\%")
rownames(risktable) <- c("Marginal T1","Marginal T2","Marginal T3", "Marginal T4","Marginal T5",
                         "Marginal T6","Marginal T7","Marginal T8", "Marginal T9","Marginal Total",
                         "Independent Aggregate","Copula Aggregate","Silo Aggregate")
risktable <- round(risktable)



# Table 7: Risk reduction from diversification
riskredutable <- -risktable[11:13,] + risktable[c(10,10,10),]
rownames(riskredutable) <- c("Independent", "Copula", "Silo")




