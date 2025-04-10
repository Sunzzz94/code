library(dplyr)
library(ggplot2)
library(ggsci)
library(factoextra)
library(readxl)
EC <- read.csv('EC_nation.csv',header = T)  
cluster <- read.csv('Cluster.csv',header = T,row.names = 1)
df <- scale(cluster)
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')
EAPC <- EAPC[,c(2,7,8)]
country_name <- subset(EC,EC$year==1990 & 
                         EC$age=='Age-standardized' & 
                         EC$metric== 'Rate' &
                         EC$measure=='Incidence') 
country <- country_name$location 
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_incidence <- EAPC_cal[,c(1,2)]
names(EAPC_incidence)[2] <- 'EAPC_incidence'
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='DALYs (Disability-Adjusted Life Years)')

EAPC <- EAPC[,c(2,7,8)]

country_name <- subset(EC,EC$year==1990 & 
                         EC$age=='Age-standardized' & 
                         EC$metric== 'Rate' &
                         EC$measure=='DALYs (Disability-Adjusted Life Years)') 
country <- country_name$location  
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_DALY <- EAPC_cal[,c(1,2)]
names(EAPC_DALY)[2] <- 'EAPC_DALY'