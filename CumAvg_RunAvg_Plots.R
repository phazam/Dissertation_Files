library(MplusAutomation)


o<- readModels("F:/patrice_dissertation/Study1/Linear/n90/150t/difuse/high/output", 
               recursive = FALSE, quiet = FALSE)

nparam <- nrow(o[[1]]$parameters$unstandardized)

params<-matrix(0,nrow=length(out),ncol=14)
params<-matrix(0,nrow=length(o),ncol=nparam)
for(i in 1:length(o)){
  for(d in 2:ncol(params)){
    
    params[i,d]<-as.numeric(o[[i]][[a]][[b]][[c]][d-1])
    
  }
  
}

params.d <- as.data.frame(params) #I think this is what I did for params.d.


#running avg

library(zoo)

ggplot(params.d, aes(params[,14], params[,6])) + 
  geom_point(position=position_jitter(1,3), pch=21, fill="#FF0000AA") + # this one has a "jitter", they say it helps with visualizing it.
  geom_line(aes(y=rollmean(params[,6], 3, na.pad=TRUE))) +
  theme_bw()

ggplot(params.d, aes(params.d[,14], params.d[,6])) + 
  labs(title = "  Cummulative Averate Estimate the Between Slope on x2, 2000 Replications", 
       x = "Replications", y = "Cummulative Average") +
  geom_point(pch=21, fill="#FF0000AA") +
  geom_line(aes(y=rollmean(params.d[,6], 3, na.pad=TRUE))) +
  theme_bw()



#normal cum avg



o<- readModels("F:/patrice_dissertation/Study1/Cubic/n30/150t/difuse/high/output", 
               recursive = FALSE, quiet = FALSE)

nparam <- nrow(o[[1]]$parameters$unstandardized)

params<-matrix(0,nrow=length(o),ncol=nparam)
for(i in 1:length(o)){
  for(d in 2:ncol(params)){
    
    params[i,d]<-as.numeric(o[[i]][[9]][[1]][[3]][d])
    
  }
  
}

for (a in 1:15){
avg.y3 <- params[,a]

for(i in 1:length(avg.y3)) {
  avg.y3[i] <- mean(params[1:i,a])}

plot(1:length(params[,a]),avg.y3,type='l',main="Cummulative Mean Estimate for Eta on Y3, 2000 Replications"
     ,ylim = c(mean(avg.y3)-(sd(avg.y3)*4), mean(avg.y3)+(sd(avg.y3)*4) ))
abline(c(mean(params[,a]),0))
}









avg.par8 <- params[,8]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,8])}

plot(1:length(params[,8]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y8, 2000 Replications")
abline(c(mean(params[,8]),0))



avg.par9 <- params[,9]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,9])}

plot(1:length(params[,9]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y9, 2000 Replications")
abline(c(mean(params[,9]),0))




avg.par10 <- params[,10]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,10])}

plot(1:length(params[,10]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y10, 2000 Replications")
abline(c(mean(params[,10]),0))




avg.par11 <- params[,11]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,11])}

plot(1:length(params[,11]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y11, 2000 Replications")
abline(c(mean(params[,11]),0))





avg.par12 <- params[,12]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,12])}

plot(1:length(params[,12]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y12, 2000 Replications", 
     ylim = c((mean(params[,12]) - (sd(params[,12])/2)), (mean(params[,12]) + (sd(params[,12])/2))) )
abline(c(mean(params[,12]),0))





avg.par13 <- params[,13]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,13])}

plot(1:length(params[,13]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y13, 2000 Replications", 
  ylim = c((mean(params[,13]) - (sd(params[,13])/2)), (mean(params[,13]) + (sd(params[,13])/2))) )
abline(c(mean(params[,13]),0))





avg.par14 <- params[,14]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,14])}

plot(1:length(params[,14]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y14, 2000 Replications", 
ylim = c((mean(params[,14]) - (sd(params[,14])/2)), (mean(params[,14]) + (sd(params[,14])/2))) )
abline(c(mean(params[,14]),0))


avg.par15 <- params[,15]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,15])}

plot(1:length(params[,15]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y15, 2000 Replications", 
ylim = c((mean(params[,15]) - (sd(params[,15])/2)), (mean(params[,15]) + (sd(params[,15])/2))) )
abline(c(mean(params[,15]),0))

avg.par16 <- params[,16]

for(i in 1:length(avg.y2)) {
  avg.y2[i] <- mean(params[1:i,16])}

plot(1:length(params[,16]),avg.y2,type='l',main="Cummulative Mean Estimate for Eta on Y16, 2000 Replications", 
     ylim = c((mean(params[,16]) - (sd(params[,16])/2)), (mean(params[,16]) + (sd(params[,16])/2))) )
abline(c(mean(params[,16]),0))



