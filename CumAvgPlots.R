
ad <- get_admissible("F:/patrice_dissertation/testoutput",
  file_filter=".txt", rep_filter = "rep", warnings = "default",
                           recursive = FALSE, move = TRUE, silent=FALSE)

ad2 <- get_admissible("C:/Users/Maintainer/Desktop/Patrice/Mplusautomation/Analyses/cubic/study1/n90/150t/difuse/high/knots3/onethousand/output",
                     file_filter=".txt", rep_filter = "rep", warnings = "default",
                     recursive = FALSE, move = TRUE, silent=FALSE)

o<- readModels("F:/patrice_dissertation/testoutput", 
               recursive = FALSE, quiet = FALSE)

o2 <- readModels("C:/Users/Maintainer/Desktop/Patrice/Mplusautomation/Analyses/cubic/study1/n30/150t/difuse/high/knots3/eighthundred/output", 
                 recursive = FALSE, quiet = FALSE)

nparam <- nrow(o[[1]]$parameters$unstandardized)

#params<-matrix(0,nrow=length(o),ncol=13)
params<-matrix(0,nrow=length(o),ncol=nparam)
for(i in 1:nrow(params)){
  for(d in 1:ncol(params)){
    
    params[i,d]<-as.numeric(o[[i]][[9]][[1]][[3]][d])
    
  }
  
}

# 1. Open jpeg file
jpeg("F:/patrice_dissertation/Study1/Cubic/plots/rplot.jpg")
#params<- params[1:256,]
#names <- c("NA","eta_eta1","Y2residw","Etaresidw","s_x2","s_x3","s_x4",y_x1,Int_s,Y2residt,sresidt,Y2means, "NA",Y2vars, "NA"
n <- vector()
hat_se <- vector()
par(mfrow=c(3,4))
for (f in c(2:12, 14)){
  avg.y3 <- params[,f]
  hat_se[f] <- sd(params[,f])
  #68% rule
  se<- 0.005
  n[f] = round((hat_se[f]/se)^2,0)
  for(i in 1:length(avg.y3)) {
    avg.y3[i] <- mean(params[1:i,f])}
  
  plot(1:length(params[,f]),
       avg.y3,
       #round(avg.y3,2),
       type='l', ylab = "Estimate", xlab = paste0("Replication\nEst. # reps needed ", n[f])
              ,ylim = c(mean(avg.y3)-(sd(avg.y3)), mean(avg.y3)+(sd(avg.y3)) )
       )
       
  abline(c(mean(params[,f]),0))
  abline(v=n[f], lty=2)
}
# 3. Close the file
dev.off()






nparam <- nrow(o2[[1]]$parameters$unstandardized)

params<-matrix(0,nrow=length(o2),ncol=nparam)
for(i in 1:nrow(params)){
  for(d in 1:ncol(params)){
    
    params[i,d]<-as.numeric(o2[[i]][[9]][[1]][[3]][d])
    
  }
  
}
#params<- params[1:256,]
#names <- c("NA","eta_eta1","Y2residw","Etaresidw","s_x2","s_x3","s_x4",y_x1,Int_s,Y2residt,sresidt,Y2means, "NA",Y2vars, "NA"
n <- vector()
hat_se <- vector()
par(mfrow=c(3,4))
for (f in c(2:12, 14)){
  avg.y3 <- params[,f]
  hat_se[f] <- sd(params[,f])
  #68% rule
  se<- 0.005
  n[f] = round((hat_se[f]^2/se)^2,0)
  for(i in 1:length(avg.y3)) {
    avg.y3[i] <- mean(params[1:i,f])}
  
  plot(1:length(params[,f]),
       avg.y3,
       #round(avg.y3,2),
       type='l', ylab = "Estimate", xlab = paste0("Replication", "/n", 
                                                  "convergence met at n = ", n[f])
       ,ylim = c(mean(avg.y3)-(sd(avg.y3)), mean(avg.y3)+(sd(avg.y3)) )
       #,ylim = c(mean(avg.y3)-.01, mean(avg.y3)+.01)
  )
  
  abline(c(mean(params[,f]),0))
}


