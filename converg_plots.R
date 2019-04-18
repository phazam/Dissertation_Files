create_plots <- function(x, saveto) {
  ## Packages
  packages <- c("expss", "filesstrings")
  package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  files <- x
  ## Split each file name in 2 after the rep_filter term, save what comes after
  title <- files[[1]][[1]]$title
  nparam <- nrow(files[[1]]$parameters$unstandardized)
  params<-matrix(0,nrow=length(files),ncol=nparam)  #vector of length #outputs
  #est_mean <- matrix(0,nrow=1, ncol = 13)
  for(i in 1:length(files)){
    for(d in 1:ncol(params)){
      params[i,d]<-as.numeric(files[[i]][[9]][[1]][[3]][d])
    }
}
# 1. Open jpeg file
jpeg(paste0(saveto,"/rplot2",format(Sys.time(),"%b%d%Y"),".jpg"))
#params<- params[1:256,]
names <- c("NA","eta","Y2residw","Etaresidw","s_x2","s_x3","s_x4","y_x1","Int_s","Yresidt","sresidt","Y_wSubj", "NA", "Yvar_wSubj","NA")
n <- vector()
hat_se <- vector()
par(mfrow=c(3,4))
for (f in c(2:12, 14)){
  avg.y3 <- params[,f]
  hat_se[f] <- sd(params[,f])
  #Precision using 68% rule
  #se<- 0.16
  #Precision using 95% rule
  se<- 0.025
  n[f] = round((hat_se[f]/se)^2,0)
  for(i in 1:length(avg.y3)) {
    avg.y3[i] <- mean(params[1:i,f])}
  
  plot(1:length(params[,f]),
       avg.y3,
       #round(avg.y3,2),
       type='l', ylab = names[f], xlab = paste0("Replication\nEst. # reps needed ", n[f])
       #,ylim = c(mean(avg.y3)-(sd(avg.y3)), mean(avg.y3)+(sd(avg.y3)) )
  )
  
  abline(c(mean(params[,f]),0))
  abline(v=n[f], lty=2)
}
# 3. Close the file
dev.off()

}