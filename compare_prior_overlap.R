Compare_overlap <- function (pop_mean, change_mean, perc_var){
  # perc_var - percent of population value for prior variance
  # change_mean - number of standard deviations to change population mean by for prior
  # pop_mean - population mean
  
  ## Check if tidiverse is installed and loaded
  packages <- c("sm")
  package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  
  v <- matrix(rep(0), nrow=length(perc_var), ncol=length(pop_mean))
  m <- matrix(rep(0), nrow=length(change_mean), ncol=length(pop_mean))
  hyperparam <- matrix(rep(0), nrow=nrow(v), ncol=ncol(v)*2)
  
  for (p in 1:length(pop_mean)){
    # for (vp in 1:length(perc_var)){
    # for (vp in 1:length(perc_var)){
    v[1,p] <- abs(pop_mean[p]*perc_var[1])
    v[2,p] <- abs(pop_mean[p]*perc_var[2])
    v[3,p] <- abs(pop_mean[p]*perc_var[3])
    #v <- t(v)
  }
  for (p in 1:length(pop_mean)){
    for (mx in 1:length(change_mean)){
      m[1,p] <- pop_mean[p]+(change_mean[1]*sqrt(v[1,p]))
      m[2,p] <- pop_mean[p]+(change_mean[2]*sqrt(v[2,p]))
      m[3,p] <- pop_mean[p]+(change_mean[3]*sqrt(v[3,p]))
    }
  }
  
  
  
  hyperparam <- as.data.frame(cbind(v, m),row.names = c("weak", "info", "wrong"))
  a <- matrix(rep(0), nrow=10000, ncol=length(pop_mean)+1)
  weakb <- matrix(rep(0), nrow=10000, ncol=length(pop_mean)+1)
  infob <- matrix(rep(0), nrow=10000, ncol=length(pop_mean)+1)
  wrongb <- matrix(rep(0), nrow=10000, ncol=length(pop_mean)+1)
  
  for (p in 1:length(pop_mean)){
    for (mx in 1:length(change_mean)){
      for (vp in 1:length(perc_var)){
        a[,p+1] <- (rnorm(10000,pop_mean[p],(abs(pop_mean[p])*.1)))
        a[,1] <- rep(1, nrow(a))
        weakb[,p+1]<-rnorm(10000, m[1,p ], v[1,p])
        weakb[,1] <- rep(2, nrow(weakb))
        
        infob[,p+1]<-rnorm(10000, m[2,p], v[2,p])
        infob[,1] <- rep(3, nrow(infob))
        
        wrongb[,p+1]<-rnorm(10000, m[3,p], v[3,p])
        wrongb[,1] <- rep(4, nrow(wrongb))
      }
    }
  }
  weakb <- rbind(a,weakb)
  infob <- rbind(a,infob)
  wrongb <- rbind(a,wrongb)
  
  #  ab<- cbind(weakb[,1], 
  #             infob[,1], wrongb[,1], weakb[,2], infob[,2], wrongb[,2])
  #  ab<- as.data.frame(ab)
  names <- c("weak_Parameter_1", "Weak_Parameter_2", "Weak_Parameter_3", "Weak_Parameter_4", "Weak_Parameter_5"
             ,"Info_Parameter_1", "Info_Parameter_2", "Info_Parameter_3", "Info_Parameter_4", "Info_Parameter_5"
             ,"Wrong_Parameter_1", "Wrong_Parameter_2", "Wrong_Parameter_3", "Wrong_Parameter_4", "Wrong_Parameter_5")
  
  
  # for(p in 1:length(pop_mean)) {
  #      for(i in 1:9){
  #   mypath <- file.path("F:/patrice_dissertation/Study1/Cubic/plots",paste("myplot_", names[i], ".jpeg", sep = ""))
  #  jpeg(file=mypath)
  # mytitle = paste("my title is", names[i])
  #for(po in 1:(length(pop_mean)*3)){}
  # po[p] <- 
  weak.f <- factor(weakb[,1], levels= c(1,2),
                   labels = c("Prior", "Likelihood")) 
  info.f <- factor(infob[,1], levels= c(1,3),
                   labels = c("Prior", "Likelihood")) 
  wrong.f <- factor(wrongb[,1], levels= c(1,4),
                    labels = c("Prior", "Likelihood")) 
  
  par(mfrow=c(3,5))
  #"Weak Condition: 0 SD Mean, 50% Variance"
  plot(density(weakb[1:10000,2]),lty=1, xlim=c(-1,2), xlab = "Eta", ylim=c(0,8), main = "")
  lines(density(weakb[10001:20000,2]),  lty=2)
  legend(1,8, levels(weak.f), lty = 1:2, cex = 0.5, xjust=.2)
  
  plot(density(weakb[1:10000,3]),lty=1, xlim=c(-2,4), xlab = "X1", ylim=c(0,4), main = "")
  lines(density(weakb[10001:20000,3]),  lty=2)
  legend(2.5,4, levels(weak.f), lty = 1:2,cex = 0.5, xjust=.4 )
  
  plot(density(weakb[1:10000,4]),lty=1, xlim=c(-2,4), xlab = "X2", ylim=c(0,4), main = "")
  lines(density(weakb[10001:20000,4]),  lty=2)
  legend(2.8,4, levels(weak.f), lty = 1:2,cex = 0.5, xjust=.5)
  
  plot(density(weakb[1:10000,5]),lty=1, xlim=c(-3,1), xlab = "X3", ylim=c(0,4), main = "")
  lines(density(weakb[10001:20000,5]),  lty=2)
  legend(0,4, levels(weak.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  plot(density(weakb[1:10000,6]),lty=1, xlim=c(-1,4), xlab = "X4",  ylim=c(0,4), main = "")
  lines(density(weakb[10001:20000,6]),  lty=2)
  legend(2.8,4, levels(weak.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  #"Info. Condition: 0 SD Mean, 10% Variance"
  plot(density(infob[1:10000,2]),lty=1, xlim=c(-1,2), xlab = "Eta",  ylim=c(0,8), main = "")
  lines(density(infob[10001:20000,2]),  lty=2)
  legend(1.3,8, levels(info.f), lty = 1:2, cex = 0.5, xjust=.2)
  
  plot(density(infob[1:10000,3]),lty=1, xlim=c(-2,4), xlab = "X1", ylim=c(0,4), main = "")
  lines(density(infob[10001:20000,3]),  lty=2)
  legend(2.5,4, levels(info.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  plot(density(infob[1:10000,4]),lty=1, xlim=c(-2,4), xlab = "X2",ylim=c(0,4), main = "")
  lines(density(infob[10001:20000,4]),  lty=2)
  legend(2.8,4, levels(info.f), lty = 1:2,cex = 0.5, xjust=.5)
  
  plot(density(infob[1:10000,5]),lty=1, xlim=c(-3,1), xlab = "X3", ylim=c(0,4), main = "")
  lines(density(infob[10001:20000,5]),  lty=2)
  legend(0,4, levels(info.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  plot(density(infob[1:10000,6]),lty=1, xlim=c(-1,4), xlab = "X4", ylim=c(0,4), main = "")
  lines(density(infob[10001:20000,6]),  lty=2)
  legend(2.8,4, levels(info.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  
  #main = "Wrong Condition: 1 SD Mean, 10% Variance",
  plot(density(wrongb[1:10000,2]),lty=1, xlim=c(-1,2), xlab = "Eta",  ylim=c(0,8), main = "")
  lines(density(wrongb[10001:20000,2]),  lty=2)
  legend(1.3,8, levels(wrong.f), lty = 1:2, cex = 0.5, xjust=.2)
  
  plot(density(wrongb[1:10000,3]),lty=1, xlim=c(-2,4), xlab = "X1", ylim=c(0,4), main = "")
  lines(density(wrongb[10001:20000,3]),  lty=2)
  legend(2.5,4, levels(wrong.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  plot(density(wrongb[1:10000,4]),lty=1, xlim=c(-2,4), xlab = "X2",  ylim=c(0,4), main = "")
  lines(density(wrongb[10001:20000,4]),  lty=2)
  legend(2.8,4, levels(wrong.f), lty = 1:2,cex = 0.5, xjust=.5)
  
  plot(density(wrongb[1:10000,5]),lty=1, xlim=c(-3,1), xlab = "X3", ylim=c(0,4), main = "")
  lines(density(wrongb[10001:20000,5]),  lty=2)
  legend(0,4, levels(wrong.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  plot(density(wrongb[1:10000,6]),lty=1, xlim=c(-1,4), xlab = "X4",  ylim=c(0,4), main = "")
  lines(density(wrongb[10001:20000,6]),  lty=2)
  legend(2.8,4, levels(wrong.f), lty = 1:2,cex = 0.5, xjust=.4)
  
  #    sm.density.compare(wrongb[,6], wrongb[,1], xlab="wrong Parameter 5")
  #    legend("topright", levels(wrong.f), lty = 1:2)
  
  #* legend(locator(1), levels(cyl.f), lty = 1:2)
  
  # }
  #   dev.off()
  # }
  #}
}


#Compare_overlap(pop_mean = c(.5,1,1,-1,1), change_mean = c(0,0,1), perc_var = c(.5, .1, .1))
