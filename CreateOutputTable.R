create_output <- function(x, silent=TRUE, rep_filter= "_rep", residuals, nsims) {
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
  est<-matrix(0,nrow=length(files),ncol=nparam)  #vector of length #outputs
  #est_mean <- matrix(0,nrow=1, ncol = 13)
  L <- matrix(0,nrow=length(files),ncol=nparam)
  U <- matrix(0,nrow=length(files),ncol=nparam)
  bias<-matrix(0,nrow=nparam,ncol=1)
  for(i in 1:length(files)){
    for(d in 1:ncol(est)){
      est[i,d]<-as.numeric(files[[i]][[9]][[1]][[3]][d])
      L[i,d]<-as.numeric(files[[i]][[9]][[1]][[6]][d])
      U[i,d]<-as.numeric(files[[i]][[9]][[1]][[7]][d])
    }
    DIC <- mean(as.numeric(files[[i]][[7]]$DIC))
  }
  for(i in 1:length(files)){
    rows<-paste(files[[i]][[9]][[1]][[1]],files[[i]][[9]][[1]][[2]])
    # files[[i]] <- as.data.frame(files[[i]][[9]][[1]])#[3:8], row.names = files[[i]][[9]][[1]][1])
  }
  if (residuals == "high"){
    bias[3,] <- mean(((as.numeric(est[,3])-0.5)/.5)*100)
    bias[4,] <- mean(((as.numeric(est[,4])-2)/2)*100)
    bias[10,] <- mean(as.numeric(est[,10])-0)
    bias[11,] <- mean(((as.numeric(est[,11])-0.5)/0.5)*100)
    Y2residw <- sqrt(mean((est[,3]-0.5)^2))
    Etaresidw <- sqrt(mean((est[,4]-2)^2))
    Y2residt <- sqrt(mean((est[,10]-0)^2))
    sresidt <- sqrt(mean((est[,11]-0.5)^2))
    true_value <- c(1.00,0.50,0.50,2.00,1.00,-1.00,1.00,1.00,0.00,0.00,0.50,
                    0.00,0.00,0.50,0.00)
  }
  if (residuals == "low"){
    bias[3,] <- mean(((as.numeric(est[,3])-0.2)/.2)*100)
    bias[4,] <- mean(((as.numeric(est[,4])-1)/1)*100)
    bias[10,] <- mean(as.numeric(est[,10])-0)
    bias[11,] <- mean(((as.numeric(est[,11])-0.2)/0.2)*100)
    Y2residw <- sqrt(mean((est[,3]-0.2)^2))
    Etaresidw <- sqrt(mean((est[,4]-1)^2))
    Y2residt <- sqrt(mean((est[,10]-0)^2))
    sresidt <- sqrt(mean((est[,11]-0.2)^2))
    true_value <- c(1.00,0.50,0.20,1.00,1.00,-1.00,1.00,1.00,0.00,0.00,0.20,
                    0.00,0.00,0.50,0.00)
  }
  bias[1,] <- "NA"
  bias[2,] <- mean(((as.numeric(est[,2])-0.5)/.5)*100)
  bias[5,] <- mean(((as.numeric(est[,5])-(1))/1)*100)
  bias[6,] <- mean(((as.numeric(est[,6])-(-1))/-1)*100)
  bias[7,] <- mean(((as.numeric(est[,7])-(1))/1)*100)
  bias[8,] <- mean(((as.numeric(est[,8])-1)/1)*100)
  bias[9,] <- mean((as.numeric(est[,9])-0))
  bias[12,] <- mean((as.numeric(est[,12])-0))
  bias[13,] <- "NA"
  bias[14,] <- mean(((as.numeric(est[,14])-0.5)/0.5)*100)
  bias[15,] <- "NA"
  eta_eta1 <- sqrt(mean((est[,2]-0.5)^2))
  s_x2 <- sqrt(mean((est[,5]-(1))^2))
  s_x3 <- sqrt(mean((est[,6]-(-1))^2))
  s_x4 <- sqrt(mean((est[,7]-(1))^2))
  y_x1 <- sqrt(mean((est[,8]-1)^2))
  Int_s <- sqrt(mean((est[,9]-0)^2))
 # Y2residt <- sqrt(mean((est[,10]-0)^2))
  Y2means <- sqrt(mean((est[,10]-0)^2))
  Y2vars <- sqrt(mean((est[,12]-0.5)^2))
  rmse<-rbind("NA",eta_eta1,Y2residw,Etaresidw,s_x2,s_x3,s_x4,y_x1,Int_s,Y2residt,sresidt,Y2means, "NA",Y2vars, "NA")
  est_mean <- colMeans(est)
  est <- as.data.frame(cbind(est_mean), row.names = c("ETA.by.Y2","eta_eta1","Y2residw","Etaresidw","s_x2","s_x3","s_x4",
                                                      "y_x1","Int_s","Y2residt","sresidt","Y2means", "NA","Y2vars", "NA2"))
  meanL <- round(colMeans(L), digits = 2)
  meanU <- round(colMeans(U), digits = 2)
  power <- mean(L>0 | U<0)
  CI <- paste0("(",meanL,",", meanU,")")
  results <- cbind(true_value,est, "95%CI"=CI, bias, rmse)
  convergence <- noquote(paste0(round((length(files)/nsims)*100, digits = 4), "%"))
  DIC <- as.data.frame(rbind(DIC,power, convergence), row.names = c("DIC", "Power", "convergence"))
  colnames(DIC) <-"Diagnostics"
  #  results = apply_labels(results,
  #                        true_value = "True Value",
  #                        est_mean  = "Estimate",
  #                        CI = "95% CI",
  #                        bias = "Bias",
  #                        rmse = "RMSE")
  #  DIC = apply_labels(DIC, )
  #print(title)
  #print(results)
  #print(DIC)
  # print(power)
  list("Title" = title, "Results" = results, "Diagnostics"=DIC)
}