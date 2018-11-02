# The code in this file is copied (and subsequently modified) with permission from the SCCS package
# developed by Yonas Ghebremichael-Weldeselassie, Heather Whitaker, and Paddy Farrington
#
# One major modification: removed possibility to specify covariates for censoring models

fitModelsAndPickBest <- function(data) {

  fitCensorModel <- function(model, data){
    # This function gives a matrix created by multiplying
    # (Pointwise multiplication) a Matrix M by each column of Matrix S
    Yproduct <- function(S, M){
      product <- matrix(NA, nrow(S), ncol(S)*ncol(M))

      for (i in 1:ncol(S)) {
        product[,(1 + ncol(M)*(i-1)):(ncol(M)*i)] <- S[,i]*M
      }
      return
      product
    }

    #--------------------------------------------------------#
    #       Exponential- Weibull (Age) mixture Model         #
    #--------------------------------------------------------#
    mod_ewad2<-function(p, astart, aevent, aend, present){
      #   Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      #   Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))
      #
      #   Dmatrixstart <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(astart)))
      #   Dmatrixstartlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(astart))))

      #   thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaA <- rep(p[1], length(astart))
      #   thetaB <- Dmatrixstartlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      thetaB <- p[2] + p[3] * log(astart)
      #   eta    <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      eta <- p[4] + p[5] * aevent
      #   gamma0 <- Dmatrixstartlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      gamma0 <- p[6] + p[7] * log(astart)

      lamA<-exp(-thetaA)            # 1/rho in the paper
      lamB<-exp(-thetaB)            # 1/mu
      pi0 <-exp(eta)/(1+exp(eta))   # pi
      nu0<-exp(gamma0)              # nu

      lik<- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-aevent))+
                               (1-pi0)*nu0*lamB*((aend*lamB)^(nu0-1))*exp(-((aend*lamB)^nu0-(aevent*lamB)^nu0))) +
               present *log(pi0*exp(-lamA*(aend-aevent))+
                              (1-pi0)*exp(-((aend*lamB)^nu0-(aevent*lamB)^nu0))))
      l<-(-2)*sum(lik)
      #writeLines(paste(paste(p, collapse = ","), " L =", l))
      return (l)
    }

    #--------------------------------------------------------#
    #       Exponential- Weibull (Interval) mixture Model    #
    #--------------------------------------------------------#
    mod_ewid2 <-function(p, aevent, aend, present, Dmatrix){

      #   Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      #   Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))

      #   thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaA <- rep(p[1], length(aevent))
      #   thetaB <- Dmatrixeventlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      thetaB <- p[2] + p[3] * log(aevent)
      #   eta <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      eta <- p[4] + p[5] * aevent
      #   gamma0 <- Dmatrixeventlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      gamma0 <- p[6] + p[7] * log(aevent)


      lamA<-exp(-thetaA)            # 1/rho in the paper
      lamB<-exp(-thetaB)            # 1/mu
      pi0 <-exp(eta)/(1+exp(eta))   # pi
      nu0<-exp(gamma0)              # nu

      int <- aend-aevent
      lik<-

        ((1-present)*log(pi0*lamA*exp(-lamA*int)+
                           (1-pi0)*nu0*lamB*((int*lamB)^(nu0-1))*exp(-((int*lamB)^nu0))) +

           present *log(pi0*exp(-lamA*int)+
                          (1-pi0)*exp(-((int*lamB)^nu0))))
      l<-(-2)*sum(lik)
      #writeLines(paste(paste(p, collapse = ","), " L =", l))
      l
    }

    #--------------------------------------------------------#
    #       Exponential- Gamma (Age) mixture Model           #
    #--------------------------------------------------------#
    mod_egad2<-function(p, astart, aevent, aend, present, Dmatrix){

      #   Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      #   Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))
      #
      #   Dmatrixstart <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(astart)))
      #   Dmatrixstartlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(astart))))

      #   thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaA <- rep(p[1], length(astart))
      #   thetaB <- Dmatrixstartlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      thetaB <- p[2] + p[3] * log(astart)
      #   eta    <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      eta <- p[4] + p[5] * aevent
      #   gamma0 <- Dmatrixstartlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      gamma0 <- p[6] + p[7] * log(astart)


      lamA <-exp(-thetaA)            # 1/rho in the paper
      lamB <-exp(-thetaB)            # 1/mu
      pi0  <-exp(eta)/(1+exp(eta))   # pi
      nu0  <-exp(gamma0)              # nu

      rate0 <-nu0*lamB

      lik<-((1-present)*log(pi0*lamA*exp(-lamA*(aend-aevent))+
                              (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/ifelse(pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F)==0,0.000000001, pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F))) +
              present *log(pi0*exp(-lamA*(aend-aevent))+
                             (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/ifelse(pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F)==0, 0.000000001, pgamma(aevent,shape=nu0,rate=rate0,lower.tail=F))))

      l <-(-2)*sum(lik)
      #writeLines(paste(paste(p, collapse = ","), " L =", l))
      l
    }

    #--------------------------------------------------------#
    #       Exponential- Gamma (Interval) mixture Model      #
    #--------------------------------------------------------#
    mod_egid2<-function(p, aevent, aend, present, Dmatrix){

      #   Dmatrixevent <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(aevent)))
      #   Dmatrixeventlog <- cbind(Dmatrix, Yproduct(Dmatrix, as.matrix(log(aevent))))

      #   thetaA <- Dmatrix%*%p[1:ncol(Dmatrix)] #  (rho)
      thetaA <- rep(p[1], length(aevent))
      #   thetaB <- Dmatrixeventlog%*%p[(ncol(Dmatrix)+1):(3*(ncol(Dmatrix)))] # log(u(t,y))
      thetaB <- p[2] + p[3] * log(aevent)
      #   eta <- Dmatrixevent%*%p[((3*(ncol(Dmatrix))) + 1):(5*(ncol(Dmatrix)))]  # log(pi(t,y))
      eta <- p[4] + p[5] * aevent
      #   gamma0 <- Dmatrixeventlog%*%p[((5*(ncol(Dmatrix))) + 1):(7*(ncol(Dmatrix)))]  # log(nu(t,y))
      gamma0 <- p[6] + p[7] * log(aevent)

      lamA<-exp(-thetaA)            # 1/rho in the paper
      lamB<-exp(-thetaB)            # 1/mu
      pi0 <-exp(eta)/(1+exp(eta))   # pi
      nu0<-exp(gamma0)              # nu

      rate0 <-nu0*lamB

      int <- aend - aevent
      lik<-((1-present)*log(pi0*lamA*exp(-lamA*int)+
                              (1-pi0)*dgamma(int,shape=nu0,rate=rate0)) +
              present *log(pi0*exp(-lamA*int)+
                             (1-pi0)*pgamma(int,shape=nu0,rate=rate0,lower.tail=F)))

      l<-(-2)*sum(lik)
      #writeLines(paste(paste(p, collapse = ","), " L =", l))
      l
    }

    npar <- 7
    p0 <- rep(0.1, times=npar)   # inital values
    result <- tryCatch({
      if (model == 1){
        fit <- nlm(mod_ewad2, p=p0, astart=data$astart/365.25, aevent=data$aevent/365.25, aend=data$aend/365.25, present=data$present,
                   hessian = FALSE, iterlim=1000)
      } else if (model == 2){
        fit <- nlm(mod_ewid2, p=p0, aevent=data$aevent/365.25, aend=data$aend/365.25, present=data$present,
                   hessian = FALSE, iterlim=1000)
      } else if (model == 3){
        fit <- nlm(mod_egad2, p=p0, astart=data$astart/365.25, aevent=data$aevent/365.25, aend=data$aend/365.25, present=data$present,
                   hessian = FALSE, iterlim=1000)
      } else {
        fit <- nlm(mod_egid2, p=p0, aevent=data$aevent/365.25, aend=data$aend/365.25, present=data$present,
                   hessian = FALSE, iterlim=1000)
      }
      list(model = model, p = fit$estimate, aic = 2*npar + fit$minimum)
    }, error = function(e) {
      missing(e)  # suppresses R CMD check note
      list(model = model, p = rep(0,npar), aic = 999999999)
    })
    return(result)
  }
  writeLines("Fitting censoring models")
  cluster <- ParallelLogger::makeCluster(4)
  results <- ParallelLogger::clusterApply(cluster, 1:4, fitCensorModel, data)
  ParallelLogger::stopCluster(cluster)
  for (i in 1:4){
    if (results[[i]]$aic == 999999999) {
      if (results[[i]]$model == 1) {
        warning("Could not fit exponential - Weibull (Age) mixture Model")
      } else if (results[[i]]$model == 2) {
        warning("Could not fit exponential - Weibull (Interval) mixture Model")
      } else if (results[[i]]$model == 3) {
        warning("Could not fit exponential - Gamma (Age) mixture Model")
      } else {
        warning("Could not fit exponential - Gamma (Interval) mixture Model")
      }
    }
  }

  aics <- unlist(ParallelLogger::selectFromList(results, "aic"))
  minAic <- min(aics)
  best <- results[[which(aics == minAic)[1]]]
  if (best$model == 1) {
    writeLines("Best fit using exponential - Weibull (Age) mixture Model")
  } else if (best$model == 2) {
    writeLines("Best fit using exponential - Weibull (Interval) mixture Model")
  } else if (best$model == 3) {
    writeLines("Best fit using exponential - Gamma (Age) mixture Model")
  } else {
    writeLines("Best fit using exponential - Gamma (Interval) mixture Model")
  }
  return(list(p = best$p, model = best$model, aic = minAic))
}
