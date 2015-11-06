library("testthat")


# The code below is taken verbatim (with permission) from the SCCS package by
# Yonas Ghebremichael-Weldeselassie, Heather Whitaker, and Paddy Farrington.


### Functions used to calculate the weights to be used as offset in the new model ###

#--------------------------------------------------------#
#       Weight function for Exponential- Weibull (Age)   #
#                    mixture Model                       #
#--------------------------------------------------------#

# p<-p_ewad2


wsmall_ewad2<-function(t,p, present,astart,aend, Dmatrix){

  thetaA  <- p[which.max(Dmatrix)]
  thetaB  <- p[(length(Dmatrix))+ (which.max(Dmatrix))] +   p[2*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(astart))
  eta     <- p[3*(length(Dmatrix))+ (which.max(Dmatrix))] + p[4*(length(Dmatrix))+ (which.max(Dmatrix))]*t
  gamma0  <- p[5*(length(Dmatrix))+ (which.max(Dmatrix))] + p[6*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(astart))

  lamA <-(exp(-thetaA))            # 1/rho in the paper
  lamB <-(exp(-thetaB))            # 1/mu
  pi0  <-(exp(eta)/(1+exp(eta)))   # pi
  nu0  <-(exp(gamma0))              # nu

  val <- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-t))+
                            (1-pi0)*nu0*lamB*((aend*lamB)^(nu0-1))*exp(-((aend*lamB)^nu0-(t*lamB)^nu0))) +
            present *log(pi0*exp(-lamA*(aend-t))+
                           (1-pi0)*exp(-((aend*lamB)^nu0-(t*lamB)^nu0))))
  exp(val)
}



#--------------------------------------------------------------#
#       Weight function for Exponential- Weibull (Interval)    #
#                    mixture Model                             #
#--------------------------------------------------------------#

# p<-p_ewid2


wsmall_ewid2<-function(t, p, present, aend, Dmatrix){

  thetaA  <- p[which.max(Dmatrix)]
  thetaB  <- p[(length(Dmatrix))+ (which.max(Dmatrix))] +   p[2*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(t))
  eta     <- p[3*(length(Dmatrix))+ (which.max(Dmatrix))] + p[4*(length(Dmatrix))+ (which.max(Dmatrix))]*t
  gamma0  <- p[5*(length(Dmatrix))+ (which.max(Dmatrix))] + p[6*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(t))


  lamA<-exp(-thetaA)            # 1/rho in the paper
  lamB<-exp(-thetaB)            # 1/mu
  pi0 <-exp(eta)/(1+exp(eta))   # pi
  nu0<-exp(gamma0)              # nu

  int<-aend-t

  val<- ((1-present)*log(pi0*lamA*exp(-lamA*int)+
                           (1-pi0)*nu0*lamB*((int*lamB)^(nu0-1))*exp(-((int*lamB)^nu0))) +

           present *log(pi0*exp(-lamA*int)+
                          (1-pi0)*exp(-((int*lamB)^nu0))))

  exp(val)
}

#--------------------------------------------------------#
#       Weight function for Exponential- Gamma (Age)     #
#                    mixture Model                       #
#--------------------------------------------------------#

# p<-p_egad2


wsmall_egad2 <- function(t,p,present,astart,aend,Dmatrix){

  thetaA  <- p[which.max(Dmatrix)]
  thetaB  <- p[(length(Dmatrix))+ (which.max(Dmatrix))] +   p[2*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(astart))
  eta     <- p[3*(length(Dmatrix))+ (which.max(Dmatrix))] + p[4*(length(Dmatrix))+ (which.max(Dmatrix))]*t
  gamma0  <- p[5*(length(Dmatrix))+ (which.max(Dmatrix))] + p[6*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(astart))

  lamA <-exp(-thetaA)            # 1/rho in the paper
  lamB <-exp(-thetaB)            # 1/mu
  pi0  <-exp(eta)/(1+exp(eta))   # pi
  nu0  <-exp(gamma0)             # nu

  rate0 <-nu0*lamB

  # val<- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-t))+
  #                 (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/pgamma(t,shape=nu0,rate=rate0,lower.tail=F)) +
  #                    present*log(pi0*exp(-lamA*(aend-t))+
  #                 (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/pgamma(t,shape=nu0,rate=rate0,lower.tail=F)))

  val<- ((1-present)*log(pi0*lamA*exp(-lamA*(aend-t))+
                           (1-pi0)*dgamma(aend,shape=nu0,rate=rate0)/ifelse(pgamma(t,shape=nu0,rate=rate0,lower.tail=F)==0,0.000000001, pgamma(t,shape=nu0,rate=rate0,lower.tail=F))) +
           present *log(pi0*exp(-lamA*(aend-t))+
                          (1-pi0)*pgamma(aend,shape=nu0,rate=rate0,lower.tail=F)/ifelse(pgamma(t,shape=nu0,rate=rate0,lower.tail=F)==0, 0.000000001, pgamma(t,shape=nu0,rate=rate0,lower.tail=F))))



  exp(val)
}

#--------------------------------------------------------#
#       Weight function for Exponential- Gamma (Interval)#
#                    mixture Model                       #
#--------------------------------------------------------#

# p<-p_egid2

wsmall_egid2 <- function(t,p,present,astart,aend,Dmatrix) {


  thetaA  <- p[which.max(Dmatrix)]
  thetaB  <- p[(length(Dmatrix))+ (which.max(Dmatrix))] +   p[2*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(t))
  eta     <- p[3*(length(Dmatrix))+ (which.max(Dmatrix))] + p[4*(length(Dmatrix))+ (which.max(Dmatrix))]*t
  gamma0  <- p[5*(length(Dmatrix))+ (which.max(Dmatrix))] + p[6*(length(Dmatrix))+ (which.max(Dmatrix))]*(log(t))

  lamA<-exp(-thetaA)            # 1/rho in the paper
  lamB<-exp(-thetaB)            # 1/mu
  pi0 <-exp(eta)/(1+exp(eta))   # pi
  nu0<-exp(gamma0)              # nu

  rate0 <-nu0*lamB

  int <-aend-t

  val<- ((1-present)*log(pi0*lamA*exp(-lamA*int)+
                           (1-pi0)*dgamma(int,shape=nu0,rate=rate0)) +
           present *log(pi0*exp(-lamA*int)+
                          (1-pi0)*pgamma(int,shape=nu0,rate=rate0,lower.tail=F)))
  exp(val)
}


test_that("Weight functions match those in SCCS package", {
  p <- c(0.1,0.2,0.1,0.2,0.1,0.2,0.1)
  present <- 1
  astart <- 1
  aend <- 10
  start <- 1
  end <- 2
  Dmatrix <- c(0)

  w1 <- SelfControlledCaseSeries:::testEwad(p, present, astart, aend, start, end)
  w2 <- integrate(wsmall_ewad2, lower = start, upper = end, p = p, present = present, astart = astart, aend = aend, Dmatrix = Dmatrix)$value
  expect_equal(w1,w2, tolerance = 1E-6)

  w1 <- SelfControlledCaseSeries:::testEwid(p, present, astart, aend, start, end)
  w2 <- integrate(wsmall_ewid2, lower = start, upper = end, p = p, present = present, aend = aend, Dmatrix = Dmatrix)$value
  expect_equal(w1,w2, tolerance = 1E-6)

  w1 <- SelfControlledCaseSeries:::testEgad(p, present, astart, aend, start, end)
  w2 <- integrate(wsmall_egad2, lower = start, upper = end, p = p, present = present, astart = astart, aend = aend, Dmatrix = Dmatrix)$value
  expect_equal(w1,w2, tolerance = 1E-6)

  w1 <- SelfControlledCaseSeries:::testEgid(p, present, astart, aend, start, end)
  w2 <- integrate(wsmall_egid2, lower = start, upper = end, p = p, present = present, aend = aend, Dmatrix = Dmatrix)$value
  expect_equal(w1,w2, tolerance = 1E-6)

})
