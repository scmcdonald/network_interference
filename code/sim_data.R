

# time issue is scale of data - need to log the data

library(igraph)
library(tidyverse)
library(BigVAR)
set.seed(4)

N = 4
s_epsilon = 0.01
s_alpha = 0
s_beta = 0

p = 1/N

rho_1 = .95
rho_2 = 0


y_0 = rep(0, N)


alpha = rnorm(N, mean = 0, sd = s_alpha)
beta = rnorm(N, mean = 0, sd = s_beta)
epsilon = rnorm(N, mean = 0, sd = s_epsilon)


G <- erdos.renyi.game(N, p, type=c("gnp"), directed = FALSE, loops = F) %>%
  as_adjacency_matrix(sparse = F)


periods = 200
Y <- matrix(nrow = N, ncol = periods + 1)
colnames(Y) <- 0:periods

Y[, "0"] <- rnorm(N, mean = 0, sd = 1)

for(t1 in 1:periods){
  
  t = t1 - 1
  
  y_t <- Y[, paste(t)]
  
  
  y_t1 <- alpha + beta + rho_1 * y_t + (rho_2 * G %*% y_t )+ epsilon
  
  Y[, paste(t1)] <- y_t1
  
}


identity = diag(N)

comparison_matrix = ((rho_1 * identity) +( rho_2 *G))
# compute frobenius distance





# get prediction date, start and end dates of actual data

period = 1
lag = 1

mod <-constructModel(t(Y) ,
                     gran = c(50, 10),
                     h = period, 
                     p = lag,
                     struct = "HLAGOO",
                     recursive = T,
                     verbose=TRUE,
                     IC=TRUE,
                    model.controls=list(intercept=FALSE))

result <- cv.BigVAR(mod)

coefs <- coef(result)




#model=BigVAR.fit(t(Y),p = 1,
 #                struct = "Basic",
  #               lambda=10, 
   #              intercept=F)


model=BigVAR.fit(t(Y),p = 1,
                 struct = "Basic",
                 lambda=1e-20, 
                 intercept=F)

mod_subset <- model[,,1][1:4, ]

library(vars)

model <- VAR(t(Y), p = 1)


### ignore after here

VARX = list()
p = 1
k <- ncol(Y)
VARX <- FALSE
s <- p
s1 <- 0
m <- 0

VARXCons <- function(Y1, X1, k, p, m, s, oos = FALSE, contemp = FALSE) {
  .Call('_BigVAR_VARXCons', PACKAGE = 'BigVAR', Y1, X1, k, p, m, s, oos, contemp)
}

Z1 <- VARXCons(Y, matrix(0, nrow = nrow(Y)), k, p, 0, 0)

trainZ <- Z1[2:nrow(Z1), , drop = F]

trainY <- matrix(Y[(p + 1):nrow(Y), ], ncol = k)

lambda <- 1e-20
gran2 <- length(lambda)

k1 <- k
s <- 0
group <- "Basic"
# s needs to be s+ s1 create indices for group structures
create_group_indexes <- function(group, p, k, gran2, VARX = FALSE, k1 = NULL, s = NULL) {
  starting_eigvals <- NULL
  groups <- NULL
  compgroups <- NULL
  activeset <- NULL
  if (VARX) {
    if (group == "Lag") {
      
      groups <- groupfunVARX(p, k, k1, s)
      compgroups <- groupfunVARXcomp(p, k, k1, s)
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
    } else if (group == "SparseLag") {
      
      groups <- groupfunVARX(p, k, k1, s)
      compgroups <- groupfunVARXcomp(p, k, k1, s)
      starting_eigvals <- list()
      
      for (i in 1:(p + s)) {
        
        starting_eigvals[[i]] <- matrix(runif(k1, -1, 1), ncol = 1)
        
      }
      
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
      
    } else if (group == "OwnOther") {
      groups <- diaggroupfunVARX(p, k, k1, s)
      compgroups <- diaggroupfunVARXcomp(p, k, k1, s)
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
      
    } else if (group == "SparseOO") {
      groups <- diaggroupfunVARX(p, k, k1, s)
      compgroups <- diaggroupfunVARXcomp(p, k, k1, s)
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
    }
    
    
  } else {
    if (group == "Lag") {
      
      groups <- .groupfuncpp(p, k)
      
      
      compgroups <- .groupfuncomp(p, k)
      
      
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
    } else if (group == "SparseLag") {
      
      groups <- .groupfuncpp(p, k)
      compgroups <- .groupfuncomp(p, k)
      
      starting_eigvals <- list()
      
      for (i in 1:p) {
        
        starting_eigvals[[i]] <- matrix(runif(k, -1, 1), ncol = 1)
      }
      
      
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
      
      
    } else if (group == "OwnOther") {
      
      groups <- .lfunction3cpp(p, k)
      compgroups <- .lfunctioncomp(p, k)
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      
    } else if (group == "SparseOO") {
      
      groups <- .lfunction3cpp(p, k)
      compgroups <- .lfunctioncomp(p, k)
      activeset <- rep(list(rep(rep(list(0), length(groups)))), gran2)
      starting_eigvals <- list()
      
      for (i in 1:(2 * p)) {
        
        starting_eigvals[[i]] <- matrix(runif(length(groups[[i]]), -1, 1), ncol = 1)
      }
      
    }
  }
  return(list(groups = groups, compgroups = compgroups, starting_eigvals = starting_eigvals, activeset = activeset))
}


grps <- create_group_indexes(group, p, k, gran2)

groups <- grps$group
compgroups <- grps$compgroups
activeset <- grps$activeset
starting_eigvals <- grps$starting_eigvals
beta <- array(0, dim = c(k, k * p + 1, gran2 * length(alpha)))

.BigVAR.fit <- function(group, beta, trainZ, trainY, lambda, tol, p, m = 0, k1, k,
                        s = 0, s1 = 0, MN = FALSE, C, intercept = TRUE, separate_lambdas, dual, activeset = NULL,
                        starting_eigvals = NULL, groups = NULL, compgroups = NULL, VARX = FALSE, alpha = NULL,
                        palpha=NULL, gamma=3) {
  if (is.null(s)) {
    s <- 0
  }
  if (is.null(s1)) {
    s1 <- 0
  }
  if (is.null(m)) {
    m <- 0
  }
  pre_proc <- pre_process(trainY, trainZ, C, MN, intercept)
  
  if (separate_lambdas) {
    if (is.vector(lambda)) {
      lambda <- matrix(lambda, nrow = 1)
    }
    gran2 <- nrow(lambda)
  }
  
  
  trainY <- pre_proc$Y
  trainZ <- pre_proc$Z
  YMean <- pre_proc$YMean
  ZMean <- pre_proc$ZMean
  C <- pre_proc$C
  
  if (group == "Basic") {
    
    beta <- .lassoVARFistX(beta, trainZ, trainY, lambda, tol, p, MN, k,
                           k1, s + s1, m, C, YMean, ZMean, separate_lambdas)
    
    
  }
  
  
  if(group=="MCP"|group=="SCAD")
  {
    
    beta <- .MCPFit(beta,trainZ,trainY,lambda,tol,p,MN,k,k1,s,m,C,group,gamma, YMean, ZMean)
    
  }
  
  if (group == "BasicEN") {
    
    beta <- .lassoVARFistXEN(beta, trainZ, trainY, lambda, alpha, tol, p, MN, k,
                             k1, s + s1, m, C, YMean, ZMean, separate_lambdas)
    
  }
  
  
  if (group == "Lag") {
    
    GG <- .GroupLassoVAR1(beta, groups, compgroups, trainY, trainZ, lambda, activeset,
                          tol, p, MN, k, k1, s + s1, C, YMean, ZMean)
    
    beta <- GG$beta
    
    activeset <- GG$active
    
  }
  
  if (group == "SparseLag") {
    
    if (VARX) {
      
      if (!dual) {
        GG <- .SparseGroupLassoVARX(beta, groups, compgroups, trainY, trainZ,
                                    lambda, alpha, INIactive = activeset, tol, starting_eigvals, p, MN,
                                    k, s + s1, k1, C, YMean, ZMean)
        
      } else {
        
        GG <- .SparseGroupLassoVARXDual(beta, groups, compgroups, trainY,
                                        trainZ, lambda, alpha, INIactive = activeset, tol, starting_eigvals,
                                        p, MN, k, s + s1, k1, C, YMean, ZMean)
        
      }
    } else {
      
      if (!dual) {
        GG <- .SparseGroupLassoVAR(beta, groups, compgroups, trainY, trainZ,
                                   lambda, alpha, INIactive = activeset, tol, starting_eigvals, p, MN,
                                   C, YMean, ZMean)
        
      } else {
        GG <- .SparseGroupLassoVARDual(beta, groups, compgroups, trainY,
                                       trainZ, lambda, alpha, INIactive = activeset, tol, starting_eigvals,
                                       p, MN, C, YMean, ZMean)
        
        
      }
    }
    
    beta <- GG$beta
    
    activeset <- GG$active
    
    starting_eigvals <- GG$q1
    
  }
  
  if (group == "OwnOther") {
    
    if (VARX) {
      
      GG <- .GroupLassoOOX(beta, groups, compgroups, trainY, trainZ, lambda,
                           activeset, tol, p, MN, k, k1, s + s1, C, YMean, ZMean)
      
    } else {
      
      
      GG <- .GroupLassoOO(beta, groups, compgroups, trainY, trainZ, lambda, activeset,
                          tol, p, MN, C, YMean, ZMean)
    }
    
    beta <- GG$beta
    
    activeset <- GG$active
    
  }
  
  if (group == "SparseOO") {
    if (VARX) {
      
      GG <- .SparseGroupLassoVAROOX(beta, groups, compgroups, trainY, trainZ,
                                    lambda, alpha, INIactive = activeset, tol, p, MN, k1, s + s1, k, dual,
                                    C, YMean, ZMean)
      
    } else {
      
      GG <- .SparseGroupLassoVAROO(beta, groups, compgroups, trainY, trainZ,
                                   lambda, alpha, INIactive = activeset, tol, starting_eigvals, p, MN,
                                   dual, C, YMean, ZMean)
      
      starting_eigvals <- GG$q1
      
    }
    
    beta <- GG$beta
    
    activeset <- GG$active
    
  }
  
  if (group == "Tapered") {
    
    beta <- .lassoVARTL(beta, trainZ, trainY, lambda, tol, p, MN, palpha, C, YMean,
                        ZMean)
  }
  
  if (group == "EFX") {
    
    beta <- .EFVARX(beta, trainY, trainZ, lambda, tol, MN, k1, s, m, p, C, YMean,
                    ZMean)
    
  }
  
  if (group == "HLAGC") {
    beta <- .HLAGCAlg(beta, trainY, trainZ, lambda, tol, p, MN, C, YMean, ZMean,
                      separate_lambdas)
    
  }
  
  if (group == "HLAGOO") {
    beta <- .HLAGOOAlg(beta, trainY, trainZ, lambda, tol, p, MN, C, YMean, ZMean,
                       separate_lambdas)
  }
  
  if (group == "HLAGELEM") {
    
    beta <- .HLAGElemAlg(beta, trainY, trainZ, lambda, tol, p, MN, C, YMean, ZMean,
                         separate_lambdas)
    
  }
  
  if (group == "BGR") {
    trainZ <- rbind(1, trainZ)
    beta <- BGRGridSearch(trainY, trainZ, p, lambda, as.numeric(MN))
  }
  
  ## if (group == "MCP" | group == "SCAD") {
  
  ##     beta <- .MCPFit(beta, trainZ, trainY, lambda, tol, p, MN, k, k1, s, m, C, YMean,
  ##         ZMean, group, lambda)
  
  ## }
  
  
  
  if (!exists("activeset")) {
    activeset <- NULL
  }
  if (!exists("starting_eigvals")) {
    starting_eigvals <- NULL
  }
  return(list(beta = beta, activeset = activeset, starting_eigvals = starting_eigvals))
  
}


beta <- .lassoVARFistX(beta, trainZ, trainY, lambda, tol, p, MN, k,
                       k1, s + s1, m, C, YMean, ZMean, separate_lambdas)

.lassoVARFistX <- function(B, Z, Y, lambda, eps, p, MN, k, k1, s, m, C, YMean, ZMean, separate_lambdas = FALSE) {
  if (!is.matrix(Y)) {
    Y <- matrix(Y, ncol = 1)
  }
  
  tk <- 1/max(Mod(eigen(Z %*% t(Z), only.values = TRUE)$values))
  
  B1 <- abind::adrop(B[, 2:dim(B)[2], 1, drop = F], 3)
  
  
  nc <- apply(B, 3, ncol)[1]
  
  BINI <- B[, 2:nc, , drop = F]
  beta <- gamloopFista(BINI, Y, Z, as.matrix(lambda), eps, as.matrix(YMean), as.matrix(ZMean), B1, k, p, tk, k1, s, separate_lambdas)
  
  if (MN) {
    beta <- adjust_mn_var(beta, C)
  }
  return(beta)
  
}

gamloopFista <- function(beta_, Y, Z, gammgrid, eps, YMean2, ZMean2, B1, k, p, tk, k1, s, sep_lambda = FALSE) {
  .Call('_BigVAR_gamloopFista', PACKAGE = 'BigVAR', beta_, Y, Z, gammgrid, eps, YMean2, ZMean2, B1, k, p, tk, k1, s, sep_lambda)
}
tol = 1e-04

## browser()
temp <- .BigVAR.fit(group, beta, trainZ, trainY, lambda, tol, p, m, k1, k,
                    s, s1, MN = F, C, intercept = F, separate_lambdas = F, dual, activeset, starting_eigvals,
                    groups, compgroups, VARX, alpha, palpha)
beta <- temp$beta

####


library(glmnet)

test <- cv.glmnet(x = Y[, "49":"50"], y = Y[, "51"], alpha = 1)
best_lambda <- test$lambda.min


best_model <- glmnet(x = Y[, "49":"50"], y = Y[, "51"], alpha = 1, lambda = best_lambda, intercept = F)
coef(best_model)

Y[, "49":"50"] %*% best_model$beta

model.df <- as.data.frame(model)[, -1]
colnames(model.df) <- 1:N


test <- BigVar.fit(t(Y))

pre_process(Y= t(Y),intercept = F)


pre_process <- function(Y, Z, C1, MN, intercept) {
  k <- ncol(Y)
  if (MN) {
    YC <- MN_prior(Y, Z, C1)
    Y <- YC$Y
    C <- YC$C
  }
  
  Y <- t(Y)
  
  if (intercept) {
    YMean <- c(apply(Y, 1, mean))
    ZMean <- c(apply(Z, 1, mean))
    if (k > 1) {
      Y <- Y - YMean %*% t(c(rep(1, ncol(Y))))
    } else {
      Y <- Y - mean(Y)
    }
    Z <- Z - ZMean %*% t(c(rep(1, ncol(Z))))
    
  } else {
    YMean <- rep(0, nrow(Y))
    ZMean <- rep(0, nrow(Z))
  }
  Y <- t(Y)
  return(list(Y = Y, Z = Z, YMean = YMean, ZMean = ZMean, C = C))
  
}




#Z = VARXLagCons(Y,p=1,oos=TRUE)$Z

#solve(model[,,1][, -1], Y[, "0"])

#model[,,1][, -1] %*% Y[, "0"]





library(vars)
var_est <- VAR(y = t(Y[, 1:10]), p = 1)

var_est_summary <- summary(var_est)


#saveRDS(result, file = here( paste("data/result/sim_result_", prediction_date, "_L", 1,".rds", sep = "")))

coefficients <- coef(result)



