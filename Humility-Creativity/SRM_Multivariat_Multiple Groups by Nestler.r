## ============================================================
## This file contains R code with examples from:
##
## Nestler, S.(2015). Restricted maximum likelihood estimation for parameters of the Social Relations Model. 
##
## This script file is licensed under a CC-BY 4.0 license. 
## For a human-readable summary of that license, 
## see http://creativecommons.org/licenses/by/4.0/
##
## (c) 2015 Steffen Nestler (steffen.nestler@uni-muenster.de) 
## 
## ============================================================

## The following function implements a Fisher scoring algorithm to estimate the parameters
## in a multivariate multiple groups SRM. Please, do not make any changes to the function.
## How to use the function is explained in the "Main program" part below.

LMM.SRM_MultivariatMultipleGroups <- function(starts,y,X,Zg,Z1,Z2,Z3,difcrit=10e-5,maxit=20,TypeOfConstrain=0) { 

    # 1. Definitions for the algorithm
   dif <- 100
   
   # 2.  Compute matrices
   # 2a. Groups 
   ZgtZg <- tcrossprod(Zg)
   g.M <- rbind(cbind(ZgtZg,ZgtZg),cbind(ZgtZg,ZgtZg))
   
   # 2b. individuals
   Z1tZ1 <- tcrossprod(Z1)
   Z2tZ2 <- tcrossprod(Z2)
   Z1tZ2 <- tcrossprod(Z1,Z2)
   Z2tZ1 <- tcrossprod(Z2,Z1)
   a.M  <- rbind(cbind(Z1tZ1,Z1tZ2),cbind(Z2tZ1,Z2tZ2))
   p.M  <- rbind(cbind(Z2tZ2,Z2tZ1),cbind(Z1tZ2,Z1tZ1))
   
   Z1cZ2 <- cbind(Z1,Z2)
   Z2cZ1 <- cbind(Z2,Z1)
   ap.M <- tcrossprod(rbind(Z1cZ2,Z2cZ1),rbind(Z2cZ1,Z1cZ2))
   
   # 2c. dyads
   r.P  <- tcrossprod(Z3)
   r.M  <- diag(1,2)%x%r.P
   rr.M <- matrix(c(0,1,1,0),2,2)%x%r.P
   
   # 2d. errors     
   e.M  <- diag(1,2*ncol(r.P))
   ee.M <- matrix(c(0,1,1,0),2,2)%x%diag(1,ncol(r.P))
     
   mlist <- list(g.M,a.M,p.M,ap.M,r.M,rr.M,e.M,ee.M)
   sp.mlist <- lapply(mlist,function(x) x <- Matrix(x,sparse=TRUE))
    
   # 3. Define starting values and start number for iterations
   Var <- starts        
   l <- 1
   
   # 4. Fisher scoring  for variance parameters
   while (dif > difcrit & l <= maxit) {
	
     l  <- l + 1  
     V <- Reduce('+',Map('*',mlist,Var)) 
     Vi <- solve(V)  # this is the slowest part of the algorithm
     P <- Vi - (Vi%*%X)%*%(solve(crossprod(X,Vi)%*%X))%*%(crossprod(X,Vi))   
     
     # score vector
     sc <- matrix(0, ncol=1, nrow=length(Var))
     sc <- as.matrix(unlist(lapply(mlist,function(x) -sum(P*t(x))+((crossprod(y,P)%*%x)%*%(P%*%y)))),ncol=1,nrow=length(Var))

     # expected information matrix
     In  <- matrix(0, ncol=length(starts), nrow=length(starts))
     for (i in 1:length(Var)) {
         pa.i <- P%*%sp.mlist[[i]]          
         for (j in i:length(Var)) {
            pb.j <- P%*%sp.mlist[[j]] 
            In[i,j] <- sum(pa.i*t(pb.j))                  
          } 
     }
     In <- In + t(In) - diag(diag(In)) 
     
     # compute new variance-covariance estimates   
     newVar <- Var + (solve(In)%*%sc)
     dif <- (t(Var-newVar)%*%(Var-newVar))/(t(Var)%*%Var) 
     Var <- newVar

   }
     
   # 5. Implement method of Demidenko (2013, pp. 198-203) to replace undefined 
   # parameter estimates with admissible paramater estimates
   
   if (TypeOfConstrain==1) {
     
     tVar <- (1/Var[7])*Var   
     Ds  <- adiag(tVar[1],matrix(c(tVar[2],tVar[4],tVar[4],tVar[3]),2,2),matrix(c(tVar[5],tVar[6],tVar[6],tVar[5]),2,2))
     if (min(eigen(Ds)$values) < 0) {  
         lambda. <- diag(eigen(Ds)$values,ncol(Ds))
         lambda.[ lambda. < 0 ] <- 0
         Dplus <- (eigen(Ds)$vectors)%*%lambda.%*%t(eigen(Ds)$vectors)
         tVar[1:6] <- c(Dplus[1,1],Dplus[2,2],Dplus[3,3],Dplus[3,2],Dplus[4,4],Dplus[4,5])
      }
      Var <- Var[7]*tVar
    
   }
       
   # 6. Get standard errors of the variance parameters
   # by computing the observed information matrix
   # a. Compute fixed effects
   b <- solve(t(X)%*%Vi%*%X)%*%t(X)%*%(Vi%*%y)
   
   # b. Observed information matrix
   In  <- matrix(0, ncol=length(starts), nrow=length(starts))
   for (i in 1:length(Var)) {      
       pa.i <- P%*%sp.mlist[[i]]       
       for (j in i:length(Var)) {       
            pa.j <- P%*%sp.mlist[[j]]
            In[i,j] <- ((t(y-X%*%b)%*%Vi)%*%sp.mlist[[i]]%*%P%*%sp.mlist[[j]]%*%(Vi%*%(y-X%*%b))-0.5*(sum(pa.i*t(pa.j))))[1]                  
       } 
   }
   In <- In + t(In) - diag(diag(In)) 
   s.In <- solve(In) 
   ses <- sqrt(diag(s.In))
   if (min(eigen(In)$values) <= 0) { error <- 1; } else { error <- 0; }
   
   # 7. Compute standard errors for the fixed effects
   # 7a. Compute standard error of fixed effects and t-values
   sigma.b <- solve(t(X)%*%Vi%*%X)
   ses.b <- sqrt(diag(sigma.b))
   ts <- b/ses.b

   #  b. Satterthwaite approximation of dfs
   cma <- diag(1,ncol(X)) 
   dfs <- matrix(0,nrow=ncol(X),ncol=1)
   for (k in 1:ncol(cma)) {
      cs <- cma[,k]
      # gradient
      gr  <- as.matrix(unlist(lapply(mlist,function(x) (t(cs)%*%sigma.b)%*%t(X)%*%Vi%*%x%*%Vi%*%X%*%(sigma.b%*%cs))),ncol=1,nrow=length(Var))
      # dfs
      dfs[k,1] <- (2*(t(cs)%*%sigma.b%*%cs)^2)/(t(gr)%*%solve(In)%*%gr)
   } 
   pss <- 2*pt(-abs(ts),df=dfs)
   
   # 8. export results
   OutRan <- data.frame(Parameter=c("Group variance","Actor variance","Parner variance","Actor-Partner covariance",
                                    "Relationship variance","Relationship covariance",
                                    "Error variance","Error covariance"),
                        Estimate=round(c(Var[1],Var[2],Var[3],Var[4],Var[5],Var[6],Var[7],Var[8]),4),
                        SE=round(c(ses[1],ses[2],ses[3],ses[4],ses[5],ses[6],ses[7],ses[8]),4))
   OutFix <- round(data.frame(Effects=b,Standard_errors=ses.b,ts=ts,app_dfs=dfs,app_ps=pss),4)
   OutAlg <- data.frame(NoOfIterations=l,ConvergeCriterion=dif,Error=error)
   
   result=list(OutAlg,OutFix,OutRan)
   names(result)=c("Information_algorithm","Fixed_effects","Random_effects")
   return(result) 
   
}

# library(Matrix)
# library(plyr)
if (!require("Matrix")) install.packages("Matrix"); library(Matrix)
if (!require("magic"))  install.packages("magic");  library(magic)

# The following documentation and example is commented out.
## ++++++++++++++++++
##   Main program
## ++++++++++++++++++

# Import data

# setwd("") # change to your working directory
#setwd("C:\\Users\\owner\\Dropbox\\STAT\\Listening Projects\\SRM project\\REML")
# daten <- read.table("Hallmarketal.dat", header=T)
# 
# # Sort the data according to the matrix formulation of the SRM
# 
# daten <- daten[order(daten$Group,daten$Actor,daten$Partner,daten$Dyad,daten$Occassion),]
# daten.1 <- subset(daten,daten$Actor < daten$Partner)
# daten.1 <- daten.1[order(daten.1$Group,daten.1$Actor,daten.1$Partner,daten.1$Dyad,daten.1$Occassion),]
# daten.2 <- subset(daten,daten$Actor > daten$Partner)
# daten.2 <- daten.2[order(daten.2$Group,daten.2$Dyad,daten.2$Occassion),]
# daten   <- rbind(daten.1,daten.2)                                                

# Compute design matrices

# Zg <- with(daten.1, outer(Group, unique(Group), '==')*1)
# groups <- unique(daten.1$Group)
# for (i in 1:length(groups)) {    
#    g    <- subset(daten.1,daten.1$Group==groups[i])
#    m.Z1 <- with(g, outer(Actor, sort(unique(daten[daten$Group==groups[i],]$Actor)), '==')*1)
#    m.Z2 <- with(g, outer(Partner, sort(unique(daten[daten$Group==groups[i],]$Partner)), '==')*1)
#    m.Z3 <- with(g, outer(Dyad, sort(unique(daten[daten$Group==groups[i],]$Dyad)), '==')*1)
#    if (i == 1) { Z1 <- m.Z1; Z2 <- m.Z2; Z3 <- m.Z3;
#    } else { Z1 <- adiag(Z1, m.Z1); Z2 <- adiag(Z2,m.Z2); Z3 <- adiag(Z3,m.Z3)}
# }
# 
# # Get X and y
# X <- matrix(data=1, ncol=1, nrow=length(daten$Wert)) 
# y <- as.matrix(daten$Wert)
#   
# # Call the function to estimate the SRM parameters. Please note that the function allows you to change 
# # (a) the starting values of the variance components (the starts-vector), (b) the convergence
# # criterion of the Fisher scoring algorithm (difcrit), (c) the number of iterations of the algorithm (maxit), and
# # (d) you can choose between unconstrained maximization of the log-likelihood (TypeOfConstrain=0) or
# # unconstrained maximization and later replacement of inadmissable estimates with admissible ones.    
# 
# LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,0,1,0,1,0),y,X,Zg,Z1,Z2,Z3,difcrit=10e-5,maxit=20,TypeOfConstrain=0)
# 
# # Estimate SRM parameters with replacement of inadmissible estimates after convergence with admissible estimates
# LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,0,1,0,1,0),y,X,Zg,Z1,Z2,Z3,difcrit=10e-5,maxit=20,TypeOfConstrain=1)
# 

