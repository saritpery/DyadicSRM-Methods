################################################################################
# This file implements Nestler's code for testing the group level in SRM lenses.
# Nestler, S.(2015). Restricted maximum likelihood estimation for parameters of 
#                     the Social Relations Model. 
# 
# Authors: Prof. Avi Kluger
#          Sarit Pery
# 
# Code aim:
#    run LMM.SRM_MultivariatMultipleGroups function by Nestler on study's data
#    in this file's documentation, we'll alias the function name LMM.
#    
# Code structure:
#   four functions for running and presenting the group-level analysis: 
#   1. buildLMMDataset: subsets and formats a single construct data.
#   2. runLMM:    follows Nestler's example: 
#                 sets the paramethers and runs the LMM function using a single 
#                 construct data.
#   3. LMMPublicationOutput: Builds on the LMM analysis results per construct, 
#                 and adds standardized estimates, t-values, and p-values.
#   4. rmle2Word: saves the study's rmle tables in a MSWord document.
# Main:
#  For each Construct run the 3 functions.
#  Save an R image with the group-level data of the study.
################################################################################


# Clean the environment for a fresh start --------------------------------------
rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots


# Load data & packages ---------------------------------------------------------
source("00_utility Functions.R")
load("listeningLong.Rimage")
#load("JDC.Humility.FullRR.ReadyforAnalaysis") 

# Load the LLM function from source Code by Nestler, S.(2015).
# The original source code includes an example that we commented out.
source("SRM_Multivariat_Multiple Groups by Nestler.r") 

ipak (c("magic", "Matrix", "apaTables", "TripleR", "tidyverse", "reshape2"))

# Utility parameters and functions ---------------------------------------------
idCols <- c("a.id", "p.id", "group.id", "Actor", "Partner")

# adjust listening dataset
x <- rename(x, group.id = Group, a.id = Actor, p.id = Partner)
x$Actor <- as.numeric(sub(".*:", "", x$a.id) )
x$Partner <- as.numeric(sub(".*:", "", x$p.id))
  

# only biVariate constructs are analyzed in group-level SRM

constructs <- c("L")
consNames <- c("Listening")
writeDoc <- TRUE

# buildLMMDataset returns a dataset ready for LMM analysis per construct
buildLMMDataset <- function(cons, dataset = x, runtripleR = FALSE){
  # The names convention aims to mimic the names convention used by Nestler
  # in his example. This is done for easy review of code vs. example.
  # daten = the dataset name
  # wert  = value in German.

  # 1. identify the relevant columns, and subset the data ----------------------
  uni <- FALSE #Indication for a single item variable
  varcols <- grep(paste0(cons, ".$"), names(dataset), value = TRUE)
  if (length(varcols) == 0) {
    uni <- TRUE
    varcols <- grep(paste0(cons, "$"), names(dataset), value = TRUE)
  }
  daten <- dataset[,c(idCols, varcols)]

  # 2. make the data longer: each item becomes an Occassion in a separate row
  #    the names are set according to Nestler's example for easy review.
  daten <- melt(daten, id.vars = idCols, measure.vars = varcols, 
                variable.name = "Occassion", value.name = "Wert")
  if (uni) daten$Occassion <- 1 else
    daten$Occassion <- as.integer(gsub(cons, "", daten$Occassion))
  daten$Wert <- as.integer(daten$Wert)
  # drop NA rows
  daten <- na.omit(daten)
  
  # 3. set a numbering for each dyad in the groups -----------------------------
  daten$dyad <- apply(daten[,c("Actor", "Partner")], 1,
                      function (m) {10*min(m) + max(m)})

  daten$dyad <- as.integer(as.factor(daten$dyad)) 
  daten <- select(daten, -all_of(c("Actor", "Partner")))
  
  return(daten)
} # End of buildLMMDataset function

runLMM <- function(daten){
  # This code is taken directly from Nestler's example
  # with additional documentation.
    
  #adjust names of daten to match Nestler's 
  daten <- daten %>% 
    rename(Group = group.id, Dyad = dyad, Actor = a.id, Partner = p.id)
  
  # Sort the data according to the matrix formulation of the SRM

  daten <- daten[order(daten$Group,daten$Actor,daten$Partner,daten$Dyad,daten$Occassion),]
  daten.1 <- subset(daten,daten$Actor < daten$Partner)
  daten.1 <- daten.1[order(daten.1$Group,daten.1$Actor,daten.1$Partner,daten.1$Dyad,daten.1$Occassion),]
  daten.2 <- subset(daten,daten$Actor > daten$Partner)
  daten.2 <- daten.2[order(daten.2$Group,daten.2$Dyad,daten.2$Occassion),]
  daten   <- rbind(daten.1,daten.2)                                                

  # Compute design matrices
  
  # Zg is a dummy code matrix for rows-groups.
  # It has a column for each group, a row for each row in daten1
  # the value in Zg is 1 for the right group-column, 0 for all the rest of the columns.
  Zg <- with(daten.1, outer(Group, unique(Group), '==')*1)
  for (i in 1:length(groups)) {  # for each group  
    gid <- groups[i]
    g    <- subset(daten.1,daten.1$Group==gid) # a single group in daten1
    dtg <- daten[daten$Group==gid,] # the group's data in the full daten 
   
    # the following matrices are dummy codes for actor, partner and dyad
    # where the columns are the full list of a/p/d in the group in daten
    # the rows are the daten1 group's rows
    # and the value is 1 if this is the right a/p/d column and 0 otherwise.
    m.Z1 <- with(g, outer(Actor, sort(unique(dtg$Actor)), '==')*1)
    m.Z2 <- with(g, outer(Partner, sort(unique(dtg$Partner)), '==')*1)
    m.Z3 <- with(g, outer(Dyad, sort(unique(dtg$Dyad)), '==')*1)
    
    # Z1-Z3 will hold respectively, the a/p/d matrices, 
    #   bound together corner-to-corner.
    if (i == 1) { Z1 <- m.Z1; Z2 <- m.Z2; Z3 <- m.Z3;
    } else { Z1 <- adiag(Z1, m.Z1); Z2 <- adiag(Z2,m.Z2); Z3 <- adiag(Z3,m.Z3)}
  }
  # Get X and y
  # x, y are matrices of size 1X(values length).
  # x has 1s as its data
  # y holds all the ratings.
  # This means, that y, Zg, Z1-Z3 together hold all the required data for analysis.
  # together they have the data with group, and a,p,d information. 
  X <- matrix(data=1, ncol=1, nrow=length(daten$Wert)) 
  y <- as.matrix(daten$Wert)
  
  # Call the function to estimate the SRM parameters. 
  # Please note that the function allows you to change 
  # (a) the starting values of the variance components (the starts-vector), 
  # (b) the convergence criterion of the Fisher scoring algorithm (difcrit), 
  # (c) the number of iterations of the algorithm (maxit), and
  # (d) you can choose between unconstrained maximization of the log-likelihood 
  #     (TypeOfConstrain=0) or unconstrained maximization and later replacement 
  #     of inadmissable estimates with admissible ones.    
  
  #LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,0,1,0,1,0),y,X,Zg,Z1,Z2,Z3,
  #                                   difcrit=10e-5,maxit=20,TypeOfConstrain=0)
  
  # Estimate SRM parameters with replacement of inadmissible estimates after 
  # convergence with admissible estimates
  fit <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,0,1,0,1,0),y,X,
                                           Zg,Z1,Z2,Z3,difcrit=10e-5,maxit=20,
                                           TypeOfConstrain=1)
  return(fit)
} # End of runLMM function

LMMPublicationOutput <- function(fit, daten, consName){
  estimates <- as.data.frame(fit[[3]])
  TotalVariance <- sum(estimates[c(1:3, 5, 7), "Estimate"])
  estimates$standardized <- estimates$Estimate
  estimates$standardized [c(1:3, 5, 7)] <-  (estimates$Estimate [c(1:3, 5, 7)] ) /
    TotalVariance 
  estimates$standardized [4] <- estimates$Estimate [4]/ 
    (estimates$Estimate [2] * estimates$Estimate [3])
  estimates$standardized [6] <- estimates$Estimate [6]/ estimates$Estimate [5]
  estimates$standardized [8] <- estimates$Estimate [8]/ estimates$Estimate [7]
  estimates$t                <- estimates$Estimate / estimates$SE 
  estimates$p                <- (1- pt(abs(estimates$t) , nrow(daten))) *2
  estimates[, -1]            <- round(estimates[, -1], 3)
  a <- paste0(c("\n",consName, ": SRM estimates based on RMLE\n\n"))
  cat(a)
  print(estimates)
  return(estimates)
}

rmle2Word <- function(rmlelist, fileName = ""){
  ipak("rtf")
  rmleF <- RTF(fname(fileName, fileExtention = ".doc"))
  addParagraph(rmleF, "Study group-level SRM Analysis\n\n\n")
  for (i in 1:length(constructs)) {
    # the addParagraph doesn't work in a loop. 
    headerCons <- paste0(c("\n",consNames[i], ": SRM estimates based on RMLE\n"))
    addParagraph(rmleF, headerCons)
    # Bypass for header: 
    t <- as.data.frame(rmlelist[[i]])
    names(t) <- gsub("Parameter", 
                     paste("Parameter - ", consNames[i]),
                     names(t))
    addTable(rmleF, t)
  }
  done(rmleF)
} # End of rmle2Word function
################################################################################
##                       MAIN PROGRAM                                        ###
################################################################################
  
rmleTables <- list() #All output RMLE tables will be saved here. 

for (i in 1:length(constructs)) {
  cons <- constructs[i]
  xcons <- buildLMMDataset(cons = constructs[i])
  fitCons <- runLMM(xcons)
  estimates <- LMMPublicationOutput(fitCons, xcons, consName = consNames[i])
  rmleTables[[i]] <- estimates
} # for loop for each construct
if (writeDoc) rmle2Word(rmleTables, fileName = "ListnRmle Test")
save.image("ListeningrmleUsingNewScript")