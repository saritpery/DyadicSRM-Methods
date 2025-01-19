# File purpose: Extract all tripleR output for all constructs.
#               Saves an RImage file "..._tripleROutput.RImage"
#               This can be used for comparison purposes, summary tables etc.
#               The file defines a function that might then go to utilities.
# Author:       Sarit Pery
# Date:         2022

# Preperations and parameters ==================================================
rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# The extractTripleRoutput function returns a list with the following 8 items:
# vlist     The list of variables analyzed 
# vTitles   The titles of the analyzed variables
# varComp   The variances-covariances of all constructs, incuding reliabilities
# biComp    The biVariate covariances
# biCor     The perceiver-target correlations (including reciprocities)
# uniFits   The univariate tripleR fit models named by the variable name/title
# biFits    The bivariate tripleR fit models named by both variable names
# data.x    The full data frame, named data.x to avoid collisions 
#   
#   TIP: to extract all these variables from the list into your environment:
#         tr <- extractTripleRoutput()
#         list2env(tr, envir = environment())
extractTripleRoutput <- function(x, vlist, 
                                 pid = "p.id", tid = "t.id", groupid = "group.id", 
                                 corType = "standardized", # or "estimate"
                                 includeBiVar = TRUE, rrstyle = "perception", 
                                 varTitles = NULL, 
                                 saveImg = TRUE, savedImagePrefix = "", 
                                 savedImageSufx = "TripleROutput.RImage" ){
  # x             the data in a long format suited for tripleR RR analysis
  # vlist         the constructs. can hold items or item1/item2 for constructs.
  # corType       "standardized" or "estimate"
  # includeBiVar  include the biVariate analyses of all variables combinations
  # varTitles     the variables names, if NULL vlist is taken.
  # saveImg       if T the output list is both saved as image and returned,
  #               if F the output list is just returned.
  # savedImagePrefix, savedImageSufx  Together these hold the output Rimage name              
  #               if no prefix given, then the project name is used (studyName)
  
  # # debugging lines:
  # pid <- "a.id"; tid <- "p.id"; groupid <- "group.id"; rrstyle = RRstyle;
  # varTitles <- varnames; includeBiVar <- TRUE; savedImagePrefix = ""
  # savedImageSufx = "_tripleROutput.RImage"
  
  ipak (c("TripleR", "tidyverse"))
  # if no prefix was given, then use studyName if it exists.
  if (savedImagePrefix == "" & (studyName %in% ls())) 
    savedImagePrefix <- studyName
  
  # Handle output RRstyle (perception vs. behavior)
  RR.style(rrstyle)
  varianceType <- c("actor variance", "partner variance",
                    "relationship variance", "error variance",
                    "actor-partner covariance", "relationship covariance")
  suffx <- c(".a",".p")
  if (rrstyle == "perception") { # settings, and corrections to tripleR type names
    suffx <- c(".p", ".t")
    varianceType <- gsub("actor", "perceiver", varianceType)
    varianceType <- gsub("partner", "target", varianceType)
  }
  effSuffx <- paste0("\\", suffx)
  if (is.null(varTitles)) varTitles <- vlist
  
  # preliminary settings
  cn <- length(vlist)
  fits <- biFits <- list()
  biCor <- data.frame(matrix(nrow = cn*2, ncol = cn*2))
  names(biCor) <- paste0(rep(varTitles, each = 2), suffx)
  rownames(biCor) <- names(biCor)
  fnesting <- paste0(pid, "*", tid, " | ", groupid)
  x <- as.data.frame(x)
  
  for (i in 1:length(vlist)) {
    # For every construct collect all uniVariate and biVariate analyses
    ci <- vlist[i]; vti <- varTitles[i]
    print(paste0("Collecting univariate data for: ", vti))
    fit <- RR(as.formula(paste(ci, "~ ", fnesting)), data = x, gm = TRUE)
    fits[[vti]] <- fit
    varCompi <- fit$varComp
    varCompi$Const <- vti
    varCompi$type <- varianceType
    biCor[i*2-1,i*2] <- varCompi[5, corType] #perceiver-target cov.
    #extract reliabilities
    pn <- grep(effSuffx[1], names(fit$effects))
    tn <- grep(effSuffx[2], names(fit$effects))
    rel.p <- attributes(fit$effects[pn][,1])$reliability
    rel.t <- attributes(fit$effects[tn][,1])$reliability
    rel.d <- attributes(fit$effectsRel$relationship)$reliability
    if (is.null(rel.d)) rel.d <- NA  # in case of a single item
    varCompi$reliability <- c(rel.p, rel.t, rel.d, NA, NA, NA)
    
    if (i == 1) varComp <- varCompi else varComp <- rbind(varComp, varCompi)
    # bivariate analyses
    if (includeBiVar)
      for (j in (i+1):length(vlist)) {
        if (i == length(vlist)) next()
        cj <- vlist[j]
        biTitle <- paste(varTitles[i], "+", varTitles[j])
        print(paste0("Collecting bivariate data for: ", biTitle))
        t <- try(fit <- RR(as.formula(paste(ci, "+", cj, "~ ", fnesting)), 
                           data = x, varlist = vlist, gm = TRUE))
        if (grepl("err", class(t), ignore.case = TRUE)) next()
        biFits[[biTitle]] <- fit
        biCompij <- fit$bivariate
        biCompij$const1 <- varTitles[i]
        biCompij$const2 <- varTitles[j]
        if (i == 1 & j == 2) biComp <- biCompij else biComp <- rbind(biComp, biCompij)
        ip <- i*2 - 1 ; it <- i*2
        jp <- j*2 - 1; jt <- j*2
        biCor[ip, jp] <- biCompij[1,corType]
        biCor[it, jt] <- biCompij[2,corType]
        biCor[ip, jt] <- biCompij[3,corType]
        biCor[it, jp] <- biCompij[4,corType]
      }
  }

  # sort the columns of the data
  varComp <- select(varComp, Const, everything())
  biComp <- select(biComp, const1, const2, everything())
  # save the list of output, with or w/o the bivariate
  if (includeBiVar) tripleRoutput <- list(vlist = vlist, varTitles = varTitles, 
                                          varComp = varComp, 
                                          biComp = biComp, biCor = biCor, 
                                          uniFits = fits, biFits = biFits, data.x = x)
  if (!includeBiVar) tripleRoutput <- list(vlist = vlist, varTitles = varTitles, 
                                           varComp = varComp, uniFits = fits, 
                                           data.x = x)
  
  if(saveImg) save(tripleRoutput, file = paste0(savedImagePrefix, savedImageSufx))
  return(tripleRoutput)
} # End of functionextractTripleRoutput


load("main.RImage")
load(imgName(".FullRR.Pairwise"))

# Extract actor/partner effects with grand mean.
vlist <- c("H1/H2", "Creat1/Creat2", "Contrib1/Contrib2", 
           "Sf1/Sf2",  "NonTalk/NonTalk","Shy/Shy")

tr.output <- extractTripleRoutput(x, vlist = vlist, pid = "a.id", tid = "p.id", 
                                  varTitles = varnames)

list2env(tr.output, envir = environment())
varComp[,3:9] <- round(varComp[,3:9],3)
uniFits$Shy
