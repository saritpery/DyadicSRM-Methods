# Utility functions ============================================================

# Author:         Sarit Pery, The Hebrew university of Jerusalem
# Version:        2.0  
# 
# Version update 2.0:
#     caution:    is caution required when updating version in other projects? NO
#     date:       29/12/2022
#     update content:
#                 1. initiating version control for "99_utility Functions.R" 
#                 2. extractTripleRoutput() 
#                    Including vast documentation and a vignette. 
#                 3. APIM.lmer()
#                 4. APIMformulaShow() An enhancement for showAPIM to enable 
#                    dv & IVs as input.

# File description - Code wise =================================================
#     Includes utility functions, used across all my projects. 
#     Is loaded into main.RImage of each project, and hence should be available 
#     for all code files.
#     TIP: use the outline button (at the top right of this tile) to view
#          all available functions in this file. 


# ipak function: install and load multiple R packages. =========================
# check to see if packages are installed. Install them if they are not, 
# then load them into the R session.
# Author: Prof. Avi Kluger
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, 
                                        require, character.only = TRUE))
}

# fname() generates time-stamped file names in a unified name structure. =======
fname <- function(title = "",sep = "_", fileExtention = ".csv",
                  study = "", includeStudyName = TRUE){
  if (substr(fileExtention,1,1) != ".") # add a . to file extention if missing.
    fileExtention <- paste0(".", fileExtention)
  if (includeStudyName & study == "" & exists("studyName", envir = globalenv()))
    study <- studyName
  if (study != "") study <- paste0(study, " ")
  fname <- paste0(study, title, sep, 
                  format(Sys.time(), "%d%b%Y_%H%M"), 
                  fileExtention)
}

# imgName() generates an R image file name in a unified structure ==============
imgName <- function(title = "", study = "", ext = ".RImage"){
  if (study=="" & exists("studyName", envir = globalenv())) study <- studyName
  imgName <- paste0(study, title, ext )
  return(imgName)
}

# improveTables() handles transition from behavior to perception styles ========
# This is a rather specific function that might not be relevant for other projects
# created for the HumPerf project.
improveTables <- function(t,tTitle = "", v="", vtitle="", 
                          rrStyle = "behavior",
                          rmSpaceLines = FALSE, addNoteExt = FALSE, 
                          noteExt = ". SRM Variables correlations is added as calculated in biVariate analyses." ){
  t$table.body <- as.data.frame(t$table.body)
  if (v != "") t$table.body$Variable <- gsub(paste0(v, "$"), vtitle, t$table.body$Variable)
  t$table.title <- paste0(tTitle, "\n", t$table.title)
  if (rrStyle == "perception") {
    t$table.body$Variable <- gsub(".a$", ".perceiver", t$table.body$Variable)
    t$table.body$Variable <- gsub(".p$", ".target", t$table.body$Variable)}
  if (rmSpaceLines) {
    spaceLines <- apply(t$table.body, 1, function(a) all(a == " "))
    t$table.body <- t$table.body[!spaceLines,]
  }
  if(addNoteExt) t$table.note <- paste0(t$table.note, noteExt)
  return(t)
}

# reportNKD()  =================================================================
#   counts the number of N(participants),k(groups),d(dyadic ratings) in the data
reportNKD <- function(x){
  # The function relies on our convention of names
  # a.id, p.id, group.id
  
  res <- list()
  # count the number of unique dyads
  x$A <- as.numeric(gsub(".*:", "", x$a.id))
  x$P <- as.numeric(gsub(".*:", "", x$p.id))
  x$dyadn <- apply(x[,c("group.id", "A", "P")], 1, function(m) paste0(m[1], ":", min(m[2], m[3]), max(m[2], m[3])))
  x$dyadn[x$a.id == x$p.id] <- NA
  
  # report the data:
  res$dyads <- length(unique(x$dyadn))-1
  res$ngroups <- length(unique(x$group.id))
  res$nparticipants <- length(unique(c(x$a.id, x$p.id)))
  res$dyadicRatings <- nrow(x)
  
  return(res)
}

# showAPIM() DUMMY-CODES-APIM better presentation results ======================
# 
# The showAPIM function, receives a DV construct, and the list of IV constructs,
# it builds the model and prints the output w/o the dummy codes rows.
# it returns the fit to enable further investigation.
showAPIM <- function(f, FIT = FALSE, mlMethod = "REML", dataset = pw, 
                     anal.level = "dyadic",
                     summaryInfo = TRUE, elementsNo = FALSE){
  # f             The formula,
  # FIT           the model to be printed 
  #               If FALSE the model is generated.
  # summaryInfo   If FALSE only the model's coefficients are presented 
  #                        (cleaned from dummy codes)
  #               If TRUE  the model's summary parameters (i.e AIC, residual...))
  # for debugging: FIT = FALSE; mlMethod = "REML"; dataset = pw
  if(!FIT) 
    # run the analysis:
    # obviously, some formulas won't converge. the following use of 'try'
    # enables to continue the run even if the model calculation failed.
    
    # correlation=corCompSymm: 
    #       Calculate the correlation of the error terms of the dyad members,
    #       which is exactly what we want because we want to know their effect 
    #       on one another after controlling for their individual tendencies.
    
    t <- try(FIT <- gls(as.formula(f),
                        na.action = na.omit, method = mlMethod, verbose = TRUE,
                        correlation = corCompSymm (form = ~1|dyad),
                        data = cbind(dataset)))
  
  # if the fit calculations converged, then, 
  # calculate the number of rows and interaction rows, and display it.
  # return the fit for further analysis if required.
  if (!"try-error" %in% class(t)) {
    dummycodeAnalysis <- str_count(f, "aDum|pDum") > 0
    if(!dummycodeAnalysis) {
      print(round(data.frame(summary(FIT)$tTable), 2))
      return(FIT)
    }
    # calculate number of parameters: 
    elementsCount <- str_count(f, "\\+|\\*")
    dropCount <- str_count(f, "aDum|pDum|tDum") + str_count(f, "\\:") 
    interactionCount <- str_count(f, "\\*|\\:")
    nr <- nrow(data.frame(summary(FIT)$tTable))
    displayRows <- c(2:(2 + (elementsCount - dropCount)))
    if (interactionCount > 0) 
      displayRows <- c(displayRows, ((nr - interactionCount + 1):nr))
    # If elementsNo is given, override the displayRows calculation
    if (elementsNo) displayRows <- c(2:(2 + elementsNo))
    
    header <- ifelse(
      anal.level == "dyadic", "APIM analysis including relationship scores at the dyadic level: ",
      ifelse(anal.level == "individual","APIM analysis using raw ratings: ", 
             "APIM analysis: "))
    # header <- "APIM analysis including relationship scores at the dyadic level: "
    cat(paste0("\n", header, "\n",
               paste0(rep("=", nchar(header)), collapse = ""), "\n",
               "\nTHE MODEL: \n",
               f, "\n\n"
    ))
    print(round(data.frame(summary(FIT)$tTable[displayRows,]), 2))
    if (summaryInfo) {
      cat(paste("\nAIC:", round(AIC(FIT),3), "    BIC:", round(BIC(FIT), 3),
                "    logLik:", round(logLik(FIT), 3),
                "\nResidual standard error:", round(sigma(FIT), 3)))
    }
    return(FIT)
  } else {print("Model does not converge for the data")}
} # end of showAPIM function

# The APIMformulaShow function, receives a DV construct, and IV constructs,
# it generates the APIM formula with dummy-codes for showAPIM.
# As default it runs showAPIM.
APIMformulaShow <- function(dv = varnames[1], iv = varnames[2], 
                            suf = "_rel", AB = ".AB", BA = ".BA",
                            anal.level = "dyadic",
                            runShowAPIM = TRUE, dataset = pw){
  # prepare the formula:
  sufAB <- paste0(suf, AB)
  sufBA <- paste0(suf, BA)
  ivs <- paste0(rep(iv, each = 2), 
                rep(c(paste0(sufAB, " "), sufBA), length(iv)),
                collapse = " + ")
  f <- paste0(dv, paste0(sufAB, " ~ "), ivs, " + aDum + pDum")
  if (runShowAPIM) 
    return(showAPIM(f, dataset = dataset, anal.level = anal.level)) else return(f)
}


# APIM.lmer() run APIM analyses on SRM output, using lmer. =====================
# The equivalent of showAPIM (gls+dummy codes) for lmer using cross effects
APIM.lmer <- function(dv, rel.iv, ind.iv = NULL, 
                      k = c("a.id", "p.id", "dyad"), df = pw, remlTF = TRUE,
                      dv.is.rel = TRUE){
  dvc <- ifelse(!dv.is.rel, dv,
                ifelse(grepl("_rel.BA|_rel.AB", dv), dv, 
                       ifelse(grepl("_rel", dv), 
                              paste0(dv, ".AB"),
                              paste0(dv, "_rel.AB"))))
  rel.ivc <- paste0(rep(rel.iv, each = 2), 
                    rep(c("_rel.AB ", "_rel.BA" ), length(rel.iv)),
                    collapse = " + ")
  ind.ivc <- ifelse(is.null(ind.iv), "", 
                    paste(" + ", paste0(ind.iv, collapse = " + ")))
  rand <- paste0(paste0("(1 | ", k, ")"), collapse = " + ")
  f <- paste0(dvc, " ~ ", rel.ivc, ind.ivc, " + ", rand)
  t <- try(fit <- lmer(as.formula(f), data = df, REML = remlTF))
  
  header <- "APIM analysis: "
  cat(paste0(header, "\n",
             paste0(rep("=", nchar(header)), collapse = ""),
             "\nTHE MODEL: \n", f, "\n\n"))
  
  if (!"try-error" %in% class(t)) {
    s <- summary(fit)$coefficients[-1,] # fixed coefs w/o intercept (==0)
    s <- round(s,3)
    print(s)
    #s <- confint(fit)
    return(fit)
  } else {print("Model does not converge for the data"); return(t)}
}



# extractTripleRoutput() gahters tripleR analyses of all variables. ============
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
                                 savedImageSufx = "TripleROutput" ){
  # x             the data in a long format suited for tripleR RR analysis
  # vlist         the constructs. can hold items or item1/item2 for constructs.
  # corType       "standardized" or "estimate"
  # includeBiVar  include the biVariate analyses of all variables combinations
  # varTitles     the variables names, if NULL vlist is taken.
  # saveImg       if T the output list is both returned and saved as image,
  #               if F the output list is just returned.
  # savedImagePrefix, savedImageSufx  Together these hold the output Rimage name              
  #               The prefix handles the situation of several 3R outputs in the
  #               same project.
  
  # # debugging lines:
  # pid <- "a.id"; tid <- "p.id"; groupid <- "group.id"; rrstyle = RRstyle;
  # varTitles <- varnames; includeBiVar <- TRUE; savedImagePrefix = ""
  # savedImageSufx = "tripleROutput"
  
  ipak (c("TripleR", "tidyverse"))
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
  
  if(saveImg) save(tripleRoutput, 
                   file = imgName(paste0(savedImagePrefix, savedImageSufx)))
  return(tripleRoutput)
} # End of function extractTripleRoutput

# An example use of extractTripleRoutput function: =============================
# load("main.RImage")
# load(imgName(".FullRR.Pairwise"))
# 
# # Extract actor/partner effects with grand mean.
# vlist <- c("H1/H2", "Creat1/Creat2", "Contrib1/Contrib2", 
#            "Sf1/Sf2",  "NonTalk/NonTalk","Shy/Shy")
# 
# tr.output <- extractTripleRoutput(x, vlist = vlist, pid = "a.id", tid = "p.id", 
#                                   varTitles = varnames)
# 
# list2env(tr.output, envir = environment())
# varComp[,3:9] <- round(varComp[,3:9],3)
# uniFits$Shy


# projectCodeFiles() returns a list of the project's R code files ============
# I think this function is redundant.
projectCodeFiles <- function(sortFiles = TRUE, exclude = "", #exclude = "^00", 
                             allRCode = TRUE ){
  # sortFiles = TRUE sorts the file by their numbering. if FALSE, not sorted.
  # allRCode = FALSE returns only file names beginning with numbers 0/1.
  #            I'm not sure why I wanted that.
  # exclude  = "" returns all the files (no excluding)
  #          = pattern of files to exclude. 
  #            for example exclude = "^00" excludes file names that begin(^) 
  #            with 00 which is the main project file.
  ipak("tidyverse")
  codefiles <- list.files(path = ".", pattern = "\\.R$", ignore.case = TRUE)
  if (!allRCode) codefiles <- grep("^0|^1", codefiles, value = TRUE)
  if (exclude != "") {
    excFiles <- grep(exclude, codefiles, value = TRUE)
    codefiles <- codefiles[! (codefiles %in% excFiles)] 
  }
  if (sortFiles) {
    fsort <- data.frame(files = codefiles)
    fsort$n <- substr(fsort$files, 1, 2)
    try(fsort$n <- as.numeric(fsort$n))
    fsort <- arrange(fsort, fsort$n)
    codefiles <- fsort$files
  }
  return(codefiles)
}