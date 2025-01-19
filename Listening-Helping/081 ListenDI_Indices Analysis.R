################################################################################

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation 
                                              # for up to 10 digits
####################### Load Packages ##########################################
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, 
# then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
    suppressPackageStartupMessages(sapply(pkg, 
                                        require, character.only = TRUE))
}

#TripleR depends on ggplot2; data.table is for rbindlist; gdata for writing
#output for SOREMO
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))
####################### Load Packages END ######################################


# SRM - Second stage analysis
# Explore variances of SRM indices at both individual and dyadic levels
# ==============================================================================

load("StudyReadyforAnalaysis")

#Clean the environment:
rm(list=setdiff(ls(),c( "x", "ipak")))

# Explore team level indices
# ================================

    APeffects <- getEffects(~ actor.id*partner.id | group.id, data = x, gm = FALSE,  
                      varlist = c("LP1/LP2", "IP1/IP2", "SP1/SP2", "HP1/HP2"))
    names(APeffects) [3:10] <- c("ListeningA", "ListeningP",
                                 "IntimacyA" , "IntimacyP", 
                                 "SpeakingA" , "SpeakingP", 
                                 "HelpingA"  , "HelpingP" ) 
    
    # describe(APeffects[, -c(1:2)])
    # sapply(APeffects[, -c(1:2)], stem)
    
    teamInd <- setNames(as.data.frame(unique(x$group.id)), c("group.id"))
    teamInd <- arrange(teamInd, group.id)
    APeffects <- arrange(APeffects, group.id)
    
    teamInd$maxInd <- APeffects %>%  group_by(group.id) %>% 
        select(-id) %>% summarise_all(max) %>% select(-group.id)
    teamInd$minInd <- APeffects %>% group_by(group.id) %>% select(-id) %>% 
        summarise_all(min) %>% select(-group.id)
    teamInd$varInd <- APeffects %>% group_by(group.id) %>% select(-id) %>% 
        summarise_all(var) %>% select(-group.id)
    
    
    varMaxInd <- maxInd %>% select(-group.id) %>%  summarise_all(var)    
    sdMaxInd <- maxInd %>% select(-group.id) %>%  summarise_all(sd)    
    varMinInd <- minInd %>% select(-group.id) %>%  summarise_all(var)    
    varVarInd <- varInd %>% select(-group.id) %>%  summarise_all(var)    

# Explore dyadic level indices
# ================================

    varlist <- c("LP1/LP2", "IP1/IP2",  "SP1/SP2", "HP1/HP2")
    varnames <- c("Listening.REL", "Intimacy.REL", "Speech.REL", "Helping.REL")
    
    EFFECTS.C <- data.frame()
    # names(EFFECTS.C)<- c("group.id","partner.id","actor.id", varnames)
    formule <- ~actor.id * partner.id | group.id 
    for (v in 1:length(varlist)) {
        # v <- 2
        f1 <- formula(paste(varlist[v], paste(as.character(formule), 
                                              collapse = "")))
        FIT <- RR(f1, data = x)
        ifelse (v == 1,
                EFFECTS.C <- FIT$effectsRel,
                EFFECTS.C <- cbind( EFFECTS.C, FIT$effectsRel$relationship))
        colnames(EFFECTS.C)[v+4] <- varnames[v]
        
    }
    colnames(EFFECTS.C) [2] <- "partner.id"     # the origin produced by RR is the construct name.
    
    head(EFFECTS.C, 12)
    
    # EFFECTS.Raw    <- na.omit(x)
    # EFFECTS.Raw$actor.id    <- as.factor(EFFECTS.Raw$actor.id)
    # head(EFFECTS.Raw, 8)
    # 
    # names(EFFECTS.Raw)
    # EFFECTS.Raw <- EFFECTS.Raw[, c("actor.id","partner.id", "Listening.Raw", "Intimacy.Raw", "Speech.Raw", "Helping.Raw")]
    # EFFECTS.Raw[, 3:6] <- round(EFFECTS.Raw[,3:6], 1)
    # EFFECTS.C    <- merge(EFFECTS.C, EFFECTS.Raw, by = c("actor.id", "partner.id"))
    # head(EFFECTS.C, 8)
    # 
    EFFECTS.C      <- EFFECTS.C[order(EFFECTS.C$dyad), ]
    head(EFFECTS.C, 8) 
    
    #--------------------- Create Pairwise Structure ------------
    
    pw1        <- EFFECTS.C [ c(TRUE,FALSE), ]
    names(pw1) <- gsub(".REL", "_AB", names(pw1))
    pw2        <- EFFECTS.C [!c(TRUE,FALSE), ]
    names(pw2) <- names(pw1)
    duplicCols   <- names(pw1)[1:4]
    temp1         <- cbind(pw1, select(pw2, -duplicCols))
    temp2         <- cbind(pw2, select(pw1, -duplicCols))
    PAIRWISE           <- rbind(temp1, temp2)
    
    names(PAIRWISE) <- c(names(pw1), 
                         gsub("_AB", "_BA", names(select(pw2, -duplicCols))))
    
    PAIRWISE      <- PAIRWISE[order(PAIRWISE$dyad), ]
    
    PAIRWISE$partnum              <- rep(1:2, times = nrow(PAIRWISE) / 2)
    head(PAIRWISE, 12)
    
    #Clean the environment
    rm(list= c("pw1", "pw2", "temp1", "temp2", "duplicCols" ))

    #----------------------- Review Dyadic Indices ----------------    
    
    