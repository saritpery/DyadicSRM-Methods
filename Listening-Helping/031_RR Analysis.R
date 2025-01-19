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
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych"))
####################### Load Packages END ######################################
# SRM - RR ANALYSIS:
# ==============================================================================

load("StudyReadyforAnalaysis")

# Univariate Analysis for all variables: 
# --------------------------------------

FIT.L <- RR(LP1/LP2     ~ actor.id*partner.id | group.id, data = x)
FIT.I <- RR(IP1/IP2     ~ actor.id*partner.id | group.id, data = x)
FIT.S <- RR(SP1/SP2     ~ actor.id*partner.id | group.id, data = x)
FIT.H <- RR(HP1/HP2     ~ actor.id*partner.id | group.id, data = x)

(FIT.L)

APeffects <- getEffects(~ actor.id*partner.id | group.id, data = x, gm = FALSE,  
                  varlist = c("LP1/LP2", "IP1/IP2", "SP1/SP2", "HP1/HP2"))
names(APeffects) [3:10] <- c("ListeningA", "ListeningP",
                             "IntimacyA" , "IntimacyP", 
                             "SpeakingA" , "SpeakingP", 
                             "HelpingA"  , "HelpingP" ) 
str(APeffects)


# gather the entire effects scores: 
# ---------------------------------
getEffects(~actor.id*partner.id | group.id)


# look into the actor and partner effects: 
# ------------------------------

describe(APeffects[, -c(1:2)])
sapply(APeffects[, -c(1:2)], stem)



plot(dx$Listening.ij,dx$Listening.ji)
abline(lm(dx$Listening.ij~dx$Listening.ji), col="red") # regression line (y~x) 

plot(dx$Intimacy.ij,dx$Intimacy.ji)
abline(lm(dx$Intimacy.ij~dx$Intimacy.ji), col="red") # regression line (y~x) 

# Bivariate Analysis for all variables: 
# --------------------------------------
(FIT.LI <- RR(LP1/LP2 + IP1/IP2     ~ actor.id*partner.id | group.id, data = x))
(FIT.LH <- RR(LP1/LP2 + HP1/HP2     ~ actor.id*partner.id | group.id, data = x))
(FIT.LS <- RR(LP1/LP2 + SP1/SP2     ~ actor.id*partner.id | group.id, data = x))
(FIT.SI <- RR(SP1/SP2 + IP1/IP2     ~ actor.id*partner.id | group.id, data = x))
(FIT.SH <- RR(SP1/SP2 + HP1/HP2     ~ actor.id*partner.id | group.id, data = x))
(FIT.IH <- RR(IP1/IP2 + HP1/HP2     ~ actor.id*partner.id | group.id, data = x))

# SEM paths analysis (Univariate Analysis): 
# -----------------------------------------
# The coming section is only relevant for this specific Study 3

# head(FIT.L$effectsRel, 20)
# APeffects <- cbind(FIT.L$effects, FIT.S$effects[, 3:4])
# APeffects <- cbind(APeffects, FIT.I$effects[, 3:4])
# APeffects <- cbind(APeffects, FIT.H$effects[, 3:4])

library(lavaan)
library(semTools)
library(semPlot)
# APeffects$LS.a <- APeffects$LP1.a*APeffects$SP1.a
# APeffects$LS.a <- resid(lm(APeffects$LS.a ~ APeffects$LP1.a + APeffects$SP1.a))

ActorModel <- '
                  HelpingA ~ ListeningA + SpeakingA + IntimacyA
                  IntimacyA ~  SpeakingA  + ListeningA 
                
                 ListeningA ~~ SpeakingA
'
fit <- sem(ActorModel, data = APeffects, fixed.x = FALSE)
summary(fit, standardized=TRUE, fit=TRUE)

semPaths(fit, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 25,
         edge.label.cex = 1.2,
         label.prop = .8,
         cardinal = FALSE,
         sizeMan = 10,
         layout = "tree",
         edge.label.position = .7)
title("Actor Model",line=3)

ActorModel <- '
                  HelpingA ~ ListeningA + SpeakingA + IntimacyA
                  IntimacyA ~  same*SpeakingA  + same*ListeningA 
                
                 ListeningA ~~ SpeakingA
'
fit1 <- sem(ActorModel, data = APeffects, fixed.x = FALSE)
summary(fit, standardized=TRUE, fit=TRUE)

anova(fit1, fit)



PartnerModel <- '
                  HelpingP ~ ListeningP + SpeakingP + IntimacyP
                  IntimacyP ~  SpeakingP  + ListeningP 

                  ListeningP ~~ SpeakingP
'
fit <- sem(PartnerModel, data = APeffects, fixed.x = FALSE)
summary(fit, standardized=TRUE, fit=TRUE)

semPaths(fit, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 25,
         edge.label.cex = 1.2,
         label.prop = .8,
         cardinal = FALSE,
         sizeMan = 10,
         layout = "tree",
         edge.label.position = .7)
title("Partner Model",line=3)

Relation <- FIT.L$effectsRel[, 4:5]
names(Relation) <- c("Dyad", "ListeningR")
Relation$SpeakingR <- FIT.S$effectsRel[, 5]
Relation$IntimacyR <- FIT.I$effectsRel[, 5]
Relation$HelpingR  <- FIT.H$effectsRel[, 5]


RelationModel <- '
        HelpingR ~ ListeningR + SpeakingR + IntimacyR
        IntimacyR ~  SpeakingR  + ListeningR 

        ListeningR ~~ SpeakingR
'
fit <- sem(RelationModel, data = Relation, 
           fixed.x = FALSE)
summary(fit, standardized=TRUE, fit=TRUE)

# Creating a design factor for the nested structure
library(lavaan.survey)
design <- svydesign(ids= ~ Dyad, nest=TRUE, data=Relation)

# Combining SEM and the design factor

fit.total1 <- lavaan.survey(lavaan.fit=fit, survey.design=design)  

summary(fit.total1, standardized=TRUE, fit=TRUE)

    semPaths(fit.total1, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 25,
         edge.label.cex = 1.2,
         label.prop = .8,
         cardinal = FALSE,
         sizeMan = 10,
         layout = "tree",
         edge.label.position = .7)
title("Relationship Model",line=3)


