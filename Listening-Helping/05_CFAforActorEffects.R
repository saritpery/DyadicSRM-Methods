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

load("StudyReadyforAnalaysis")


# gather the entire effects scores: 
# ---------------------------------
apEffectsFullData <- 
    getEffects(~actor.id*partner.id | group.id, data = fullData, 
               gm = FALSE,
               varlist = c(paste0("L", 1:4), paste0("I", 1:3), 
                           paste0("H", 1:3), paste0("S", 1:3)))
apEffectsFullData[, -c(1:2)] <- round(apEffectsFullData[, -c(1:2)], 3)
write.csv(apEffectsFullData, file = "apEffectsStudy3.csv")


ipak(c("lavaan", "semTools", "semPlot"))

figStudy3 <- '
        Listening =~ L1.a + L2.a + L3.a + L4.a
        Intimacy  =~ I1.a + I2.a + I3.a
        Helping   =~ H1.a + H2.a + H3.a
        Speech    =~ S1.a + S2.a + S3.a
        '
figStudy3 <- '
        Listening =~ L1.a + L2.a + L3.a + L4.a
        Intimacy  =~ I1.a + I2.a + I3.a
        Helping   =~ H1.a + H2.a + H3.a
        Speech    =~ S1.a + S2.a + S3.a

        Listening ~~ li*Intimacy
        Listening ~~ Helping
        Listening ~~ Speech
        Intimacy ~~ Helping
        Intimacy ~~ Speech
        Helping  ~~ Speech
'
fit <- cfa(figStudy3, data = apEffectsFullData, fixed.x = FALSE)
summary(fit, fit.measures =TRUE)

#semPaths(fit, intercepts = FALSE)
semPaths(fit, "std", 
         rotation = 1, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 10,
         edge.label.cex = 0.8 ,
         label.prop = .8,
         cardinal = FALSE)

title("Actor CFA",line=3)


figStudy3IntimacyAndListen <-   '
    ListInt =~ L1.a + L2.a + L3.a + L4.a + I1.a + I2.a + I3.a
    Helping   =~ H1.a + H2.a + H3.a
    Speech    =~ S1.a + S2.a + S3.a
'
figStudy3HelpingAndListen <- '
    ListHelp  =~ L1.a + L2.a + L3.a + L4.a + H1.a + H2.a + H3.a
    Intimacy  =~ I1.a + I2.a + I3.a
    Speech    =~ S1.a + S2.a + S3.a
'
figStudy3SpeechAndListen <- '
    Listening =~ L1.a + L2.a + L3.a + L4.a + S1.a + S2.a + S3.a
    Intimacy  =~ I1.a + I2.a + I3.a
    Helping   =~ H1.a + H2.a + H3.a
'
figStudy3IntimacyAndHelping <- '
    Listening =~ L1.a + L2.a + L3.a + L4.a
    Intimacy  =~ I1.a + I2.a + I3.a + H1.a + H2.a + H3.a
    Speech    =~ S1.a + S2.a + S3.a
'
figStudy3IntimacyAndSpeech <- '
    Listening =~ L1.a + L2.a + L3.a + L4.a
    Intimacy  =~ I1.a + I2.a + I3.a + S1.a + S2.a + S3.a
    Helping   =~ H1.a + H2.a + H3.a
'

figStudy3SpeechAndHelping <-     '
    Listening =~ L1.a + L2.a + L3.a + L4.a
    Intimacy  =~ I1.a + I2.a + I3.a
    Helping   =~ H1.a + H2.a + H3.a + S1.a + S2.a + S3.a
'
fitIntimacyAndListen <- cfa(figStudy3IntimacyAndListen, 
                        data = apEffectsFullData, fixed.x = FALSE)
fitHelpingAndListen  <- cfa(figStudy3HelpingAndListen, 
                        data = apEffectsFullData, fixed.x = FALSE)
fitSpeechAndListen   <- cfa(figStudy3SpeechAndListen, 
                        data = apEffectsFullData, fixed.x = FALSE)
fitIntimacyAndHelping<- cfa(figStudy3IntimacyAndHelping, 
                        data = apEffectsFullData, fixed.x = FALSE)
fitIntimacyAndSpeech <- cfa(figStudy3IntimacyAndSpeech, 
                        data = apEffectsFullData, fixed.x = FALSE)
fitSpeechAndHelping  <- cfa(figStudy3SpeechAndHelping, 
                        data = apEffectsFullData, fixed.x = FALSE)

anova(fit, fitIntimacyAndListen)
anova(fit, fitHelpingAndListen)
anova(fit, fitSpeechAndListen)
anova(fit, fitIntimacyAndHelping)
anova(fit, fitIntimacyAndSpeech)
anova(fit, fitSpeechAndHelping)
resid(fit, type="standardized")

figStudy3WithoutI3 <- 
    '
Listening =~ L1.a + L2.a + L3.a + L4.a
Intimacy  =~ I1.a + I2.a 
Helping   =~ H1.a + H2.a + H3.a
Speech    =~ S1.a + S2.a + S3.a
'
fitWithoutI3 <- cfa(figStudy3WithoutI3, data = apEffectsFullData, fixed.x = FALSE)
summary(fitWithoutI3, fit.measures =TRUE)
resid(fitWithoutI3, type="standardized")