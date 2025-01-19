rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("StudyReady4APIM2.image")
source("99_Utility functions.R") # required for the showAPIM function.
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse",
        "sjPlot", "nlme"))

fit0 <- showAPIM("Helping_rel.BA ~ Listening_rel.BA * Listening_rel.AB + 
                  aDum + pDum")
fit <- showAPIM("Helping_rel.BA ~ Listening_rel.BA * Listening_rel.AB + 
                  Listening_target.A:Listening_target.B + 
                  Listening_perc.A:Listening_perc.B +
                  aDum + pDum")
# drop the interaction between the relationship effects
fit <- showAPIM("Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                  Listening_target.A:Listening_target.B + 
                  Listening_perc.A:Listening_perc.B +
                  aDum + pDum")
# perception remains non significant
fit <- showAPIM("Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                  Listening_target.A:Listening_target.B + 
                  aDum + pDum")
fit <- showAPIM("Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                  Listening_perc.A:Listening_perc.B +
                  aDum + pDum")

# cross perceiver-target indices
fit <- showAPIM("Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                  Listening_target.A:Listening_target.B + 
                  Listening_perc.A:Listening_perc.B +
                  Listening_perc.A:Listening_target.B +
                  Listening_target.A:Listening_perc.B +
                  aDum + pDum")

# The following is the most significant model yet
fit2 <- showAPIM("Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                 Listening_perc.A:Listening_target.B +
                 aDum + pDum")
fit0 <- showAPIM("Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                  aDum + pDum")

anova(fit0, fit2)

fit2 <- lm(Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                 Listening_perc.A:Listening_target.B, data = pw)
fit0 <- lm(Helping_rel.BA ~ Listening_rel.BA + Listening_rel.AB, data = pw)

# Intimacy predictions
fit <- showAPIM("Intimacy_rel.BA ~ Listening_rel.BA * Listening_rel.AB + 
                  Listening_target.A:Listening_target.B + 
                  Listening_perc.A:Listening_perc.B +
                  Listening_perc.A:Listening_target.B +
                  Listening_target.A:Listening_perc.B +
                  aDum + pDum")
fit <- showAPIM("Intimacy_rel.BA ~ Listening_rel.BA + Listening_rel.AB + 
                  Listening_target.A:Listening_target.B + 
                  Listening_perc.A:Listening_perc.B +
                  Listening_target.A:Listening_perc.B +
                  aDum + pDum")
# It seems that individual-level interactions don't affect relational intimacy
fit <- showAPIM("Intimacy_rel.BA ~ Listening_rel.BA + Listening_rel.AB +
                  Listening_target.A:Listening_target.B + 
                  aDum + pDum")
