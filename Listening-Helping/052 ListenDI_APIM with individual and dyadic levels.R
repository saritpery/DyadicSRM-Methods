rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName("Ready4APIM2"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse",
        "nlme"))
# ipak("sjPlot")

fit0 <- showAPIM("help_rel.BA ~ listen_rel.BA * listen_rel.AB + 
                  aDum + pDum")
fit <- showAPIM("help_rel.BA ~ listen_rel.BA * listen_rel.AB + 
                  listen_target.A:listen_target.B + 
                  listen_perc.A:listen_perc.B +
                  aDum + pDum")
# drop the interaction between the relationship effects
fit <- showAPIM("help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                  listen_target.A:listen_target.B + 
                  listen_perc.A:listen_perc.B +
                  aDum + pDum")
# perception remains non significant
fit <- showAPIM("help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                  listen_target.A:listen_target.B + 
                  aDum + pDum")
fit <- showAPIM("help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                  listen_perc.A:listen_perc.B +
                  aDum + pDum")

# cross perceiver-target indices
fit <- showAPIM("help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                  listen_target.A:listen_target.B + 
                  listen_perc.A:listen_perc.B +
                  listen_perc.A:listen_target.B +
                  listen_target.A:listen_perc.B +
                  aDum + pDum")

# The following is the most significant model yet
fit2 <- showAPIM("help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                 listen_perc.A:listen_target.B +
                 aDum + pDum")
fit0 <- showAPIM("help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                  aDum + pDum")

anova(fit0, fit2)

fit2 <- lm(help_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                 listen_perc.A:listen_target.B, data = pw)
fit0 <- lm(help_rel.BA ~ listen_rel.BA + listen_rel.AB, data = pw)

# intimacy predictions
fit <- showAPIM("intimacy_rel.BA ~ listen_rel.BA * listen_rel.AB + 
                  listen_target.A:listen_target.B + 
                  listen_perc.A:listen_perc.B +
                  listen_perc.A:listen_target.B +
                  listen_target.A:listen_perc.B +
                  aDum + pDum")
fit <- showAPIM("intimacy_rel.BA ~ listen_rel.BA + listen_rel.AB + 
                  listen_target.A:listen_target.B + 
                  listen_perc.A:listen_perc.B +
                  listen_target.A:listen_perc.B +
                  aDum + pDum")
# It seems that individual-level interactions don't affect relational intimacy
fit <- showAPIM("intimacy_rel.BA ~ listen_rel.BA + listen_rel.AB +
                  listen_target.A:listen_target.B + 
                  aDum + pDum")

