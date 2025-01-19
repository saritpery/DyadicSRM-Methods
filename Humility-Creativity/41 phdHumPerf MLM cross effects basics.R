################################################################################

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation 
                                              # for up to 10 digits

# SRM - RR ANALYSIS:
# ==============================================================================

load("main.RImage")
load(imgName(".FullRR.ReadyforAnalaysis"))

ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse",
        "lme4"))
RR.style("perception")

x <- as.data.frame(x)
t0RR.h <- RR(Hum.raw ~ a.id*p.id | group.id, data = x, na.rm = TRUE); t0RR.h
t0RR.cr <- RR(Creat.raw ~ a.id*p.id | group.id, data = x, na.rm = TRUE); t0RR.h
x <- na.omit(x)

m0 <- lmer(Hum.raw ~ (1|a.id) + (1|p.id), x)
summary(m0)

# Add the dyad level (according to the tripleR indication for easier comparison)
RRdyadic <- t0RR.h$effectsRel
x <- left_join(x, select(RRdyadic, a.id, p.id, dyad), by = c("a.id", "p.id"))
m0d <- lmer(Hum.raw ~ (1|a.id) + (1|p.id) + (1|dyad), x)
summary(m0d)

m0g <- lmer(Hum.raw ~ (1|a.id) + (1|p.id) + (1|group.id), x)
summary(m0g)
# confint(m0g, oldNames = FALSE)

m0gd <- lmer(Hum.raw ~ (1|a.id) + (1|p.id) + (1|group.id) + (1|dyad), x)
summary(m0gd)
# confint(m0gd, oldNames = FALSE)

# compare empty model results
getLmerVars <- function(m, param = c("a.id", "p.id", "group.id", "dyad")){
  vc <- unlist(summary(m)$varcor)
  res <- vc[param]
  res <- c(res, sigma(m)^2)
  return(res)
}
h.varTable <- data.frame("variances" = c("perceiver var", "target var", 
                                       "groups var", "dyads var", "residual"))
h.varTable$tripleR <- t0RR.h$varComp$estimate[c(1,2,4,4,3)]
h.varTable$PT <- getLmerVars(m0)
h.varTable$PT.Grp <- getLmerVars(m0g)
h.varTable$PT.Dyad <- getLmerVars(m0d)
h.varTable$PT.Grp.Dyad <- getLmerVars(m0gd)

# one can see that for Humility, the best model (lowest resid) is the pt.dyad
# which probably explains more of the variance by the dyadic reciprocity.
h.varTable   
           
# CREATIVITY VARIANCES =========================================================
cr0 <- lmer(Creat.raw ~ (1|a.id) + (1|p.id), x)
summary(cr0)

## Add the dyad level (according to the tripleR indication for easier comparison)
# RRdyadic <- t0RR.cr$effectsRel
# x <- left_join(x, select(RRdyadic, a.id, p.id, dyad), by = c("a.id", "p.id"))
cr0d <- lmer(Creat.raw ~ (1|a.id) + (1|p.id) + (1|dyad), x)
summary(cr0d)

cr0g <- lmer(Creat.raw ~ (1|a.id) + (1|p.id) + (1|group.id), x)
summary(cr0g)
# confint(cr0g, oldNames = FALSE)

cr0gd <- lmer(Creat.raw ~ (1|a.id) + (1|p.id) + (1|group.id) + (1|dyad), x)
summary(cr0gd)
# confint(cr0gd, oldNames = FALSE)

cr.varTable <- data.frame("variances" = c("perceiver var", "target var", 
                                       "groups var", "dyads var", "residual"))
cr.varTable$tripleR <- t0RR.cr$varComp$estimate[c(1,2,4,4,3)]
cr.varTable$PT <- getLmerVars(cr0)
cr.varTable$PT.Grp <- getLmerVars(cr0g)
cr.varTable$PT.Dyad <- getLmerVars(cr0d)
cr.varTable$PT.Grp.Dyad <- getLmerVars(cr0gd)

# one can see that for Creativity, the best model (lowest resid) is the pt.dyad
# which probably explains more of the variance by the dyadic reciprocity.
cr.varTable   

fitHCr <- lmer(Creat.raw ~ (1|a.id) + (1|p.id) + (1|dyad), x))

fitHCreativity <- RR(H1/H2 + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitHCreativity

fitHContribution <- RR(H1/H2 + Contrib1/Contrib2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitHContribution

fitIntroCreativity <- RR(NonTalk/Shy + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitIntroCreativity

fitIntroContribution <- RR(NonTalk/Shy + Contrib1/Contrib2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitIntroContribution

fitShyCreativity <- RR(Shy/Shy + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitShyCreativity

fitShyContribution <- RR(Shy/Shy + Contrib1/Contrib2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitShyContribution

fitShyCreativity <- RR(Shy/Shy + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitShyCreativity

fitNonTalkativeCreativity <- RR(NonTalk/NonTalk + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitNonTalkativeCreativity

fitHSafe <- RR(Sf1/Sf2 + H1/H2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitHSafe

fitSafeCreativity <- RR(Sf1/Sf2 + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitSafeCreativity

fitSafeContrib <- RR(Sf1/Sf2 + Contrib1/Contrib2 ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitSafeContrib

fitShySafe <- RR(Shy/Shy + Sf1/Sf2 ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitShySafe

fitShyH <- RR(Shy/Shy + H1/H2 ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitShyH

fitNonTalkativeH <- RR(NonTalk/NonTalk + H1/H2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitNonTalkativeH
(RR(Creat1/Creat2 + Contrib1/Contrib2 ~ a.id*p.id | group.id, data = x, na.rm = TRUE))
(RR(NonTalk/NonTalk + Contrib1/Contrib2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE))
(RR(NonTalk/NonTalk + Sf1/Sf2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE))
(RR(NonTalk/NonTalk + Shy/Shy     ~ a.id*p.id | group.id, data = x, na.rm = TRUE))
