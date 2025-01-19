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

ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))
RR.style("perception")

# BiVariate Analysis for all variables: 
# --------------------------------------
x <- as.data.frame(x)

fitHCreativity <- RR(H1/H2 + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE)
fitHCreativity

(fithcr <- RR(Hum1/Hum2 + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE))

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

# Humility constructs' single items observation
# ---------------------------------------------
ll <- as.data.frame(longInput)
RR(Hum1 + Hum2 ~ a.id*p.id|group.id, data = ll)
RR(Hum3 ~ a.id*p.id|group.id, data = ll)
# Hum1 - Admit when they don't know how to do something
#        PercV = .24, targetV = .15, genRecip = No, dydRecip = 0.25
# Hum2 - Takes notice of others' strengths
#        PercV = .24, targetV = .11, genRecip = No, dydRecip = 0.32
# Hum3 - Open to the advise of others
#        PercV = .16, targetV = .27, genRecip = No, dydRecip = 0.30
