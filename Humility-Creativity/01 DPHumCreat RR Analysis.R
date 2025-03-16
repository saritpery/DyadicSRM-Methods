################################################################################
# House Cleaning:
rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation 
                                              # for up to 10 digits

# SRM - RR ANALYSIS:
# ==============================================================================

load("main.RImage")

ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))
RR.style("perception")

# BiVariate Analysis for all variables: 
# --------------------------------------
x <- as.data.frame(x)

(fitHCreativity <- RR(H1/H2 + Creat1/Creat2     ~ p.id*t.id | group.id, data = x, na.rm = TRUE))

(fitHSafe <- RR(Sf1/Sf2 + H1/H2     ~ p.id*t.id | group.id, data = x, na.rm = TRUE))

(fitSafeCreativity <- RR(Sf1/Sf2 + Creat1/Creat2     ~ p.id*t.id | group.id, data = x, na.rm = TRUE))
