rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak (c("psych", "tidyverse", "nlme", "lme4", "TripleR"))

pw <- nonScaledPW

cor(pw$Hum_rel.AB, pw$Hum_rel.BA) #0.3,  Humility dyadic reciprocity = 0.52

