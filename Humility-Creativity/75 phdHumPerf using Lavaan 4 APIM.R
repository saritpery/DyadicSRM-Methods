rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file:
# ==============================================================================
# 1. verify the basics for running APIM with lavaan

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak(c("TripleR", "tidyverse", "nlme", "lavaan"))  

# The constraints for an indistinguishable APIM
# Six equality constraints over the distinguishable model.
# * equal actor effects (a1 = a2)
# * equal partner effects (p12 = p21) * equal X means mx1 = mx2
# * equal X variances vx1 = vx2
# * equal Y intercepts i1 = i2
# * equal Y error variances ve1 = ve2

# I think the data should be like pw, but with a single row for each dyad
x <- pw[unique(pw$dyad),]

APIM_I <- '

                  Creat_rel.AB  ~ a*Hum_rel.AB + p*Hum_rel.BA 
                  Creat_rel.BA  ~ a*Hum_rel.BA + p*Hum_rel.AB
                  Hum_rel.AB ~ mx*1  # mx = The hum_rel mean (should be zero)
                  Hum_rel.BA ~ mx*1
                  Creat_rel.AB ~ iy*1 # iy = intercept for Creativity_rel (should be zero)
                  Creat_rel.BA ~ iy*1
                  Hum_rel.AB ~~ vx*Hum_rel.AB  #vx = Hum variance
                  Hum_rel.BA ~~ vx*Hum_rel.BA
                  Creat_rel.AB ~~ ve*Creat_rel.AB #ve = error variance, the residual
                  Creat_rel.BA ~~ ve*Creat_rel.BA
                  Hum_rel.AB ~~ cx*Hum_rel.BA      #cx = cov, dyadic reciprocity?
                  Creat_rel.AB ~~ cy*Creat_rel.BA  # cy = cov of errors, rho? 
                  
                 # k := p/a
'

# Change to "bootstrap = 5000" to get reliable values for the confidence interval.  
apimi <- sem(APIM_I,fixed.x=FALSE, data = x,missing="fiml"
             ,se = "boot",bootstrap= 50)
summary(apimi, fit.measures = TRUE)
# results: 
#   a = 0.36
#   p = 0.191
#   cx = 0.627
#   cy = -0.042 not significant
#   variances:
#   vx = hum = 1.393
#   ve = creat = 0.79

APIM_I <- '

                  Creat_rel.AB  ~ a*Hum_rel.AB + p*Hum_rel.BA + aDum + pDum
                  Creat_rel.BA  ~ a*Hum_rel.BA + p*Hum_rel.AB + aDum + pDum
                  Hum_rel.AB ~ mx*1  # mx = The hum_rel mean (should be zero)
                  Hum_rel.BA ~ mx*1
                  Creat_rel.AB ~ iy*1 # iy = intercept for Creativity_rel (should be zero)
                  Creat_rel.BA ~ iy*1
                  Hum_rel.AB ~~ vx*Hum_rel.AB  #vx = Hum variance
                  Hum_rel.BA ~~ vx*Hum_rel.BA
                  Creat_rel.AB ~~ ve*Creat_rel.AB #ve = error variance, the residual
                  Creat_rel.BA ~~ ve*Creat_rel.BA
                  Hum_rel.AB ~~ cx*Hum_rel.BA      #cx = cov, dyadic reciprocity?
                  Creat_rel.AB ~~ cy*Creat_rel.BA  # cy = cov of errors, rho? 
                  
                 # k := p/a
'

# Change to "bootstrap = 5000" to get reliable values for the confidence interval.  
apimi <- sem(APIM_I,fixed.x=FALSE, data = x,missing="fiml"
             ,se = "boot",bootstrap= 50)
summary(apimi, fit.measures = TRUE)
# results: 
#   a = 0.36
#   p = 0.191
#   cx = 0.627
#   cy = -0.042 not significant
#   variances:
#   vx = hum = 1.393
#   ve = creat = 0.79