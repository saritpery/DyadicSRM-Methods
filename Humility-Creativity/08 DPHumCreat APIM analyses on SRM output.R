rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".ReadyforAPIM"))

ipak(c("TripleR", "nlme"))  


# Humility - Creativity analyses:
# ============================================
# test the main hypothesis
  fit <- APIMformulaShow("Creat", "Hum")
  fit <- APIMformulaShow("Creat", c("Safety", "Hum"))
  fit <- APIMformulaShow("Creat", c("Safety"))
  fit <- APIMformulaShow("Safety", "Hum")
