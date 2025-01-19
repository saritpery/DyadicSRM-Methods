rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak(c("TripleR", "nlme"))  

pw <- dyScaledPW   # Verify I'm working on the correct scaling option

# creativity analyses:
# ============================================
# test the main hypothesis
  fit <- APIMformulaShow("Creat", "Hum")
  fit <- APIMformulaShow("Creat", c("Safety", "Hum"))
  fit <- APIMformulaShow("Creat", c("Safety"))
  fit <- APIMformulaShow("Safety", "Hum")

# test for the opposite causality
  fit <- APIMformulaShow("Hum", c("Safety"))
  
  fit <- APIMformulaShow("Hum", "Creat")
  fit <- APIMformulaShow("Hum", c("Safety", "Creat"))
  fit <- APIMformulaShow("Safety", "Creat")
  fit <- APIMformulaShow("Safety", c("Hum", "Creat"))

# contribution analysis:
# ============================================
  
  # test the main hypothesis
  fit <- APIMformulaShow("Contr", "Hum")
  fit <- APIMformulaShow("Contr", c("Safety", "Hum"))
  fit <- APIMformulaShow("Contr", c("Safety"))
  fit <- APIMformulaShow("Safety", "Hum")
  
  # test for the opposite causality
  fit <- APIMformulaShow("Hum", c("Safety"))
  fit <- APIMformulaShow("Hum", "Contr")
  fit <- APIMformulaShow("Hum", c("Safety", "Contr"))
  fit <- APIMformulaShow("Safety", "Contr")
  fit <- APIMformulaShow("Safety", c("Hum", "Contr"))

# test for alternative effects - Introversion
# =============================================
  fit <- APIMformulaShow("Shy", c("Safety"))
  fit <- APIMformulaShow("Shy", c("Safety", "Hum"))
  fit <- APIMformulaShow("Creat", c("Safety", "Shy"))
  fit <- APIMformulaShow("Creat", c("Hum", "Shy"))
  fit <- APIMformulaShow("Safety", "Shy")
  
  fit <- APIMformulaShow("NonTalk", c("Safety"))
  fit <- APIMformulaShow("NonTalk", c("Safety", "Hum"))
  
  fit <- APIMformulaShow("Safety", "NonTalk")
  fit <- APIMformulaShow("Creat", c("Safety", "NonTalk"))
  