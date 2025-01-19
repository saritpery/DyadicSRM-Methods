
# Humility Performance study - Individual level mediation
# ==============================================================================
# In this file we perform mediation tests at the individual level. 
# 1. We have a mediation hypothesis at the dyadic level. 
#    Here we perform it at the individual level, with all sorts of variation
#    to rule out existence of correlations other than our hypotheses.
# 2. All sorts of demographic moderation at the individual level.
# ==============================================================================

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# SRM - RR ANALYSIS:
# ==============================================================================

load("main.RImage")
load(imgName(".FullRR.ReadyforAnalaysis"))

ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse",
        "lavaan", "semPlot"))

x <- as.data.frame(x)

# Calculate all the scores at the individual level (actor and partner scores 
# for all constructs for all participants)
apEffects <- getEffects(~ a.id*p.id | group.id, data = x, 
                        varlist = c("H1/H2", "Creat1/Creat2", "Contrib1/Contrib2", 
                                    "Sf1/Sf2",  "NonTalk","Shy"))
names(apEffects) <- gsub('[12]', '', names(apEffects))

apa.cor.table(select(apEffects, H.p, Sf.a, Creat.p))

# MEDIATION HYPOTHESIS:
# --------------------
# The hypothesis was H.p -> Sf.a -> Creat.p or Contrib.p
# The hypothesis was framed at the dyadic level, and I'm adding here
#     the test for the individual level.
# ==================================================================

ipak("diagram")

# First I added the H.a to the model to see if it had a substantial effect.
# it didn't, so on to the next model that will describe the hypothesis.
m <- sem(
  'Creat.p ~ Sf.a + H.p + H.a
      Sf.a ~ H.p
      H.a ~~ 0*H.p', data = apEffects)

semPaths(m, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree")
modindices(m)

# Almost the hypothesis model, only with H.p instead of H.a
m <- sem(
  'Creat.p ~ Sf.a + H.p
      Sf.a ~ H.p', data = apEffects)
modindices(m)
semPaths(m, "std", 
         rotation = 1, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "spring")
modindices(m)

summary(m)
# The actual hypothesis model:
m <- sem(
  'Creat.p ~ b* Sf.a + c* H.a
       Sf.a ~ a* H.a
       # indirect effect (a*b)
       ab := a*b
       # total effect
       total := c + (a*b)', 
  data = apEffects)


semPaths(m, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree")

summary(m, standardized = T)

# Same model at the partner level (where we hope that the correlation
# with contribution won't work at any level)
m <- sem(
  'Creat.p ~ Sf.p + H.p
      Sf.p ~ H.p', data = apEffects)
modindices(m)

semPaths(m, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree")

summary(m, standardized = T)

# Analysis of mediation at the individual level with contribution instead of Creativity

# The actual hypothesis model:
m <- sem(
  'Contrib.p ~ b* Sf.a + c* H.a
       Sf.a ~ a* H.a
       # indirect effect (a*b)
       ab := a*b
       # total effect
       total := c + (a*b)', 
  data = apEffects)
semPaths(m, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree")
modindices(m)
summary(m, standardized = T)

# Same model at the partner level (where we hope that the correlation
# with contribution won't work at any level)
m <- sem(
  'Contrib.p ~ Sf.p + H.p
      Sf.p ~ H.p', data = apEffects)
semPaths(m, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree")
modindices(m)
summary(m, standardized = T)

m0 <- 'H =~ Hum1 + Hum2 + Hum3
             Sf =~ Safe1 + Safe2 
             Contrib =~ Contrib1 + Contrib2
             Creat =~ Cre1 + Cre2 + Cre3'
fit0 <- sem(m0, data = longInput)
fitHSf <- update(fit0, add =  'H ~~ 1*Sf')
fitHCont <- update(fit0, add =  'H ~~ 1*Contrib')
fitHCreat <- update(fit0, add =  'H ~~ 1*Creat')
fitContrCreat <- update(fit0, add =  'Contrib ~~ 1*Creat')
fitContrSf <- update(fit0, add =  'Contrib ~~ 1*Sf')
fitCreatSf <- update(fit0, add =  'Creat ~~ 1*Sf')

anova(fit0, fitHCont)
anova(fit0, fitHCreat)
anova(fit0, fitHSf)
anova(fit0, fitContrCreat)
anova(fit0, fitContrSf)
anova(fit0, fitCreatSf)

summary(fit0, standardized = TRUE)
fitMeasures(fit0)
fitMeasures(fit0, c("cfi","rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
resid(fit0, type = "standardized")
resid(fit0, type = "cor")
semPaths(fit0)
semPaths(fit0, "std", 
         #rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         #exoCov = FALSE, # Exclude the covariance
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         #sizeInt = 15,
         edge.label.cex = 1,
         #label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10)

# General Moderation GENDER/DEMOGRAPHIC test                               ----
# ==============================================================================

# Gender: 
#   (in the survey 1=F, 2=M
#   Here we do Gender-1 So the results refer to):
#   0 = Female; 1 = Male
#   
# Attach the moderators (Gender, Age, Tenure) 
# to the EFFECTS according to the id
moderators <- c("Gender", "Age", "Tenure")
GenderTitle <- "Gender(Female=0;Male=1)"
d <- x %>% select(id = a.id, any_of(moderators)) %>% distinct()
d <- d %>% mutate(Gender = Gender - 1)

apEffects <- left_join(apEffects, d, by = "id")

# In order to have reasonable size correlation tables, we'll split between 
# partner effects and actor effects. 
# of course, we can look at it all together:
# 
# apa.cor.table(select(apEffects, -id, group.id))

# moderators with partner effects:
t <- apa.cor.table(select(apEffects, ends_with(".t"), all_of(moderators)))
t <- improveTables(t, "Gender", GenderTitle, 
                   rrStyle = RRstyle,
                   tTitle = "Target Scores correlation table",
                   rmSpaceLines = TRUE)
t

# moderators with actor effects: 
t <- apa.cor.table(select(apEffects, ends_with(".p"), all_of(moderators)))
t <- improveTables(t, "Gender", GenderTitle, 
                   rrStyle = RRstyle,
                   tTitle = "Perceiver Scores correlation table",
                   rmSpaceLines = TRUE)
t
