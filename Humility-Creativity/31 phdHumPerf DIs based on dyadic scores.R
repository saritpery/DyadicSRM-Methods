rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# Define DIs Dyadic indices based on dyadic scores
# 
# 1. Test DIs as outcomes and as predictors
# 2. Test Scaled vs. non-scaled scores 
#    There is no significant difference between scaled and non-scaled equations. 
#    But it's more appropriate to use the non-scaled analyses for the non-APIM regressions.    
# 3. Test Edwards' recommendation against abs-delta as a predictor.
#    In their paper, Edwards et.al claim that instead of 
#    mean (complementary) + abs-delta (supplementary) of the predictors,  
#    one better use: X1 + X2 + X1^2 + X2^2 + X1*X2.
#    where, X1,X2 are the DSs (e.g: H_rel.AB, H_rel.BA).
#    When the DI is the outcome, those only have meaning when together, 
#    and they're interchangeable when separated. 
#    So, if I'm trying to predict a dyadic score, it makes sense. 
#    But if I'm trying to predict a DI, it doesn't. 
#    To view the use when predicting a DI, I added equivalent DIs as predictors
#    to see the comparison. 
#    In this dataset, the delta didn't provide any significant or substantial 
#    coef, so it was difficult to compare the two methods. 
#    W/o much meaning, it provided similar results. 
# 4. DI predicting DS
#    The method of analysis is APIM, hence: 
#    - we use the scaled scores, and the DIs based on the scaled scores. 
#    - The dataset used is the pw, with two entries per dyad, and with the scaled 
#      dyadic scores. 
# 5. Maximum/minimum DIs
#     
# Important notes:    
# 1. DIs are only relevant when there is a significant dyadic variance, 
#    Meaning, it's not relevant for the single-item constructs of shy and non-talkative. 
#    This is because these scores are confounded with the error, w/o a 'factor
#    analysis' to verify they are aligned and represent something meaningful.
# 2. When DIs are the outcome,
#    We should have a single row per dyad, since dyad members become indistinguishable.
# 
# Code Flow:
# 1. Prepare the data: 
# * Define DIs for all dyadic constructs (latent only - based on at least 2 items).
# * nsdlong = the long format dataset, based on nspw, with a single row per dyad.

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM")) 
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))
# ipak("sjPlot")

dyvarnames <- varnames[1:4] # The dyadic varnames include only 2-items constructs.

# Shorten the name
nspw <- nonScaledPW

# Create DIs (dyadic indices): 
for (i in dyvarnames) {
  v  <- paste0(c(i,i), c("_rel.AB", "_rel.BA"))
  v2 <- paste0(c(i,i), c("_rel2.AB", "_rel2.BA"))
  nspw[,paste0(i, "_relMean.di")] <- rowMeans(nspw[,v], na.rm = TRUE)
  nspw[,paste0(i, "_relDelta.di")] <- abs(nspw[,v[1]] - nspw[,v[2]])
  nspw[,paste0(i, "_relSum.di")] <- nspw[,v[1]] + nspw[,v[2]]
  min1 <- min(nspw[, v[1]], na.rm = TRUE)
  nspw[,paste0(i, "_relPosProd.di")] <- (nspw[,v[1]]-min1) * (nspw[,v[2]] - min1)
  nspw[,paste0(i, "_relProd.di")] <- nspw[,v[1]] * nspw[,v[2]]
  nspw[,paste0(i, "_relHigher.di")] <- pmax(nspw[,v[1]], nspw[,v[2]], na.rm = TRUE)
  nspw[,paste0(i, "_relLower.di")] <- pmin(nspw[,v[1]], nspw[,v[2]], na.rm = TRUE)
  nspw[,paste0(i, "_ABHigher")] <- nspw[,v[1]]>nspw[,v[2]]
  sqr <- mutate(nspw, sqr = across(all_of(v), ~.x^2))
  nspw[, v2] <- sqr$sqr
  nspw[,paste0(i, "_rel2Prod.di")] <- nspw[,v2[1]] * nspw[,v2[2]]
  nspw[,paste0(i, "_rel2Sum.di")] <- nspw[,v2[1]] + nspw[,v2[2]]
  nspw[,paste0(i, "_rel2Mean.di")] <- rowMeans(nspw[,v2], na.rm = TRUE)
  
  # I wish to compare the scaled results with the non-scaled resutls. 
  pw[,paste0(i, "_relMean.di")] <- rowMeans(pw[,v], na.rm = TRUE)
  pw[,paste0(i, "_relDelta.di")] <- abs(pw[,v[1]] - pw[,v[2]])
  pw[,paste0(i, "_relSum.di")] <- pw[,v[1]] + pw[,v[2]]
  min1 <- min(pw[, v[1]], na.rm = TRUE)
  pw[,paste0(i, "_relProd.di")] <- pw[,v[1]] * pw[,v[2]]
  pw[,paste0(i, "_relPosProd.di")] <- (pw[,v[1]]-min1) * (pw[,v[2]] - min1)
  pw[,paste0(i, "_relHigher.di")] <- pmax(pw[,v[1]], pw[,v[2]], na.rm = TRUE)
  pw[,paste0(i, "_relLower.di")] <- pmin(pw[,v[1]], pw[,v[2]], na.rm = TRUE)
  pw[,paste0(i, "_ABHigher")] <- pw[,v[1]]>pw[,v[2]]
  
  sqr <- mutate(pw, sqr = across(all_of(v), ~.x^2))
  pw[, v2] <- sqr$sqr
  pw[,paste0(i, "_rel2Prod.di")] <- pw[,v2[1]] * pw[,v2[2]]
  pw[,paste0(i, "_rel2Mean.di")] <- rowMeans(pw[,v2], na.rm = TRUE)
}
rm(list = c("sqr", "i", "v", "v2", "min1"))

# once our outcome variable is per dyad (and not per directional dyad), we need
# to have a single row per dyad as our dataset and not two rows per dyad. 
nsdlong <- group_by(nspw, dyad) |> filter(row_number()==1) |> 
  select(-aDum, -pDum) |> ungroup()

# Display table of DIs =========================================================
dis <- select(nsdlong, dyad, ends_with(".di")) |>
  # select(dyad, Creat_relMean.di, starts_with("Hum")) |>
  select(dyad, Creat_relMean.di, Hum_relMean.di, Hum_relDelta.di, Hum_relPosProd.di, 
  Hum_rel2Mean.di) |> 
  mutate(across(-dyad,round, 3))
names(dis) <- c("Dyad", "Creativity Mean.di", "Humility Mean.di",
                "Humility Delta.di", "Humility Product.di",
                "Humility SquaresMean.di")

write.csv(dis, file = fname("Dyadic Indices on DSs"))
 
# DIs as outcomes and predictors ===============================================

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relDelta.di", data = nsdlong))
confint(fit) 
# resutls: only the mean is substaintial (0.48) and significant CI[0.37, 0.59]

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di", data = nsdlong))
confint(fit) 
# resutls: only the mean is substaintial (0.47) and significant CI[0.37, 0.58]

(fit <- lm("Creat_relMean.di ~ Hum_relSum.di", data = nsdlong))
confint(fit) 
# resutls: only the sum is substaintial (0.47) and significant CI[0.36, 0.57]

(fit <- lm("Creat_relMean.di ~ Hum_relSum.di + Hum_relPosProd.di + Hum_rel2Mean.di", data = nsdlong))
confint(fit) 
# resutls: still only the sum is substaintial (0.24) and significant CI[0.18, 0.29]

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relProd.di + Hum_rel2Mean.di", data = nsdlong))
confint(fit) 
# resutls: still only the mean is substaintial (0.46) and significant CI[0.35, 0.57]

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relPosProd.di + Hum_rel2Mean.di", data = nsdlong))
confint(fit) 
# resutls: None of the elements is significant. 

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relPosProd.di", data = nsdlong))
confint(fit) 
# resutls: After dropping the non substaintial and not significant sum of squares, 
# still only the mean is substaintial (0.83) and significant 

(fit <- lm("Creat_relMean.di ~ Safety_relMean.di + Safety_relDelta.di", data = nsdlong))
confint(fit) 
# resutls: only the mean is substaintial (0.42) and significant

(fit <- lm("Creat_relMean.di ~ Safety_relMean.di + Safety_relProd.di + Safety_rel2Prod.di", data = nsdlong))
confint(fit) 
# resutls: still only the mean is substaintial (0.45) and significant CI[0.36, 0.61]

## Add DIs based on Individual scores
#(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_TTProd.di", data = nsdlong))
#confint(fit) 
# resutls: The product of the Humility target scores didn't add to the mean rel creativity.
# Maybe my take on that is that we lose a lot of information by dropping the level
# of inquiry.

# compare scaled and non-scaled analysis: ======================================
# There is no significant difference between the scaled and non-scaled equations. 
dlong <- group_by(pw, dyad) |> filter(row_number()==1) |> select(-aDum, -pDum) |> ungroup()

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relDelta.di", data = dlong))
confint(fit) 
# resutls: very similar to the scaled results: 
#          Only the mean is substaintial (0.48 vs. 0.47) and significant
(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relProd.di + Hum_rel2Prod.di", data = dlong))
confint(fit) 
# resutls: very similar to the scaled results: 
#          still only the mean is substaintial (0.44 vs. 0.43) and significant


# Dyadic Scores as the outcome of DIs ==========================================
# The following APIM analyses use pw (scaled with dummy codes)  as the dataset

fit <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + 
                                Hum_relDelta.di + aDum + pDum")
# results: The delta isn't substantial nor significant. 

fit <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + 
                                 aDum + pDum")
# results: After dropping the not relevant delta element, 
#  The coefficients are 0.3 and 0.16 respectively. 

fit <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + Hum_relPosProd.di + 
                 Hum_rel2.AB + Hum_rel2.BA + aDum + pDum")
confint(fit) 
# results: still only the mean is substantial (0.45) and significant CI[0.34, 0.56]



fit <- showAPIM("Creat_rel.AB ~ Safety_rel.AB + Safety_rel.BA + 
                                 Safety_relDelta.di + aDum + pDum")
# results: The delta isn't substantial nor significant. 

# Min/Max DIs ==================================================================
# In this section: 
#   1. using DIs of maximum as outcome and predictors of creativity by humility
#   2. Some examples of collinearity that result with NA as coefficient
#   3. short version of tests for Creat-mean ~ Hum
#   4. short version of tests for Safety-mean ~ Hum
#   5. short version of tests for Safety-mean ~ Creat

(fit <- lm("Creat_relHigher.di ~ Hum_relMean.di + Hum_relDelta.di", data = nsdlong))
confint(fit) 
# results: delta isn't significant. mean (0.51)

(fit <- lm("Creat_relHigher.di ~ Hum_relMean.di", data = nsdlong))
confint(fit) 
# results: mean (0.52) significant

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di", data = nsdlong))
confint(fit) 
# results: mean (0.47) significant

# collinearity tests
(fit <- lm("Creat_relHigher.di ~ Hum_relMean.di + Hum_relDelta.di +
                                      Hum_relHigher.di", data = nsdlong))
confint(fit) 
# results: Higher's effect is NA. It is highly correlated with the mean. 
sum(is.na(nsdlong$Hum_relHigher.di)) # zero NAs
lm(Hum_relHigher.di ~ Hum_relMean.di + Hum_relDelta.di, data = nsdlong)
cor(nsdlong$Hum_relMean.di, nsdlong$Hum_relHigher.di) # 0.9

(fit <- lm("Creat_relHigher.di ~ Hum_relLower.di + Hum_relHigher.di", data = nsdlong))
confint(fit) 
cor(select(nsdlong, Hum_relLower.di, Hum_relHigher.di)) #0.64

# results: All significant, but rather low
#         Intercept 0.49
#         Hum_lower  0.27
#         Hum_higher 0.24

# Only 59% of the dyad members, uniquley higher on creativity, are also 
# the ones uniquley higher on humility! 
sum(nsdlong$Creat_ABHigher == nsdlong$Hum_ABHigher)/nrow(nsdlong)

# So, Is it the higher/lower humility that effects the higher Creativity, or is it 
#   the unique Humility levels of themselves or their partner? 
#   For example: 
#   Say, that in the dyad of Amy and Bob, 
#     Creat_rel.AmyBob > Creat_rel.BobAmy
#     It means Bob became more uniquley creative in their dyad, and we want to 
#     know what predicts this higher rise in creativity than expected. 
#   Does it stem from a unique humility formed in the dyad (Hum_relHigher) by 
#   either of the dyad members? 
#   or maybe it stems from the specifics of 
#     how Amy views Bob as humble: Hum_rel.AmyBob = Hum_CreatHiger.AB, 
#     or maybe how Bob views Amy uniquely humble: Hum_rel.BobAmy = Hum_CreatHiger.BA?
#   To find out about the last two options, we define the following: 
# We enter the Humility dss of the same direction as the more creative: 
# (if Creat_rel.AB > Creat_rel.BA, 
#   then Hum_CreatHiger.AB <- Hum_rel.AB; Hum_CreatHiger.BA <- Hum_rel.BA
#   otherwise, Hum_CreatHiger.AB <- Hum_rel.BA; Hum_CreatHiger.BA <- Hum_rel.AB)
nsdlong$Hum_CreatHigher.AB <- 
  ifelse(nsdlong$Creat_ABHigher, nsdlong$Hum_rel.AB, nsdlong$Hum_rel.BA)
nsdlong$Hum_CreatHigher.BA <- 
  ifelse(nsdlong$Creat_ABHigher, nsdlong$Hum_rel.BA, nsdlong$Hum_rel.AB)


(fit <- lm("Creat_relHigher.di ~ Hum_CreatHigher.AB + 
                                    Hum_CreatHigher.BA", data = nsdlong))
confint(fit) 
# results: All significant, with very similar low coefficients. 
#         Intercept 0.47
#         Hum_rel of the person higher on Creat  0.28
#         Hum_rel of the person "lower" on Creat  0.24

# Same tests for the Creat mean as outcome (same results as higher.DI)
(fit <- lm("Creat_relMean.di ~ Hum_relLower.di + Hum_relHigher.di", data = nsdlong))
confint(fit) 
(fit <- lm("Creat_relHigher.di ~ Hum_CreatHigher.AB + 
                                    Hum_CreatHigher.BA", data = nsdlong))
confint(fit) 
(fit <- lm("Creat_relHigher.di ~ Hum_relLower.di + Hum_relHigher.di", data = nsdlong))
confint(fit) 

# Same tests for Higher unique Safety on Humility: 

(fit <- lm("Safety_relHigher.di ~ Hum_relMean.di + Hum_relDelta.di", data = nsdlong))
confint(fit) 
# results: only the mean is substantial and significant (0.51) 

(fit <- lm("Safety_relHigher.di ~ Hum_relLower.di + Hum_relHigher.di", data = nsdlong))
confint(fit) 
# resutls: All significant though weak
#         Intercept 0.41
#         Hum_lower  0.21
#         Hum_higher 0.30

# Only 59% of the dyad members, uniquley higher on psychological safety, are also 
# the ones uniquley higher on humility! 
sum(nsdlong$Safety_ABHigher == nsdlong$Hum_ABHigher)/nrow(nsdlong)

# So, Is it the higher Humer that effects the higher Safety, or is it the Huming
# levels of himself or their partner? 
# First we enter the Huming dss of the higher on Safety direction 
# (if Safety_rel.AB > Safety_rel.BA, 
#   then Hum_SafetyHiger.AB <- Hum_rel.AB; Hum_SafetyHiger.BA <- Hum_rel.BA
#   otherwise, Hum_SafetyHiger.AB <- Hum_rel.BA; Hum_SafetyHiger.BA <- Hum_rel.AB)
nsdlong$Hum_SafetyHigher.AB <- 
  ifelse(nsdlong$Safety_ABHigher, nsdlong$Hum_rel.AB, nsdlong$Hum_rel.BA)
nsdlong$Hum_SafetyHigher.BA <- 
  ifelse(nsdlong$Safety_ABHigher, nsdlong$Hum_rel.BA, nsdlong$Hum_rel.AB)

(fit <- lm("Safety_relHigher.di ~ Hum_SafetyHigher.AB + 
                                    Hum_SafetyHigher.BA", 
           data = nsdlong))
confint(fit) 
# resutls: The same person's unique perception on humility is significant (0.4).
#          The other person's isn't significant and weaker. 

# Same for unique Safety on unique Creativity

(fit <- lm("Safety_relHigher.di ~ Creat_relMean.di + Creat_relDelta.di", data = nsdlong))
confint(fit) 
# results: All is significant. mean is 0.52, delta 0.07

(fit <- lm("Safety_relHigher.di ~ Creat_relLower.di + Creat_relHigher.di", data = nsdlong))
confint(fit) 
# resutls: All significant though weak
#         Intercept 0.38
#         Creat_lower  0.19
#         Creat_higher 0.33

(fit <- lm("Safety_relMean.di ~ Creat_relMean.di", data = nsdlong))
confint(fit) 
# resutls: 0.55 significant.

# I checked and only about 60% of the people higher in Safety are the ones higher
# in Creat. 
# So, Is it the higher Creat feeling person that effects the higher Safety, 
# or is it the Creat levels of themselves or of their partner? 
# First we enter the Creat dss of the higher on Safety direction 
# (if Safety_rel.AB > Safety_rel.BA, 
#   then Creat_SafetyHiger.AB <- Creat_rel.AB; Creat_SafetyHiger.BA <- Creat_rel.BA
#   otherwise, Creat_SafetyHiger.AB <- Creat_rel.BA; Creat_SafetyHiger.BA <- Creat_rel.AB)
nsdlong$Creat_SafetyHigher.AB <- 
  ifelse(nsdlong$Safety_ABHigher, nsdlong$Creat_rel.AB, nsdlong$Creat_rel.BA)
nsdlong$Creat_SafetyHigher.BA <- 
  ifelse(nsdlong$Safety_ABHigher, nsdlong$Creat_rel.BA, nsdlong$Creat_rel.AB)

(fit <- lm("Safety_relHigher.di ~ Creat_SafetyHigher.AB + 
                                    Creat_SafetyHigher.BA", data = nsdlong))
confint(fit) 
# resutls: without the effects of the higher and lower Creat, 
#         then the Creat ds of the Safetyer and their partner are significant.
#         But the Creat of the same person, counts more. 
#         So, unlike in Huming-Creat, in Creat-Safety, the way the person
#         feels drives the unique Safety, above the general level of Creat in the dyad. 
#         
#         Intercept 0.39
#         Creat_rel of the person higher on Safety  0.37
#         Creat_rel of the person "lower" on Safety  0.19

