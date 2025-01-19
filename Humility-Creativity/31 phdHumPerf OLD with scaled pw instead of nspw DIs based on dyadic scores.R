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
#    and they're interchangable when separated. 
#    So, if I'm trying to predict a dyadic score, it makes sense. 
#    But if I'm trying to predict a DI, it doesn't. 
#    To view the use when predicting a DI, I added equivalent DIs as predictors
#    to see the comparison. 
#    In this dataset, the delta didn't provide any significant or substaintial 
#    coef, so it was difficult to compare the two methods. 
#    W/o much meaning, it provided similar results. 
# 4. DI predicting DS
#    The method of analysis is APIM, hence: 
#    - we use the scaled scores, and the DIs based on the scaled scores. 
#    - The dataset used is the pw, with two entries per dyad, and with the scaled 
#      dyadic scores. 
# 5. Maximum/minimum DIs
#     
# Improtant notes:    
# 1. DIs are only relevant when there is a significant dyadic variance, 
#    Meaning, it's not relevant for the single-item constructs of shy and non-talkative. 
#    This is because these scores are confounded with the error.
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
dlong <- group_by(pw, dyad) |> filter(row_number()==1) |> select(-aDum, -pDum)
# dlong$Hum_TTProd.di <- dlong$Hum_target.A * dlong$Hum_target.B

# Display table of DIs =========================================================
dis <- select(dlong, dyad, ends_with(".di")) |>
  # select(dyad, Creat_relMean.di, starts_with("Hum")) |>
  select(dyad, Creat_relMean.di, Hum_relMean.di, Hum_relDelta.di, Hum_relPosProd.di, 
  Hum_rel2Mean.di) |> 
  mutate(across(everything(),round, 3))
names(dis) <- c("Dyad", "Creativity Mean.di", "Humility Mean.di",
                "Humility Delta.di", "Humility Product.di",
                "Humility SquaresMean.di")

write.csv(dis, file = fname("Dyadic Indices on DSs"))
 
# DIs as outcomes and predictors ===============================================

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relDelta.di", data = dlong))
confint(fit) 
# resutls: only the mean is substaintial (0.47) and significant CI[0.36, 0.57]

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di", data = dlong))
confint(fit) 
# resutls: only the mean is substaintial (0.47) and significant CI[0.36, 0.57]

(fit <- lm("Creat_relMean.di ~ Hum_relSum.di", data = dlong))
confint(fit) 
# resutls: only the mean is substaintial (0.47) and significant CI[0.36, 0.57]

(fit <- lm("Creat_relMean.di ~ Hum_relSum.di + Hum_relPosProd.di + Hum_rel2Mean.di", data = dlong))
confint(fit) 
# resutls: still only the sum is substaintial (0.23) and significant CI[0.17, 0.28]

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relProd.di + Hum_rel2Mean.di", data = dlong))
confint(fit) 
# resutls: still only the mean is substaintial (0.45) and significant CI[0.34, 0.56]

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relPosProd.di + Hum_rel2Mean.di", data = dlong))
confint(fit) 
# resutls: None of the elements is significant. 

(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relPosProd.di", data = dlong))
confint(fit) 
# resutls: After dropping the non substaintial and not significant sum of squares, 
# still only the mean is substaintial (0.45) and significant CI[0.34, 0.56]

(fit <- lm("Creat_relMean.di ~ Safety_relMean.di + Safety_relDelta.di", data = dlong))
confint(fit) 
# resutls: only the mean is substaintial (0.45) and significant CI[0.34, 0.57]

(fit <- lm("Creat_relMean.di ~ Safety_relMean.di + Safety_relProd.di + Safety_rel2Prod.di", data = dlong))
confint(fit) 
# resutls: still only the mean is substaintial (0.48) and significant CI[0.36, 0.61]

# Add DIs based on Individual scores
(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_TTProd.di", data = dlong))
confint(fit) 
# resutls: The product of the Humility target scores didn't add to the mean rel creativity.
# Maybe my take on that is that we lose a lot of information by dropping the level
# of inquiry.

# compare scaled and non-scaled analysis: ======================================
# There is no significant difference between the scaled and non-scaled equations. 
nsdlong <- group_by(nspw, dyad) |> filter(row_number()==1) |> select(-aDum, -pDum)
(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relDelta.di", data = nsdlong))
confint(fit) 
# resutls: very similar to the scaled results: 
#          Only the mean is substaintial (0.48) and significant CI[0.37, 0.59]
(fit <- lm("Creat_relMean.di ~ Hum_relMean.di + Hum_relProd.di + Hum_rel2Prod.di", data = nsdlong))
confint(fit) 
# resutls: very similar to the scaled results: 
#          still only the mean is substaintial (0.44) and significant CI[0.33, 0.56]


# Dyadic Scores as the outcome of DIs
fit <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + 
                                 Hum_relDelta.di + aDum + pDum")
# results: The delta isn't substaintial nor significant. 

fit <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + 
                                 aDum + pDum")
# results: After dropping the not relevant delta element, 
#  The coefficients are 0.3 and 0.16 respectively. 

fit <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + Hum_relPosProd.di + 
                 Hum_rel2.AB + Hum_rel2.BA + aDum + pDum")
confint(fit) 
# resutls: still only the mean is substaintial (0.45) and significant CI[0.34, 0.56]



fit <- showAPIM("Creat_rel.AB ~ Safety_rel.AB + Safety_rel.BA + 
                                 Safety_relDelta.di + aDum + pDum")
# results: The delta isn't substaintial nor significant. 
