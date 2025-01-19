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
#    where, X1,X2 are the DSs (e.g: listen_rel.AB, listen_rel.BA).
#    When the DI is the outcome, those only have meaning when together, 
#    and they're interchangable when separated. 
#    So, if I'm trying to predict a dyadic score, it makes sense. 
#    But if I'm trying to predict a DI, it doesn't. 
#    To view the use when predicting a DI, I added equivalent DIs as predictors
#    to see the comparison. 
#    In this dataset, the delta didn't provide any significant or substaintial 
#    coef, so it was difficult to compare the two methods. 
#    W/o much meaning, it provided similar results.
#    ============================
#    REMOVE PRIOR TO PUBLICATION
#    ============================
# 3b.A minor diversion to look into the product.di (of ds vs. positive ds), 
#    cor vs. regression, and scaled vs. non-scaled. 
#    The coefficient of the prod.di and the coefficient of the PosProduct.di 
#    are identical when in the Edwards equation. 
#    That bothered me.
# 4. DI predicting DS
#    The method of analysis is APIM, hence: 
#    - we use the scaled scores, and the DIs based on the scaled scores. 
#    - The dataset used is the pw, with two entries per dyad, and with the scaled 
#      dyadic scores. 
# 5. Maximum/minimum DIs
#    
# Important notes:    
# 1. DIs are only relevant when there is a significant dyadic variance, 
#    luckily all constructs in the study have that.
# 2. When DIs are the outcome,
#    We should have a single row per dyad, since dyad members become indistinguishable.
# 
# Code Flow:
# 1. Prepare the data: 
# * Define DIs for all dyadic constructs (latent only - based on at least 2 items).
# * nsdlong = the long format dataset, based on nspw, with a single row per dyad.

load("main.RImage")
load(imgName("Ready4APIM2"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))
# ipak("sjPlot")

# Shorten the name
nspw <- nonScaledPW

# Create DIs (dyadic indices): 
for (i in varnames) {
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
  # select(dyad, Creat_relMean.di, starts_with("listen")) |>
  select(dyad, intimacy_relMean.di, listen_relMean.di, listen_relDelta.di, 
         listen_relPosProd.di, listen_rel2Mean.di) |> 
  mutate(across(-dyad,round, 3))
names(dis) <- c("Dyad", "Intimacy Mean.di", "Listening Mean.di",
                "Listening Delta.di", "Listening Product.di",
                "Listening SquaresMean.di")

# write.csv(dis, file = fname("Dyadic Indices on DSs"))
 
# DIs as outcomes and predictors ===============================================

(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relDelta.di", data = nsdlong))
confint(fit) 
# resutls:  both mean and delta are significant, 
#           mean  (1.10) CI[1.01, 1.18], 
#           delta (0.11) CI[0.16, 0.21]

(fit <- lm("intimacy_relMean.di ~ listen_relSum.di + listen_relDelta.di", data = nsdlong))
confint(fit) 
# resutls:  both sum and delta are significant, of course, the sum is exactly half 
#           the coefficient of the mean - as should be. it's the coefficient for 
#           numbers which are twice the mean. 
#           sum   (0.55) CI[0.50, 0.59], 
#           delta (0.11) CI[0.16, 0.21]

# Edwards with mean instead of sum
(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relPosProd.di + listen_rel2Mean.di", data = nsdlong))
confint(fit) 
# resutls: only the mean is significant mean 1.31 CI[0.76, 1.87]
#          posProd -0.03150 CI[-0.107, 0.044]


# Divertion to test the difference between product of ds, and product of (positive ds)
  # In the analyses with and w/o positivity transformation of the product, 
  # the coefficients and CIs of the product with and w/o positivity transformation
  # were identical. Both for the scaled and non-scaled analyses. 
  # It bothered me. 
  # But once I put only the product.di in the equation, the difference emerged.
  # I still don't know why it is identical when controlling for the other dis.
  # 
  # # Edwards with the product w/o positivity transformation
  # # This analysis was just an internal test, but it's not relevant and therefore grayed out.
  # (fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relProd.di + listen_rel2Mean.di", data = nsdlong))
  # confint(fit) 
  # # resutls: only the mean is significant mean 1.09 CI[0.99, 1.18]
  # #          Prod -0.03150 CI[-0.107, 0.044]
  # #          Wierd. No difference from the positivity transformation. 
  # #          This is true for both scaled and non-scaled scores. 
  # 
  # view(select(dlong, dyad, starts_with("listen_rel")))
  # cor(dlong$listen_relPosProd.di, dlong$listen_relProd.di)    # 0.3538697
  # cor(dlong$intimacy_relMean.di, dlong$listen_relPosProd.di)  # 0.8176077
  # cor(dlong$intimacy_relMean.di, dlong$listen_relProd.di)     # 0.1531975
  # (fit <- lm("intimacy_relMean.di ~ listen_relProd.di", data = dlong))      # 0.13743
  # (fit <- lm("intimacy_relMean.di ~ listen_relPosProd.di", data = dlong))   # 0.1291
  # # non-scaled
  # (fit <- lm("intimacy_relMean.di ~ listen_relProd.di", data = nsdlong))    # 0.15234
  # (fit <- lm("intimacy_relMean.di ~ listen_relPosProd.di", data = nsdlong)) # 0.1431
  # Q: What is the core difference between cor and lm? 
  # A: Correlation quantifies the strength of the linear relationship between 
  #    a pair of variables, whereas regression expresses the relationship in the 
  #    form of an equation.
  #    The correlation is 1 for Y=2X and for Y=0.001X.

# Edwards direct equation (sum instead of mean)
(fit <- lm("intimacy_relMean.di ~ listen_relSum.di + listen_relPosProd.di + listen_rel2Mean.di", data = nsdlong))
confint(fit) 
# resutls: only the sum is significant sum 0.66 CI[0.38, 0.94]. The product's
# coef is very low (-0.03), so we'll drop the element.

(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_rel2Mean.di", data = nsdlong))
confint(fit) 
# resutls: After dropping the non substaintial and not significant product.di, 
# still only the mean is substaintial (1.08) and significant CI[0.99, 1.17]
# Therefore, there's some difference between mean+delta, and the Edwards equation.
# The delta was significant (though not substaintial), whereas in the Edwards 
# equation, only the mean was relevant.

(fit <- lm("help_relMean.di ~ intimacy_relMean.di + intimacy_relDelta.di", data = nsdlong))
confint(fit) 
# resutls: only the mean is substaintial (0.53) and significant CI[0.49, 0.59]

(fit <- lm("help_relMean.di ~ listen_relMean.di + listen_rel2Mean.di", data = nsdlong))
confint(fit) 
# Only the mean is relevant.


# Add DIs based on Individual scores
  (fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_TTProd.di", data = nsdlong))
  confint(fit) 
  # results: The product of the listening target scores didn't add to the mean rel intimacyivity.
  # Maybe my take on that is that we lose a lot of information by dropping the level
  # of inquiry.

# compare scaled and non-scaled analysis: ======================================
# There is no significant difference between the scaled and non-scaled equations. 

# once our outcome variable is per dyad (and not per directional dyad), we need
# to have a single row per dyad as our dataset and not two rows per dyad. 
dlong <- group_by(pw, dyad) |> filter(row_number()==1) |> select(-aDum, -pDum)

# Scaled: DIs as outcomes and predictors =======================================

(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relDelta.di", data = dlong))
confint(fit) 
# resutls:  both mean and delta are significant, 
#           mean  (0.91) CI[0.83, 0.98], 
#           delta (0.09) CI[0.13, 0.17]

(fit <- lm("intimacy_relMean.di ~ listen_relSum.di + listen_relDelta.di", data = dlong))
confint(fit) 
# resutls:  both sum and delta are significant, of course, the sum is exactly half 
#           the coefficient of the mean - as should be. it's the coefficient for 
#           numbers which are twice the mean. 
#           sum  (0.45) CI[0.42, 0.49], 
#           delta (0.09) CI[0.13, 0.17]

# Edwards with mean instead of sum
(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relPosProd.di + listen_rel2Mean.di", data = dlong))
confint(fit) 
# resutls: only the mean is significant mean 1.09 CI[0.63, 1.55]
#          posProd -0.02841 CI[-0.096, 0.039]

# Edwards with the product w/o positivity transformation
# This analysis was just an internal test, but it's not relevant and therefore grayed out.
(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relProd.di + listen_rel2Mean.di", data = dlong))
confint(fit) 
# resutls: only the mean is significant mean 0.54 CI[0.31, 0.78]
#          Prod -0.02841 CI[-0.096, 0.039]
#          Wierd. No difference from the positivity transformation. 
# Compare with no scaling: 

# Edwards direct equation (sum instead of mean)
(fit <- lm("intimacy_relMean.di ~ listen_relSum.di + listen_relPosProd.di + listen_rel2Mean.di", data = dlong))
confint(fit) 
# resutls: only the sum is significant sum 0.54 CI[0.31, 0.78]

(fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_relPosProd.di", data = dlong))
confint(fit) 
# resutls: After dropping the non substaintial and not significant sum of squares, 
# still only the mean is substaintial (0.45) and significant CI[0.34, 0.56]

(fit <- lm("help_relMean.di ~ intimacy_relMean.di + intimacy_relDelta.di", data = dlong))
confint(fit) 
# resutls: only the mean is substaintial (0.45) and significant CI[0.34, 0.57]

(fit <- lm("help_relMean.di ~ intimacy_relMean.di + intimacy_relProd.di + intimacy_rel2Prod.di", data = dlong))
confint(fit) 
# resutls: still only the mean is substaintial (0.48) and significant CI[0.36, 0.61]

# # Add DIs based on Individual scores
# (fit <- lm("intimacy_relMean.di ~ listen_relMean.di + listen_TTProd.di", data = dlong))
# confint(fit) 
# # resutls: The product of the listening target scores didn't add to the mean rel intimacy.
# # Maybe my take on that is that we lose a lot of information by dropping the level
# # of inquiry.

# Dyadic Scores as the outcome of DIs ==========================================
fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB + listen_rel.BA + aDum + pDum")
# results: all coef. are very significant
#   listen.AB 0.62 p=0.00
#   listen.BA 0.29 p=0.00

fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB + listen_rel.BA + 
                                 listen_relDelta.di + aDum + pDum")
# results: all coef. are very significant
#   listen.AB 0.62 p=0.00
#   listen.BA 0.29 p=0.00
#   delta     0.28 p=0.01

fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB*listen_relDelta.di + 
                                  listen_rel.BA + aDum + pDum")
# results: No interaction with the delta. I think it has to be like that, 
#           because the delta is the same for both dyad's scores. So an interaction
#           would suggest that both scores effect the outcome exactly in the same way. 
#   listen.AB 0.62 p=0.00
#   listen.BA 0.29 p=0.00
#   delta     0.28 p=0.01
#   lstn.AB*delta 0.00 p=0.98...

fit <- showAPIM("intimacy_rel.AB ~ listen_rel.higher+listen_relDelta.di + 
                                  listen_rel.lower + aDum + pDum")
# results: It doesn't converge. It can't converge because all the rhs are dis
#          where the outcome is directional.

# The following analysis isn't right because these are all DIs, so it should 
# be analyzed on nsdlong and not pw or nspw. otherwise I'm doubling the data.
# fit <- showAPIM("intimacy_rel.higher ~ listen_rel.higher+ 
#                                   listen_rel.lower + aDum + pDum")
# # results: 
# #   listen.higher 0.82 p=0.00
# #   listen.lower  0.09 p=0.23
# #   So the better unique listener, determines the better creativity, 
# #   and the less so listener, doesn't effect the higher creativity!!!!!!!!

fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB + listen_rel.BA + 
                                 aDum + pDum")
# results: interestingly nothing really changed.
#   listen.AB 0.62 p=0.00
#   listen.BA 0.29 p=0.00

fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB + listen_rel.BA + listen_relPosProd.di + 
                 listen_rel2.AB + listen_rel2.BA + aDum + pDum")
# results: product and rel2.BA are not substantial and not significant
#   listen.AB, listen.BA 0.9, 0.57  p=0.00, 0.04
#   rel2.AB   0.09 p=0.05

# dropping the non significant elements: 
fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB + listen_rel.BA + 
                 listen_rel2.AB + aDum + pDum")
# results: the square last element isn't significant any more. 

# dropping the non significant elements: 
fit <- showAPIM("intimacy_rel.AB ~ listen_rel.AB + listen_rel.BA + aDum + pDum")
# results: back to 0.62 and 0.29

fit <- showAPIM("help_rel.AB ~ listen_rel.AB + listen_rel.BA + 
                                 listen_relDelta.di + aDum + pDum")
# results: The delta isn't substantial nor significant. 

fit <- showAPIM("help_rel.AB ~ intimacy_rel.AB + intimacy_rel.BA + 
                                 intimacy_relDelta.di + aDum + pDum")
# results: The delta isn't substantial nor significant. 

# Min/Max DIs ==================================================================
# In this section: 
#   1. using DIs of maximum as outcome and predictors of intimacy by listening
#   2. Some examples of collinearity that result with NA as coefficient
#   3. short version of tests for intimacy-mean ~ listening
#   4. short version of tests for help-mean ~ listening
#   5. short version of tests for help-mean ~ intimacy

(fit <- lm("intimacy_relHigher.di ~ listen_relMean.di + listen_relDelta.di", data = nsdlong))
confint(fit) 
# results: All significant, mean is strong (1.1)

# collinearity tests
(fit <- lm("intimacy_relHigher.di ~ listen_relMean.di + listen_relDelta.di +
                                      listen_relHigher.di", data = nsdlong))
confint(fit) 
# results: Higher's effect is NA. It is highly correlated with the mean. 
sum(is.na(nsdlong$listen_relHigher.di)) # zero NAs
lm(listen_relHigher.di ~ listen_relMean.di + listen_relDelta.di, data = nsdlong)
cor(nsdlong$listen_relMean.di, nsdlong$listen_relHigher.di) # 0.9

(fit <- lm("intimacy_relHigher.di ~ listen_relLower.di + listen_relHigher.di", data = nsdlong))
confint(fit) 
cor(select(nsdlong, listen_relLower.di, listen_relHigher.di)) #0.65

# results: All significant and substantial
#         Intercept 0.30
#         listen_lower  0.36
#         listen_higher 0.74

# I checked and only about 64% of the people higher in intimacy are the ones higher
# in listening. 
# So, Is it the higher listener that effects the higher intimacy, or is it the listening
# levels of himself or their partner? 
# First we enter the listening dss of the higher on intimacy direction 
# (if intimacy_rel.AB > intimacy_rel.BA, 
#   then listen_intimacyHiger.AB <- listen_rel.AB; listen_intimacyHiger.BA <- listen_rel.BA
#   otherwise, listen_intimacyHiger.AB <- listen_rel.BA; listen_intimacyHiger.BA <- listen_rel.AB)
nsdlong$listen_intimacyHigher.AB <- 
  ifelse(nsdlong$intimacy_ABHigher, nsdlong$listen_rel.AB, nsdlong$listen_rel.BA)
nsdlong$listen_intimacyHigher.BA <- 
  ifelse(nsdlong$intimacy_ABHigher, nsdlong$listen_rel.BA, nsdlong$listen_rel.AB)

# collinearity
(fit <- lm("intimacy_relHigher.di ~ listen_relLower.di + listen_relHigher.di +
                                      listen_intimacyHigher.AB + 
                                      listen_intimacyHigher.BA", data = nsdlong))
confint(fit) 
# results: The contributions of the same person or the partner aren't significant.
#         The higher and lower do stand high
#         Intercept 0.29
#         listen_lower  0.47
#         listen_higher 0.79.
#         The listening ds of the member higher on intimacy is close to significance
#         But the listening ds of the member lower on intimacy results in NA. 
#         what does it mean? Usually that the item is highly correlated (collinearity)
#         The next row tests the correlation with the rest of the items: 
lm("listen_intimacyHigher.BA ~ listen_relLower.di + listen_relHigher.di +
                                       listen_intimacyHigher.AB", data = nsdlong)
#     It shows a coefficient of 1 with both lower and higher DIs and -1 with .BA

(fit <- lm("intimacy_relHigher.di ~ listen_intimacyHigher.AB + 
                                    listen_intimacyHigher.BA", data = nsdlong))
confint(fit) 
# results: without the effects of the higher and lower listeners, then the listening
#         ds of the listening of the same person and their partner, 
#         come up very similarly.
#         This strengthen the understanding that we don't really know the person
#         that is affected by the better unique listening, just that when there
#         is a unique listening levels, then some magic will occur to one of the 
#         dyad members. 
#         I find it fascinating! 
#         
#         Intercept 0.46
#         listen_rel of the person higher on intimacy  0.66
#         listen_rel of the person "lower" on intimacy  0.44

# Same tests for the Intimacy mean as outcome (same results as higher.DI)
(fit <- lm("intimacy_relMean.di ~ listen_relLower.di + listen_relHigher.di", data = nsdlong))
confint(fit) 
(fit <- lm("intimacy_relHigher.di ~ listen_intimacyHigher.AB + 
                                    listen_intimacyHigher.BA", data = nsdlong))
confint(fit) 
(fit <- lm("intimacy_relHigher.di ~ listen_relLower.di + listen_relHigher.di", data = nsdlong))
confint(fit) 

# Same tests for Higher unique help on listening: 

(fit <- lm("help_relHigher.di ~ listen_relMean.di + listen_relDelta.di", data = nsdlong))
confint(fit) 
# results: only the mean is substantial and significant (0.6) CI[0.58, 0.77]

(fit <- lm("help_relHigher.di ~ listen_relLower.di + listen_relHigher.di", data = nsdlong))
confint(fit) 
# resutls: All significant and though weak
#         Intercept 0.36
#         listen_lower  0.27
#         listen_higher 0.40

# I checked and only about 60% of the people higher in help are the ones higher
# in listening. 
# So, Is it the higher listener that effects the higher help, or is it the listening
# levels of himself or their partner? 
# First we enter the listening dss of the higher on help direction 
# (if help_rel.AB > help_rel.BA, 
#   then listen_helpHiger.AB <- listen_rel.AB; listen_helpHiger.BA <- listen_rel.BA
#   otherwise, listen_helpHiger.AB <- listen_rel.BA; listen_helpHiger.BA <- listen_rel.AB)
nsdlong$listen_helpHigher.AB <- 
  ifelse(nsdlong$help_ABHigher, nsdlong$listen_rel.AB, nsdlong$listen_rel.BA)
nsdlong$listen_helpHigher.BA <- 
  ifelse(nsdlong$help_ABHigher, nsdlong$listen_rel.BA, nsdlong$listen_rel.AB)

(fit <- lm("help_relHigher.di ~ listen_helpHigher.AB + 
                                    listen_helpHigher.BA", 
           data = nsdlong))
confint(fit) 
# resutls: without the effects of the higher and lower listeners, then the listening
#         ds of the listening of the same person and their partner, 
#         come up very similarly.
#         This strengthen the understanding that we don't really know the person
#         that is affected by the better unique listening, just that when there
#         is a unique listening levels, then some magic will occur to one of the 
#         dyad members. 
#         I find it fascinating! 
#         
#         Intercept 0.42
#         listen_rel of the person higher on help  0.38
#         listen_rel of the person "lower" on help  0.29

# Same for unique help on unique intimacy

(fit <- lm("help_relHigher.di ~ intimacy_relMean.di + intimacy_relDelta.di", data = nsdlong))
confint(fit) 
# results: All is significant. mean is 0.56, delta 0.12

(fit <- lm("help_relHigher.di ~ intimacy_relLower.di + intimacy_relHigher.di", data = nsdlong))
confint(fit) 
# resutls: All significant though weak
#         Intercept 0.31
#         intimacy_lower  0.17
#         intimacy_higher 0.40

# I checked and only about 60% of the people higher in help are the ones higher
# in intimacy. 
# So, Is it the higher intimacy feeling person that effects the higher help, 
# or is it the intimacy levels of themselves or of their partner? 
# First we enter the intimacy dss of the higher on help direction 
# (if help_rel.AB > help_rel.BA, 
#   then intimacy_helpHiger.AB <- intimacy_rel.AB; intimacy_helpHiger.BA <- intimacy_rel.BA
#   otherwise, intimacy_helpHiger.AB <- intimacy_rel.BA; intimacy_helpHiger.BA <- intimacy_rel.AB)
nsdlong$intimacy_helpHigher.AB <- 
  ifelse(nsdlong$help_ABHigher, nsdlong$intimacy_rel.AB, nsdlong$intimacy_rel.BA)
nsdlong$intimacy_helpHigher.BA <- 
  ifelse(nsdlong$help_ABHigher, nsdlong$intimacy_rel.BA, nsdlong$intimacy_rel.AB)

(fit <- lm("help_relHigher.di ~ intimacy_helpHigher.AB + 
                                    intimacy_helpHigher.BA", data = nsdlong))
confint(fit) 
# resutls: without the effects of the higher and lower intimacy, 
#         then the intimacy ds of the helper and their partner are significant.
#         But the intimacy of the same person, counts more. 
#         So, unlike in listening-intimacy, in intimacy-help, the way the person
#         feels drives the unique help, above the general level of intimacy in the dyad. 
#         
#         Intercept 0.39
#         intimacy_rel of the person higher on help  0.37
#         intimacy_rel of the person "lower" on help  0.19

