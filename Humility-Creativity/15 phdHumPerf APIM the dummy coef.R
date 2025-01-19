rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# Dummy codes coefficients
# ============================================
# In this file I follow my wonder about the meaning of the dummy codes coefs. 
# Conclusion/Results:
# When predicting dss, the dummy codes all have coefficient zero. 
# But the evaluation of the model changes. 
# I thought it would affect the degrees of freedom, but it doesn't. Yet, 
# the evaluations of the model improve (or perhaps just change).
# When predicting the raw dyadic ratings, then each perceiver/target has their
# own fixed effect. 
# 
# The thinking process...
# =======================
# The dummy codes represent each perceiver and target, 
# They are represented as fixed effects, because there are dummy codes 
# per actual perceiver and target. 
# Hence, I wonder, if they hold some potential dyadic level information about 
# the perceivers and targets, mainly around the individual variances (cilantro)? 
# 
# What does it mean to have a specific person's effect on the ds of another 
# construct? 
# the dss of Joe on humility will have a linear effect on his dss on creativity. 
# There shouldn't be any regression coef for the dummy codes. If there were, it 
# means there is a joined portion of the dss of a specific perc/target, but this 
# portion should have gone to the perceiver/target scores and not remain in dss. 
# Therefore, the coefs should all equal zero. Which indeed happens. 
# So, what is influenced? Is it an adjustment of the model's quality? 
# In his book (2006, p. 210),Kenny says we can prepare a pairwise dataset, and can
# immediately perform an APIM analysis. He adds there's a minor complication of the
# degrees of freedom: for each group, we lose (n-1) degrees of freedom because 
# we dropped the perceivers, and (n-1) dfs for dropping the targets. The solution
# is one of two: 1. create 2(n-1) dummy codes per group. 2. treat the perceivers
# and targets as categorical variables, and control for their effects in the 
# analysis. The latter doesn't make sense, because, at least in gls, it's just 
# like the dummy codes, only it creates a singularity problem that we solved 
# for the dummy codes. 
# I think the effects should always be zero. True. 
# The only change is the evaluation of the model (I think it improves it).
# The coef of the dummy codes, therefore, should not be zero when the variables
# aren't dss. 

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak(c("TripleR", "nlme", "tidyverse"))  

pw <-nonScaledPW    # Verify I'm working on the correct scaling option

# The dummy codes procedure: 
# create a dummy code for each perceiver and target, 
# remove one from each.
# remove one target for each group. 
# A total of: (n-1)+(n-k-1) dummy codes. Not as written in the JAP paper where it's 
# written we have (n-1)^2 codes. 
 
# short analysis
# ============================================

f <- APIMformulaShow("Creat", "Hum", runShowAPIM = FALSE)
# "Creat_rel.AB ~ Hum_rel.AB  + Hum_rel.BA + aDum + pDum"
# fit <- APIMformulaShow("Creat", "Hum")
fit <- gls(as.formula(f),
           na.action = na.omit, verbose = TRUE, method = "ML",
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw))

coefs <- as.data.frame(fit$coefficients)
names(coefs)[1] <- "coef"
coefs$coef <- round(coefs$coef, 8) # all dummy-codes coefs are zero
anova(fit)

fitNoDummies <- gls(Creat_rel.AB ~ Hum_rel.AB  + Hum_rel.BA,
                    na.action = na.omit, verbose = TRUE, method = "ML",
                    correlation = corCompSymm (form = ~1|dyad),
                    data = cbind(pw))
fitNoDummies

# comparing the results of with/without dummy codes analysis: 
#   The coefficients of the dss of humility stay the same.
#   The intercept changes (because the dummies are fixed effects, so they impact
#   the intercept but not the slopes).
#                               No dummies    with dummies
#   Log-restricted-likelihood:  -539.8433     -426.4349
#   Rho                         -0.07291148   0.09301945
#   DF-total                    424           424
#   DF-residual                 421           189
#   Residual SE                 0.8552407     1.286585
anova(fit, fitNoDummies)

# the following doesn't converge because the actor and partner form singularity
fitCategorical <- gls(Creat_rel.AB ~ Hum_rel.AB  + Hum_rel.BA + a.id + p.id,
                      na.action = na.omit, verbose = TRUE,
                      correlation = corCompSymm (form = ~1|dyad),
                      data = cbind(pw))


# Test the same for an outcome that isn't decomposed, and therefore, might
# be correlated with a perceiver/target
fit <- gls(Creat_raw.AB ~ Hum_rel.AB  + Hum_rel.BA + aDum + pDum,
           na.action = na.omit, verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw))
fitNoDummies <- gls(Creat_raw.AB ~ Hum_rel.AB  + Hum_rel.BA,
                    na.action = na.omit, verbose = TRUE,
                    correlation = corCompSymm (form = ~1|dyad),
                    data = cbind(pw))
coefs <- as.data.frame(fit$coefficients)
names(coefs)[1] <- "coef"
coefs$coef <- round(coefs$coef, 8) # dummy-codes coefs are NOT zero
fit;fitNoDummies
# comparing the results of with/without dummy codes analysis: 
#   The coefficients of the dss of humility stay the same.
#   The intercept changes (because the dummies are fixed effects, so they impact
#   the intercept but not the slopes).
#                               No dummies    with dummies
#   Log-restricted-likelihood:  -928.9099     -434.3797
#   Rho                         0.1089466     0.1715374
#   DF-total                    424           424
#   DF-residual                 421           189
#   Residual SE                 2.159001      1.357977

# using the adjusted APIM for raw ratings 
(fit <- gls(Creat_raw.AB ~ Hum_raw.AB  + Hum_raw.BA + aDum + pDum,
           na.action = na.omit, verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw)))
# The empty model
(fit0 <- gls(Creat_raw.AB ~ aDum + pDum,
           na.action = na.omit, verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw)))
anova(fit0) # we see the F-test for the dummy codes is significant, suggesting
            # we expect actor/partner variances.
# Here, we're supposed to test the interaction between the raw ratings and perceiver/target
# but, it doesn't convert.
(fit <- gls(Creat_raw.AB ~ Hum_raw.AB:a.id + Hum_raw.AB + Hum_raw.BA + aDum + pDum,
           na.action = na.omit, verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw)))
anova(fit0) # We see the DFs are ok, 
fit <- gls(Creat_raw.AB ~ Hum_raw.AB:a.id + Hum_raw.BA:p.id,
           na.action = na.omit, verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw))
fit <- gls(Creat_raw.AB ~ Hum_raw.AB + Hum_raw.AB:p.id + Hum_raw.BA + aDum + pDum,
           na.action = na.omit, verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = cbind(pw))
fitNoDummies <- gls(Creat_raw.AB ~ Hum_raw.AB  + Hum_raw.BA,
                    na.action = na.omit, verbose = TRUE,
                    correlation = corCompSymm (form = ~1|dyad),
                    data = cbind(pw))
coefs <- as.data.frame(fit$coefficients)
names(coefs)[1] <- "coef"
coefs$coef <- round(coefs$coef, 8) # dummy-codes coefs are definitely not zero
fit;fitNoDummies
# comparing the results of with/without dummy codes analysis: 
#   The coefficients of the dss of humility stay the same.
#   The intercept changes (because the dummies are fixed effects, so they impact
#   the intercept but not the slopes).
#                               No dummies    with dummies
#   Log-restricted-likelihood:  -880.7135     -430.1525
#   Rho                         -0.0713969    0.1303102
#   DF-total                    424           424
#   DF-residual                 421           189
#   Residual SE                 1.915021      1.318753

