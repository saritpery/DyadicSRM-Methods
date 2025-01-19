rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file:
# ==============================================================================
# 1. An Equivalent of the previous file, where I ran the analyses of the 2018 
#    article of Kenny about dsAPIM. 
#    In this file I run the dsAPIM analysis on the Humility-performance decomposed
#    data, with a focus on the empty model, and interactions tests for the 
#    individual level items (dyadic indices that probably have some interaction 
#    with the dummy codes),
# 2. I test the adjusted Rho significance test (APIM dyadic reciprocity of the DV)
#    using my equations that don't require uniform group size or full data.

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak(c("TripleR", "tidyverse", "nlme"))  

pw <- nonScaledPW   # Verify I'm working on the correct scaling option

# The basic analysis is: 
fit <- APIMformulaShow("Creat", "Hum")
fit <- gls(Creat_rel.AB ~ Hum_rel.AB  + Hum_rel.BA + aDum + pDum,
           na.action = na.omit, method = "REML", verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = pw)
# and then we would like to explore the addition of the individual-based indices:
fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
# Results: 0.21, 0.36, 0.22 all significant.
(fitHCreativity <- RR(H1/H2 + Creat1/Creat2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE))

# The empty model
# ==============================================================================
  # * look at the empty model and what we can learn from it.
  # * Explain the F values of the dummies 
  # * Play with the empty model (the form, empty model for attractive, the coefficients)
  fit0 <- gls(Creat_rel.AB ~ aDum + pDum,
           na.action = na.omit, method = "REML", verbose = TRUE,
           correlation = corCompSymm (form = ~1|dyad),
           data = pw)  

  all(round(summary(fit0)$coefficients,2)==0) # all are zero of course
  # Rho: I'm not sure what the rho stands for in the case of the empty model 
  # with dss. Rho = 0.2355636. 
  # It's the correlation between the ds of the dyad members after controlling 
  # for the perceiver/target dummy codes. Hence, very similar to the SRM dyadic
  # reciprocity. The SRM dyadic reciprocity is 0.248, so very close.
  # when I take a single row for each dyad, and make a cov between the creat_rel.AB and .BA
  # I get 0.2179745, divide with the relationship reliability (0.913): 
  # 0.2387453.
  # when I take a single row for each dyad, and make a cor between the creat_rel.AB and .BA
  # I get 0.223, divide with the relationship reliability: 0.251
  coef(fit0$model$corStruct,unconstrained=FALSE) 
  summary(fit0) # DF  424 total; 190 residual
  anova(fit0)   # DF as expected (Pdum 102; Adum 132). 
                # F-values of aDum and pDum are zero with no significance.
                # This is expected because these are ds that are already 
                # controlled for the perceiver/target. 
  # Explanation to the dummy codes F values in the empty model from 2018 article:
  # ============================================================================
  # Summary: 
  # The F-values's significance suggest the existence of perceiver/target SRM
  # variance in the data. 
  # When the data is dss, then no such variance is expected. 
  # When the data is ratings, then usually we expect perceiver/target variances.
  # 

# The complete model
# ==============================================================================

  # The model with only dss doesn't have any issues with anything.
  # I want to explore the addition of DIs that might interact with the 
  # perceiver/target.  
  fit <- gls(Creat_rel.AB ~ Hum_rel.AB  + Hum_rel.BA + 
               Hum_target.A:Hum_target.B + aDum + pDum,
             na.action = na.omit, method = "REML", verbose = TRUE,
             correlation = corCompSymm (form = ~1|dyad),
             data = pw)
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
  # coef: 0.21, 0.36, 0.22 all significant.
  # with REML the significance drops a little, but still significant (0.05 for the DI)
  anova(fit)   # DF as expected (132, 101)
               # All F values of the predictors are significant
               # no actor/partner dummy codes significance which is great!
  coef(fit$model$corStruct,unconstrained=FALSE) # Rho -0.099 = APIM dyadic reciprocity
  # it means that the correlation of dyadic creativity after controlling for
  # humility items is ~-0.1
  summary(fit)$sigma   # sd of residual 0.84
  
  # test the significance of the Rho: 
  # F, rho, dyadic reciprocity equations (un-even groups with missing values)
  # =========================================================================
  # In the case of missing values, 
  #   the calculation of degrees of freedom might be a bit biased 
  #   according to the selection of dyads.
  #   Assuming the missing values are of random pattern, the calculation is acceptable.
  # 
  # The calculation of degrees of freedom:
  # for each group: remove one actor and two partners, calculate no. of dyads
  # f.dfd = The degrees of freedom of dyads 
  #         after accounting for perceivers and targets:
  #         the number of dyads after removing a participant and a partner from 
  #         each group.
  # f.dfn = The degrees of freedom after accounting for the number of groups = 
  #       =  f.dfd - number of groups
  
  f.dfd <- pw %>%
    group_by(group.id) %>%
    # Randomly select one ActorN to filter out associated rows as actor/partner
    mutate(ap_to_remove = sample(unique(a.id), 1)) %>%
    filter(!a.id %in% ap_to_remove, !p.id %in% ap_to_remove) %>%
    # Randomly select an additional PartnerN to filter out associated rows
    filter(!p.id %in% sample(unique(p.id), 1)) %>%
    ungroup() %>%
    # Count unique Dyads
    summarise(distinct_dyads = n_distinct(dyad)) %>%
    pull(distinct_dyads)
  
  ng <- length(unique(pw$group.id)) #number of groups
  f.dfn <- f.dfd-ng
  
  # calculate the quantile using the rho of the model:
  # The rho is the nonindepencdence between the DV of the dyad members after
  # controlling for the individual tendencies (dummy codes) and the predictors
  rho <- as.numeric(coef(fit$model$corStruct,unconstrained=FALSE)) #rho = -0.1
  (F <- (1+rho)/(1-rho))
  
  # The probability to get the F given our degrees of freedom:
  plow <- pf(F,f.dfn,f.dfd)
  
  # The significance of the model:
  2 * min(plow, 1 - plow) 
  cat("The rho of the model is: ", round(rho, 2), 
      "\nIt represents the dependency between the dyad member's DV",
      "\n after controlling for the APIM predictors and the individual tendencies.",
      "\nThe significance of the rho is: ", 2 * min(plow, 1 - plow) )
  # The rho isn't significant (p=0.35). The creativity SRM dyadic reciprocity 
  # isn't significant as well. 
  
  # Try a similar model w/o the DI:
  fit2 <- gls(Creat_rel.AB ~ Hum_rel.AB  + Hum_rel.BA + aDum + pDum,
             na.action = na.omit, method = "REML", verbose = TRUE,
             correlation = corCompSymm (form = ~1|dyad),
             data = pw)
  (rho <- as.numeric(coef(fit2$model$corStruct,unconstrained=FALSE)))
  # rho is 0.096
  (F <- (1+rho)/(1-rho))
  # The probability to get the F given our degrees of freedom:
  plow <- pf(F,f.dfn,f.dfd)
  # The significance of the model:
  2 * min(plow, 1 - plow)  
  # The rho isn't significant (p=0.35). The creativity SRM dyadic reciprocity 
  # isn't significant as well. 
  cat("The rho of the model is: ", round(rho, 2), 
      "\nIt represents the dependency between the dyad member's DV",
      "\n after controlling for the APIM predictors and the individual tendencies.",
      "\nThe significance of the rho is: ", 2 * min(plow, 1 - plow) )
  
  # Try a similar model w/o the DI:
  fit3 <- gls(Safety_rel.AB ~ Hum_rel.AB  + Hum_rel.BA + aDum + pDum,
              na.action = na.omit, method = "REML", verbose = TRUE,
              correlation = corCompSymm (form = ~1|dyad),
              data = pw)
  (rho <- as.numeric(coef(fit3$model$corStruct,unconstrained=FALSE)))
  # rho is 0.25
  (F <- (1+rho)/(1-rho))
  # The probability to get the F given our degrees of freedom:
  plow <- pf(F,f.dfn,f.dfd)
  # The significance of the model:
  2 * min(plow, 1 - plow)
  cat("The rho of this APIM is: ", round(rho, 2), 
      "\nIt represents the dependency between the dyad member's DV",
      "\n after controlling for the APIM predictors and the individual tendencies.",
      "\nThe significance of the rho is: ", 2 * min(plow, 1 - plow) )
  # So, for this construct, the correlation between the dyad members is 
  # significant after controlling for humility.
  (fitHSafety <- RR(H1/H2 + Sf1/Sf2     ~ a.id*p.id | group.id, data = x, na.rm = TRUE))
  # The SRM dyadic reciprocity for safety is 0.36 (p=0.051)
  # and the interpersonal bivariate cov is 0.343, so there is a correlation
  # between dyadic humility and safety. It makes sense.
   
  # Try a similar model w/o the DI:
  fit4 <- showAPIM("Safety_rel.AB ~ Creat_rel.AB + Creat_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "REML")
  
  (rho <- as.numeric(coef(fit4$model$corStruct,unconstrained=FALSE)))
  # rho is 0.23
  (F <- (1+rho)/(1-rho))
  # The probability to get the F given our degrees of freedom:
  plow <- pf(F,f.dfn,f.dfd)
  # The significance of the model:
  2 * min(plow, 1 - plow)
  cat("The rho of this APIM is:", round(rho, 2), 
      "\nIt represents the dependency between the dyad member's DV",
      "\n   after controlling for the APIM predictors and the individual tendencies.",
      "\nThe significance of the rho is: p =", round(2 * min(plow, 1 - plow),3) )
  # So, for Safety, the correlation between the dyad members is 
  # significant after controlling for creativity and humility...
  
  
  # Interaction test
  # ==============================================================================
  # The ds of course don't interact with the dummy codes.
  # However, the DI might.
  fit4 <- showAPIM("Safety_rel.AB ~ Creat_rel.AB + Creat_rel.BA + 
                Hum_target.A:Hum_target.B + aDum + pDum")
  pw$HumTT.prod <- pw$Hum_target.A*pw$Hum_target.B
  # we put the same analysis with a dyadic index:
  fit4 <- showAPIM("Safety_rel.AB ~ Creat_rel.AB + Creat_rel.BA + 
                HumTT.prod + aDum + pDum")
  # And now we can test the interaction.
  # A significant strong interaction would suggest that a substantial
  # portion of the effect of the item is hidden in the dummy codes' 
  # coefficients, and it might be to the extent that the analysis isn't to 
  # be trusted.
  # However, since most of the items have no interaction with the perceiver/target,
  # then the analysis will be legit most of the times.
  # for most interactions the model is singular.
  # As is for this case.
  # None of the interactions converges
  fit4i <- gls(Safety_rel.AB ~ Creat_rel.AB + HumTT.prod:aDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = pw)
  # Don't run the following, it takes forever and doesn't converge...
  # fit4i <- gls(Safety_rel.AB ~ Creat_rel.AB + HumTT.prod:aDum:pDum,
  #              na.action = na.omit, method = "REML", verbose = TRUE,
  #              correlation = corCompSymm (form = ~1|dyad),
  #              data = pw)
  
  fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  HumTT.prod + aDum + pDum")
  # all interactions are singular:
  fit1i <- gls(Creat_rel.BA ~ Hum_rel.AB + HumTT.prod:pDum ,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = pw)
  # what if i test it without the dyadic nesting? 
  # In this case the analysis by itself isn't in the right level of 
  # analysis (should be done at dyad level and not directional dyad level).
  # But can it teach me about the contribution of the interaction to the 
  # explained variance?
  fit1i.lm <- lm(Creat_rel.BA ~ HumTT.prod*pDum + aDum, pw)
  fit1.0.lm <- lm(Creat_rel.BA ~ HumTT.prod + aDum + pDum, pw)
  summary(fit1.0.lm)$sigma^2 # residual variance = 1.907 
  summary(fit1i.lm)$sigma^2  # residual variance with interaction = 1.73
  