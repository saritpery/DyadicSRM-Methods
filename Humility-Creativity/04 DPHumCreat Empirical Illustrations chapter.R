rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# 1. Verify terminology is of perception (perceiver-target) and not behavior.
# 2. Gather all illustrations output in a single location.


load("main.RImage")
load(imgName(".ReadyforAPIM"))

ipak (c("psych", "tidyverse", "nlme"))

# Preparing the data ===========================================================
  # Individual level
  ind <- left_join(ptEffects, select(x, id=p.id, Age, HSelf), by = "id") %>% 
  distinct() %>% 
  mutate(Self_Humility = round(HSelf, 2)) %>% 
  select(id, group.id, starts_with("Creat"), starts_with("Hum"), Age, Self_Humility)

# Dyadic level
dyads <- select(pw, all_of(idCols), starts_with("Creat"), starts_with("Hum")) %>% 
  mutate(di_Hum_ds.mean = rowMeans(select(., Hum_rel.AB, Hum_rel.BA), na.rm = TRUE), 
         di_Creat_ds.gap = abs(Creat_rel.AB - Creat_rel.BA), 
         di_Hum_target.product = Hum_target.A * Hum_target.B, 
         di_Creat_perc.product = Creat_perc.A * Creat_perc.B, 
         di_Hum_target.mean = (Hum_target.A + Hum_target.B)/2 ) %>% 
  mutate(across(starts_with("di_"), ~round(.x, digits = 3))) %>% 
  left_join(select(ind, p.id = id, Age.A = Age, selfHum.A = Self_Humility), by = "p.id") %>% 
  left_join(select(ind, t.id = id, Age.B = Age, selfHum.B = Self_Humility), by = "t.id") %>% 
  mutate(di_Age.mean = rowMeans(select(., Age.A, Age.B), na.rm = TRUE),
         di_Age.gap = abs(Age.A - Age.B), 
         di_SelfHum.mean = rowMeans(select(., selfHum.A, selfHum.B), na.rm = TRUE),
         di_SelfHum.gap = abs(selfHum.A - selfHum.B)) %>% 
  select(group.id, dyad, starts_with("di_")) %>% 
  group_by(dyad) %>% filter(row_number()==1) %>% ungroup %>% 
  setNames(gsub("di_", "", names(.))) %>% 
  select(group.id, dyad, starts_with("Creat"), starts_with("Hum"), everything())
  
# Directional dyadic level: pairwise 2-per-dyad entry
  # Refine the dummy codes names for display purposes
  a <- attr(pw$pDum, "dimnames")[2][[1]]
  attr(pw$pDum, "dimnames")[2][[1]] <- gsub("pw\\$a\\.id", "pd", a)
  a <- attr(pw$tDum, "dimnames")[2][[1]]
  attr(pw$tDum, "dimnames")[2][[1]] <- gsub("pw\\$p\\.id", "td", a)
  
  # Chose the relavant columns from pw for the demonstration
  dPW <- pw %>% 
    select(all_of(idCols), starts_with("Creat_rel"), starts_with("Hum_rel"),
           matches("Hum_target.(A|B)"), tDum, pDum, Creat_raw.AB, starts_with("Safety_rel")) 
    
# Directional dyadic level: pw 1-per-dyad entry
oneDyPW <- dPW %>% 
  # keep a single row per dyad
  group_by(dyad) %>% filter(row_number()==1) %>% ungroup %>% 
  # keep only relevant columns
  select(group.id, A.id=p.id, B.id=t.id, dyad, everything())

# dsAPIM implementation ========================================================
  # general demo of code 
  # The formula:  Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + pDum + tDum
  # The gls code: 

  fit <- gls(Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA + pDum + tDum,
             na.action = na.omit, method = "REML", verbose = TRUE,
             correlation = corCompSymm (form = ~1|dyad),
             data = pw)

  # Eq. (4) in methodology, Eq. 13 in illustrations.
  fit0 <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                  pDum + tDum", mlMethod = "ML", dataset = dPW)
  # Results: 0.16, 0.31 all significant.

  # Eq. (5) in methodology, Eq. 14 in illustrations.
  fit1 <- showAPIM("Creat_raw.AB ~ Hum_rel.AB + Hum_rel.BA + 
                   pDum + tDum", mlMethod = "ML", dataset = dPW)
  # The coefficients are the same as the ones of the prediction of uniqueness, 
  # so it's probably ok because the dyadic hum mostly affects only the dyadic creativity.
  fit0 <- showAPIM("Creat_raw.AB ~ Hum_rel.AB + Hum_rel.BA + 
                    Hum_rel.AB:pDum + Hum_rel.BA:tDum", mlMethod = "ML", dataset = dPW)
  anova(fit0) # and we can see the interaction isn't significant.

  # Eq. (6) in methodology, Eq. 15 in illustrations.
  fit0 <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                  Safety_rel.AB + Safety_rel.BA + 
                  pDum + tDum", mlMethod = "ML", dataset = dPW)

  # Eq. (7) in methodology, Eq. 16 in illustrations. 
  # Create Target product in dPW.
  dPW <- dPW %>% mutate(Hum_target.product = Hum_target.A * Hum_target.B) %>% 
    relocate(Hum_target.product, .before = any_of("tDum"))
  
  fit0 <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                  Hum_target.product +
                  pDum + tDum", mlMethod = "REML", dataset = dPW)
  
  # Eq. (8) in methodology, Eq. 17 in illustrations. 

  dPW <- dPW %>% 
    left_join(select(dyads, dyad, "SelfHum.gap", "Age.mean"), by = "dyad") %>% 
    relocate(any_of(c("pDum", "tDum")), .after = last_col()) %>% 
    relocate(contains("target"), .after = last_col())
  
  fit0 <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                  SelfHum.gap + Age.mean +
                  pDum + tDum", mlMethod = "REML", dataset = dPW)
  
  
  
   # Eq. (12) in methodology, Eq. 19 in illustrations. 
  # Create other dyadic indices
  
  dPW <- dPW %>% left_join(select(dyads, dyad, "Hum_target.mean"), by = "dyad") %>% 
    relocate(any_of(c("pDum", "tDum")), .after = last_col())
  
  fit0 <- showAPIM("Creat_raw.AB ~ Hum_rel.AB + Hum_rel.BA +
                   Hum_target.mean ", mlMethod = "ML", dataset = dPW)
  
  cor(dPW$Creat_raw.AB, dPW$Hum_target.mean)
  cor(dPW$Creat_raw.AB, dPW$Hum_rel.AB)
  lm(Creat_raw.AB ~ Hum_rel.AB + Hum_rel.BA + Hum_target.mean, data = dPW)
  gls(Creat_raw.AB ~ Hum_rel.AB + Hum_rel.BA + Hum_target.mean + pDum + tDum,
      na.action = na.omit, method = "REML", verbose = TRUE,
      correlation = corCompSymm (form = ~1|dyad),
      data = dPW)
  
  fitTest <- showAPIM("Creat_raw.AB ~ Hum_rel.AB + Hum_rel.BA +
                   Hum_target.mean:tDum", mlMethod = "ML", dataset = dPW)
  anova(fitTest)
   # Eq. (13) in methodology, Eq. 20 in illustrations. 
  # Interaction
  
  dPW <- dPW %>% left_join(select(dyads, dyad, "Hum_target.mean"), by = "dyad") %>% 
    relocate(any_of(c("pDum", "tDum")), .after = last_col())
  
  fit0 <- showAPIM("Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                   Hum_target.product:SelfHum.gap  + pDum + tDum", mlMethod = "ML", dataset = dPW)
ַ  
  
  