rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# Define Individuals' indices based on dyadic scores
# 
# The variance of dyadic scores per participant: 
# 1. For each participant calculate the variance of their dyadic scores
#    Both as perceiver and as target. 
# 2. Check if there is a variance between the participants' variances. 
#    There is a variance for all the constructs. 
#    If I run it on the scaled pw, I get much higher results. 
#    This suggests that for every construct, some participants view/are viewed 
#    uniquely whereas some are viewed/view according to their norms. 
# 3. Check if there is a personal tendency to perceive others as uniquely different, 
#    and if there is a personal tendency to be perceived as uniquely different. 
#    We checking the variance of participants across different constructs. 
#    The results are more yes than no.
# 4. Check if there is a correlation between unique perceivers and unique targets. 
#    The answer is not really.  
#    
# 5. All the above is only relevant when there is a significant dyadic variance, 
#    Meaning, it's not relevant for the single-item constructs of shy and non-talkative. 
#    This is because these scores are confounded with the error.
# 
# 6. Prepare all also for a demo dataset with humility and creativity only. 
# 

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))
# load(imgName(".FullRR.ReadyforAPIM")) # This is wrong to use because the ratings are scaled
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))
# ipak("sjPlot")

pw <- nonScaledPW # Choose the right scaling option (nothing is scaled) 
demo <- select(pw, all_of(idCols), starts_with("Hum_rel"), starts_with("Creat_rel")) %>% 
  rename(t.id = p.id, p.id = a.id) %>% arrange(p.id)

# collect the names of the dyadic scores (ds)
ds <- grep("_rel", names(pw), value = TRUE)
ds.demo <- grep("_rel", names(demo), value = TRUE)

# for each participant we calculate the variance of the DSs as a perceiver/target:
dsVars <- pw |> group_by(a.id) |> 
  summarise(across(all_of(ds), \(a) var(a, na.rm = TRUE)))
demo.dsVars <- demo |> group_by(p.id) |> 
  summarise(across(all_of(ds.demo), \(a) round(var(a, na.rm = TRUE), 6))) %>% 
  #rename(id = p.id) %>% 
  setNames(gsub("rel.AB", "Perc.Var", names(.))) %>% 
  setNames(gsub("rel.BA", "Targ.Var", names(.)))
  
  describe(demo.dsVars[,-1])
  # Save an image for further analysis using cilantro and perceiver variance.
  # cilantro <- select(dsVars, a.id, ends_with(".BA"), -starts_with("Shy"), -starts_with("Non"))
  # names(cilantro) <- gsub("_rel.BA", "_cilant.A", names(cilantro))
  # cilantro$trait_cilant.A <- sqrt(rowMeans(select(cilantro, -a.id), na.rm = TRUE))
  # save(cilantro, file = imgName("cilantro"))
names(demo.dsVars) <- c("id", "Humility connoisseur", "Humility Cilantro", 
                        "Creativity connoisseur", "Creativity Cilantro")
  
# let's look at the variance of the variances to see if there is a real difference
# between different people in how they perceive others on the different constructs: 
# we see a substantial variance ranging between 2.7 and 5.6.
rname <- c("Uniqueness effect", "Variance")
t(dsVars |> select(-a.id) |> 
    summarise(across(everything(), \(a) var(a, na.rm = TRUE))) |> 
    summarise(across(everything(), \(a) round(a, digits = 1))))
demo.dsVars |> select(-id) |> 
  summarise(across(everything(), \(a) var(a, na.rm = TRUE))) |> 
  summarise(across(everything(), \(a) round(a, digits = 1))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% 
  setNames(c("Uniqueness effect", "   Variance"))

# Variance "reciprocity"
# =======================
# Do people who tend to view others uniquely, also tend to be viewed uniquley?
# Result: Not really.
#  
# for each construct: check the correlation between people's variance-target and 
# variance-perceiver.
# The reason for this test is that at the construct level there is no difference 
# in the variances of target and perceiver. But at the individuals' level, the 
# cor are quite low. 
# RESULTS: people who tend to see others uniquely don't necessarily tend to be
# viewed uniquely. A little more so for Humility (0.43), but quite low for all
# the rest of the constructs.
ptVarsCor <- diag(cor(select(dsVars, ends_with(".AB")), 
                      select(dsVars, ends_with(".BA"))))
names(ptVarsCor) <- varnames
(ptVarsCor <- round(ptVarsCor, 2))

# Variances correlations
# =======================
# Look at the correlations of all the variances ()
VarsCor <- cor(select(dsVars, -a.id, -starts_with("Shy"), -starts_with("Non")))
VarsCor <- as.data.frame(round(VarsCor, 2))
(VarsCor <- select(VarsCor, ends_with(".AB")))
names(VarsCor) <- gsub("rel.AB", "Perceiver Variance", names(VarsCor))
rownames(VarsCor)<- gsub("rel.AB", "Perceiver Variance", rownames(VarsCor))
rownames(VarsCor)<- gsub("rel.BA", "Target Variance", rownames(VarsCor))


# can we see a pattern of perceiving and being perceived? 
library(psych)
# do people tend to see others as similar across constructs? 
# alpha of 0.57 (but shy and nontalkative drop the alpha)
alpha(select(dsVars, ends_with(".AB")))

# do people tend to see others as similar across constructs if we drop introvert? 
# alpha of 0.72 CI [0.65, 0.79]
alpha(select(dsVars, ends_with(".AB"), -starts_with("shy"), -starts_with("non")))

# do people tend to be seen by others as similar across constructs? 
# alpha of 0.59
alpha(select(dsVars, ends_with(".BA")))

# do people tend to be seen by others as similar across constructs if we drop introvert? 
# alpha of 0.73 CI [0.66, 0.8]
alpha(select(dsVars, ends_with(".BA"), -starts_with("shy"), -starts_with("non")))

prDsVars <- mutate(dsVars, across(.cols = -a.id, round, digits = 3)) |> 
  mutate(across(a.id, as.character))
# maybe later I will invest in preparing an export for the tables.
write.csv(prDsVars, fname("dsVars"))
describe(select(dsVars,-a.id))

# Look specifically at cilantro effects
load(imgName("cilantro"))
cn <- cilantro
varnames <- varnames[1:4] # drop the single items variables

# Check if the target score is negatively correlated with the cilantro effect
cn <- left_join(cn, select(pw, a.id, ends_with("target.A"), 
                           ends_with("perc.A")), by = "a.id") |> distinct()
pt_cilantro <- data.frame(v = varnames, t_cilant = NA, p_cilant = NA)
for (i in varnames) {
  tv <- cn[,paste0(i, "_target.A")]
  pv <- cn[,paste0(i, "_perc.A")]
  cv <- cn[,paste0(i, "_cilant.A")]
  pt_cilantro[pt_cilantro$v == i, "t_cilant"] <- cor(tv,cv)
  pt_cilantro[pt_cilantro$v == i, "p_cilant"] <- cor(pv,cv)
}
pt_cilantro
