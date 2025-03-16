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
#    Meaning, it's not relevant for single-item constructs.
#    This is because these scores are confounded with the error.
# 
# 6. Prepare all also for a demo dataset with humility and creativity only. 
# 

load("main.RImage")
load(imgName(".ReadyforAPIM"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))

# collect the names of the dyadic scores (ds)
ds <- grep("_rel", names(pw), value = TRUE)

# for each participant we calculate the variance of the DSs as a perceiver/target:
dsVars <- pw %>%  group_by(p.id) %>%  
  summarise(across(all_of(ds), \(a) var(a, na.rm = TRUE))) %>% 
  #rename(id = p.id) %>% 
  setNames(gsub("rel.AB", "Perc.Var", names(.))) %>% 
  setNames(gsub("rel.BA", "Targ.Var", names(.)))
  
# let's look at the variance of the variances to see if there is a real difference
# between different people in how they perceive others on the different constructs: 
# we see a substantial variance ranging between 2.7 and 5.6.
rname <- c("Uniqueness effect", "Variance")
t(dsVars %>%  select(-p.id) %>% 
    summarise(across(everything(), \(a) var(a, na.rm = TRUE))) %>% 
    summarise(across(everything(), \(a) round(a, digits = 1))))
