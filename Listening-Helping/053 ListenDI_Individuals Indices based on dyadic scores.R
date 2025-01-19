rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# Define Individual Indices based on dyadic scores
# 
# The variance of dyadic scores per participant: 
# 1. For each participant calculate the variance of their dyadic scores
#    Both as perceivers and as targets. 
# 2. Check if there is a variance between the participants' variances. 
#    There is a variance for all the constructs. 
# 3. Check if there is a personal tendency to perceive others as uniquely different, 
#    and if there is a personal tendency to be perceived as uniquely different. 
#    The results are more yes than no, especially if we drop the shy and non-talkative. 
#    This might be due to the fact that these scores are confounded with the error 
#    
load("main.RImage")
# load(imgName("Ready4APIM2")) # This is wrong to use because the ratings are scaled
load(imgName("StudyPairWise"))
ipak (c("psych", "tidyverse"))

# collect the names of the dyadic scores (ds)
ds <- grep("_rel", names(pw), value = TRUE)

# for each participant we calculate the variance of the DSs as a perceiver/target:
dsVars <- pw |> group_by(a.id) |> 
    summarise(across(all_of(ds), var, na.rm = TRUE))

# let's look at the variance of the variances to see if there is a real difference
# between different people in how they perceive others on the different constructs: 
# we see a substantial variance ranging between 2.7 and 5.6.
t(dsVars |> select(-a.id) |> summarise(across(everything(), var)) |> 
      summarise(across(everything(), round, digits = 1)))

# can we see a pattern of perceiving and being perceived? 

# do people tend to see others as similar across constructs? 
# alpha of 0.73 CI [0.66, 0.8]
psych::alpha(select(dsVars, ends_with(".AB")))

# do people tend to be seen by others as similar across constructs? 
# alpha of 0.76 CI [0.7, 0.82]
psych::alpha(select(dsVars, ends_with(".BA")))


prDsVars <- mutate(dsVars, across(.cols = -a.id, round, digits = 3)) |> 
    mutate(across(a.id, as.character))
# maybe later I will invest in preparing an export for the tables.
write.csv(prDsVars, fname("dsVars"))

describe(select(dsVars,-a.id))
