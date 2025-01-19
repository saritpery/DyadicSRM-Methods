rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# Attenuation Meaning:
# ====================
# When correlating two constructs that might be somewhat noisy,
# the correlation might be weaker than it should be. 
# Hence, the common correction is to divide by their reliabilities product (square root)
# as a noise measure. 
# Since the noise is supposed to have a random behavior, then the reliabilities 
# product should amend stuff.

# Attenuation for SRM scores correlations - Kenny, 1997:
# ======================================================
# in Malloy,Albright&Kenny, 1997 - consistency across groups of KP's consensus,
# Kenny suggested that the correlation of the target scores across groups 
# should be attenuated to amend for the relationship-scores average that is hidden
# inside the target scores. 
# I wondered if that might predict the individual level covariances directly 
# from the perceiver/target scores. 

# Results: 
# ========
# The attenuation inflates the correlations in a substantial manner, 
# and probably inflated the results in the relevant paper as well. 

load("main.RImage")
load(imgName(".FullRR.Pairwise"))
load(imgName("TripleROutput")) # The code for the img creation is commented out 
                               # in the following section.
ipak (c("TripleR", "apaTables", "tidyverse", "psych"))
RR.style("perception")

# # Extract actor/partner effects with grand mean.
# vlist <- c("H1/H2", "Creat1/Creat2", "Contrib1/Contrib2",
#            "Sf1/Sf2",  "NonTalk/NonTalk","Shy/Shy")
# 
# tr.output <- extractTripleRoutput(x, vlist = vlist, pid = "a.id", tid = "p.id",
#                                   varTitles = varnames)


# # compare individual level covariances
names(apEffects) <- gsub("_perc", ".p", names(apEffects))
names(apEffects) <- gsub("_target", ".t", names(apEffects))
list2env(tripleRoutput, envir = environment())
reliabilities <- varComp |> group_by(Const) |> 
  mutate(rel.p = reliability[[1]], 
         rel.t = reliability[[2]]) |> 
  select(Const, rel.p, rel.t) |> 
  distinct() |> t()
rels <- as.numeric(c(reliabilities[-1,] ))
t <- cor(select(apEffects, any_of(names(biCor))))
attent <- correct.cor(t, rels)
diffAttenSRM <- biCor - attent
t(biCor) - attent
