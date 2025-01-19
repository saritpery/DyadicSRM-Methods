rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation 
# for up to 10 digits

load("main.RImage")

mm <- read.csv("CrossEffPaperData.csv", stringsAsFactors = TRUE)
names(mm)[1] <- "Subj"

ipak("tidyverse")
# ipak (c("srm", "lavaan", "TripleR", "ggplot2", "data.table", "apaTables", 
#         "psych", "tidyverse"))

# Evaluate the random parameters as they appear in the paper:
# These are the parameters for the varCov matrix SIGMA
sd(unique(mm$RandSubInt))     # 28.11 sigma-hat-Sint
sd(unique(mm$RandSubSOA))     # 9.65  sigma-hat-Ssoa
sd(unique(mm$ItemInt))        # 24.50 sigma-hat-i
sd(mm$residual)               # 8.55  sigma-hat-epsilon
cor(mm$RandSubInt, mm$RandSubSOA) # -0.71  rho-hat-Sint-Ssoa

ipak("lme4")


paper.lmer <- lmer(RT ~ SOA + (1 | item) + (1 + SOA | Subj), data = mm)
isSingular(paper.lmer)
rePCA(paper.lmer)
summary(paper.lmer)
# We saw a corr of -1 between the Subject and the interaction between the Subject and SOA. 
# So we should drop the correlation to reach a better model.

paper.lmer1 <- lmer(RT ~ SOA + (1 | item) + (1 | Subj) + (1|SOA:Subj), data = mm)
summary(paper.lmer1)
# We see a small variance explained by the interaction, so we build the third model 
# w/o any interaction term. 

paper.lmer2 <- lmer(RT ~ SOA + (1 | item) + (1 | Subj), data = mm)
summary(paper.lmer2)
print(paper.lmer2, corr=FALSE) # The corr parameter doesn't appear to do anything or exist

# And now we compare the models
anova(paper.lmer2, paper.lmer1, paper.lmer) # all models comparison
anova(paper.lmer2, paper.lmer) # zero and final as in the paper
