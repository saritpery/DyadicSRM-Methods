
# This file test how the reliabilities of the constructs are mixed into the 
# calculations of correlations.

# Humility Performance study - Individual level moderation
# ==============================================================================
# In this file we perform moderation tests at the individual level. 
# 1. Prepare the demographic data with actor-partner effects including gm (grand mean)
# 2. correlate the demographic data with the actor-partner effects, 
# 3. Imputate the actor-partner correlations from the tripleR analyses.
# ==============================================================================

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# General Moderation GENDER/DEMOGRAPHIC test                               ----
# ==============================================================================

load("main.RImage")
load(imgName(".FullRR.ReadyforAnalaysis"))

ipak (c("TripleR", "apaTables", "tidyverse"))
RR.style("perception")

x <- as.data.frame(x)

# Extract actor/partner effects with grand mean.
vlist <- c("H1/H2", "Creat1/Creat2", "Contrib1/Contrib2", 
           "Sf1/Sf2",  "NonTalk/NonTalk","Shy/Shy")
moderators <- c("Gender", "Age", "Tenure")
GenderTitle <- "Gender(Female=0;Male=1)"
srmCorNote <- TRUE

# Calculate all the scores at the individual level (actor and partner scores 
# for all constructs for all participants)
apEffects <- getEffects(~ a.id*p.id | group.id, data = x, 
                        varlist = vlist, gm = FALSE)
names(apEffects) <- gsub("[12]", "", names(apEffects))
# Gender: 
#   (in the survey 1=F, 2=M
#   Here we do Gender-1 So the results refer to):
#   0 = Female; 1 = Male
#   
# Attach the moderators (Gender, Age, Tenure) 
# to the EFFECTS according to the id
d <- x %>% select(id = a.id, any_of(moderators)) %>% distinct() %>% 
  mutate(Gender = Gender - 1)
apEffects <- left_join(apEffects, d, by = "id")

reliabilities.p <- reliabilities.t <- c()
for (i in vlist) {
  fit <- RR(as.formula(paste(i, "~ a.id*p.id | group.id")), 
            data = x, varlist = vlist, gm = TRUE)
  f1.p <- fit$effects[grep("\\.p", names(fit$effects))]
  reliabilities.p <- c(reliabilities.p, attributes(f1.p[,1])$reliability)
  f1.t <- fit$effects[grep("\\.t", names(fit$effects))]
  reliabilities.t <- c(reliabilities.t, attributes(f1.t[,1])$reliability)
}
cn <- length(vlist)
reliabTable.tt <- data.frame(matrix(rep(reliabilities.t, cn), nrow = cn), 
                          row.names = paste0(vlist, ".t"))

colnames(reliabTable.tt) <- rownames(reliabTable.tt)
t <- cor(select(apEffects, ends_with(".t"), ends_with(".p")))
# for (i in 1:cn) {
#   for (j in 1:cn) {
#     t[i,j] <- t[i,j] / sqrt(reliabilities.t[i]*reliabilities.t[j])
#   }}

# The following adjusts the correlations according to the reliabilities, 
# and puts it above the diagonal. The reliabilities are put in the diagonal.
correct.cor(t, c(reliabilities.t, reliabilities.p))



fith <- RR(as.formula(paste(vlist[1],"+", vlist[2], "~ a.id*p.id | group.id")), 
           data = x, gm = TRUE)


# In order to have reasonable size correlation tables, we'll split between 
# target and perceiver effects.

# moderators with partner effects:
t <- apa.cor.table(select(apEffects, ends_with(".t"), all_of(moderators)), 
                   filename = fname("CorTarget", fileExtention = ".doc"))
t <- apa.cor.table(select(apEffects, ends_with(".t")))

# moderators with actor effects: 
t <- apa.cor.table(select(apEffects, ends_with(".p"), all_of(moderators)),
                   filename = fname("CorPerceiver", fileExtention = ".doc"))
