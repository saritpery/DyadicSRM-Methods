rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation 
# for up to 10 digits

load("main.RImage")
load(imgName(".FullRR.Pairwise"))

ipak(c("tidyverse", "lme4"))
pwc <- pw
rm(list = c("pw"))

# Scale all effects columns and all rating columns
relCols <- grep("\\.AB|\\.BA", names(pwc)) # The effects columns
pwc[,relCols] <- apply(pwc[,relCols], 2, function(a) {as.numeric(scale(a))})
mainCont <- c("Hum", "Safety", "Creat")
# make id columns to be a factor to act as grouping parameters
pwc[,idCols] <- apply(pwc[,idCols], 2, as.factor)

lmer(Creat_rel.AB ~ Hum_rel.BA  + (1|dyad), data = pwc)

CrH <- lmer(Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
              (1|a.id) + (1|p.id) + (1|dyad), data = pwc)
summary(CrH)
CrH1 <- lmer(Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
              (1|a.id) + (1|p.id)
             , data = pwc)
CrH1 <- lm(Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA , data = pwc)

# test the main hypothesis
fitCH <- (lmer(Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                 (1|a.id) + (1|p.id), data = pwc))
fitCSH <- (lmer(Creat_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                  Safety_rel.AB + Safety_rel.BA +
                  (1|a.id) + (1|p.id), data = pwc))
fitCS <- (lmer(Creat_rel.AB ~ 
                 Safety_rel.AB + Safety_rel.BA +
                 (1|a.id) + (1|p.id), data = pwc))
fitSH <- (lmer(Safety_rel.AB ~ Hum_rel.AB + Hum_rel.BA +
                 (1|a.id) + (1|p.id), data = pwc))
summary(fitCH); summary(fitCSH); summary(fitCS); summary(fitSH)

# Further investigation of relationship scores dependencies of an actor: 
fitH0 <- lmer(Hum_rel.AB ~ (1|a.id) + (1|p.id), data = pwc)
fitH0 <- lmer(Hum_rel.AB ~ (1|a.id) , data = pwc)
summary(fitH0)

# Calculate the variance of the relationship scores for each participant, for
# each construct, once for the relationship scores the participant perceives, 
# and once for the relationship scores where the participant is the target.
vars <- data.frame(id = unique(pwc$a.id))
for (v in varnames) {
  vp <- grep(paste0(v, "_rel.AB"), names(pwc), value = TRUE)
  vt <- grep(paste0(v, "_rel.BA"), names(pwc), value = TRUE)
  pwtemp <- select(pwc, a.id, all_of(c(vp, vt)))
  names(pwtemp) <- c("id", "vp", "vt")
  a <- group_by(pwtemp, id) %>%  
    summarise(pvar = var(vp), tvar = var(vt))
  names(a) <- c("id", paste0(c("p.", "t."), v, "var"))
  vars <- left_join(vars, a, by = "id")
}
varsWOShy <- vars[, -grep("Shy", names(vars))]
vars2items <- varsWOShy[, -grep("Non", names(varsWOShy))]
# Lets study the relationship scores variance sources!
vsource <- pivot_longer(vars, cols = -id,
                         names_pattern = "(.)\\.(.+)var", 
                         names_to = c("pt", "const"), 
                         values_to = "var")
vsourceWOShy <- pivot_longer(varsWOShy, cols = -id,
                             names_pattern = "(.)\\.(.+)var", 
                             names_to = c("pt", "const"), 
                             values_to = "var")
vsource2items <- pivot_longer(vars2items, cols = -id,
                              names_pattern = "(.)\\.(.+)var", 
                              names_to = c("pt", "const"), 
                              values_to = "var")
vsource <- vsource2items[vsource2items$const != "Contr",]

dyadEffVariance <- lmer(var ~ (1 | id) + (1 | pt:id) +  (1|const), data = vsource)
summary(dyadEffVariance)
fit <- dyadEffVariance

lmerCrossVariances <- function(fit, randTitles = NA ){
  v <- VarCorr(fit)
  # verify the titles of random effects
  if (!is.na(randTitles)) 
    if (length(randTitles) != length(v))
      if (length(randTitles) == (length(v) + 1)) 
        randTitles <- c(randTitles, "Residual") else 
        {print ("Number of random effects in randTitles parameter, don't match the model")
          exit()}
  if (is.na(randTitles)) randTitles <- c(names(v), "Residual")
  ExplainedVar <- data.frame("Relationship Effects Variance source" = randTitles,
                             "Variance" = NA, "Variance std." = NA,
                             "Correlation" = NA)
  for (i in 1:length(v)) { #v's length is the number of random effects in the model
    ExplainedVar[i, "Variance"] <- attr(v[[i]], "stddev")
    ExplainedVar[i, "Correlation"] <- attr(v[[i]], "correlation")
    }  
  ExplainedVar$Variance[nrow(ExplainedVar)] <- sigma(fit)^2 # add the residuals' variance
  ExplainedVar$Variance.std. <- paste0(round(
    100*ExplainedVar$Variance/sum(ExplainedVar$Variance), 1), "%")
  return(ExplainedVar)
}
randoms <- c("Participant (ID)", 
             "Participant:Role (perceiver or target)", 
             "Construct","error variance")
v <- VarCorr(dyadEffVariance) # a list of varCorrs for each random parameter
# attributes(v1[[1]])
sigmas <- c()
for (i in 1:length(v)) {
  sigmas <- c(sigmas, attr(v[[i]], "stddev"))}  
sigmas <- c(sigmas, sigma(dyadEffVariance)) # add the residuals' sigma (stddev)
ExplainedVar <- data.frame("Relationship Effects Variance source" = randoms, 
                           "Variance" = round(sigmas^2, 2), 
                           "Variance std." = paste0(round(100*sigmas^2/sum(sigmas^2), 1), "%"))
dyadEffVariance1 <- lmer(var ~ (1 | id) + (1 | pt:id) + (1|const)
                         + (1 | id:const), data = vsource)
summary(dyadEffVariance1)

# dyadEffVariance1 <- lmer(var ~ (1 | id) + (1 | pt:id) + (1|const)
#                          + (1 | id:const) + (1 | pt:const), data = vsource)

anova(dyadEffVariance, dyadEffVariance1) # no real difference

# same analysis w/o Shy
# ======================
dyadEffVariance <- lmer(var ~ (1 | id) + (1 | pt:id) +  (1|const), data = vsourceWOShy)
summary(dyadEffVariance)

randoms <- c("Participant (ID)", 
             "Participant:Role (perceiver or target)", 
             "Construct","error variance")
v <- VarCorr(dyadEffVariance) # a list of varCorrs for each random parameter
# attributes(v1[[1]])
sigmas <- c()
for (i in 1:length(v)) {
  sigmas <- c(sigmas, attr(v[[i]], "stddev"))}  
sigmas <- c(sigmas, sigma(dyadEffVariance)) # add the residuals' sigma (stddev)
ExplainedVar <- data.frame("Relationship Effects Variance source" = randoms, 
                           "Variance" = round(sigmas^2, 2), 
                           "Variance std." = paste0(round(100*sigmas^2/sum(sigmas^2), 1), "%"))
dyadEffVariance1 <- lmer(var ~ (1 | id) + (1 | pt:id), data = vsourceWOShy)
summary(dyadEffVariance1)

# dyadEffVariance1 <- lmer(var ~ (1 | id) + (1 | pt:id) + (1|const)
#                          + (1 | id:const) + (1 | pt:const), data = vsource)

anova(dyadEffVariance, dyadEffVariance1) # 

# Same analysis for constructs with 2 items
# =========================================

dyadEffVariance <- lmer(var ~ (1 | id) + (1 | pt:id) + (1|const)
                         + (1 | id:const), data = vsource2items)

summary(dyadEffVariance)

randoms <- c("Participant (ID)", 
             "Participant:Role (perceiver or target)", 
             "Construct","Participant:construct", "error variance")
v <- VarCorr(dyadEffVariance) # a list of varCorrs for each random parameter
# attributes(v1[[1]])
sigmas <- c()
for (i in 1:length(v)) {
  sigmas <- c(sigmas, attr(v[[i]], "stddev"))}  
sigmas <- c(sigmas, sigma(dyadEffVariance)) # add the residuals' sigma (stddev)
ExplainedVar <- data.frame("Relationship Effects Variance source" = randoms, 
                           "Variance" = round(sigmas^2, 2), 
                           "Variance std." = paste0(round(100*sigmas^2/sum(sigmas^2), 1), "%"))
dyadEffVariance1 <- lmer(var ~ (1 | id) + (1 | pt:id) +  (1|const), data = vsource2items)
summary(dyadEffVariance1)
dyadEffVariance2 <- lmer(var ~ (1 | id) + (1 | pt:id) 
                         + (1 | id:const), data = vsource2items)
summary(dyadEffVariance2)

# dyadEffVariance1 <- lmer(var ~ (1 | id) + (1 | pt:id) + (1|const)
#                          + (1 | id:const) + (1 | pt:const), data = vsource)

anova(dyadEffVariance, dyadEffVariance1, dyadEffVariance2) # the largest model is best


# Investigation of vars through cfa and psych
# ===============================================
cor(vars[,-1])
ipak("psych")
ipak("lavaan")
alpha(vars[,-1])
alpha(vars[, ])
describe(vars$p.Humvar)
pcols <- grep("p.", names(vars), value = TRUE)
tcols <- grep("t.", names(vars), value = TRUE)
alpha(vars[,pcols])
alpha(vars[,tcols])
uniqnessFactors <- 
  cbind(
    paste("pf =~ ", paste0(pcols, collapse = " + ")),
    paste("tf =~ ", paste0(tcols, collapse = " + ")))
fpt <- cfa(uniqnessFactors, data = vars, std.lv = TRUE)
summary(fpt)


# # group the humility score by dyad, and mark the direction of the scores
# pwc <- pivot_longer(pwc, cols = c("Hum_rel.AB", "Hum_rel.BA"), 
#                     names_prefix = "Hum_rel.", names_to = "Direct",
#                     values_to = "Hum_rel")

# locate the zero model for predicting humility relational scores by group
H0 <- lmer(Hum_raw.AB ~ Direct + Hum_rel + (1 + Direct| a.id) + (1 | p.id), data = pwc )

# we see that Direct doesn't provide real input as an influence on the slope with a.id
H0.1 <- lmer(Hum_raw.AB ~ Direct + Hum_rel + (1 | a.id:Direct) + (1 | p.id), data = pwc )

H0.2 <- lmer(Hum_raw.AB ~ Hum_rel + (1 | a.id:Direct) + (1 | p.id), data = pwc )