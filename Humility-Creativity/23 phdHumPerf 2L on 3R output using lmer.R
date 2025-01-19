rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load((imgName(".FullRR.Pairwise")))

# Define an APIM analysis and display function 
# ==============================================
ipak(c("lme4"))  
kpt.dg <- c("a.id", "p.id", "dyad", "group.id")
kpt.g <- c("a.id", "p.id", "group.id")
kpt <- c("a.id", "p.id")
kpt.d <- c("a.id", "p.id", "dyad")

pw[,idCols] <- apply(pw[,idCols], 2, as.factor)
APIM.lmer <- function(dv, rel.iv, ind.iv = NULL,
                      k = c("a.id", "p.id", "dyad"), df = pw,
                      dv.is.rel = TRUE){
  dvc <- ifelse(!dv.is.rel, dv,
                ifelse(grepl("_rel.BA|_rel.AB", dv), dv, 
                       ifelse(grepl("_rel", dv), 
                              paste0(dv, ".AB"),
                              paste0(dv, "_rel.AB"))))
  rel.ivc <- paste0(rep(rel.iv, each = 2), 
                    rep(c("_rel.AB ", "_rel.BA" ), length(rel.iv)),
                    collapse = " + ")
  ind.ivc <- ifelse(is.null(ind.iv), "", 
                    paste(" + ", paste0(ind.iv, collapse = " + ")))
  rand <- paste0(paste0("(1 | ", k, ")"), collapse = " + ")
  f <- paste0(dvc, " ~ ", rel.ivc, ind.ivc, " + ", rand)
  t <- try(fit <- lmer(as.formula(f), data = df, REML = TRUE))
  
  header <- "APIM analysis: "
  cat(paste0(header, "\n",
             paste0(rep("=", nchar(header)), collapse = ""),
             "\nTHE MODEL: \n", f, "\n\n"))
  
  if (!"try-error" %in% class(t)) {
    s <- summary(fit)$coefficients[-1,] # fixed coefs w/o intercept (==0)
    s <- round(s,3)
    print(s)
    #s <- confint(fit)
    return(fit)
  } else {print("Model does not converge for the data"); return(t)}
}

# creativity analyses:
# ============================================

# test the main hypothesis
  fit <- APIM.lmer("Creat", "Hum")
  fit <- APIM.lmer("Creat", c("Safety", "Hum"))
  fit <- APIM.lmer("Creat", c("Safety"))
  fit <- APIM.lmer("Safety", "Hum")

# test for the opposite causality
  fit <- APIM.lmer("Hum", c("Safety"))
  
  fit <- APIM.lmer("Hum", "Creat")
  fit <- APIM.lmer("Hum", c("Safety", "Creat"))
  fit <- APIM.lmer("Safety", "Creat")
  fit <- APIM.lmer("Safety", c("Hum", "Creat"))

# Test for individual and dyadic levels together ===============================
  fit <- APIM.lmer(dv = "Creat", rel.iv = c("Hum"), 
                   ind.iv = c("Hum_perc.A:Hum_perc.B", 
                              "Hum_target.A:Hum_target.B"))
  fit1 <- APIM.lmer(dv = "Creat", rel.iv = c("Hum"), 
                   ind.iv = c("Hum_target.A:Hum_target.B"))
  fit <- APIM.lmer(dv = "Creat", rel.iv = c("Hum"), 
                   ind.iv = c("Hum_target.A:Hum_target.B", 
                              "Creat_target.A:Creat_target.B"))
  
# contribution analysis:
# ============================================
  
  # test the main hypothesis
  fit <- APIM.lmer("Contr", "Hum")
  fit <- APIM.lmer("Contr", c("Safety", "Hum"))
  fit <- APIM.lmer("Contr", c("Safety"))
  fit <- APIM.lmer("Safety", "Hum")
  
  # test for the opposite causality
  fit <- APIM.lmer("Hum", c("Safety"))
  fit <- APIM.lmer("Hum", "Contr")
  fit <- APIM.lmer("Hum", c("Safety", "Contr"))
  fit <- APIM.lmer("Safety", "Contr")
  fit <- APIM.lmer("Safety", c("Hum", "Contr"))

## test for alternative effects - Introversion
## =============================================
  fit <- APIM.lmer("Shy", c("Safety"))
  fit <- APIM.lmer("Shy", c("Safety", "Hum"))
  fit <- APIM.lmer("Creat", c("Safety", "Shy"))
  fit <- APIM.lmer("Creat", c("Hum", "Shy"))
  fit <- APIM.lmer("Safety", "Shy")
  
  fit <- APIM.lmer("NonTalk", c("Safety"))
  fit <- APIM.lmer("NonTalk", c("Safety", "Hum"))
  
  fit <- APIM.lmer("Safety", "NonTalk")
  fit <- APIM.lmer("Creat", c("Safety", "NonTalk"))
  