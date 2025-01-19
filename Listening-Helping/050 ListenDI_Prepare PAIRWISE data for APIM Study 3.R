# SRM - RR ANALYSIS - Preperation for APIM
# ========================================

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName("Ready4Analaysis"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))

x <- as.data.frame(x)

# prepare relational effects table (attrRelEff)
# =============================================

varlist <- c("LP1/LP2", "IP1/IP2",  "SP1/SP2", "HP1/HP2")
varnames <- c("Listening", "Intimacy", "Speech", "Helping")

# rename the following to match future studies conventions
x <- rename(x, a.id = actor.id, p.id = partner.id)
names(x) <- gsub(".Raw", ".raw", names(x))

idCols <- c("group.id","a.id", "p.id", "dyad" )
for (i in 1:length(varlist)) {
  fit <- RR(as.formula(paste(varlist[i], " ~ a.id*p.id|group.id")), 
            data=x, gm = TRUE, na.rm = TRUE)
  if (grepl("/", varlist[i])) names(fit$effectsRel)[2] <- "p.id"
  ifelse (i == 1, attrRelEff <- fit$effectsRel,
          attrRelEff <- left_join(attrRelEff, fit$effectsRel, by = idCols))
  names(attrRelEff)[length(names(attrRelEff))] <- paste0(varnames[i], "_rel")
}

# prepare dyadic and individual scores together
# ==============================================
  apRelEffects <-  attrRelEff
  # calculate the individual scores
  apEffects <- getEffects(~ a.id*p.id | group.id, data = x, gm = TRUE,
                          varlist = varlist)
  # enter the perceiver scores
  names(apEffects) <- c("a.id", "group.id", paste0(rep(varnames, each = 2), c("_perc.A", "_target.A")) )
  apRelEffects <- left_join(apRelEffects, apEffects, by = c("a.id", "group.id"))
  # enter the target scores
  names(apEffects) <- c("p.id", "group.id", paste0(rep(varnames, each = 2), c("_perc.B", "_target.B")) )
  apRelEffects <- left_join(apRelEffects, apEffects, by = c("p.id", "group.id"))
  # adjust the apeffects names to represent individual scores again for future use
  names(apEffects) <- gsub(".B", "", names(apEffects))
  apEffects <- rename(apEffects, id = p.id)

# prepare the raw ratings table (raws)
# ==============================================
  
  idColsRaw <- c("group.id","a.id", "p.id")
  raws <- x[,c(idColsRaw, paste0(varnames, ".raw"))]
  raws[,idColsRaw] <- apply(raws[,idColsRaw], 2, as.factor)
  
# Prepare the pairwise table from both raw and relationship effects
# ==================================================================
  
  pw1 <- attrRelEff[c(TRUE, FALSE), ]
  names(pw1) <- gsub("_rel", "_rel.AB", names(pw1))
  pw1 <- left_join(pw1, raws, by = idColsRaw)
  names(pw1) <- gsub(".raw", "_raw.AB", names(pw1))
  
  pw2 <- attrRelEff[c(FALSE, TRUE), ]
  pw2 <- left_join(pw2, raws, by = idColsRaw)
  names(pw2) <- names(pw1)
  
  pw12 <- cbind(pw1, pw2)
  pw21 <- cbind(pw2, pw1)
  pw <- rbind(pw12, pw21)
  names(pw) <- c(names(pw1), 
                 gsub("_rel", "_rel.BA", names(attrRelEff)),
                 gsub(".raw", "_raw.BA", names(raws[grep(".raw", names(raws))])))
  pw <- pw[ , unique(names(pw))]
  pw <- pw[order(pw$dyad), ]
  pw$partnum <- c(1,2)
  
# Add individual scores to the pw:
# ================================
  # add individual scores for each dyad in the pw data set
  pw <- pw %>%  
    left_join(select(apRelEffects, -contains("_rel")), 
              by = idCols) 
  #shorten the names in pw, and move to lower-case names
  names(pw) <- tolower(names(pw))
  names(pw) <- gsub("ing", "", names(pw))
  names(pw) <- gsub("(\\..*)", "\\U\\1", names(pw), perl = TRUE)
  names(pw) <- gsub("\\.ID", "\\.id", names(pw))
  pw <- select(pw, all_of(idCols), everything(), -partnum)
  varnames <- tolower(gsub("ing", "",varnames))
  rm(list = setdiff(ls(), c("apRelEffects", "idCols",
                            "apEffects", "x", "pw", "varnames")))
  load("main.RImage")
save.image(imgName("StudyPairWise"))
