# SRM - build a pairwise format for APIM
# ========================================
# In this file we generate pw = the pairwise dataframe:
#   
#   pw:       holds for each dyad, the directional relationship scores of all the SRM 
#             constructs. in a pair-wise format such that each row contains the 
#             data of both i to j, and j to i.
#             In addition we add to the pw: 
#             * Raw ratings - the averaged original ratings of i to j and j to i
#             * PT scores - perceiver and target scores - the individual scores

# Prepare the environment 
# ========================

  rm(list = ls())                               #Clean the Global Environment
  cat ("\014")                                  #Clean the R console
  
  load("main.RImage")
  ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))
  x <- as.data.frame(x)

# prepare relational effects table (attrRelEff)
# ==============================================

  varlist <- c("H1/H2", "Creat1/Creat2", "Sf1/Sf2")
  varnames <- c("Hum", "Creat", "Safety")
  idCols <- c("group.id","p.id", "t.id", "dyad" )
  for (i in 1:length(varlist)) {
    fit <- RR(as.formula(paste(varlist[i], " ~ p.id*t.id|group.id")), 
              data=x, gm = FALSE, na.rm = TRUE)
    if (grepl("/", varlist[i])) names(fit$effectsRel)[2:3] <- c( "p.id","t.id")
    ifelse (i == 1, attrRelEff <- fit$effectsRel,
            attrRelEff <- left_join(attrRelEff, fit$effectsRel, by = idCols))
    names(attrRelEff)[length(names(attrRelEff))] <- paste0(varnames[i], "_rel")
  }

# prepare the raw ratings table (raws)
# ==============================================

  idColsRaw <- c("group.id","p.id", "t.id")
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
  
# Add individual-level scores to the pairwise table
# =================================================
  # In this section we add the individual scores (perceiver&target=actor&partner):
  # 1. Relative (natural) scores (e.g: Hum_perc.A)
  # calculate the individual scores
  ptEffects <- getEffects(~ p.id*t.id | group.id, data = x, varlist = varlist)
  # enter the perceiver scores
  names(ptEffects) <- c("p.id", "group.id", paste0(rep(varnames, each = 2), c("_perc.A", "_target.A")) )
  pw <- left_join(pw, ptEffects, by = c("group.id", "p.id"))
  # enter the target scores
  names(ptEffects) <- gsub(".A$", ".B", names(ptEffects))
  ptEffects <- rename(ptEffects, t.id = p.id)
  pw <- left_join(pw, ptEffects, by = c("group.id", "t.id"))
  # adjust the ptEffects names to represent individual scores again for future use
  names(ptEffects) <- gsub(".B", "", names(ptEffects))
  ptEffects <- rename(ptEffects, id = t.id)
  
# Organize the columns of pw
# ==========================
  # determine the order of columns: id columns, dyadic scores, individual 
  #       scores, raw ratings.
  niceOrder <- c(idCols, paste0(rep(varnames, each = 2), c("_rel.AB", "_rel.BA")),
                 paste0(rep(paste0(rep(varnames, each = 2), c("_perc", "_target")), 2),
                        rep(c(".A", ".B"), each = 2)),
                 paste0(rep(varnames, each = 2), c("_raw.AB", "_raw.BA")),
                 "partnum")
                 
  pw <- select(pw, all_of(niceOrder))  
  
  rm(list = setdiff(ls(), c("idCols", "ptEffects", "x", "pw", "varnames")))
  load("main.RImage")
  save.image(imgName(".Pairwise"))

