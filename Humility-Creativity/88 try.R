rm(list = ls())                              #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation for up to 10 digits

library(tidyverse)
library(TripleR)

data("likingLong")
names(likingLong)
x <- likingLong

f <- RR(liking_a/liking_b + metaliking_a/metaliking_b ~ perceiver.id*target.id, x, na.rm = TRUE )
f
efrel <- f$univariate[[1]]$effectsRel
idCols <- names(efrel)[1:3]
efrel <- left_join(efrel, f$univariate[[2]]$effectsRel, by = idCols)
names(efrel) <- c(idCols, "lik.rel", "meta.rel")
ap <- f$univariate[[2]]$effects
names(ap) <- c("id", "metaA.p", "metaA.t")
efrel <- left_join(efrel, ap, by = c("actor.id" = "id"))
names(ap) <- c("id", "metaB.p", "metaB.t")
efrel <- left_join(efrel, ap, by = c("partner.id" = "id"))
efrel$tt.sum <- efrel$metaA.t + efrel$metaB.t
efrel$tt.delta <- abs(efrel$metaA.t - efrel$metaB.t)
efrel$pp.sum <- efrel$metaA.p + efrel$metaB.p
efrel$tt.prod <- efrel$metaA.t * efrel$metaB.t
efrel$pp.prod <- efrel$metaA.p * efrel$metaB.p

efrel1 <- efrel[c(F,T),]
efrel2 <- efrel[c(T,F),]
round(cor(select(efrel, -any_of(idCols))), 2)
round(cor(select(efrel1, -any_of(idCols))), 2)
round(cor(select(efrel2, -any_of(idCols))), 2)
