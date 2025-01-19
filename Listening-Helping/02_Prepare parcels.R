

# SRM - RR ANALYSIS PREPERATIONS #2
# ==============================================================================

# Prepare parcels and Constructs from parcels
# -------------------------------------------

# We use the RR function, that can only handle 2 questions per construct at a time!
# Hence we need to prepare parcels so that each construct can be described by 2 parcels.

# Note regarding the order/sort of the data: The matrix2long function that we use, sorts the files by partner and then by actor. 
#     So it's ready for the SRM analysis, and it's consistent through out the questions files.


Alpha.Check <- FALSE # Turn to TRUE in the beginning of work, to verify that the items are right

load("StudyData")

L.Parcels <- L1[,1:3]
L.Parcels$L13 <-  rowMeans(cbind(L1$value,L3$value))
L.Parcels$L24 <-  rowMeans(cbind(L2$value,L4$value))
L.Parcels$L.Raw <- rowMeans(cbind(L1$value,L2$value,L3$value,L4$value))

Listening <- as.data.frame(cbind(L1$value,L2$value,L3$value, L4$value))

if (Alpha.Check) {
    library(apaTables)
    library(psych)
    apa.cor.table(Listening, show.conf.interval = FALSE)
    alpha(Listening)
}


I.Parcels <- I1[,1:3]
I.Parcels$I1    <- I1$value
I.Parcels$I2    <- I2$value
I.Parcels$I3    <- I3$value
I.Parcels$I.Raw <- rowMeans(cbind(I1$value,I2$value,I3$value))

I.Parcels$I23   <- rowMeans(cbind(I2$value,I3$value))
I.Parcels$I12   <- rowMeans(cbind(I1$value,I2$value))
I.Parcels$I13   <- rowMeans(cbind(I1$value,I3$value))


Intimacy <- as.data.frame(cbind(I1$value,I2$value,I3$value))
if (Alpha.Check) {
    apa.cor.table(Intimacy, show.conf.interval = FALSE)
    alpha(Intimacy)
}


H.Parcels <- H1[,1:3]
H.Parcels$H1 <- H1$value
H.Parcels$H23 <- rowMeans(cbind(H2$value,H3$value))
H.Parcels$H.Raw <- rowMeans(cbind(H1$value,H2$value,H3$value))


Helping <- as.data.frame(cbind(H1$value,H2$value,H3$value))
if (Alpha.Check) {
    apa.cor.table(Helping, show.conf.interval = FALSE)
    alpha(Helping)
} 

S.Parcels <- S1[,1:3]
S.Parcels$S1 <- S1$value
S.Parcels$S23 <- rowMeans(cbind(S2$value,S3$value))
S.Parcels$S.Raw <- rowMeans(cbind(S1$value,S2$value,S3$value))


Speaking <- as.data.frame(cbind(S1$value,S2$value,S3$value))
if (Alpha.Check) {
    apa.cor.table(Speaking, show.conf.interval = FALSE)
    alpha(Speaking)
}

# Here we'll change the assignment if we wish to use different parcels. 
# The rest of the calculation will remain the same.
x <- L.Parcels[,1:3]
x$LP1 <- L.Parcels$L13
x$LP2 <- L.Parcels$L24
x$IP1 <- I.Parcels$I1
x$IP2 <- I.Parcels$I23
x$SP1 <- S.Parcels$S1
x$SP2 <- S.Parcels$S23
x$HP1 <- H.Parcels$H1
x$HP2 <- H.Parcels$H23

# enter the construct means so the raw means calculation will be right.
x$Listening.Raw   <- L.Parcels$L.Raw
x$Intimacy.Raw    <- I.Parcels$I.Raw
x$Speech.Raw      <- S.Parcels$S.Raw
x$Helping.Raw     <- H.Parcels$H.Raw


x <- x[order(x$partner.id,x$actor.id),]

# we'll compute dx = dyadic x, where we'll add for each dyad the dyadic ratings in the same row

# Next two lines produced error.  AVI fixed with the following dx defintion

# desired.columns <- c("group.id","actor.id","partner.id","Listening.Raw","Intimacy.Raw","Speech.Raw","Helping.Raw")
# dx <- x[,match(desired.columns,colnames(x))]

dx <- x[,c("group.id","actor.id","partner.id","Listening.Raw","Intimacy.Raw","Speech.Raw","Helping.Raw")]
names(dx) <- gsub("Raw","ij",names(dx))
names(dx) <- gsub("actor","i",names(dx))
names(dx) <- gsub("partner","j",names(dx))

#remove NAs from non self report
dx <- dx[-which(dx$i.id == dx$j.id),] #when i=j, the rating is NA.
#put place holders for each construct (temporary)
dx$Listening.ji <- dx$Intimacy.ji <- dx$Speech.ji <- dx$Helping.ji <- dx$Listening.ij

# add the dyadic result in the same raw:
for (l in 1:nrow(dx)) {
    # l <- 1
    if (l > nrow(dx)) break() # since we delete the rows of the dyad the row no will shorten
    i <- dx$i.id[l]
    j <- dx$j.id[l]
    co.line <- which(dx$j.id == i & dx$i.id == j)
    dx$Listening.ji[l]  <- dx$Listening.ij[co.line]
    dx$Intimacy.ji[l]   <- dx$Intimacy.ij[co.line]
    dx$Speech.ji[l]     <- dx$Speech.ij[co.line]
    dx$Helping.ji[l]    <- dx$Helping.ij[co.line]
    
    dx <- dx[-co.line,] # keep only one row for each dyad
}

# clean the environment     
rm(list = c("desired.columns", "Alpha.Check", "co.line", "Answers.Files.List", "i", "j", "l"))

save.image("StudyReadyforAnalaysis")

# Prepare image with new code conventions 
# ==============================================================================
rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
load("StudyReadyforAnalaysis")
rm(list = setdiff(ls(), c("x") ))
load("main.RImage")
save.image(imgName("Ready4Analaysis"))
