################################################################################
# Bond & Malloy 2023 offer an alternative way to calculate scores
################################################################################

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation 
# for up to 10 digits
load("main.RImage")
load(imgName(".FullRR.ReadyforAnalaysis"))
ipak (c("tidyverse", "TripleR"))

# Appendix C
# R Program to Compute Effect Predictions for the Data in Warner et al. (1979)
BondMalloySingleGroupEffects <- function(data, group = 1) {
# The function expects a matrix of a round-robin
# Where the number of rows and columns is identical.
# The rows are the actors, the columns are the partners, and the cells hold the
# ratings.
  
num<-length(data)
nact<-num**.5        

# set parameters
ntot<-nact*(nact-1)   # ntot = number of actual ratings
ndy<-ntot/2           # ndy = number of dyads
neff<-2*nact+ntot     # neff = number of effects (actor+partner+dyadic effects)

# organize a vector (????) of the dv in sorted by dyads
dv<-rep(0,ntot)
k<-0
for (i in 1:nact) {
  for (j in i:nact) {
    if (j>i) {k<-k+1
    dv[k]<-data[i,j] # dv is in the order [1,2], [2,1], [1,3], [3,1], ... , [7,8], [8,7]
    k<-k+1
    dv[k]<-data[j,i]} }}

# mgrand = the grand mean duplicated
mgrand<-rep(1,ntot)
mgrand<-mean(dv)*mgrand

# D, V matrixes of all effects sizes square.
D<-matrix(0,neff,neff) # D is var-cov matrix for effects, set up to be block diagonal
V<-matrix(0,ntot,ntot) # V is var-cov matrix for observed scores -- set up in order of dv

Act<-rep(0,ntot)  # For each rating, indicates the actor
Par<-rep(0,ntot)  # For each rating, indicates the partner
ac<-matrix(0,ntot,nact) # Each row is the rating, the col indicates (1 or 0) the actor
pa<-matrix(0,ntot,nact) # Each row is the rating, the col indicates (1 or 0) the partner

k<-0
for (i in 1:nact) {
  for (j in i:nact) {
    if (i!=j)
    {k<-k+1
    Act[k]<-i # For each data point, first
    Par[k]<-j # and second subscripts.
    ac[k,i]<-1
    pa[k,j]<-1
    k<-k+1
    Act[k]<-j # Subscripts for the reciprocal data point.
    Par[k]<-i
    ac[k,j]<-1
    pa[k,i]<-1 }
  } } # Fill Act, Par, ac, pa.
# I guess this was a mistake, because the following row was in the loop,
#   setting Z to zero in every round of the loop
Z<-matrix(0,ntot,neff) # indicator matrix for each effect (col) in each data point (row)
for (i in 1:nact) { 
  j<-2*(i-1)+1
  k<-2*i
  Z[,j]<-ac[,i]
  Z[,k]<-pa[,i] }
for (i in 1:ntot) {
  j<-2*nact+i
  Z[i,j]<-1 }
mact<-rowSums(data)/(nact-1) # marginal means and grand mean
mpar<-colSums(data)/(nact-1)
mtot<-mean(mpar)
WKS<-matrix(0,nact,2) # WKS[,1] (and WKS[,2]) are WKS actor (and partner) eﬀect predicons
WKSrel<-matrix(0,nact,nact) # WKS relaonship eﬀect predicons
WKSco<-rep(0,3) # Compung WKS predicons
WKSco[1]<-(nact-1)**2/(nact*(nact-2))
WKSco[2]<-(nact-1)/(nact*(nact-2))
WKSco[3]<-(nact-1)/(nact-2)
for (i in 1:nact) {
  WKS[i,1]<-WKSco[1]*mact[i]+WKSco[2]*mpar[i]-WKSco[3]*mtot
  WKS[i,2]<-WKSco[1]*mpar[i]+WKSco[2]*mact[i]-WKSco[3]*mtot }
k<-0
for (i in 1:nact) {
  for (j in i:nact) {
    if (i!=j) {WKSrel[i,j]<-data[i,j]-WKS[i,1]-WKS[j,2]-mtot
    WKSrel[j,i]<-data[j,i]-WKS[j,1]-WKS[i,2]-mtot } }}
SCM<-matrix(0,nact,2) # SCM[,1] (and SCM[,2]) are SCM actor (and partner) eﬀect predictions
SCMrel<-matrix(0,nact,nact) # SCM relationship effect predictions
c<-matrix(0,5,5) # Matrix for MOM estimation of SRM parms
c[1,1]<-ntot-nact
c[1,4]<-c[1,1]
c[2,2]<-c[1,1]
c[2,4]<-c[1,1]
c[3,3]<-c[1,1]
c[3,5]<-c[1,1]
c[4,1]<-ntot-(nact-1)
c[4,2]<-ntot-(nact-1)
c[4,3]<-2*(1-nact)
c[4,4]<-ntot-1
c[4,5]<- -1
c[5,1]<-ntot
c[5,2]<-ntot
c[5,4]<-ntot
c[5,3]<- -2*ntot
c[5,5]<- -ntot
cin<-solve(c)

ss<-rep(0,5) # Compung Sums of Squares and Cross-Products
ss[4]<-var(dv)*(ntot-1)
for (i in 1:nact) {
  for (j in 1:nact) {
    if (i!=j) {
      ss[1]<-ss[1]+(data[i,j]-mpar[j])**2
      ss[2]<-ss[2]+(data[i,j]-mact[i])**2
      ss[3]<-ss[3]+(data[i,j]-mpar[j])*(data[j,i]-mact[j])
      ss[5]<-ss[5]+(data[i,j]-data[j,i])**2 }
  } }
ss[5]<-ss[5]/2
est<-cin%*%ss # These are the SRM parameter esmates. In order, they are
# actor var, partner var, act-par cov, relaon var, dyadic cov
if (est[1]<0) est[1]<-0 # Replacing inadmissible var/cov esmates.
if (est[2]<0) est[2]<-0
if (est[4]<0) est[4]<-0
xp<-(est[1]*est[2])**.5
if (est[3]>xp) {est[3]<-xp}
if (est[3]< -xp) {est[3]<- -xp}
if (est[5]>est[4]) {est[5]<-est[4]}
if (est[5]< -est[4]) {est[5]<- -est[4]}


# my addition: add row names to est and print it 
# ============================================================================
rownames(est) <- c("actor var", "partner var", "act-par cov", 
                   "relation var", "dyadic cov")
print(est)

for (i in 1:ntot) {
  for (j in i:ntot) {
    if ((Act[i]==Act[j]) & (Par[i]!=Par[j])) V[i,j]<-est[1]
    if ((Act[i]!=Act[j]) & (Par[i]==Par[j])) V[i,j]<-est[2]
    if ((Act[i]==Par[j]) & (Act[j]!=Par[i])) V[i,j]<-est[3]
    if ((Act[j]==Par[i]) & (Act[i]!=Par[j])) V[i,j]<-est[3]
    if ((Act[i]==Act[j]) & (Par[i]==Par[j]))
      V[i,j]<-est[1]+est[2]+est[4]
    if ((Act[i]==Par[j]) & (Act[j]==Par[i]))
      V[i,j]<-est[5]+2*est[3]
    V[j,i]<-V[i,j] } }
Vin<-solve(V) # Now we have V-inverse
for (i in 1:nact) { # Preparing to compute the SCM effect-predicons
  j<-2*(i-1)+1
  k<-2*i
  D[j,j]<-est[1] # D is the var-cov matrix for SRM effects
  D[k,k]<-est[2]
  D[j,k]<-est[3]
  D[k,j]<-est[3] }
for (i in 1:ndy) {
  j<-2*nact+2*(i-1)+1
  k<-j+1
  D[j,j]<-est[4]
  D[k,k]<-est[4]
  D[j,k]<-est[5]
  D[k,j]<-est[5] }
# F is the matrix relang SRM effects (in rows)
F<-t(Z%*%D) # to observed scores (in columns)

SCMeffs<-F%*%Vin%*%(dv-mgrand)
SCMvars<-F%*%Vin%*%t(F)
for (i in 1:nact) {
  k<-2*(i-1)+1
  SCM[i,1]<-SCMeffs[k]
  k<-k+1
  SCM[i,2]<-SCMeffs[k] }
k<-2*nact
for (i in 1:nact) {
  for (j in i:nact) {
    if (j>i) {k<-k+1
    SCMrel[i,j]<-SCMeffs[k]
    k<-k+1
    SCMrel[j,i]<-SCMeffs[k] } }}

# Make the output presentable and applicable
ps <- paste0(group, ":", 1:nact)
SCM <- as.data.frame(SCM)
names(SCM) <- c("actor Eff", "partner Eff")
SCM <- mutate(SCM, id = ps) %>% select(id, everything())

SCMrel <- as.data.frame(SCMrel)
names(SCMrel) <- ps
SCMrel <- mutate(SCMrel, actor = ps) %>% select(actor, everything())
longSCMrel <- pivot_longer(SCMrel, cols = all_of(ps), names_to = "partner", 
                           values_to = "relationship") %>% 
  as.data.frame() %>% 
  filter(actor != partner)
return(list(SCM = SCM, SCMrel = SCMrel, longSCMrel = longSCMrel, est = est))
}

# if i understand correctly, data is a single group data. no missing values, 
#   with a diagonal of zeros instead of NAs.
#   The example is the data of a single RR of 8 people. 
#   The data is provided here in a vector, but it simulates a matrix
#   with the rows are the actors, and the columns are the partners.
data<- c( 0, 52.67, 69.33, 54.33, 69.67, 77, 67, 76,
          36, 0, 32.67, 33, 32.33, 39.67, 42.67, 46.33,
          29.33, 51.67, 0, 32.67, 33.67, 30.33, 39.67, 27.33,
          33.67, 61.33, 68, 0, 45.33, 51, 53.33, 73.33,
          62.67, 56.67, 74.33, 60, 0, 42.33, 60.67, 66.67,
          34.67, 41.33, 67.33, 34.67, 40.33, 0, 64.67, 50,
          54.67, 46.67, 63.67, 40, 41, 40, 0, 64.33,
          47.67, 65.67, 79.33, 47.67, 41.67, 47.33, 51.67, 0)
# turn data data into a matrix
data <- matrix(t(data), nrow = nact)
effList <- BondMalloySingleGroupEffects(data)
effList

# The function should be called for each item, for each group
items <- paste0("Hum", 1:3)
groups <- unique(longInput$group.id)
for (item in items) {
  for (g in groups) {
    gidata <- longInput %>% filter(group.id == g) %>% 
      arrange(Actor) %>% 
      select(a.id, p.id, all_of(item))
    gidata[is.na(gidata)] <- 0
    gidata <- pivot_wider(gidata, names_from = p.id, values_from = item)
    gidata <- as.matrix(select(gidata, -a.id))
    effList <- BondMalloySingleGroupEffects(gidata, group = g)
  }
  
}
