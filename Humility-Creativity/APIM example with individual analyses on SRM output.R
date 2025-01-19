Wrm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("StudyReadyforAPIM")
library(TripleR)

Listening <- PAIRWISE

Listening$Intimacy.Raw_AB  <- as.numeric(scale(Listening$Intimacy.Raw_AB))
Listening$Intimacy.Raw_BA  <- as.numeric(scale(Listening$Intimacy.Raw_BA))
Listening$Listening.Raw_AB <- as.numeric(scale(Listening$Listening.Raw_AB))
Listening$Listening.Raw_BA <- as.numeric(scale(Listening$Listening.Raw_BA))
Listening$Speech.Raw_AB <- as.numeric(scale(Listening$Speech.Raw_AB))
Listening$Speech.Raw_BA <- as.numeric(scale(Listening$Speech.Raw_BA))
Listening$Helping.Raw_AB <- as.numeric(scale(Listening$Helping.Raw_AB))
Listening$Helping.Raw_BA <- as.numeric(scale(Listening$Helping.Raw_BA))

Listening$Intimacy_AB  <- as.numeric(scale(Listening$Intimacy_AB))
Listening$Intimacy_BA  <- as.numeric(scale(Listening$Intimacy_BA))
Listening$Listening_AB <- as.numeric(scale(Listening$Listening_AB))
Listening$Listening_BA <- as.numeric(scale(Listening$Listening_BA))
Listening$Speech_AB <- as.numeric(scale(Listening$Speech_AB))
Listening$Speech_BA <- as.numeric(scale(Listening$Speech_BA))
Listening$Helping_AB <- as.numeric(scale(Listening$Helping_AB))
Listening$Helping_BA <- as.numeric(scale(Listening$Helping_BA))

               DV_AB = Listening$Intimacy.Raw_AB
               DV_BA = Listening$Intimacy.Raw_BA
               IV_AB = Listening$Listening.Raw_AB
               IV_BA = Listening$Listening.Raw_BA
               CV_AB = Listening$Speech.Raw_AB
               CV_BA = Listening$Speech.Raw_BA
               OV_AB = Listening$Helping.Raw_AB
               OV_BA = Listening$Helping.Raw_BA
               MyData = Listening


library(nlme)

               MyData$ActorN   <- as.factor(MyData$ActorN)
               MyData$PartnerN <- as.factor(MyData$PartnerN)
               
               #Then we create ng (g being the number of groups and n the number per group)
               #dummy variables using model.matrix.   
               MyData$Adum     <- model.matrix(~MyData$ActorN)
               MyData$Pdum     <- model.matrix(~MyData$PartnerN)
               
               #To avoid singularity we drop one dummy for Actor and we drop one per group for
               #Partner. For this example, there are 27 groups of 4 persons and so 27 dummies 
               #are dropped. 
               
               MyData$AdumP     <- MyData$Adum[, -1]
               #Note the parameter for REPlication is number of persons within group minus one.
               MyData$PdumP     <- MyData$Pdum[, c( FALSE, rep(TRUE, dim(MyData$Pdum)[1]/
                                                                   dim(MyData$Pdum)[2])) ]
               
               fit
               fit             <- gls(DV_AB ~  IV_AB + IV_BA + CV_AB + CV_BA + AdumP + PdumP,
                                      na.action = na.omit, method = "REML", verbose = TRUE,
                                      correlation = corCompSymm (form = ~1|DyadN),
                                      data = cbind(MyData))
               fit             <- gls(Helping.Raw_AB ~  Intimacy.Raw_AB + Intimacy.Raw_BA + 
                                          Listening.Raw_AB + Listening.Raw_BA + 
                                          Speech.Raw_AB    + Speech.Raw_BA +
                                          AdumP + PdumP,
                                      na.action = na.omit, method = "REML", verbose = TRUE,
                                      correlation = corCompSymm (form = ~1|DyadN),
                                      data = cbind(MyData))
               fit             <- gls(Helping_AB ~  
                                          Intimacy_AB  + Intimacy_BA +
                                         # Listening.Raw_AB + Listening.Raw_BA +
                                         Listening_AB + Listening_BA +
                                          Speech_AB    + Speech_BA +
                                          AdumP + PdumP,
                                      na.action = na.omit, method = "REML", verbose = TRUE,
                                      correlation = corCompSymm (form = ~1|DyadN),
                                      data = cbind(MyData))
               
               fit             <- gls(Intimacy_AB  ~
                                          Listening_AB + Listening_BA +
                                          Speech_AB    + Speech_BA +
                                          AdumP + PdumP,
                                      na.action = na.omit, method = "REML", verbose = TRUE,
                                      correlation = corCompSymm (form = ~1|DyadN),
                                      data = cbind(MyData))

               # fit             <- gls(Intimacy_AB  ~
               #                            Listening_AB + Listening_BA +
               #                            (Listening_AB - Listening_BA) +
               #                            AdumP + PdumP,
               #                        na.action = na.omit, method = "REML", verbose = TRUE,
               #                        correlation = corCompSymm (form = ~1|DyadN),
               #                        data = cbind(MyData))
               
               round(data.frame(summary(fit)$tTable[2:7,]), 2)
               
             

APIM.from.SRM <- function(DV_AB, IV_AB, IV_BA, MyData) {
  
  
###Empty Model### 
#ActorN and PartnerN are the actor and partner numbers.  
#We turn these into factors or categorical variables.  

MyData$ActorN   <- as.factor(MyData$ActorN)
MyData$PartnerN <- as.factor(MyData$PartnerN)

#Then we create ng (g being the number of groups and n the number per group)
#dummy variables using model.matrix.   
MyData$Adum     <- model.matrix(~MyData$ActorN)
MyData$Pdum     <- model.matrix(~MyData$PartnerN)

#To avoid singularity we drop one dummy for Actor and we drop one per group for
#Partner. For this example, there are 27 groups of 4 persons and so 27 dummies 
#are dropped. 

MyData$AdumP     <- MyData$Adum[, -1]
#Note the parameter for REPlication is number of persons within group minus one.
MyData$PdumP     <- MyData$Pdum[, c( FALSE, rep(TRUE, dim(MyData$Pdum)[1]/
                                                 dim(MyData$Pdum)[2])) ]

#Run a gls model--summary(fit), not here, will give all the proper results
#except for proper SRM actor effect.
fit             <- gls(DV_AB ~  AdumP + PdumP,
                   na.action = na.omit, method = "REML", verbose = TRUE,
                   correlation = corCompSymm (form = ~1|DyadN),
                   data = cbind(MyData, 
                                DV_AB = MyData[[DV_AB]], 
                                IV_AB = MyData[[IV_AB]], 
                                IV_BA = MyData[[IV_BA]]))
# summary(fit)
#Extract only relevant estimates from fit
Variance        <- summary(fit)$sigma^2
Rho             <- coef(fit$model$corStruct,unconstrained=FALSE)
Partner.Effect  <- anova(fit)[3, ]

###Extract dfd from nlme object###
## split string at non-digits
s                  <- strsplit(attr(Partner.Effect, "label"), "[^[:digit:]]")
## convert strings to numeric ("" become NA)
solution           <- as.numeric(unlist(s))
## remove NA and duplicates
Partner.Effect.dfd <- unique(solution[!is.na(solution)])


###Calculate F and p for Rho######################
#Calculate number of groups (27 in Kenny's example)
ng              <- length(table(MyData$Group)); 
#Calculate number of person within group (4 in Kenny's example
##was change above and thus correct here)
np              <-  dim(MyData$PdumP)[1]/dim(MyData$PdumP)[2];
dfn             <-  round(((np-1)*(np-2)/2 -1)* ng, 1)
dfd             <-  round((np -1)*(np-2)*ng/2, 1)
F.Rho           <-  (1+as.numeric(coef(fit$model$corStruct,unconstrained=FALSE)))/
                    (1-as.numeric(coef(fit$model$corStruct,unconstrained=FALSE)))
plow            <- pf(F.Rho,dfn,dfd)
p               <- 2 * min(plow, 1 - plow)


###Tricking nlme to get proper dfs we prepare another set of dummy codes
###for SRM actor effect
MyData$PdumA     <-  MyData$Pdum[,-1]
MyData$AdumA     <-  MyData$Adum[, c( FALSE, rep(TRUE, dim(MyData$Adum)[1]/
                                                   dim(MyData$Adum)[2])) ]
fit             <- gls(DV_AB ~  AdumA + PdumA,
                      na.action = na.omit, method = "REML", verbose = TRUE,
                      correlation = corCompSymm (form = ~1|DyadN),
                      data = cbind(MyData, 
                                   DV_AB = MyData[[DV_AB]], 
                                   IV_AB = MyData[[IV_AB]], 
                                   IV_BA = MyData[[IV_BA]]))

Actor.Effect = anova(fit)[3, ]

s                <- strsplit(attr(Actor.Effect, "label"), "[^[:digit:]]")
solution         <- as.numeric(unlist(s))
Actor.Effect.dfd <- unique(solution[!is.na(solution)])


cat("        ############### Estimates for an empty model ##############\n
    SRM parameter\t\tEstimate\tDF\tF\tp\n
    ------------------------------------------------------------------------\n
    Relationship variance\t", 
      round(Variance, 3),"\n
    Dyadic reciprocity (Rho)\t", 
        round(Rho, 3), "\t\t", 
        paste0(dfn,",",dfd), "\t", 
        round(F.Rho, 2), "\t", 
        p, "\n
    Actor variance\t\t\t\t",   
        paste0(Actor.Effect$numDF,",", Actor.Effect.dfd), "\t", 
        round(Actor.Effect$`F-value`, 2), "\t", 
        Actor.Effect$`p-value`, "\n
    Partner variance\t\t\t\t", 
        paste0(Partner.Effect$numDF,",",Partner.Effect.dfd), "\t",
        round(Partner.Effect$`F-value`, 2), "\t", 
        Partner.Effect$`p-value`,
sep = "")

# 
fit             <- gls(DV_AB ~  IV_AB + IV_BA + AdumP + PdumP,
                       na.action = na.omit, method = "REML", verbose = TRUE,
                       correlation = corCompSymm (form = ~1|DyadN),
                       data = cbind(MyData, 
                                    DV_AB = MyData[[DV_AB]], 
                                    IV_AB = MyData[[IV_AB]], 
                                    IV_BA = MyData[[IV_BA]]))

#Extract only relevant estimates from fit
Variance        <- summary(fit)$sigma^2
Rho             <- coef(fit$model$corStruct,unconstrained=FALSE)
Partner.Effect  <- anova(fit)[5, ]

###Extract dfd from nlme object###
## split string at non-digits
s                  <- strsplit(attr(Partner.Effect, "label"), "[^[:digit:]]")
## convert strings to numeric ("" become NA)
solution           <- as.numeric(unlist(s))
## remove NA and duplicates
Partner.Effect.dfd <- unique(solution[!is.na(solution)])


###Calculate F and p for Rho######################
#Calculate number of groups (27 in Kenny's example)
ng              <- length(table(MyData$Group)); 
#Calculate number of person within group (4 in Kenny's example
##was change above and thus correct here)
np              <-  dim(MyData$PdumP)[1]/dim(MyData$PdumP)[2];
dfn             <-  ((np-1)*(np-2)/2 -1)* ng
dfd             <-  (np -1)*(np-2)*ng/2
F.Rho           <-  (1+as.numeric(coef(fit$model$corStruct,unconstrained=FALSE)))/
                    (1-as.numeric(coef(fit$model$corStruct,unconstrained=FALSE)))
plow            <-  pf(F.Rho,dfn,dfd)
p               <-  2 * min(plow, 1 - plow)


###Tricking nlme to get propoer dfs we prepare another set of dummy codes
###for SRM actor effect
MyData$PdumA     <-  MyData$Pdum[,-1]
MyData$AdumA     <-  MyData$Adum[, c( FALSE, rep(TRUE, dim(MyData$Adum)[1]/
                                                   dim(MyData$Adum)[2])) ]
fit             <- gls(DV_AB ~  IV_AB + IV_BA + AdumA + PdumA,
                       na.action = na.omit, method = "REML", verbose = TRUE,
                       correlation = corCompSymm (form = ~1|DyadN),
                       data = cbind(MyData, 
                                    DV_AB = MyData[[DV_AB]], 
                                    IV_AB = MyData[[IV_AB]], 
                                    IV_BA = MyData[[IV_BA]]))


Actor.Effect = anova(fit)[5, ]

s                <- strsplit(attr(Actor.Effect, "label"), "[^[:digit:]]")
solution         <- as.numeric(unlist(s))
Actor.Effect.dfd <- unique(solution[!is.na(solution)])


APIMeffects     <- data.frame(summary(fit)$tTable[2:3,])

cat("\n\n   ################# Estimates for an APIM model ############################\n
    Parameter\t\t\t\tEstimate\tSE\tt\tp\n
    ---------------------------------------------------------------------------\n
    APIM actor effect (AB on AB)\t", 
      round(APIMeffects [1, 1], 3), "\t\t",
      round(APIMeffects [1, 2], 3), "\t", 
      round(APIMeffects [1, 3], 2), "\t",
      APIMeffects [1, 4],"\n
    APIM partner effect (BA on AB)\t",
      round(APIMeffects [2, 1], 3), "\t\t",
      round(APIMeffects [2, 2], 3), "\t", 
      round(APIMeffects [2, 3], 2), "\t",
      APIMeffects [2, 4],"\n
    ---------------------------------------------------------------------------\n\n\n
   Conditional SRM parameter\t\tEstimate\tDF\tF\tp\n
    ---------------------------------------------------------------------------\n
    Relationship variance\t\t", 
      round(Variance, 3),"\n
    Dyadic reciprocity (Rho)\t\t", 
      round(Rho, 3), "\t\t", 
      paste0(dfn,",",dfd), "\t", round(F.Rho, 2), "\t", 
      p, "\n
    Actor variance\t\t\t\t\t",   
      paste0(Actor.Effect$numDF,",", Actor.Effect.dfd), "\t", 
      round(Actor.Effect$`F-value`, 2), "\t", 
      Actor.Effect$`p-value`, "\n
    Partner variance\t\t\t\t\t",
      paste0(Partner.Effect$numDF,",",Partner.Effect.dfd), "\t",
      round(Partner.Effect$`F-value`, 2), "\t", 
      Partner.Effect$`p-value`, "\n
    ---------------------------------------------------------------------------\n",
sep = "")

### Another option for printing, not complete. ###
# x <-  c("Relationship Variance",  round(Variance, 3))
# x <-  as.data.frame(rbind (x, c("Dyadic reciprocity (Rho)", round(Rho, 3))))
# colnames(x) <- c("Conditional SRM parameter", "Effect")
# rownames(x) <- NULL
# print(x)

}



######### Listening Raw Data 

APIM.from.SRM (DV_AB = "Intimacy.Raw_AB",
               IV_AB = "Listening.Raw_AB",
               IV_BA = "Listening.Raw_BA",
               MyData = Listening)
Listening$Intimacy.Raw_AB  <- as.numeric(scale(Listening$Intimacy.Raw_AB))
Listening$Listening.Raw_AB <- as.numeric(scale(Listening$Listening.Raw_AB))
Listening$Listening.Raw_BA <- as.numeric(scale(Listening$Listening.Raw_BA))

APIM.from.SRM (DV_AB = "Intimacy.Raw_AB",
               IV_AB = "Listening.Raw_AB",
               IV_BA = "Listening.Raw_BA",
               MyData = Listening)

######### Listening Relationship Scores from TripleR

# APIM.from.SRM (DV_AB = "Intimacy_AB",
#                IV_AB = "Listening_AB",
#                IV_BA = "Listening_BA",
#                MyData = Listening)
# 
# APIM.from.SRM (DV_AB = "Listening.Raw_AB",
#                IV_AB = "Intimacy.Raw_AB",
#                IV_BA = "Intimacy.Raw_BA",
#                MyData = Listening)

########## Kenny's APIM example data

# APIM.from.SRM ("Sociable_AB",
#                "Attractive_AB",
#                "Attractive_BA",
#                MyData = MyData)


