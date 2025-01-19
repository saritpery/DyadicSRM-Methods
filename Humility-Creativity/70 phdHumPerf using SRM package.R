## This source code is based on code licensed under a CC-BY4.0 license 
## (see https://creativecommons.org/licenses/by/4.0/)

# In this file:   ==============================================================
# The SRM package provides SRM common outputs. 
# This file tries to discover the effects per person/dyad 
# generated in the process (and understand the process better)
# in order to enable second-stage analyses. 
# It still doesn't work :(


rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

x <- read.csv("JDC4SOREMOAnalysis.csv")

ipak (c("srm", "lavaan", "TripleR", "ggplot2", "data.table", "apaTables", 
        "psych", "tidyverse"))

# source("SRM_OPTIMIZER_INTERNAL.R")
# source("SRM_OPTIMIZE.R")
# x <- as.data.frame(longInput)
# 
# # Prepare a long format file for SOREMO analysis by Tom
# x <- select(x, group.id, Actor, Partner, Contrib1:Safe3)
# x <- rename(x, nonTalk = Introv1, shy = Introv2)

x <- as.data.frame(x)
srmCols <- c("Hum1", "Hum2", "Hum3", "Cre1", "Cre2", "Cre3",
                "Safe1", "Safe2", "Safe3", "Contrib1", "Contrib2")

# srm package requirements of the data: 
# 1. long format
# 2. Actor, partner id as numeric values of 1 to group size
# 3. unique group id.
# 4. Actor and maybe Partner columns should have numbers of 1 to real actor/partner numbers !!!!!

# center each srm column to enhance convergence (srm package orders...)
x[, srmCols] <- apply(x[, srmCols], 1, 
                        function(m) scale(m, center = TRUE, scale = FALSE))

# keep only full 4 groups (maybe the 4 isn't required)
x <- x[(x$Actor < 5 & x$Partner < 5),]
gn <- x |> group_by(group.id) |> summarise(count = n())
gn <- gn[gn$count == 12,]
x <- x[x$group.id %in% gn$group.id,]

# x <- x[x$group.id != 323, ]
#- we fit the cfa:

m_cfa <- '%Person
	      SafeF@A =~ 1*Safe1@A + Safe2@A + Safe3@A 
	      SafeF@P =~ 1*Safe1@P + Safe2@P + Safe3@P 
	      SafeF@A ~~ SafeF@P
	            
	      %Dyad
	      SafeF@AP =~ 1*Safe1@AP + Safe2@AP + Safe3@AP
	      SafeF@PA =~ 1*Safe1@PA + Safe2@PA + Safe3@PA
	      SafeF@AP ~~ SafeF@PA
	      '
#m <- x[x$gSize == 4, ]

# group.var is the group.id parameter. still not sure what is rrgroup_name
fit_cfa <- srm( model.syntax = m_cfa, data = x, group.var = "group.id",
                rrgroup_name = "group.id",
                optimizer = "srm", maxiter = 300, 
                person_names = c("Actor", "Partner"),
                conv_par = 1e-04, do_line_search = TRUE )
summary( fit_cfa ) 

methods(class(fit_cfa))
newsrm <- edit(srm)

semPaths(fit_cfa, "std", 
         rotation = 1, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         cardinal = FALSE)

#- we fit the path model:

m_path <- '
		  %Person
		  FN@A =~ 1*n3@A
		  FN@P =~ 1*n3@P
		  FA@A =~ 1*a3@A
		  FA@P =~ 1*a3@P

		  FA@P ~ FN@A + FN@P
		  FA@A ~ FN@A + FN@P
		  
		  n3@A ~~ 0*n3@A + 0*n3@P
		  n3@P ~~ 0*n3@P
			
		  a3@A ~~ 0*a3@A + 0*a3@P
		  a3@P ~~ 0*a3@P

		  %Dyad
		  FN@AP =~ 1*n3@AP
		  FN@PA =~ 1*n3@PA
		  FA@AP =~ 1*a3@AP
		  FA@PA =~ 1*a3@PA
		  n3@AP ~~ 0*n3@AP + 0*n3@PA
		  n3@PA ~~ 0*n3@PA
		  a3@AP ~~ 0*a3@AP + 0*a3@PA
		  a3@PA ~~ 0*a3@PA
		  '

#-- estimate model
fit_path <- srm( model.syntax = m_path, data = x, rrgroup_name = "Group", 
                 optimizer = "srm", maxiter = 300, 
                 conv_par=1e-4, do_line_search = TRUE )
summary( fit_path ) 
fit_path
# the effects are probably held in 

# fit_path$parm.table 