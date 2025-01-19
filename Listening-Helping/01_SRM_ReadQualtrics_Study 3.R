rm(list = ls())                              #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation for up to 10 digits

#--------------- Parameters 
Max.Miss <- 0               #No of missing Values allowed in a row 

# if TRUE, participants with uniform replies in ALL the fields would be removed
Rm_zero_var_replies <- TRUE 

#---------------- End of parameters input.


# IMPORTANT NOTE ABOUT THE INPUT DATA: 
# This code isn't suitable for reading the data from exported CSV file from 
# Qualtrics, because the data is formatted differently.
# For Transformation from csv to Qualtrics-like format, check the 
# Hekman's data analysis (on Humility)


# Read The Qualtics data:
# =======================

# Create the link to the survey results
# For creating a new survey file:
#      1. API2: change the user name according to the Qualtrics user
#      2. API3: Change the token from the = sign
#               Account settings
#      3. API5: copy the address of the current survey: from the distribution menu
#               in Qualtrics, probably begins with SV_ 

API1  <- "https://survey.Qualtrics.com//WRAPI/ControlPanel/api.php?API_SELECT=ControlPanel&Version=2.5&Request=getLegacyResponseData"
API2  <- "&User=sarit%40peryjoy.com&"
API3  <- "Token=6uSGbpue1UW7dfrCda6BJoauMyJFe2pyJI179N7o"
API4  <- "&Format=CSV&Labels=0&ExportTags=1"
API5_4participants  <- "&SurveyID=SV_0jpnKzMB4H97s7b"
API5_5participants  <- "&SurveyID=SV_8CDEeG3UG25xdEV"
API5_6participants  <- "&SurveyID=SV_79ftYSugu0a7ES9"

g.s.num <- 3  # Number of different group sizes available!!!!
group.size <- c(4,5,6) #group sizes


Link  <- paste0(API1, API2, API3, API4, API5_4participants)
Link  <- c(Link, paste0(API1, API2, API3, API4, API5_5participants))
Link  <- c(Link, paste0(API1, API2, API3, API4, API5_6participants))

INPUT <- list()

for(i in 1:g.s.num){
    all_content <- readLines(Link[i])
    skip_second <- all_content[-2]    #Qualtrics creates 2 header rows, and we want the second one.
    secondline  <- all_content[2]     # This is the desired header row
    
    #textconnection: read the data directly from skip_second instead of reading it from the library
    INPUT[[i]]       <- read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
    
}

#Clean the environment:
rm(list=setdiff(ls(),c( "INPUT","group.size","g.s.num")))


# Adjust current survey data
# ==========================

for(i in 1:g.s.num) {
    INPUT[[i]]       <- INPUT[[i]][, -c(1:11, ncol(INPUT[[i]]))]         #Delete Qualtrics variables
    
    colnames(INPUT[[i]]) [which(colnames(INPUT[[i]]) == "Group")] <- "group.id"
    
    # Set the Age column to the Age at when filling the form (in years)
    colnames(INPUT[[i]]) [which(colnames(INPUT[[i]]) == "Age_1")] <- "Age"
    INPUT[[i]]$Age            <- INPUT[[i]]$Age + 1939 
    INPUT[[i]]$Age            <- 2018 - INPUT[[i]]$Age 

    # Change all scales to start with 0 instead of 1
    NON.SCALE.VARIABLES <- c("group.id","Actor.id","Age")
    Non.Scale.columns   <- match(NON.SCALE.VARIABLES, names(INPUT[[i]]))
    Scale.Columns       <- 1:length(names(INPUT[[i]])) 
    Scale.Columns       <- Scale.Columns[-Non.Scale.columns]

    #Subtract one because Qualtics starts at 1 and not 0
    for (j in Scale.Columns) INPUT[[i]] [, j] <- INPUT[[i]] [, j] -1    
    
    # Replace F_F questions with F_H questions - an Error in the coding we made in qualtrics
    # Replace F_i questions with F_I questions - an Error in the coding we made in qualtrics
    colnames(INPUT[[i]]) <- gsub("F_F","F_H",colnames(INPUT[[i]]))
    colnames(INPUT[[i]]) <- gsub("F_i","F_I",colnames(INPUT[[i]]))

}

write.csv(INPUT[1], "Study2RawDataGroupSize4.csv")
write.csv(INPUT[2], "Study2RawDataGroupSize5.csv")
write.csv(INPUT[3], "Study2RawDataGroupSize6.csv")
# Data collection regarding the survey
DataReport              <- list()
DataReport$group.size   <- group.size
DataReport$initial.obs.no       <- c(nrow(INPUT[[1]]),nrow(INPUT[[2]]),nrow(INPUT[[3]]))
DataReport$initial.groups.no    <- c(length(table(INPUT[[1]]$group.id)),
                                     length(table(INPUT[[2]]$group.id)),
                                     length(table(INPUT[[3]]$group.id)) )



# Imputation for current survey
# =============================
    # in order to create the imputation files: 
    # 1. run the file but skip the coming imputation rows.
    # 2. files named "missing i" are created in your working directory for each group size
    # 3. These files contain all the rows with missing data.
    # 4. copy these files and for each file prepare the imputation file:
    #    a. remove the lines that you don't want to impute
    #    b. change the NAs to the new values
    #    c. remove the MISS column (last column in the file)
    # 5. copy the link to the imputation file for each group size and paste it here:
    # 6. now run all the file as usual.
    
    impute4  <- read.csv("https://www.dropbox.com/s/plriktzmy14hfgm/Impute4.csv?dl=1", header = TRUE)
    impute5  <- read.csv("https://www.dropbox.com/s/u51j3rx3sote3zb/Impute5.csv?dl=1", header = TRUE)
    
    impute4 <- impute4[which(!is.na(impute4$group.id)), ]
    impute5 <- impute5[which(!is.na(impute5$group.id)), ]
    DataReport$imputedRows <- nrow(impute4) + nrow(impute5)
    
    for(j in 1:nrow(impute4)) 
        INPUT[[1]][which((INPUT[[1]]$group.id == impute4$group.id[j]) & (INPUT[[1]]$Actor.id == impute4$Actor.id[j])),]<- impute4[j,]
    for(j in 1:nrow(impute5)) 
        INPUT[[2]][which((INPUT[[2]]$group.id == impute5$group.id[j]) & (INPUT[[2]]$Actor.id == impute5$Actor.id[j])),]<- impute5[j,]
    

# Debug and verification variables
x4 <- INPUT[[1]]
x5 <- INPUT[[2]]
x6 <- INPUT[[3]]


#Clean the environment:
rm(list=setdiff(ls(),c( "INPUT","group.size","g.s.num", "DataReport")))

# Ficticious data creation
# ========================

Create.Fictive.Data <- function (INPUT, group.size = 4, min.group.id = 100){
  
  # INPUT: the data file from Qualtrics after clean up. 
  #        it must have group.id and Actor.id columns.
  #        The Qualtrics creates entries that are coherent with the selected Actor.id.
  #        The function is built to create full round-robins from the 
  #        survey-test-data that Qualtrics creates.
  # group.size:     the size of the design round robin
  # min.group.id:  the number of minimum group.id that will be assigned to the groups. 
  
  # This function is based on random creation of data by Qualtrics.
  # The data creation of Qualtrics obviously doesn't create teams of team.size 
  #        according to the demands of round robin designs.
  
  # The Qualtrics creates entries that are coherent with the selected Actor.id 

      # The function transforms only the relevant group size data into fictious data.
      DataGroup <- INPUT[[group.size-3]]  
    
    
      DataGroup <- DataGroup[order(DataGroup$Actor.id),]
      row.names(DataGroup) <- NULL
      actor.freq <- table(DataGroup$Actor.id)
      
      if(length(actor.freq) != group.size) 
              {print("Data isn't sufficient to create groups of group.size"); exit()}
      
      ngroups <- min(actor.freq)
      
      Fict.Input <- DataGroup[1,]
      
      # Select data that matches the desired group size, with ngroups from each actor.id.
      first.row <- 1
      for (i in 1:group.size){
        #i <- 1
        Fict.first.id.row <- 1 + (i-1) * ngroups
        fict.last.id.row <- i * ngroups
        # add ngroups lines from each actor.id
        Fict.Input[Fict.first.id.row : fict.last.id.row,] <- 
                DataGroup[first.row : (first.row + ngroups - 1),]    
        first.row <- first.row + actor.freq[i]
      }
      
      # Now we'll set the numbers of the groups to match group size
      Fict.Input$group.id <- 
              rep(min.group.id : (min.group.id + ngroups - 1), group.size)
      return(Fict.Input)
  
  
}# Create.Fictive.Data


##################  FICTICIOUS DATA:
##################
##################
##################  If you wish to crete FICTICIOUS DATA  - mark and run the line below. 
##################                        reccomendation: don't erase the remarks signs.
##################  INPUT <- Create.Fictive.Data(INPUT = INPUT)
##################
##################
#INPUT[[1]] <- Create.Fictive.Data(INPUT = INPUT[[1]])

# drop missing round robins, or groups with too much missing data,
#    or participants with zero variance in their replies
# ===============================================================
# Debug and verification variables
x4 <- INPUT[[1]]
x5 <- INPUT[[2]]
x6 <- INPUT[[3]]

for (i in 1:g.s.num) {
    
    NON.SRM.VARIABLES <- c("group.id","Actor.id","Age", "Gender")
    Non.SRM.columns   <- match(NON.SRM.VARIABLES, names(INPUT[[i]]))
    SRM.Columns       <- 1:length(names(INPUT[[i]])) # temp vector
    SRM.Columns       <- SRM.Columns[-Non.SRM.columns]
    SRM.Q.num         <- length(SRM.Columns) / group.size[i]
    
    INPUT[[i]]      <- INPUT[[i]][which(!is.na(INPUT[[i]]$group.id)), ]

    # Handle missing replies
        INPUT[[i]]$MISS <- apply(INPUT[[i]][, SRM.Columns], 1, 
                                 function(x) sum(is.na(x)))
        missing <-  INPUT[[i]][which(INPUT[[i]]$MISS > (SRM.Q.num + Max.Miss)),] 
        write.csv(missing,paste0("missing",i,".csv"))
        DataReport$lines.dropped.because.missing.responeses[i] <- nrow(missing)
        INPUT[[i]]      <- INPUT[[i]][which(INPUT[[i]]$MISS <= 
                                                (SRM.Q.num + Max.Miss)), ] 
 
    # Handle replies with zero variance
        INPUT[[i]]$actorVar <- apply(INPUT[[i]][,SRM.Columns], 1, 
                                     var, na.rm = TRUE)
        DataReport$zero.Variance.Participants[i] <- 
            length(which(INPUT[[i]]$actorVar == 0))
        DataReport$zero.var.in.ngroups[i] <- length(unique(
            INPUT[[i]][which(INPUT[[i]]$actorVar == 0),"group.id"]))
        DataReport$groups.zero.var.replies[i] <- 
            paste(unique(INPUT[[i]][which(INPUT[[i]]$actorVar == 0),"group.id"]),
                  collapse = ",")
        if (Rm_zero_var_replies) 
            INPUT[[i]] <- INPUT[[i]][which(INPUT[[i]]$actorVar != 0), ]
  
    # Handle incomplete round-robins (with at least 4 persons)
        x   <- as.data.frame(table (INPUT[[i]]$group.id))         
    
        DataReport$downsized.groups[i]  <- length(which((x$Freq < group.size[i])
                                                        & (x$Freq >= 4)))
        DataReport$incomplete.RR[i]     <- length(which(x$Freq < 4))
        DataReport$a
        x       <- x[which(x$Freq >= 4), ]              
        # Next row is redandent because Qualtrics checks that group number is numeric.
        #x$valid <- as.numeric(as.character(x$Var1))   
        INPUT[[i]]   <- INPUT[[i]][which(INPUT[[i]]$group.id  %in% x$Var1), ] 
        DataReport$analysis.group.no[i] <- length(table(INPUT[[i]]$group.id))
        DataReport$analysis.participants.no[i] <- nrow(INPUT[[i]])
}


#Clean the environment:
rm(list=setdiff(ls(),c( "INPUT","group.size","g.s.num", "DataReport")))

# Transform each SRM question with all it's answers into separate dataframes 
# in long-format suitable for SRM-RR analysis
# ------------------------------------------------------------------------------
library(ggplot2)                                    # Loading for TripleR required package: ggplot2
library(TripleR, suppressPackageStartupMessages() ) # Suppress "Loading required package: ggplot2";
#help(package='TripleR')
library (data.table, suppressPackageStartupMessages()) # For rbindlist



longTOwide    <- function (OUT, RANGE, method = "OMIT") {
  
  #This function collects all the answers for a givven specific question.
  # The output of the function is a data frame, ready to use for RR analysis
  # Under the OUT name (which is the Question's name). 
  # The output data frame consists of 4 variables:
  #       group.id, actor-partner ids (combined with the group.id), 
  #       and the actor's response (value)
  # The number of rows is:
  #       number of groups * actors per group * partners per group.
  # so, for example, if we have 4 members in a group, and 20 groups, 
  #       then the number of rows will be: 20*4*4 = 320.
  # every question will go through the same process creating a data frame of it's own.
  # order: sort : The matrix2long function that we use, sorts the files 
  #       by partner and then by actor. So it's ready for the RR analysis
  # OUT: The name of the output. Usually the question name
  # RANGE: a list of the the columns in INPUT relevant to the question
  # method = IMPUTE add lines of NAs for missing participants
  # method = OMIT     remove answers regarding missing participants

   
  for (i in 1:g.s.num) {
      groups <- unique(INPUT[[i]]$group.id)
      INPUT[[i]] <- INPUT[[i]][order(INPUT[[i]]$Actor.id),]  # matrix2long requires sorted data

      for (g in groups) {
          # Create a matrix for each group and each variable
          x <- INPUT[[i]] [which(INPUT[[i]]$group.id == g) ,RANGE[[i]]]
          
          if (nrow(x) < group.size[i]) {         # missing participants
              if (method == "IMPUTE") {
                  miss.act <- group.size[i] - nrow(x)
                  for (m in 1:miss.act) x <- rbind(x, rep(NA,ncol(x)))
              } #IMPUTATION
              if (method == "OMIT"){
                  print(paste("IMPUTATION IN PROGRESS.group = ",g))
                  actors <- INPUT[[i]]$Actor.id[which(INPUT[[i]]$group.id == g)]
                  miss.act <- which(!(1:group.size[i] %in% actors))
                  x <- x[,-miss.act]
              } #OMIT
              
          }# missing participants
          
          y <- matrix2long (x)                  # Convert each matrix to long format
          y$group.id <- rep(g,nrow(y))          # Copy the group ID into the matrix
          y <- y[,c(4,1:3)]
          assign (paste0 ("FILE", g,i),  y)     # Export the matrix
  }} # for all groups' sizes

  # Merge all the matrixes together
  FILE                 <- rbindlist (mget(ls(pattern = "FILE")), fill = TRUE)   
  # convert actor and partner id to the expected format
  FILE$actor.id        <- paste0(FILE$group.id,  ":",  FILE$actor.id)   
  FILE$partner.id      <- paste0(FILE$group.id,  ":",  FILE$partner.id)
  FILE$value           <- as.numeric(as.character(FILE$value))
  # Set the question name to contain the FILE result in the global environment                 
  assign (OUT, FILE, envir=.GlobalEnv)                           
} #longTOwide

# Find the relevant questions for each construct. 
#   The convention is that each question is named with the following: 
#   Actor.id_Construct_number.of.question.in.the.construct.
#   For example: 
#       B_L_1 Stands for: 
#          B  -   The actor.id that the answer reffers to is B 
#          L  -   This is a question about Listening 
#          1  -   This is the first of the questions of Listening.


Questions.prefix <- c("L","I", "S", "H")
Questions.Number <- c(4,3,3,3)
Answers.Files.List <- NULL

Actor.id.letters <- list()
for (i in 1:g.s.num) Actor.id.letters[[i]] <- LETTERS[1: group.size[i]]

# The following line is required mainly for mediation analysis
# it attaches the id (group+Actor.id) to each line.
for (i in 1:g.s.num) INPUT[[i]]$id <- as.factor(paste0(INPUT[[i]]$group.id,  ":",  INPUT[[i]]$Actor.id))

for (i in 1 : length(Questions.prefix)) {       # For each Constructs
  for (j in 1 : Questions.Number[i]) {          # For each Question in the construct
     
       Specific.Questions.Columns <- vector(mode="list", length=g.s.num)
      
      for (m in 1:g.s.num) {                    # For each group.size
          
          # Prepare the list of columns of the round-robin of the current question.
          #Specific.Questions.Columns[[m]] <- NULL
          for (a in Actor.id.letters[[m]]) {    # For each actor
              
              # i <- 1; j <- 2; m<- 1; a <- "B"
              Specific.Questions.Columns[[m]] <-        
                  c( Specific.Questions.Columns[[m]],
                     which(
                         substr( colnames( INPUT[[m]]),1,3) == 
                             paste0(a,"_",Questions.prefix[i]))[j])
              
          }
      }
       longTOwide(paste0(Questions.prefix[i],j),Specific.Questions.Columns, method = "OMIT")
       Answers.Files.List <- c(Answers.Files.List, paste0(Questions.prefix[i],j))
  }
}
# Debug and verification variables
x4 <- INPUT[[1]]
x5 <- INPUT[[2]]
x6 <- INPUT[[3]]

DataReport$MeanAge          <- mean(c(INPUT[[1]]$Age, INPUT[[2]]$Age,INPUT[[3]]$Age), na.rm = TRUE)
DataReport$SDAge            <- sd(c(INPUT[[1]]$Age, INPUT[[2]]$Age,INPUT[[3]]$Age), na.rm = TRUE)
DataReport$FemalePercent    <- round (100 * sum(c(x4$Gender, x5$Gender,x6$Gender) %in% 0 )
                                      / sum (length(x4$Gender),length(x5$Gender),length(x6$Gender)),2)

# clean the environment
rm(list = "a", "i","j","m","Actor.id.letters","group.size","Specific.Questions.Columns", "longTOwide")

DataReport$groups.no <- sum(DataReport$analysis.group.no)
DataReport$participants.no <- sum(DataReport$analysis.participants.no)

DataReport

save.image("StudyData")
