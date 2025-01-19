rm(list = ls())                              #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          #Avoid scientific notation for up to 10 digits

# Parameters and utility functions ---------------------------------------------

    minGroupSize <- 4       # No of minimum members in a group (usually 4)
    Max.Miss     <- 100     # No of missing Values allowed in a row. A large 
                            # number means tripleR SRM analysis handles missing.
    
    # if TRUE, participants with uniform replies in ALL the fields would be removed
    Rm_zero_var_replies <- FALSE 
    
    load("main.RImage")
    
######---------------- End of parameters input.


# ABOUT THE INPUT DATA: 
# =====================
# This code is adjusted for reading the data from exported CSV file from Qualtrics.
# 
# The research includes data from three different surveys: 
# HumilityJDCEnglish_October272021.csv  - collected from JDC team members that
#                                         chose to answer the survey in English
# HumilityJDCHebrew_October272021.csv   - collected from JDC team members that
#                                         chose to answer the survey in Hebrew                                   
# HumilityFirms_October272021.csv       - collected from different Israeli firms.
#                                         see the paper for details. 
# All these surveys are joined into a single dataset.                                                                            


    
    ipak("tidyverse")

# Gather surveys data             ----------------------------------------------
 
    inputEng <- read.csv("HumilityJDCEnglish_October272021.csv", header = TRUE, 
                         stringsAsFactors = FALSE)[-c(1:2),]
    inputHeb <- read.csv("HumilityJDCHebrew_October272021.csv", header = TRUE,
                         stringsAsFactors = FALSE)[-c(1:2),]
    inputFirms <- read.csv("HumilityFirms_October272021.csv",
                           header = TRUE, stringsAsFactors = FALSE)[-c(1:2),]
    # add language indication to the Hebrew surveys
    inputFirms$Lang <- inputHeb$Lang <- 2
    # add the language indication column in the right position
    inputFirms <- select(inputFirms, all_of(names(inputEng)))
    inputHeb <- select(inputHeb, all_of(names(inputEng)))
    x <- as.data.frame(rbind(inputEng, inputHeb, inputFirms))
    x$monthRecorded <- months.Date(as.Date(x$StartDate)) #temporarily required to identify wrong assignment of group id.
    x$monthRecorded <- match(x$monthRecorded, month.name)
    x <- select(x, -c(StartDate:UserLanguage))   #Delete Qualtrics variables 
    x <- rename(x, group.id = Group)
    x <- x[which(x$group.id != ""),]             #Delete Rows w/o group id = choose heb language which leaves an empty row in the English survey.
    x <- as.data.frame(apply(x, 2, as.numeric))  #Qualtrics' export turn all to characters
    x$Lang[is.na(x$Lang)] <- 1                   #In English one could avoid chosing language. 2 participants did.
# Prepare SRM meta data (specify actor column etc)    

    #merge the actors column id into one column. 
    #There are several columns due to different group sizes.
    x$Actor <- apply(select(x, starts_with("Letter")), 1, 
                     function(m) sum(m, na.rm = TRUE))
 
    #merge the know columns (indicate the knowing level of the actor towards 
    #      the partner) into a single column per partner.
    for (i in 1:6) { #for each partner
      knCol <- grep(paste0("Know.\\_", i), colnames(x))
      if (length(knCol) > 1) {
        x$know <- apply(x[,knCol], 1, function(m) sum(m, na.rm = TRUE))
      } else 
        if (length(knCol) == 1) x$know <- x[,knCol]
        colnames(x) <- gsub("know", paste0("Know_", LETTERS[i], "_1"), colnames(x), ignore.case = FALSE)
        x <- select(x, -any_of(knCol))
    }
    
    x <- select(x, group.id, gSize, Actor, everything(), -starts_with("Letter") )

# Adjust current survey data ----------------------------------------------
    # Fix a specific mistake in group id:
    x[!is.na(x$group.id) & x$group.id == 707 & x$monthRecorded == 7, "group.id"] <- 708
    x <- select(x, -monthRecorded)
    #There was a mixup in the groups' numbers. so group 892=219=812.
    x$group.id[which(x$group.id == 219 | x$group.id == 812)] <- 892
    x$group.id[which(x$group.id == 204)] <- 240
    
    # remove a response of participant B in group 817.
    # The group consists of 5 persons, she answered regarding 4 participants, and
    # then she re-filled the questionnaire for all her group.
    x <- x[ - which(x$group.id == 817 & x$Actor == 2 & x$gSize == 4),]
    
    #remove group 111, 112 which were test groups for the Israeli firms survey
    x <- x[-which(x$group.id == 111 | x$group.id == 112), ]
    
    rownames(x) <- NULL 
    #remove one of the double entries of group 113 actor #4
    #we choose the one with less missing values (The person began to fill 
    #   the survey, quitted, and then filled it again using a different computer)
    m <- apply(x[x$group.id == 113 & x$Actor == 4,], 1, function(a) sum(is.na(a)))
    m <- as.numeric(names(m[which(m == max(m))]))
    x <- x[-m,]

# Convert data to long format --------------------------------------------------

    ignoredVariables  <- c(grep("Self_Hum_.", colnames(x), value = TRUE),
                           "Age", "Gender", "Mother_Ton", "Tenure", "Lang")
    NON.SRM.VARIABLES <- c("group.id", "gSize","Actor")
    SRM.Columns <- names(x)[!names(x) %in% c(NON.SRM.VARIABLES, ignoredVariables)]
    SRM.Q.num         <- 13 
        
    longInput <- x %>% haven::zap_label() %>% 
      gather(key = "Question", value = "Value", SRM.Columns) %>% 
      extract("Question", c("Const", "Partner", "item"), "([[:alnum:]]+)_(.)_(.)") %>% 
      mutate(item = paste0(Const, item)) %>% 
      select(-Const) %>% 
      mutate(Partner = match(Partner, LETTERS)) %>% 
      mutate(p.id = paste0(group.id, ":", Partner), 
             a.id = paste0(group.id, ":", Actor)) %>% 
      group_by(item) %>%
      spread(key = "item", value = "Value") %>% 
      rename(NonTalk = Introv1, Shy = Introv2) %>% 
      rename(Know = Know1) 
   
    NON.SRM.VARIABLES <- c(NON.SRM.VARIABLES, "Partner", "p.id", "a.id", "Know")
    SRM.Columns <- names(longInput)[!names(longInput) %in% 
                                      c(NON.SRM.VARIABLES, ignoredVariables)]
    
    # locate the empty rows out of the diagonal, and delete them
    tmp <- which((longInput$Actor != longInput$Partner) & 
                             (apply(longInput[, SRM.Columns], 1, 
                                    function(m) all(is.na(m))) ))
    longInput <- longInput[-tmp,]
    
    # reverse two items - Introversion1 (Non Talkative) and Safe #3
    longInput$NonTalk <- 10 - longInput$NonTalk
    longInput$Safe3 <- 10 - longInput$Safe3
    
    longInput <- select(longInput, a.id, p.id, SRM.Columns, everything())
    wideInput <- x
    
    # Clean the environment
    rm(list=setdiff(ls(),c( "longInput", "wideInput", "SRM.Columns")))
    load("main.RImage")

save.image(imgName(".RawData"))

