

# SRM - RR ANALYSIS - Mediation
# ==============================================================================

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# Before running this section: Run the data creation processes, according to the README file

load("StudyReadyforAnalaysis")

EFFECTS <- getEffects(~ actor.id*partner.id | group.id, data = x, varlist = c("LP1/LP2", "IP1/IP2", "SP1/SP2", "HP1/HP2"))
names(EFFECTS) <- gsub("LP1","Listening", names(EFFECTS))
names(EFFECTS) <- gsub("IP1","Intimacy", names(EFFECTS))
names(EFFECTS) <- gsub("SP1","Speech", names(EFFECTS))
names(EFFECTS) <- gsub("HP1","Helping", names(EFFECTS))

# Attach the mediators (currently: gender, age) to the EFFECTS according to the id
id.mediators <- data.frame()
for(i in 1:g.s.num) id.mediators <- rbind(id.mediators, INPUT[[i]][,c("id", "Gender", "Age")])
EFFECTS   <- merge(EFFECTS, id.mediators,  by = "id")
EFFECTS$Gender.group.pcent <- EFFECTS$Gender - ave(EFFECTS$Gender, EFFECTS$group.id)
EFFECTS$Age.group.pcent    <- EFFECTS$Age    - ave(EFFECTS$Age,    EFFECTS$group.id)

library(apaTables)

apa.cor.table(EFFECTS[,c("Gender", "Age", "Gender.group.pcent", "Age.group.pcent",
                         "Listening.a", "Listening.p", "Intimacy.a" , "Intimacy.p", 
                         "Speech.a"   , "Speech.p","Helping.a"  , "Helping.p")],
              show.conf.interval = FALSE)

# apa.cor.table(EFFECTS[,c("Gender", "Age", "Listening.a", "Listening.p")],show.conf.interval = FALSE)
# apa.cor.table(EFFECTS[,c("Gender", "Age", "Intimacy.a" , "Intimacy.p")] ,show.conf.interval = FALSE)
# apa.cor.table(EFFECTS[,c("Gender", "Age", "Speech.a"   , "Speech.p")]   ,show.conf.interval = FALSE)
# apa.cor.table(EFFECTS[,c("Gender", "Age", "Helping.a"  , "Helping.p")]  ,show.conf.interval = FALSE)


# save.image("StudyReadyforAnalaysis")
