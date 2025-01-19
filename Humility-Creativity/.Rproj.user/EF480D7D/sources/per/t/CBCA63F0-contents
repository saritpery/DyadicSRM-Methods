

# SRM - RR ANALYSIS PREPERATIONS #2
# ==============================================================================

# Prepare parcels and Constructs from items
# -------------------------------------------

# We use the tripleR package, that can only handle 2 items per construct 
# at a time! Hence we need to prepare parcels so that each construct can be 
# described by 2 parcels.

# Note regarding the order/sort of the data: 
# The data needs to be sorted by partner and then by actor for the SRM analysis.

rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

Alpha.Check <- FALSE  # Turn to TRUE in the beginning of work, to verify that the items are right

load("main.RImage")
load(imgName(".FullRR.RawData"))

ipak("tidyverse")
if (Alpha.Check) {
    ipak(c("apaTables", "psych"))
    const <- c("Contrib", "Cre", "Hum", "Introv", "Safe")
    for (cst in const) {
        if (cst == "Introv") alphCheckDF <- longInput[, c("NonTalk", "Shy")] else
            alphCheckDF <- longInput[, grep(paste0("^",cst), names(longInput))]
        apa.cor.table(alphCheckDF, show.conf.interval = FALSE)
        print(psych::alpha(alphCheckDF))
    }
    # alpha check for self-humility items: 
    alphCheckDF <- as.data.frame(select(wideInput, starts_with("Self_Hum")))
    apa.cor.table(alphCheckDF, show.conf.interval = FALSE)
    psych::alpha(alphCheckDF)
}



# Parceling choices:
# 1. We drop item #3 from the psychological safety because it is not part of the
#    construct as seen in the alpha test. A closer look might reveal that indeed
#    item #3 is fundamentally different.
# 2. Shy and non-talkative does not build introversion scale (or any joined scale)
#    again according to the alpha test.
# ---------------------------
x <- rowwise(longInput) %>% 
    mutate(H1 = mean(c(Hum1, Hum3), na.rm = TRUE),
           H2 = Hum2,
           Creat1 = mean(c(Cre1, Cre3), na.rm = TRUE),
           Creat2 = Cre2,
           Sf1 = Safe1,
           Sf2 = Safe2,
           Shy = Shy,
           NonTalk = NonTalk,
           HSelf = mean(c(Self_Hum_1, Self_Hum_2, Self_Hum_3), na.rm = TRUE)) %>% 
    mutate(Hum.raw = mean(c(Hum1, Hum2, Hum3), na.rm = TRUE),
           Creat.raw = mean(c(Cre1, Cre2, Cre3), na.rm = TRUE),
           Safety.raw = mean(c(Safe1, Safe2), na.rm = TRUE),
           Contr.raw = mean(c(Contrib1, Contrib2), na.rm = TRUE),
           Shy.raw = Shy,
           NonTalk.raw = NonTalk,
           Introv.raw = mean(c(Shy, NonTalk), na.rm = TRUE))

x <- x[order(x$p.id,x$a.id),]

x <- select(x, a.id, p.id, H1, H2, Creat1, Creat2, Contrib1, Contrib2, Sf1, Sf2, 
            NonTalk, Shy, HSelf, everything())

# clean the environment  
rm(list = setdiff(ls(), c("longInput", "wideInput", "x")))
load("main.RImage")

save.image(imgName(".FullRR.ReadyforAnalaysis"))


