rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))
ipak(c("tidyverse"))
# The code aims to produce SRM data with the following attributes: 
#   - There are perceiver, target and relationship variances
#   - BiVariate data (x,y) both with two items (x1, x2, y1, y2)
#   - A third variable z exists. z is not part of the data collection or analysis.
#   - The effect of x on y varies, for each perceiver, according to z, and might be 
#     in different directions.
#   - An example: 
#     x - russian accent
#     z - gender
#     y - liking
#     People's liking is affected by the combination of accent and gender, such 
#     that some people like women with russian accent but not men,
#     and some the opposite.
# Results:
#     The case is definitely possible.
#     We only look for linear correlations, and the correlation described between
#     x and y isn't linear (because of z).
#     I didn't manage to produce it, but it should happen. 
#     In fact, one of the reasons for dyadic variance I mention in my dissertation
#     is just that: the non linear connection between x and y, pushes otherwise 
#     linear connections into a dyadic variance.


# sample parameters
k     <- 20   # number of groups
ng    <- 5    # number of people per group
n     <- k*ng # sample size
vmin  <- 0    # minimum rating of xs
vmax  <- 11   # maximum rating of xs
mean1 <- 6
mean2 <- 5
sd1   <- 3
sd2   <- 3

# create a data set (ind) of individual attributes
#   each row is an individual in the study with the following attributes:
#   attributes as target:
#   v1, v2 - two indicators of x1, x2 at the individual level (the "true" x1,x2)
#   z      - the hidden attribute
#   attributes as perceiver: 
#   likes01   - 0 or 1 for liking high x of individuals with  z==0/1 respectively
#   percBias - some individual acquiescence bias (in order to gain perceiver variance)

  ind <- data.frame(matrix(nrow = n, ncol = 0))
  ind$v1 <- rnorm(n, mean = mean1, sd = sd1)
  ind$v2 <- ind$v1 * (1 + runif(n, min = -0.1, max = 1.2))
  cor(ind$v1, ind$v2)

  #ind$z <- rnorm(n, mean = mean2, sd = sd2)
  ind$z <- round(runif(n)) # 0 or 1 for categorical variable
  ind <- round(ind, 0)
  #ind[ind > vmax] <- vmax ; ind[ind < vmin] <- vmin
  ind$v <- rowMeans(ind[, c("v1", "v2")], na.rm = TRUE)
  # ind$vtimesZ <- ind$v * ind$z
  # ind$vtimesZ <- ind$z
  ind$likesz1 <- round(runif(n)) # 0 or 1 for liking high x when z==1
  ind$likesz0 <- round(runif(n)) # 0 or 1 for liking high x when z==0
  ind$percBias <- runif(n, min = -0.001, max = 0.01)

  ind$person <- rep(1:ng)
  ind$group.id <- rep(1:k, each = ng)

  # Create a long format SRM rating data  
  d <- data.frame(matrix(nrow = k*ng*ng, ncol = 0))
  d$group.id <- rep(1:k, each = ng*ng)
  d$perceiver <- rep(rep(1:ng, each = ng),k)
  d$target <- rep(rep(1:ng, ng), k)
  idColsx <- c("group.id", "person")
  d <- left_join(d, ind[,c(idColsx, "likesz0", "likesz1", "percBias")], 
                 by = c("group.id", "perceiver" = "person"))
  d <- left_join(d, ind[, c(idColsx, "z", "v1", "v2") ], 
                 by = c("group.id", "target" = "person"))
  
  # prepare dyadic raw ratings of x based on v and the perceiver bias
  d$x1 <- d$v1 * d$percBias * (1+ runif(nrow(d), min = -0.3, max = 1.5))
  d$x2 <- d$v2 * d$percBias * (1+ runif(nrow(d), min = -0.3, max = 1.5))
  d$x <- rowMeans(d[,c("x1", "x2")])
  cor(d$v1, d$x1); cor(d$v2, d$x2)
  cor(d$x1, d$x2)
  
  # reverse the score of v-target for disliking perceivers
  #d$vtimesZ[d$likes == 0] <- max(d$vtimesZ) + min(d$vtimesZ) - d$vtimesZ[d$likes == 0]
  # d$vtimesZ <- d$z
  d[,c("vz1", "vz2")] <- d[,c("v1", "v2")]
  rows2reverse <- which((d$z == 0 & d$likesz0 == 0) | (d$z == 1 & d$likesz1 == 0))
  d[rows2reverse, c("vz1", "vz2")] <- 
    apply(d[rows2reverse, c("v1", "v2")], 2, 
          function(dc) {
            if(min(dc < 0)) dc <- dc - min(dc)
            dc <- max(dc)+ min(dc) - dc
            return(dc)})
  d$vz <- rowMeans(d[,c("vz1", "vz2")])
  # prepare ratings of y based on vz and the perciver bias
  d$y1 <- d$vz * d$percBias * (1+ runif(nrow(d), min = -0.1, max = 1.2))
  d$y2 <- d$vz * d$percBias * (1+ runif(nrow(d), min = -0.1, max = 1.2))
  d$y <- rowMeans(d[, c("y1", "y2")])
  cor(d$vz, d$y1)
  cor(d$vz, d$y2)
  cor(d$x, d$y)

d[d$perceiver == d$target, c("x1", "x2", "y1", "y2")] <- NA
d$p.id <- paste0(d$group.id, ":", d$perceiver)
d$t.id <- paste0(d$group.id, ":", d$target)


d <- as.data.frame(d)
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))
RR.style("perception")

fit <- RR(x1/x2 + y1/y2     ~ p.id*t.id | group.id, data = d, na.rm = TRUE)
fit
