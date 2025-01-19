a <- data.frame(p = rep(1:4, 8), t = rep(5:6, 16))
a <- as.data.frame(apply(a, 2, as.factor))
b <- as.data.frame(model.matrix(~a$p))
m <- cbind(a,b[,-1])
