require(fitdistrplus)

data.set <- read.csv("data.set")
count.A <- subset(data.set, category == "A")
count.B <- subset(data.set, category == "B")



# All Data

plotdist(gen.var.trans.pot$spores.rounded, histo = TRUE, demp = TRUE)

spores.nb <- fitdist(gen.var.trans.pot$spores.rounded, "nbinom")
plot(spores.nb)
LL.nb <- logLik(spores.nb)

spores.p <- fitdist(gen.var.trans.pot$spores.rounded, "pois")
plot(spores.p)
LL.p <- logLik(spores.p)

cdfcomp(list(spores.p, spores.nb),legendtext = c("Poisson", "negative binomial"))

gofstat(list(spores.p, spores.nb),fitnames = c("Poisson", "negative binomial"))

# Group A

plotdist(count.A$count, histo = TRUE, demp = TRUE)

A.count.nb <- fitdist(count.A$count, "nbinom")
plot(A.count.nb)
A.LL.nb <- logLik(A.count.nb)

A.count.p <- fitdist(count.A$count, "pois")
plot(A.count.p)
A.LL.p <- logLik(A.count.p)

cdfcomp(list(A.count.p, A.count.nb),legendtext = c("Poisson", "negative binomial"))

gofstat(list(A.count.p, A.count.nb),fitnames = c("Poisson", "negative binomial"))

# Group B

plotdist(count.B$count, histo = TRUE, demp = TRUE)

B.count.nb <- fitdist(count.B$count, "nbinom", method = "mle")

plot(B.count.nb)
B.LL.nb <- logLik(B.count.nb)

B.count.p <- fitdist(count.B$count, "pois")
plot(B.count.p)
B.LL.p <- logLik(B.count.p)

cdfcomp(list(B.count.p, B.count.nb),legendtext = c("Poisson", "negative binomial"))

gofstat(list(B.count.p, B.count.nb),fitnames = c("Poisson", "negative binomial"))