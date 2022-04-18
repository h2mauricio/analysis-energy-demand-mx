library(fitdistrplus)

data(groundbeef)
str(groundbeef)

plotdist(groundbeef$serving)

descdist(groundbeef$serving, boot=1000)
fw <- fitdist(groundbeef$serving, "weibull")
fg <- fitdist(groundbeef$serving,"gamma")
fln <- fitdist(groundbeef$serving,"lnorm")
par(mfrow=c(2, 2))
denscomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
qqcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
cdfcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))
ppcomp(list(fw,fln,fg), legendtext=c("Weibull", "lognormal", "gamma"))



# Discrete data
# Reference https://r-forge.r-project.org/scm/viewvc.php/*checkout*/JSS/fitdistrplus/paper2JSS.pdf?revision=236&root=riskassessment&pathrev=236
data(toxocara)
str(toxocara)

(ftoxo.P <- fitdist(toxocara$number, "pois"))
(ftoxo.nb <- fitdist(toxocara$number, "nbinom"))

plot(ftoxo.P)
plot(ftoxo.nb)

par(mfrow=c(1, 1))
cdfcomp(list(ftoxo.P,ftoxo.nb),legendtext=c("Poisson", "negative binomial"))

gofstat(list(ftoxo.P,ftoxo.nb))




(fdata.P <- fitdist(df$data, "pois"))
(fdata.nb <- fitdist(df$data, "nbinom"))
(fdata.geom <- fitdist(df$data, "geom"))
#(fdata.hyp <- fitdist(df$data, "hyper", start=list(m = 50, n = 2240, k = 30)))
#(fdata.hyp <- fitdist(df$data, distr = "hyper", fix.arg=list(size=20), start=list(prob=0.15) ))
(fdata.hyp <- fitdist(df$data, "hyper", start=list(size=2240, prob=0.2)))


plot(fdata.P)
plot(fdata.nb)
plot(fdata.geom)

par(mfrow=c(1, 1))
cdfcomp(list(fdata.P,fdata.nb, fdata.geom),legendtext=c("Poisson", "negative binomial", "geometrical"))

gofstat(list(fdata.P,fdata.nb))



plotdist(df$data)

descdist(df$data, boot=5000)
fw <- fitdist(df$data, "weibull")
fg <- fitdist(df$data,"gamma")
fln <- fitdist(df$data,"lnorm")

par(mfrow=c(2, 2))
denscomp(list(fw,fln,fg), legendtext=c("logistic", "lognormal", "gamma"))
qqcomp(list(fw,fln,fg), legendtext=c("logistic", "lognormal", "gamma"))
cdfcomp(list(fw,fln,fg), legendtext=c("logistic", "lognormal", "gamma"))
ppcomp(list(fw,fln,fg), legendtext=c("logistic", "lognormal", "gamma"))

