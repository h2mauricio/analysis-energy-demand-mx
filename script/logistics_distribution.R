library(glogis)

## simple artificial example
#Reference: https://rdrr.io/rforge/glogis/man/glogisfit.html
# https://statisticsglobe.com/logistic-distribution-in-r-dlogis-plogis-qlogis-rlogis

set.seed(2)
x <- rglogis(100000, location = 43.4662 , scale = exp(0.1995), shape = exp(13.6961))
gf <- glogisfit(x)
plot(gf)
summary(gf)

## query parameters and associated moments
coef(gf)
coef(gf, log = FALSE)
gf$parameters
gf$moments

data= df.enc.dwell$bill.days1.valid[df.enc.dwell$mes_final == 3 & df.enc.dwell$num_month_bill > 1.5 & 
                                df.enc.dwell$num_month_bill <= 2.5 ]

data_weight= df.enc.dwell$factor_sem[df.enc.dwell$mes_final == 3 & df.enc.dwell$num_month_bill > 1.5 & 
                                 df.enc.dwell$num_month_bill <= 2.5 ]

df <- data.frame(data, data_weight)

data = na.omit(data)
df = na.omit(df)

#REf: https://cran.r-project.org/web/packages/glogis/glogis.pdf

fit_model = glogisfit(data, weights = NULL, start = NULL, method = "BFGS", hessian = TRUE)

#fit_model = glogisfit(df$data, weights = df$data_weigh, start = NULL, method = "BFGS", hessian = TRUE)

par(mar=c(1,1,1,1))
summary(fit_model, log = TRUE)
plot(fit_model)

library(EnvStats)

qqplot(x=data, y=x, dist = "lnorm",  digits = 2, plot.type = "Tukey", points.col = "blue", add.line = TRUE)


#chisquare goodness of fit test
library(vcd)
gf<-goodfit(data,type= "poisson",method= "MinChisq")
summary(gf)

####

nlFit(data, freq = NULL, breaks = "FD", paramStart = NULL,
      startMethod = "Nelder-Mead",
      method = "BFGS",
      hessian = FALSE,
      plots = FALSE, printOut = TRUE)

