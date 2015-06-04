# Read data
setwd("C:\\Users\\Dillon\\Box Sync\\GitHub\\DavisBiketoSchool")
d <- read.csv('bike_data.csv')

library(rethinking)

m1 <- map2stan(
  alist(
    Rack_Count ~ dbinom(Enrollment,p),
    logit(p) ~ a + aj[School_ID] ,
    aj[School_ID] ~ dnorm(0,sigma_school),
    a ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ),
  data = d,
  start = list(sigma_school=1, a=0, aj=rep(0,11)),
  iter=2000, warmup=500, chains=1
)

m2<- map2stan(
  alist(
    Rack_Count ~ dbinom(Enrollment,p),
    logit(p) ~ a + aj[School_ID] + bRain*Rain ,
    aj[School_ID] ~ dnorm(0,sigma_school),
    a ~ dnorm(0,10),
    bRain ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ),
  data = d,
  start = list(sigma_school=1, a=0, aj=rep(0,11), bRain=0),
  iter=2000, warmup=500, chains=1
)

# WAIC results from model match those using compare().
m1
m2
compare(m1,m2)

# Precompile models and use resample()-----------------------
m1c <- map2stan(
  alist(
    Rack_Count ~ dbinom(Enrollment,p),
    logit(p) ~ a + aj[School_ID] ,
    aj[School_ID] ~ dnorm(0,sigma_school),
    a ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ),
  data = d,
  start = list(sigma_school=1, a=0, aj=rep(0,11)),
  iter=2, warmup=1, chains=1
)
m1.1 <- resample(m1c,iter=2000,warmup=500,chains=1)

m2c<- map2stan(
  alist(
    Rack_Count ~ dbinom(Enrollment,p),
    logit(p) ~ a + aj[School_ID] + bRain*Rain ,
    aj[School_ID] ~ dnorm(0,sigma_school),
    a ~ dnorm(0,10),
    bRain ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ),
  data = d,
  start = list(sigma_school=1, a=0, aj=rep(0,11), bRain=0),
  iter=2, warmup=1, chains=1
)
m2.1 <- resample(m2c,iter=2000,warmup=500,chains=1)

# WAIC results from each model do not match the results from compare()
m1.1
m2.1
compare(m1.1,m2.1)

