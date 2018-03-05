library(tidyverse)

# functions

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

# procedure

# generate data
dat <- genreg(1000)


# method 1
dat <- mutate(dat,
       yhat = 5,
       yhat1 = 5-x1,
       yhat2 = 5+2*x2,
       yhat12 = 5-x1+2*x2)


# MSE

MSE <- mean((dat$yhat - dat$y)^2)
MSE1 <- mean((dat$yhat1 - dat$y)^2)
MSE2 <- mean((dat$yhat2 - dat$y)^2)
MSE12 <- mean((dat$yhat12 - dat$y)^2)
MSE
MSE1
MSE2
MSE12

# Exercise 2
#generate data : x=1
(pb<- 0.8/(1+exp(-1)))
(pa<- 0.2)
(pc<- 1-pb-pa)



#generate data : x=-2
(pb2<- 0.8/(1+exp(2)))
(pa2<- 0.2)
(pc2<- 1-pb2-pa2)

# classifierL if x<0, then C, if x>0, then B, when x =0, use another mathod 

# generate  data 

gencla <- function(n) {
x <- rnorm(n) 
pB <- 0.8/(1+exp(-x))
y <- map_chr(pB, function(x) 
  sample(LETTERS[1:3], size=1, replace=TRUE,
         prob=c(0.2, x, 1-x-0.2)))
tibble(x=x, y=y)
}


# probability

dat2 <- gencla(1000)
dat2

dat2 <- mutate(dat2,
               yhat = sapply(x,function(x_)
                 if(x_<0) "C" else"B"))
dat2
#error rate
1-mean(dat2$yhat == dat2$y)

which git
