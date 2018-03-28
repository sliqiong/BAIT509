### LIQIONG SUN 
### Assignment 3
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ISLR)
library(quantreg)
auto_data <-mutate(Auto, mileage = as.integer(mpg>median(mpg)))
diamonds

### Exercise 1 : probabilistic forecasting
 diamond.f <- select(diamonds, c(x, y, z, cut))
# diamond.f <- as.matrix(diamond.f)
# diamonds$cut <- as.numeric(diamonds$cut)
# 
# 
# 
# diamond.lo <- loess(diamonds$cut ~ diamond.f, span=0.50)
# 
# summary(diamond.lo)
# 
diamond1 <- data.frame(x=4,y=4,z=3)
diamond2 <- data.frame(x=6,y=6,z=4)
# 

##############
d <- 0.5

myfunction <- function(x_,y_,z_){
  lst=list()
  for (i in 1:nrow(diamond.f) ){
    d_ = sqrt((x_-diamond.f$x[i])^2+(y_-diamond.f$y[i])^2+(z_-diamond.f$z[i])^2)
    if (d_ <= d){
      lst = c(diamond.f$cut[i], lst)
      # print(i)
    }
    
  }
  df <- data.frame(matrix(unlist(lst), nrow=length(lst), byrow=T))
  colnames(df) <-c("cuthat")
  df %>% group_by(cuthat) %>% summarise(count=n())
}
cutgroup1<-myfunction(diamond1$x,diamond1$y,diamond1$z)
cutgroup1$probability <- cutgroup1$count/sum(cutgroup1$count)

p1<-ggplot(data=cutgroup1, aes(x=cuthat, y=probability)) +
  geom_bar(stat="identity",fill="steelblue")
p1
#########
cutgroup2<-myfunction(diamond2$x,diamond2$y,diamond2$z)
cutgroup2$probability <- cutgroup2$count/sum(cutgroup2$count)
p2<-ggplot(data=cutgroup2, aes(x=cuthat, y=probability)) +
  geom_bar(stat="identity",fill="steelblue")
p2


###########  Q2a   ###########

mtcars$weight <- mtcars$wt*1000
fit_rq <- rq(mpg ~ wt, data=mtcars, tau=c(0.25, 0.5, 0.75))
fit_rq

my_accent <- "#d95f02"
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(alpha=1, colour=my_accent) +
  geom_quantile(colour="black") +
  theme_bw() +
  labs(x="Weight (wt) of a vehicle",
       y="Fuel efficiency (mpg) ")

yhat1 <- predict(fit_rq, newdata=data.frame(wt=3.5))
yhat1
yhat2 <- predict(fit_rq, newdata=data.frame(wt=1.5))
yhat2
###########  Q2b   ###########
auto_rq <- rq(mpg ~ weight, data=auto_data, tau=c(0.5))
auto_lm <- lm(mpg ~ poly(weight, 2), data=auto_data)

########## plot

ggplot(auto_data, aes(mpg, weight)) +
  geom_point(alpha=1, colour=my_accent) +
  geom_quantile(quantiles = 0.5, colour="black") +
  geom_quantile(quantiles = 0.5, colour="blue", formula=y~poly(x, 2))+
  theme_bw() +
  labs(x="Weight (wt) of a vehicle",
       y="Fuel efficiency (mpg) ")
  
predict.rq <- predict(auto_rq, newdata=mtcars) 
predict.lm <- predict(auto_lm, newdata=mtcars) 

err.rq <-  mean(abs(predict.rq-mtcars$mpg))
err.lm <-  mean(abs(predict.lm-mtcars$mpg))
err.rq
err.lm
##########  Q3  ##############

a_formula <- function(x) { 1+3*x }
x1 <- c(0, 1, 2)
x2 <- c(1, 4, 7)
dat <- cbind(x1, x2)
dat <- as.data.frame(dat)
ggplot(dat, aes(x=x1, y=x2)) + 
  geom_point()+
  stat_function(fun = a_formula, color="red", lwd=1) +
  geom_smooth(method=lm)

X1 <- c(3,2,4,1,2,4,4)
X2 <- c(4,2,4,4,1,3,1)
Y <- c("Red","Red","Red","Red","Blue","Blue","Blue")
obs <- cbind(X1,X2,Y)
obs <- as.data.frame(obs)
ggplot(obs, aes(x=X1, y=X2, col=Y)) + 
  geom_point(size =6)+
  scale_color_manual(values=c("#4699dd","#ff3db7"))+
  geom_abline(intercept = -0.5, slope = 1, color="black")

             