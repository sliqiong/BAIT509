# class 2 

###EXERCISE 1
library(tidyverse)
library(knitr)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

dat
# df[1:5, ] sebset 1:5 rows from data frame 


#kNN

dat$distance <- abs(dat$x)
dat.order <- arrange(dat,distance)
dat.order
dat_final <- dat.order[1:5, ]
dat_final
y_predication <- mean(dat_final$y)#2.744634
y_predication

#Loess

dat_loess <- filter(dat.order, distance <1)
y_predication2 <- mean(dat_loess$y) #2.76788
y_predication2

# [ note: what happens when r that is too small? 
#we will end up with no observation or the large error.
# tradeoff between choosing large and small values of either hyperparameters: 
#when r is small, we might overfit the data, low bias and high variane;  when r is large,  varaince is small, bias is going to be high ]

###EXERCISE 2


xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  ## YOUR CODE HERE FOR kNN
  dat$distance <- abs(dat$x-x)
  dat.order <- arrange(dat,distance)
  dat.order
  dat_final <- dat.order[1:5, ]
  dat_final
  mean(dat_final$y)#2.744634
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})



loess_estimates <- map_dbl(xgrid, function(x){
  ## YOUR CODE HERE FOR LOESS
  dat$distance <- abs(dat$x-x)
  dat.order <- arrange(dat,distance)
  dat_loess <- filter(dat.order, distance <1)
  mean(dat_loess$y) #2.76788
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y)) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()


