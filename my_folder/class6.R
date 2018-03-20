suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(tidyverse))

######## fit a default tree
fit <- tree(Sepal.Width ~ ., data=iris) 
summary(fit)

plot(fit)  # Plot the tree, without labels
text(fit)  # Add labels to the tree

predict(fit, newdata=iris) %>% 
  head

fitfull <- tree(Sepal.Width ~ ., data=iris, 
                control=tree.control(nrow(iris), 
                                     mindev=0, minsize=2))
mean((predict(fitfull) - iris$Sepal.Width)^2)

######### 
set.seed(4)
fitfull_cv <- cv.tree(fitfull)
plot(fitfull_cv$size, log(fitfull_cv$dev))

######### pruned
fit_pruned <- try(prune.tree(fitfull, best=10))
plot(fit_pruned)
text(fit_pruned)