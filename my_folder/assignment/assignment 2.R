library(dplyr)
library
##### Q1. step 1
cd_data<-read.csv(file.choose(), header=TRUE)

str(cd_data)

cd_data$DATE.new <- as.Date(cd_data$DATE, "%m/%d/%y") 
cd_data


#### 1. step 2 : split data

calibration_sample <- filter(cd_data, DATE.new <= "1997-09-30")
validation_sample <- filter(cd_data, DATE.new > "1997-09-30")

#aggregate->calibration_sample

calibration <- aggregate(calibration_sample$ID, by = list(calibration_sample$ID), FUN = length)
colnames(calibration) <- c("ID","frequency")

monetary_1 <- aggregate(calibration_sample$DOLLARS, by = list(calibration_sample$ID), FUN = mean)[2]
calibration$monetary<- monetary_1$x


last.purchase_1 <- aggregate(calibration_sample$DATE.new, by = list(calibration_sample$ID), FUN = max)[2]
calibration$last.purchase <-last.purchase_1$x

num.cds_1 <- aggregate(calibration_sample$CDS, by = list(calibration_sample$ID), FUN = sum)[2]
calibration$num.cds <-num.cds_1$x

#aggregate->validation_sample
validation <- aggregate(validation_sample$ID, by = list(validation_sample$ID), FUN = length)
colnames(validation) <- c("ID","frequency")

monetary_2 <- aggregate(validation_sample$DOLLARS, by = list(validation_sample$ID), FUN = mean)[2]
validation$monetary <- monetary_2$x

last.purchase_2 <- aggregate(validation_sample$DATE.new, by = list(validation_sample$ID), FUN=max)[2]
validation$last.purchase <-last.purchase_2$x

num.cds_2 <- aggregate(validation_sample$CDS, by = list(validation_sample$ID), FUN = sum)[2]
validation$num.cds <-num.cds_2$x
                     

####step 3: "recency" 
last_purchase_date <- max(calibration$last.purchase)
calibration$recency <- last_purchase_date - calibration$last.purchase

last_purchase_date <- max(validation$last.purchase)
validation$recency <- last_purchase_date - validation$last.purchase


#####Step 4: merge
cd_df <- merge(calibration, validation, by="ID", all.x=TRUE, all.y=TRUE)

#######Step 5: retained

t <- rep(0, nrow(cd_df))
for (i in 1:nrow(cd_df)) {
  if (is.na(cd_df[i, "frequency.y"]) || cd_df[i, "frequency.y"] <= 0) {
    t[i] <- 0
  }
  else {
    t[i] <- 1
  } 

} 

cd_df$retained <- t

#####Q2 Decile analysis

cd_df$frequency.decilex <- ntile(cd_df$frequency.x, 10)
cd_df$monetary.decilex <- ntile(cd_df$monetary.x, 10)
cd_df$recency.decilex <- ntile(cd_df$recency.x, 10)

####Calculate and plot the percent retention in each decile of recency, each decile of frequency, and each decile of monetary

f.retain <- aggregate(cd_df$retained, by = list(cd_df$frequency.decilex), FUN = sum)
colnames(f.retain) <- c("decilex", "num.retain")
total <- aggregate(cd_df$retained, by = list(cd_df$frequency.decilex), FUN = length)[2]
f.retain$percent <- f.retain$num.retain/total
f.retain

m.retain <- aggregate(cd_df$retained, by = list(cd_df$monetary.decilex), FUN = sum)
colnames(m.retain) <- c("decilex", "num.retain")
total <- aggregate(cd_df$retained, by = list(cd_df$monetary.decilex), FUN = length)[2]
m.retain$percent <- m.retain$num.retain/total
m.retain
r.retain <- aggregate(cd_df$retained, by = list(cd_df$recency.decilex), FUN = sum)
colnames(r.retain) <- c("decilex", "num.retain")
total <- aggregate(cd_df$retained, by = list(cd_df$recency.decilex), FUN = length)[2]
r.retain$percent <- r.retain$num.retain/total
r.retain
x= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
y= c(0.1383114, 0.1361901, 0.1315231, 0.1569792, 0.1531608, 0.1781926, 0.3669919, 0.3992363, 0.5663980, 0.7675011)

barplot(y, main= "Percent retention in each decile of frequency", xlab= "each decile of retention", ylab= "Percent Retention", names.arg= x, col= "forestgreen")


x= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
y= c(0.1641918, 0.2392872, 0.1960119, 0.3020789, 0.3627493, 0.3186254, 0.3347476, 0.3478999, 0.3801443, 0.3487484)


barplot(y, main= "Percent retention in each decile of monetary", xlab= "each decile of retention", ylab= "Percent Retention", names.arg= x, col= "pink")


x= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
y= c(0.7543487, 0.6109461, 0.4722104, 0.2257106, 0.1904964, 0.1607976, 0.1684345, 0.1408570, 0.1395842, 0.1310989)


barplot(y, main= "Percent retention in each decile of recency", xlab= "each decile of retention", ylab= "Percent Retention", names.arg= x, col= "orange")

#Q3 predication
#linear regression
z1 <- lm(retained ~ monetary.x + recency.x + frequency.x, data=cd_df) 
summary(z1)
cd_df$predict1 <- predict(z1)

#plot
plot(predict1 ~ monetary.x, data=cd_df, main="Linear regression prediction as a function of monetary")
plot(predict1 ~ frequency.x, data=cd_df, main="Linear regression prediction as a function of frequency")
plot(predict1 ~ recency.x, data=cd_df, main="Linear regression prediction as a function of frequency")

#Logistic regression
z2 <- glm(retained ~ monetary.x + recency.x + frequency.x, family=binomial(link='logit'), data=cd_df)
summary(z2)
accuracy(z2)
predict2 <- predict(z2)
predict2 

plot(predict2 ~ monetary.x, data=cd_df, main="Logistic regresson prediction as a function of monetary")
plot(predict2 ~ frequency.x, data=cd_df, main="Logistic regresson prediction as a function of frequency")
plot(predict2 ~ recency.x, data=cd_df, main="Logistic regresson prediction as a function of frequency")
