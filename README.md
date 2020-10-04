# ECON-B-2000-HW-3
---
#title: "HW#3 Neighborhood Data"
#author: "Emmanuel" Emily Vazquez, Erik Carlson, Joe Correa
#date: "10/4/2020"
#output: html_document




load("acs2017_ny_data.RData")
PUMA_lev <-read.csv("PUMA_levels.csv")

dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1) & (acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))

attach(dat_NYC)

#borough_f assigned to neighborhoods
borough_f <- factor(PUMA)

norm_varb <- function(X_in) {
  (max(X_in, na.rm = TRUE) - X_in)/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )}

add_test <- COSTGAS + COSTWATR + COSTELEC + COSTFUEL + FUELHEAT + FAMSIZE + unmarried

norm_1 <- norm_varb(add_test)
norm_2 <- norm_varb(HHINCOME)

data_use_prelim <- data.frame(norm_1,norm_2)

good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
levels

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 5, by= 2)) 
  
{pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough == true_data)
correct_rate <- num_correct_labels/length(true_data)
print(c(indx,correct_rate))}
