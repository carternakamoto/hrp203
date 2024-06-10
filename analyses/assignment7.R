# load data
cohort <- read.csv("raw-data/cohort.csv")
cohort$smoke <- factor(cohort$smoke)

# generate covariate balance table
library(tableone)
vars = c("female", "age", "cardiac", "cost")
t1 <- CreateTableOne(vars = vars, strata = "smoke", 
                     data = cohort, test = FALSE)
print(t1, smd = TRUE)

# fit linear model
predmod <- lm(cost ~ age + smoke, data = cohort)
summary(predmod)
cohort$pred_cost <- predict(predmod, cohort)

# generate figure based on linear model
library(ggplot2)
ggplot(cohort) +
  geom_point(aes(age, cost, color = smoke)) +
  geom_line(aes(age, pred_cost, group = smoke))
