#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model


# Load libraries for examining and ploting the data
library(dplyr)
library(ggplot2)

# load states.rds data
states.data <- readRDS("dataSets/states.rds")

# examine the data
str(states.data)
summary(states.data)
glimpse(states.data)

# exclude rows wihtout dependent variable to predict
states.data <- states.data %>% filter(!is.na(energy))

# plot dependent and independent variables
plot(states.data$metro, states.data$energy)
ggplot(states.data, aes(x = metro, y = energy))+
  geom_point()

# determine corrleation between dependent and independent variable
cor(states.data$energy, states.data$metro)

# create new liner model
model1 <- lm(energy ~ metro, data = states.data)


##   2. Print and interpret the model `summary'

# print & summarize model
# the r-squared is very low
summary(model1)
print(model1)

##   3. `plot' the model to look for deviations from modeling assumptions

plot(model1)

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?


plot(states.data$metro, states.data$energy)
ggplot(states.data, aes(x = metro, y = energy))+
  geom_point()

# Identify correlation of independent varables and crete sorted list
Independent_Variable = c("area", "college", "csat", "density", "expense", "green", "high", "house", "income", "metro", "miles", "msat", "percent", "pop", "senate", "toxic", "vsat", "waste")
Correlation = c(cor(states.data$energy, states.data$area),
                cor(states.data$energy, states.data$college),
                cor(states.data$energy, states.data$csat),
                cor(states.data$energy, states.data$density),
                cor(states.data$energy, states.data$expense),
                cor(states.data$energy, states.data$green,use="pairwise.complete.obs"),
                cor(states.data$energy, states.data$high),
                cor(states.data$energy, states.data$house),
                cor(states.data$energy, states.data$income),
                cor(states.data$energy, states.data$metro),
                cor(states.data$energy, states.data$miles),
                cor(states.data$energy, states.data$msat),
                cor(states.data$energy, states.data$percent),
                cor(states.data$energy, states.data$pop),
                cor(states.data$energy, states.data$senate),
                cor(states.data$energy, states.data$toxic),
                cor(states.data$energy, states.data$vsat),
                cor(states.data$energy, states.data$waste))
cor_summ <- data.frame(Independent_Variable, Correlation)
cor_summ <- cor_summ[order(-Correlation),]
View(cor_summ)
print(cor_summ)

# 81 r-squared with all independent variables
model2 <- lm(energy ~ metro + area + college + csat + density + expense + green + high + house + income + miles + msat + percent + pop + senate + toxic + vsat + waste, data = states.data)
summary(model2)
print(model2)
plot(model2)

# 81 r-squared after droping bottom 2 correlated independent variables (house and senate)
model3 <- lm(energy ~ metro + area + college + csat + density + expense + green + high + income + miles + msat + percent + pop + toxic + vsat + waste, data = states.data)
summary(model3)
print(model3)
plot(model3)

# 80 r-squared after dropping bottom 7 correlated independent variables (house, senate, metro, density, percentage, waste, college, pop)
model4 <- lm(energy ~ area + csat + expense + green + high + income + miles + msat + toxic + vsat, data = states.data)
summary(model4)
print(model4)
plot(model4)

# 79 r-squared after drpping bottom 11 correlated independent variables (house, senate, metro, density, percentage, waste, college, pop, income, expense, high)
model5 <- lm(energy ~ area + csat + green + miles + msat + toxic + vsat, data = states.data)
summary(model5)
print(model5)
plot(model5)

# 78 r-squared after dropping bottom 13 correlated independent variables (house, senate, metro, density, percentage, waste, college, pop, income, expense, high, msat, vsat, csat)
model6 <- lm(energy ~ area + green + miles + toxic, data = states.data)
summary(model6)
print(model6)
plot(model6)

# 78 r-squared after dropping bottom 15 correlated independent variables (without house, senate, metro, density, percentage, waste, college, pop, income, expense, high, msat, vsat, csat, miles)
model7 <- lm(energy ~  area + green + toxic, data = states.data)
summary(model7)
print(model7)
plot(model7)

# 78 r-squared after adding metro back
model8 <- lm(energy ~  metro + area + green + toxic, data = states.data)
summary(model8)
print(model8)
plot(model8)


# SUMMARY:
# (1) By reducing the independent from "all" to the 3 top positivly correlated variables (area, green and toxic);
# The multiple R2 value decreases from .814 to .780
# The adjusted R2 value increases from .708 to .765
# (2) By then adding "metro" back to the top 3 positivly correlated variables;
# The multiple R2 value increases from .780 to .781
# The adjusted R2 value decreased from .765 to .761
# CONCLUSION: Predict on area, green and toxic. Do not include metro or any of the other variables

anova(model2, model3, model4, model5, model6, model7, model8)



##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

interaction <- lm(energy ~ green*toxic, data = states.data)
summary(interaction)

anova(interaction)
coef(summary(interaction))
summary(interaction)



##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

ggplot(states.data, aes(x = green, y = energy, col = region))+
  geom_point()

region <- lm(energy ~ green * toxic + region, data = states.data)
anova(region)
coef(summary(region))
summary(region)
