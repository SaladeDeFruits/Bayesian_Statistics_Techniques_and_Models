library("car")
data("Leinhardt")
head(Leinhardt)
str(Leinhardt)
# Factor variable is counted by the levels
#

# Using pairs function to plot the scatter plots
pairs(Leinhardt)

plot(infant ~ income, data = Leinhardt)
#? Is linear model really proper,

#Skewness
hist(Leinhardt$infant)
hist(Leinhardt$income)
# we observed that the  data is strongly right skewed.
# We apply the log function to apply the linear regression

# We add two log variables into the data chart
Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data = Leinhardt)
# From this chart we could imagine linear model


###Modeling
### Usually we name the y-axis variable as response variable
lmod = lm(loginfant ~ logincome, data = Leinhardt)
summary(lmod)

dat = na.omit(Leinhardt)
Leinhardt
