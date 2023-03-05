# How to plot a correlation Matrix in R

library(datarium)
library(ggplot2)
library(reshape2)

# Load Markiting Data set from datarium Package
marketing <- datarium::marketing

# View first six lines of the data set
head(marketing)

cormat <- cor(marketing)
cormat

plot(marketing)

# Melt the dataframe

meltCormat <- melt(cormat)
meltCormat

# Plotting the correlation on heatmap using ggplot2

ggplot(data <- meltCormat,
       aes(x = Var1, y = Var2, fill = value)) + geom_tile()


