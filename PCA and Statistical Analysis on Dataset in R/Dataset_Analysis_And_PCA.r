library(tidyverse)
library(corrplot)
library(ggplot2)
library(factoextra)

#Loading the data set into a vriable called data
data = read.csv("pain_rating_julio.csv")

#Taking the data from column 2 to 29. We are omitting first
#column because it is experiment ID and not playing any key
#role in our statistical analysis
data <- data[,2:29]

#Dropping all the cells which are empty
data <- na.omit(data)

#Glimpse function will display an overview of data, like 
#Total number of rows, columns, data type of each feature
glimpse(data)

#Calculating correlation among all the features of data set
correlation <- cor(data)
view(correlation)
#view(cor(data))
#Setting margins for all the plots which we will plot later
par("mar")
par(mar = c(1,1,1,1))

# Select columns containing "HAND" for hand data
hand_data <- data[, grepl("HAND", colnames(data))]
#view(hand_data)

# Select columns containing "LEG" for eg data
leg_data <- data[, grepl("LEG", colnames(data))]
#view(leg_data)

# Calculating mean for the hand data
hand_mean <- colMeans(hand_data, na.rm = TRUE)

# Calculating mean for the leg data
leg_mean <- colMeans(leg_data, na.rm = TRUE)


means <- data.frame(c("Hand", "Leg"), c(hand_mean, leg_mean))
names(means) <- c("Type", "Mean")

# Visualizing the mean data for hand and leg using bar graph


hand_mean <- colMeans(hand_data, na.rm = TRUE)
leg_mean <- colMeans(leg_data, na.rm = TRUE)

means <- data.frame(c("Hand", "Leg"), c(hand_mean, leg_mean))
names(means) <- c("Type", "Mean")

ggplot(means, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  ggtitle("Average Scores for Hand and Leg Data") +
  ylab("Mean Score") +
  xlab("Type") +
  theme(legend.text = element_text(size = 12))

# Viewing the mean of the leg and hand data
view(hand_mean)
view(leg_mean)

# Calculating the standard deviation for the hand data
hand_sd <- apply(hand_data, 2, sd, na.rm = TRUE)

# Calculating the standard deviation for the leg data
leg_sd <- apply(leg_data, 2, sd, na.rm = TRUE)


sds <- data.frame(c("Hand", "Leg"), c(hand_sd, leg_sd))
names(sds) <- c("Type", "SD")

ggplot(sds, aes(x = Type, y = SD, fill = Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  ggtitle("Standard Deviation of Hand and Leg Data") +
  ylab("Standard Deviation") +
  xlab("Type") +
  theme(legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, max(sds$SD) * 1.2), expand = c(0, 0.1))



# Viewing the standard deviation for the hand and leg data
view(hand_sd)
view(leg_sd)


# Performing T-Test on the hand and leg data
t_test_results <- list()
for (i in 1:ncol(hand_data)) {
  t_test_results[[i]] <- t.test(hand_data[,i], leg_data[,i])
}

t_test_results


features <- data[, 2:28]

# Perform factorial analysis using PCA
pca_result <- prcomp(features, scale = TRUE)

# Print the variance explained by each principal component
summary(pca_result)

# Plot the scree plot to visualize the variance explained by each principal component
fviz_eig(pca_result, addlabels = TRUE)

# Plot the biplot to visualize the relationship between the features and principal components
fviz_pca_biplot(pca_result)
