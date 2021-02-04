library(dplyr)
library(ggplot2)
library(corrplot)
library(ggfortify)

install.packages("GGally")
library(GGally)
install.packages("ggpairs")
library(ggpairs)

data <- read.csv("carsdata.csv", header = TRUE, stringsAsFactors = FALSE)
str(data)
sapply(data, function(x) sum(is.na(x)))

# remove whitespaces
cols <- names(data)[vapply(data, is.character, logical(1))]
data[,cols] <- lapply(data[,cols], trimws)

# Convert "N/A" to R's NA
data[data=="N/A"]=NA
sapply(data, function(x) sum(is.na(x)))
data$Market.Category <- NULL # lot of NA's


# remove rows with >0 NA values
data <- data[complete.cases(data), ]


#corr
data_num <- data %>% select_if(is.numeric)
data_corr <- cor(data_num)
corrplot(data_corr, method = "number")



#plot of 2011 car brand
ggplot(data, aes(x = Make, y = MSRP)) +
  geom_col() +
  labs(x = "Make", y = "MSRP", title = "2011 Car MSRP based on Brand") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, hjust = 1))



# linear regression
model_MSRP <- lm(MSRP ~ Engine.HP + highway.MPG + city.mpg + Engine.Cylinders, 
                 data = data)
summary(model_MSRP)

# PCA plot
pca_res <- prcomp(data_num, scale. = TRUE)

autoplot(pca_res, data=data, colour="Transmission.Type")






#ggpairs(data_num)
