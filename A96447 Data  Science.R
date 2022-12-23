# import a dataset.
#set working directory.
setwd("C:/Users/Student/Desktop/a96447/Datasets")
co2_emissions_dataset <- read.csv("co2_emissions_tonnes_per_person.csv")
#questiion 1.
#exclude missing values
co2_emissions_dataset <- na.omit(co2_emissions_dataset)


#question2
#get a summery of the co2_emission over the years displayed
summary(co2_emissions_dataset)

#question3.
#Asses the mode of the emission in 2008
#mode(co2_emissions_dataset$X2008)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- co2_emissions_dataset$X2008
mode2008 <- getmode(v)

#question5
#. Are there any unusual observations within the variables of dataset1? Display these outliers if any
#boxplot of the co2_emission over the years
#boxplot for the co2_emission over the years form X1800 to X2018	

#takng a sample of 7 years
boxplot(co2_emissions_dataset$X1800,co2_emissions_dataset$X1801, co2_emissions_dataset$X1802, co2_emissions_dataset$X1803, co2_emissions_dataset$X1804, co2_emissions_dataset$X1805, co2_emissions_dataset$X1806, names = c("1800", "1801", "1802", "1803", "1804", "1805", "1806"), horizontal=TRUE, col="blue", main="Distribution of in seven years", xlab="co2_emission", ylab="years")
#boxplot of the sample
boxplot(co2_emissions_dataset$X2008)$out
#since not normallly distributed, hence outliers.


#question6
#how can you filter out the outliers
boxplot(co2_emissions_dataset$X2008)
boxplot(co2_emissions_dataset$X2008)$out
install.packages("ggstatsplot")
library(ggstatsplot)
#Create a boxplot that labels the outliers
ggbetweenstats(data = co2_emissions_dataset, 
                x = "X2008", 
                y = "country", 
                title = "Boxplot of co2_emission in 2008",
                xlab = "co2_emission",
                ylab = "country",
                outlier.label = TRUE)
ggbetweenstats(Assignment2_New, x, price, outlier.tagging = TRUE)

#OR use the IQR method
#calculate the IQR
IQRX2008 <- (co2_emissions_dataset$X2008)
IQRX2008 <- IQR(IQRX2008)
#calculate the quartiles
#For price.
quartileX2008 <- (co2_emissions_dataset$X2008)	
quartileX2008 <- quantile(quartileX2008)

#calculate the upper and lower limits
upperLimitX2008 <- quartileX2008[4] + 1.5*IQRX2008
lowerLimitX2008 <- quartileX2008[2] - 1.5*IQRX2008

#filter out the outliers
co2_emissions_dataset <- co2_emissions_dataset[co2_emissions_dataset$X2008 < upperLimitX2008,]
co2_emissions_dataset <- co2_emissions_dataset[co2_emissions_dataset$X2008 > lowerLimitX2008,]
#using subset
co2_emissions_dataset_no_outlier <- subset(co2_emissions_dataset, X2008 < upperLimitX2008 | X2008 > lowerLimitX2008)
boxplot(co2_emissions_dataset_no_outlier$X2008)


#question7
#Display the covariation between one categorical variable and one finite variable in the year 2018 of dataset 1
#using the ggplot2 package
library(ggplot2)
ggplot(co2_emissions_dataset, aes(x = country, y = X2018)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "Covariation between country and co2_emission in 2018", x = "country", y = "co2_emission")

#question8
#Show the central tendency dispersal of your dataset. Which years of co2 emissions show normal distribution and which ones don't?
#using the ggplot2 package
library(ggplot2)
# create a data frame for the 7 years
selected_years <- data.frame(co2_emissions_dataset$X1800, co2_emissions_dataset$X1801, co2_emissions_dataset$X1802, co2_emissions_dataset$X1803, co2_emissions_dataset$X1804, co2_emissions_dataset$X1805, co2_emissions_dataset$X1806)
boxplot(selected_years, names = c("1800", "1801", "1802", "1803", "1804", "1805", "1806"), horizontal=TRUE, col="blue", main="Distribution of in seven years", xlab="co2_emission", ylab="years")



### QUESTION 5: NORMAL DISTRIBUTION TESTS
#Test the overall variable for normal distribution using the Shapiro-wilk test
#Null hypothesis based on research question: The overall performance of varieties across districts is normally distributed. p-value >= 0.05 
#Alternative hypothesis: Overall performance is not normally distributed across districts. p-value =<0.05 
shapiro.test(co2_emissions_dataset$X2008)
#W = 0.95674, p-value = 0.01189

## Distribution can also be visualised using a  Q-Q plots (quantile-quantile plots). 
#Q-Q plots draw the correlation between a given sample and the normal distribution.
ggqqplot(co2_emissions_dataset$X2008, xlab = "Normal Distribution", ylab = "co2_emission", main = "Q-Q Plot of co2_emission in 2008")

# The data is not normally distributed, it’s recommended to use the non parametric one-sample Wilcoxon rank test.
#### WILCOXON ONE-SAMPLE TEST#####
#The one-sample Wilcoxon signed rank test is a non-parametric 
#alternative to one-sample t-test when the data cannot be 
#assumed to be normally distributed. 
#Used to determine whether the median of the sample is equal to a known standard value (i.e. theoretical value).
#Looking at the article m =0.8
res <- wilcox.test(dataset$OVERALL, mu = 0.8)
res
