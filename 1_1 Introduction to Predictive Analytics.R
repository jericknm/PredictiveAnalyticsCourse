#1.1 - Introduction to Predictive Analytics

# Load required packages
install.packages("tidyverse")
install.packages(c("ggplot2", "readr", "corrgram", "plotly", "rpart", 
                   "tidyr", "tree", "reshape2", "corrplot", "rpart.plot"))
required.packages = c("ggplot2", "readr", "corrgram", "plotly", "rpart", 
                      "tidyr", "tree", "reshape2", "corrplot", "rpart.plot")
lapply(required.packages, require, character.only = TRUE)

#Load data and take a quick look at the summary
data.all<-read.csv("C:/PredictiveAnalyticsCourse/Data/BreastCancerWisconsinDataSet/BreastCancerWisconsinDataSet.csv")
summary(data.all)

#Some cleaning and set up

#set the target
data.all$target[data.all$diagnosis=="M"]=1
data.all$target[data.all$diagnosis=="B"]=0

# select the variables available for training
vars <- setdiff(names(data.all), c("id", "diagnosis", "X"))
data.all <- data.all[,vars]
View(data.all)

# split data into training vs testing (we will discuss more efficient and tidier ways to do this later in the course)
set.seed(1000)
data.all$rand <- runif(nrow(data.all))
data.training <- data.all[which(data.all$rand <= 0.5),]
data.testing <- data.all[which(data.all$rand > 0.5),]
data.all$rand <- NULL
data.training$rand <- NULL
data.testing$rand <- NULL

# Check each dataset and make sure the target distributions are similar among training, testing and overall set
# what do we do if there are sampling bias?
# what other exploration should we do besides the target distribution?

Plot.Dis <- function (data) {
  # We will talk more about the ggplot package later in the course - for now can you work out what this is doing?
  ggplot(data, aes(x = target)) + 
    geom_bar(aes(fill = target, color = target)) + 
    ggtitle(paste("Distribution of target for", deparse(substitute(data)), sep = " ")) + 
    theme(legend.position = "none")
}

Plot.Dis(data.all)

# What about the distribution of other variables?
# any interesting observations from the distribution?

data.all [1:12] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

data.all [13:24] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

data.all [25:31] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# What about looking at the target by variable in the data
# since the target is binary (0 vs 1), the plots are harder to see
# Is there anything interesting that you see?

data.all[c(1:12,31)] %>% 
  gather(-target, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = target, color = target)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

data.all[c(13:24,31)] %>% 
  gather(-target, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = target, color = target)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

data.all[c(25:31)] %>% 
  gather(-target, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = target, color = target)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# Let's check the correlations shall we?
corrgram(data.all, order = NULL, lower.panel = panel.shade, 
         upper.panel = NULL, text.panel = panel.txt,
         main = "Correlation among variables")

# That was hard to read... Let's give it another try
# Calculate correlation matrix
cormat <- cor(data.all)
# Melt the correlation matrix (flatten it so we can plot it)
melted.cormat <- melt(cormat)
# Remember plotly from the welcome notes?
plot_ly(x = melted.cormat$Var1, 
        y = melted.cormat$Var2,
        z = melted.cormat$value, 
        type = "heatmap")

# GLM - This is a  simple linear model, similar to the form Y = aX + b - we will cover the specifics in Module 6
# This is how we define which variables we want the model to use to try and predict the target variable - this is equivalent to Y(target) = aX(radius_mean, texture_mean, etc.) + b
formula.model <- as.formula("target ~ radius_mean + 
                                         texture_mean +
                                         perimeter_mean +
                                         area_mean +
                                         smoothness_mean    + 
                                         compactness_mean   +
                                         concavity_mean +
                                         concave.points_mean +
                                         symmetry_mean") 

# Here we fit the model (using the function "glm") and store it in the object "Logistic.m1"
Logistic.m1 <- glm(formula = formula.model, 
                   family = binomial(link='logit'), 
                   control = list(maxit = 50), 
                   data = data.training) # The arguments inside the brackets are what the "glm" function is supposed to use to build the model.

# GLM validation - compare accuracy on training vs testing data
Logistic.m1.pred.train = round(predict(Logistic.m1, data.training, type = "response"), 0)
error = mean(Logistic.m1.pred.train != data.training$target)
print(paste('GLM Training Model Accuracy', 1-error))

Logistic.m1.pred.test = round(predict(Logistic.m1, data.testing, type = "response"), 0)
error = mean(Logistic.m1.pred.test != data.testing$target)
print(paste('GLM Testing Model Accuracy', 1-error))

Tree.m1 <- rpart(formula.model, # We can use the same formula we used for the logistic model
                 data = data.training)
rpart.plot(Tree.m1) # This creates a nice image of the tree model we just built

# Let's check the accuracy
Tree.m1.pred.train = round(predict(Tree.m1, data.training, type = "vector"), 0)
error = mean(Tree.m1.pred.train != data.training$target)
print(paste('Tree Training Model Accuracy', 1-error))

Tree.m1.pred.test = round(predict(Tree.m1, data.testing, type = "vector"), 0)
error = mean(Tree.m1.pred.test != data.testing$target)
print(paste('Tree Testing Model Accuracy', 1-error))
