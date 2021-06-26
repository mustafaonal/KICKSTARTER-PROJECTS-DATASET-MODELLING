dataset <- read.csv("C:/Users/HP/Desktop/ks-projects-201801.csv")


#extract launched and deadline year and month from launched and deadline date features
dataset$launched <- as.Date(dataset$launched, "%Y-%m-%d")
dataset$launched_year <- substr(dataset$launched, 1,4)
dataset$launched_mth <- substr(dataset$launched, 6,7)
dataset$deadline_year <- substr(dataset$deadline, 1,4)
dataset$deadline_mth <- substr(dataset$deadline, 6,7)
dataset$launched_year <- as.integer(dataset$launched_year)
dataset$launched_mth <- as.integer(dataset$launched_mth)
dataset$deadline_year <- as.integer(dataset$deadline_year)
dataset$deadline_mth <- as.integer(dataset$deadline_mth)


#missing values in data set
colSums(is.na(dataset))

#Sum of missing values
sum(is.na(dataset))

#first 5 column in data set
head(dataset)

#show each feature frequencies in plot
library(inspectdf)
library(dplyr)
data_cat <- dataset %>% inspect_cat()
data_cat %>% show_plot(high_cardinality = 1, col_palette = 2)

#most populer categories
library(ggplot2)
library(devtools)
cat.freq <- dataset %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

cat.freq$main_category <- factor(cat.freq$main_category, levels=cat.freq$main_category)

ggplot(cat.freq, aes(main_category, count, fill=count)) + geom_bar(stat="identity") +
  ggtitle("Projects by main_category") + xlab("main_category") + ylab("Frequency") +
  geom_text(aes(label=count), vjust=-0.5) +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12, angle=90), legend.position="null") +
  scale_fill_gradient(low="blue", high="red")



#most popular countries
countries <- dataset %>%
  group_by(country) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

countries$country <- factor(countries$country, levels=countries$country)

ggplot(countries, aes(country, count, fill=count)) + geom_bar(stat="identity") +
  ggtitle("Projects by country") + xlab("country") + ylab("Frequency") +
  geom_text(aes(label=count), vjust=-0.5) +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12, angle=90), legend.position="null") +
  scale_fill_gradient(low="blue", high="red")


#number of project lauch by year
lauch_year <- dataset %>%
  group_by(launched_year) %>%
  summarize(count=n())

lauch_year$launched_year <- factor(lauch_year$launched_year, levels=lauch_year$launched_year)

ggplot(lauch_year, aes(launched_year, count, fill=count)) + geom_bar(stat="identity") +
  ggtitle("Projects lauched by year") + xlab("year") + ylab("Frequency") +
  geom_text(aes(label=count), vjust=-0.5) +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12, angle=90), legend.position="null") +
  scale_fill_gradient(low="skyblue", high="blue")



#Target feature (state) info
stateMap <- sort(table(dataset$state), decreasing = TRUE)
print(stateMap)
print(length(stateMap))

#Pie chart of state
pie_percent <- round(100 * stateMap / sum(stateMap), 1)
pie(stateMap, labels = pie_percent, main = "Distribution of State Feature", 
    col = rainbow(length(stateMap)), radius = 1)
legend("topright", rownames(stateMap), fill = rainbow(length(stateMap)))


#corralation matrix
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
}

corr_matrix_data <- dataset
corr_matrix_data$ID <- encode_ordinal(dataset$ID)
corr_matrix_data$name <- encode_ordinal(dataset$name)
corr_matrix_data$category <- encode_ordinal(dataset$category)
corr_matrix_data$main_category <- encode_ordinal(dataset$main_category)
corr_matrix_data$currency <- encode_ordinal(dataset$currency)
corr_matrix_data$deadline <- encode_ordinal(dataset$deadline)
corr_matrix_data <- select(corr_matrix_data, -8)
corr_matrix_data$country <- encode_ordinal(dataset$country)
corr_matrix_data$state <- encode_ordinal(dataset$state)


#p-values and corralation values based on pearson
library(Hmisc)
rcorr(as.matrix(corr_matrix_data))

library(RColorBrewer)
library(corrplot)
M<-cor(corr_matrix_data)

corrplot(M, type = "upper", method = "number", title = "Correlation Matrix of Kickstarter Projects",
         col = colorRampPalette(c("darkblue", "white", "yellow"))(10), bg = "brown")



#Plot Distributions of the Features
library(purrr)
library(tidyr)
library(ggplot2)

corr_matrix_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#Rearranging and removing unnecessary columns
dataset <- dataset[c(4,7,9,11,13,14,15,16,17,18,19,10)]

#data set information
dim(dataset)
summary(dataset)

#Missing values are omitted from dataset
dataset <- na.omit(dataset)

#Removing unnecessary state labels
dataset <- subset(dataset, state != 'live')
dataset <- subset(dataset, state != 'canceled')
dataset <- subset(dataset, state != 'suspended')
dataset <- subset(dataset, state != 'undefined')

#Main_category-State Relationship
barplot(table(dataset$state, dataset$main_category),
        main = "Categories and Their Rates of Succcess",
        xlab = "Categories",
        ylab = "Count",
        col = c("red","green")
)
legend("topleft",
       c("Failed","Successful"),
       fill = c("red","green")
)

# Encoding categorical data - label encoding
dataset$main_category = factor(dataset$main_category,
                               levels = c('Film & Video', 'Music', 'Publishing', 'Games', 'Technology', 'Design', 'Art', 'Food', 'Fashion', 'Theater','Comics','Photography', 'Crafts',
                                          'Journalism', 'Dance'),
                               labels = c(1:15))
dataset$state = factor(dataset$state, levels = c('failed','successful'), labels = c(0,1))

#Float-Integer Conversion
dataset$main_category <- as.numeric(dataset$main_category)
dataset$pledged <- as.integer(dataset$pledged)
dataset$main_category <- as.integer(dataset$main_category)
dataset$usd.pledged <- as.integer(dataset$usd.pledged)
dataset$usd_pledged_real <- as.integer(dataset$usd_pledged_real)


#################################################################################
newdt = dataset[sample(nrow(dataset), 30000),]

newdt[-12] = scale(newdt[-12])
#################################################################################

#FEATURE SELECTION USING CROSS VALIDATION ACCURACY RESULTS

# ensure the results are repeatable

# load the library
library(mlbench)
library(caret)
detach(package:purrr)
detach(package:dplyr)
detach(package:Hmisc)
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(newdt[,1:11], newdt[,12], sizes=c(1:11), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


#FEATURE SELECTION USING FEATURE IMPORTANCE SCORE
set.seed(7)
# load the library
library(caret)
# prepare training scheme
control <- trainControl(method="cv", number=10)
# train the model
model <- train(state~., data=newdt, method="multinom",trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#USING SELECTED FEATURES
dataset_feature <- dataset[, c('usd_goal_real', 'goal','usd_pledged_real', 'pledged', 'state')]
###############################################################################
#OUTLIER DETECTION AND REMOVAL
library(dplyr)
par(mfrow = c(4,2), mar=c(4,4,3,1))
for(i in 1:4){
  
  boxplot(dataset_feature[,i], main = "Feature Before Outlier Removal")
  Q<-quantile(dataset_feature[,i],probs=c(.25, .75), na.rm = FALSE)
  iqr<-IQR(dataset_feature[, i])
  up <-  Q[2]+1.5*iqr # Upper Range
  low<- Q[1]-1.5*iqr # Lower Range
  dataset_feature<- subset(dataset_feature, dataset_feature[,i] > (Q[1] - 1.5*iqr) & dataset_feature[,i]< (Q[2]+1.5*iqr))
  
  boxplot(dataset_feature[,i], main = "Feature After Outlier Removal")
  
}

failed_state_data <- dataset_feature[dataset_feature$state == "0", ]
failed_state_data <- failed_state_data[sample(nrow(failed_state_data), nrow(dataset_feature[dataset_feature$state == "1", ])), ]
successful_state_data <- dataset_feature[dataset_feature$state != "0", ]
dataset_feature <- rbind(successful_state_data, failed_state_data)
dataset_feature = dataset_feature[sample(nrow(dataset_feature), 30000),]

#################################################################################
library(caTools)
set.seed(123)
split = sample.split(dataset_feature$state, SplitRatio = 0.70)
training_set = subset(dataset_feature, split == TRUE)
test_set = subset(dataset_feature, split == FALSE)
dataset_feature$state = factor(dataset_feature$state, levels = c(0,1), labels = c('failed','successful'))
##################################################################################



#decision tree with 10-fold cross validation
library(rpart)
library (ROCR)
n <- nrow(dataset_feature)
decision_tree_accuracy <- numeric()
recall <- numeric()
precision <- numeric()
f_measure <- numeric()
K <- 10
size <- n%/%K
vol <- runif(n)
rank <- rank(vol)
blok <- (rank -1)%/%size + 1
blok <- as.factor(blok)
for(k in 1:K) {
  classifier_4 <- rpart(state ~.,
                        data = dataset_feature[blok!=k,],
                        method = "class")
  pred <- predict(classifier_4, newdata = dataset_feature[blok==k,], type = "class")
  cm_2 <- table(dataset_feature$state[blok==k], pred)
  precision <- c(precision, cm_2[2,2] / (cm_2[2,2] + cm_2[1,2]))
  recall <- c(recall, cm_2[2,2] / (cm_2[2,2] + cm_2[2,1]))
  f_measure <- c(f_measure, (2*precision[k]*recall[k])/(precision[k] + recall[k]))
  accuracy <- sum(diag(cm_2)) / sum(cm_2)
  decision_tree_accuracy <- c(decision_tree_accuracy, sum(diag(cm_2)) / sum(cm_2))
  print(accuracy)
}
print(decision_tree_accuracy)
mean(decision_tree_accuracy)
mean(f_measure)
mean(recall)
mean(precision)
plot(decision_tree_accuracy, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy Rate - 10-fold Cross Validation")


dt_tr <- rpart(state ~.,
               data = training_set)
# Predicting the Test set results
y_pred = predict(dt_tr, newdata = test_set[-5], type = 'class')
plot(dt_tr)
# Making the Confusion Matrix
cm = table(test_set[, 5], y_pred)
accuracy_dt = (sum(diag(cm) / sum(cm)))
# plot final DT
plot(dt_tr,  type = "uniform")
text(dt_tr)


#########################
#SVM
recall <- numeric()
precision <- numeric()
f_measure <- numeric()
svm_accuracy <- numeric()

for(k in 1:K) {
  library(e1071)
  classifier = svm(formula = state ~ .,
                   data = dataset_feature[blok!=k,],
                   kernel = 'linear')
  
  pred <- predict(classifier, newdata = dataset_feature[blok==k,], type = "class")
  cm_2 <- table(dataset_feature$state[blok==k], pred)
  precision <- c(precision, cm_2[2,2] / (cm_2[2,2] + cm_2[1,2]))
  recall <- c(recall, cm_2[2,2] / (cm_2[2,2] + cm_2[2,1]))
  f_measure <- c(f_measure, (2*precision[k]*recall[k])/(precision[k] + recall[k]))
  accuracy <- sum(diag(cm_2)) / sum(cm_2)
  svm_accuracy <- c(svm_accuracy, sum(diag(cm_2)) / sum(cm_2))
  print(accuracy)
}
print(svm_accuracy)
mean(svm_accuracy)
mean(f_measure)
mean(recall)
mean(precision)
plot(svm_accuracy, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy Rate - 10-fold Cross Validation")


#knn
# Encoding categorical data
dataset_feature$state = factor(dataset_feature$state,
                     levels = c(0,1),
                     labels = c('failed', 'successful'))

library(caret)
set.seed(123)
trCtrl.lr <- trainControl(method = "cv",
                          number = 10,  #5-fold CV
                          classProbs = TRUE,
                          savePredictions = TRUE)



model.lr <- train(state ~ .,
                  data=dataset_feature,
                  method="knn",
                  tuneGrid   = expand.grid(k = 1:10),
                  metric = "Accuracy",
                  trControl = trCtrl.lr)
y_pred <- predict(model.lr, test_set[-5])
KNN_accuracy <- model.lr$results['Accuracy']
##########################################


dataframe_acc <- data.frame(KNN_Accuracy = KNN_accuracy, Decision_Tree = decision_tree_accuracy, SVM = svm_accuracy)
boxplot(dataframe_acc)