#Read in data
heart_disease_dataset <- read.csv(file = "processed.cleveland.data", header = F)

#Prepare column names
names <- c("Age",
           "Sex",
           "Chest_Pain_Type",
           "Resting_Blood_Pressure",
           "Serum_Cholesterol",
           "Fasting_Blood_Sugar",
           "Resting_ECG",
           "Max_Heart_Rate_Achieved",
           "Exercise_Induced_Angina",
           "ST_Depression_Exercise",
           "Peak_Exercise_ST_Segment",
           "Num_Major_Vessels_Flouro",
           "Thalassemia",
           "Diagnosis_Heart_Disease")

#Apply column names to the dataframe
colnames(heart_disease_dataset) <- names

#Glimpse data to verify new column names are in place
heart_disease_dataset %>% glimpse()
## Observations: 303
## Variables: 14
## $ Age                      <dbl> 63, 67, 67, 37, 41, 56, 62, 57, 63, 5...
## $ Sex                      <dbl> 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1...
## $ Chest_Pain_Type          <dbl> 1, 4, 4, 3, 2, 2, 4, 4, 4, 4, 4, 2, 3...
## $ Resting_Blood_Pressure   <dbl> 145, 160, 120, 130, 130, 120, 140, 12...
## $ Serum_Cholesterol        <dbl> 233, 286, 229, 250, 204, 236, 268, 35...
## $ Fasting_Blood_Sugar      <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1...
## $ Resting_ECG              <dbl> 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 2, 2...
## $ Max_Heart_Rate_Achieved  <dbl> 150, 108, 129, 187, 172, 178, 160, 16...
## $ Exercise_Induced_Angina  <dbl> 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1...
## $ ST_Depression_Exercise   <dbl> 2.3, 1.5, 2.6, 3.5, 1.4, 0.8, 3.6, 0....
## $ Peak_Exercise_ST_Segment <dbl> 3, 2, 2, 3, 1, 1, 3, 1, 2, 3, 2, 2, 2...
## $ Num_Major_Vessels_Flouro <fct> 0.0, 3.0, 2.0, 0.0, 0.0, 0.0, 2.0, 0....
## $ Thalassemia              <fct> 6.0, 3.0, 7.0, 3.0, 3.0, 3.0, 3.0, 3....
## $ Diagnosis_Heart_Disease  <int> 0, 2, 1, 0, 0, 0, 3, 0, 2, 1, 0, 0, 2...

#Read in data
heart_disease_dataset <- read.csv(file = "processed.cleveland.data", header = F)

#Prepare column names
names <- c("Age",
           "Sex",
           "Chest_Pain_Type",
           "Resting_Blood_Pressure",
           "Serum_Cholesterol",
           "Fasting_Blood_Sugar",
           "Resting_ECG",
           "Max_Heart_Rate_Achieved",
           "Exercise_Induced_Angina",
           "ST_Depression_Exercise",
           "Peak_Exercise_ST_Segment",
           "Num_Major_Vessels_Flouro",
           "Thalassemia",
           "Diagnosis_Heart_Disease")

#Apply column names to the dataframe
colnames(heart_disease_dataset) <- names

#Glimpse data to verify new column names are in place
heart_disease_dataset %>% glimpse()
## Observations: 303
## Variables: 14
## $ Age                      <dbl> 63, 67, 67, 37, 41, 56, 62, 57, 63, 5...
## $ Sex                      <dbl> 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1...
## $ Chest_Pain_Type          <dbl> 1, 4, 4, 3, 2, 2, 4, 4, 4, 4, 4, 2, 3...
## $ Resting_Blood_Pressure   <dbl> 145, 160, 120, 130, 130, 120, 140, 12...
## $ Serum_Cholesterol        <dbl> 233, 286, 229, 250, 204, 236, 268, 35...
## $ Fasting_Blood_Sugar      <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1...
## $ Resting_ECG              <dbl> 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 2, 2...
## $ Max_Heart_Rate_Achieved  <dbl> 150, 108, 129, 187, 172, 178, 160, 16...
## $ Exercise_Induced_Angina  <dbl> 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1...
## $ ST_Depression_Exercise   <dbl> 2.3, 1.5, 2.6, 3.5, 1.4, 0.8, 3.6, 0....
## $ Peak_Exercise_ST_Segment <dbl> 3, 2, 2, 3, 1, 1, 3, 1, 2, 3, 2, 2, 2...
## $ Num_Major_Vessels_Flouro <fct> 0.0, 3.0, 2.0, 0.0, 0.0, 0.0, 2.0, 0....
## $ Thalassemia              <fct> 6.0, 3.0, 7.0, 3.0, 3.0, 3.0, 3.0, 3....
## $ Diagnosis_Heart_Disease  <int> 0, 2, 1, 0, 0, 0, 3, 0, 2, 1, 0, 0, 2...
####################################################
# Disease distribution for age. 
# 0 - no disease
# 1 - disease
####################################################

heart_disease_data %>% group_by(age, condition) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(age, count,   fill = as.factor(condition)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Condition")

####################################################
# Chest pain type for diseased people
# You can see - Majority as condition 3 type
# 0: typical angina 1: atypical angina  Value 2: non-anginal pain Value 3: asymptomatic
####################################################

heart_disease_data %>% filter(condition == 1) %>% group_by(age, cp) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(age, count,   fill = as.factor(cp)),stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Condition") + 
  ggtitle("Age vs. Count (disease only) for various chest pain conditions") +
  scale_fill_manual(values=c("red", "blue", "green", "black"))
####################################################
# condition sex wise
####################################################
options(repr.plot.width = 20, repr.plot.height = 8) 

heart_disease_data %>% ggballoonplot(x = "age", y = "sex",
                                     size = "chol", size.range = c(5, 30), fill = "condition",show.label = FALSE,
                                     ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Age vs. Sex Map") + labs(fill = "Condition")
options(repr.plot.width = 20, repr.plot.height = 8) 
####################################################
# condition sex wise
####################################################


heart_disease_data %>% ggballoonplot(x = "age", y = "cp",
                               size = "chol", size.range = c(5, 30), fill = "sex",show.label = FALSE,
                               ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Age vs. Chest Pain Map") + labs(fill = "sex")
set.seed(2020, sample.kind = "Rounding")
# Divide into train and validation dataset
test_index <- createDataPartition(y = heart_disease_data$condition, times = 1, p = 0.2, list= FALSE)
train_set <- heart_disease_data[-test_index, ]
validation <- heart_disease_data[test_index, ]

# Converting the dependent variables to factors
train_set$condition <- as.factor(train_set$condition)
validation$condition <- as.factor(validation$condition)

ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)
knnFit <- train(condition ~ ., 
                data = train_set, method = "knn", preProcess = c("center","scale"),
                trControl = ctrl , tuneGrid = expand.grid(k = seq(1, 20, 2)))

plot(knnFit)
toc()

knnPredict <- predict(knnFit,newdata = validation )
knn_results <- confusionMatrix(knnPredict, validation$condition )

knn_results

###data train dan data testing###
library(caTools)
#randomly split data
split=sample.split(heart_disease_data$condition, SplitRatio = 0.75)
training.samples <- heart_disease_data$condition %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- heart_disease_data[training.samples, ]
test.data <- heart_disease_data[-training.samples, ]
model = glm(condition ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca +thal,
                  data=train.data,
                  family = binomial(link="logit"),
                  na.action(na.omit)
                  )
summary(model)
summary(model)$coef
