                #TO LOAD PACKAGES REQUIRES
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

                #TO DOWNLOAD THE DATA 
#Download the data set
dl <- tempfile()
download.file("https://raw.githubusercontent.com/jcaguerri/adult-income-project/master/adultdataset.csv"
, dl)
adults <- read.csv(dl)
      

                #EXPLORING THE DATA
dim(adults) #data set dimension
head(adults) #first rows of the dataset
str(adults) #varaible structure

#There are somo values "?". We are going to replace it for N/A , this let us omit it
#later and to avoid confusions
adults <- adults %>% na_if("?")
sum(is.na(adults)) #there are more than 4000 missing values

#Proportion of people that earn more or less than 50k
adults  %>% 
  summarise("less50K" = mean(income== "<=50K"), 
            "more50K" = mean(income == ">50K"),
            "total" = n())


              #ANALYZING THE VARAIBLES
#Incomes by age
        #Plot:
adults %>% na.omit() %>% 
  ggplot(aes(age, color= income, fill= income)) +
  geom_density(alpha= 0.7) +
  labs(x = "Age", y = "Frequency", title = "Age frequency by incomes")
      #youngest people seems earn less than 50K with more frequency that older people

#incomes by workclass
    #Table:
adults  %>% 
  na.omit() %>%
  group_by(workclass) %>%
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n()) %>%
  arrange(desc(more50K))
    #Plot:
adults %>% na.omit() %>% 
  ggplot(aes(workclass, color= income, fill= income)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(x = "Working Class", y = "number", title = "Working class frequency")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Incomes by education
      #Table
adults  %>% 
  na.omit() %>% 
  group_by(education) %>% 
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"), 
            "total" = n())

  #This varaiable have 15 different levels, some of them have hus a few cases
  #and many have really close relations with the income variable. So we are going
  #to group this variable by educational levels in order to get robust predictions
  #in the future and to get a better performance of our algorithm.
adults <- adults %>% mutate(education_level = case_when(
  .$education %in% c("1st-4th", "5th-6th", "7th-8th")~"2.non-finish-middle-school",
  .$education %in% c("9th", "10th", "11th", "12th")~"3.non-finish-high-school",
  .$education %in% c("Assoc-acdm", "Assoc-voc")~"5.associates",
  .$education %in% c("Some-college", "HS-grad")~"4.hs-graduate",
  .$education %in% "Bachelors" ~ "6.bachelors",
  .$education %in% "Prof-school" ~ "8.prog-school",
  .$education %in% "Preschool" ~ "1.preschool",
  .$education %in% "Doctorate" ~ "9.doctorate",
  .$education %in% "Masters" ~"7.master"))
          #We have add numbers to the levels as reference for those aren´t familiars
          #with usa educational system.
      #Plot1 (it shows mainly the distribution of educational levels across the population):
adults %>% na.omit() %>% 
  ggplot(aes(education_level, color= income, fill= income)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(x = "Education level", y = "Count", title = "Educational level frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

      #Plot2 (it shows the relation between education and incomes):
adults %>% na.omit() %>% 
  ggplot(aes(education_level , color= income, fill= income)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Education_level", y = "Proportion", title = "Incomes by educational level")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=0.24, col="blue") +
  annotate("text", x= 5.1, y= 0.64, size= 3.5, color="blue",
           label="Distribution of incomes (+50k/-50k) in general population")

#Incomes by marital status
  #This varaiable have 7 labels, two of them are Married-AF-spouse and 
  #Married-civ-spouse. Both levels can be grouped into the group "married".  
  #The level "married-spOuse-absent" could be joined to the label "Separated".
adults <- adults %>% mutate(marital_status = case_when(
  .$marital.status %in% c("Married-AF-spouse", "Married-civ-spouse")~"Married",
  .$marital.status %in% c("Separated", "Married-spouse-absent", "11th")~"separated",
  .$marital.status %in% "Divorced" ~ "divorced",
  .$marital.status %in% "Widowed" ~ "widowed",
  .$marital.status %in% "Never-married" ~ "never-married"))
    #Table:
adults  %>% 
  na.omit() %>% 
  group_by(marital_status) %>% 
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n()) %>%
  arrange(desc(more50K))
    #Plot:
adults %>% na.omit() %>% 
  ggplot(aes(marital_status, color= income, fill= income)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  labs(x = "Marital Status", y = "Count", title = "Incomes by Marital Status")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Incomes by ocupation
  #Table
adults  %>% na.omit() %>%
  group_by(occupation) %>%
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n()) %>%
  arrange(desc(more50K))
  #Plot
adults %>% na.omit() %>% 
  ggplot(aes(occupation, color= income, fill= income)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Ocupation", y = "Proportion", title = "Incomes by occupation")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=0.24, col="blue")

#Incomes by relationship
  #Table
adults  %>% na.omit() %>%
  group_by(relationship) %>%
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n()) %>%
  arrange(total)
  #Plot
adults %>% na.omit() %>% 
  ggplot(aes(income, color= income, fill= income)) +
  geom_bar( alpha = 0.8, width = 0.8) +
  facet_grid(~relationship)+
  labs(x = "Incomes", y = "Count", title = "Incomes by relationship")

#Incomes by race
  #Table:
adults  %>% na.omit() %>%
  group_by(race) %>%
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n()) %>%
  arrange(desc(more50K))
  #Plot:
adults %>% na.omit() %>% 
  ggplot(aes(race, color= income, fill= income)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Race", y = "Proportion", title = "Incomes by race")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=0.24, col="blue") +
  annotate("text", x= 4.3, y= 0.64, size= 2, color="blue",
           label="Distribution of incomes (+50k/-50k) in general population")

#Incomes by sex
  #Table:
adults  %>% na.omit() %>%
  group_by(sex) %>%
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n())
  #Plot:
adults %>% na.omit() %>% 
  ggplot(aes(sex, color= income, fill= income)) +
  geom_bar() +
  labs(x = "Sex", y = "Proportion", title = "Incomes by sex")+
  geom_bar(position = position_stack(reverse = TRUE))

#Capital variations
  #We are goint to mix the varaibles capital.loss and capital.gain into a unique
  #varaible  with the capital variation. 
adults <- adults %>% mutate(capital_variation = capital.gain - capital.loss) 
  #Table:
adults  %>% na.omit() %>%
  group_by(income) %>%
  summarise("capital_var_mean" = mean(capital_variation),
            "capital_var_min" = min(capital_variation),
            "capital_var_max" = max(capital_variation),
            "total" = n())

#Incomes by worked hours per week
adults  %>% na.omit() %>%
  group_by(income) %>%
  summarise("Mean hours per week" = mean(hours.per.week),
            "Standard deviatrion" = sd(hours.per.week))

adults %>% ggplot(aes(income, hours.per.week, fill=income))+
  geom_boxplot(alpha= 0.6)+
  labs(x = "Incomes", y = "Worked hours per week", title = "Incomes by worked hours")

#Incomes by origin
  #We´re going to group the countries by continent.
adults <- adults %>% mutate(native_continent = case_when(
  .$native.country %in% c("France", "Greece", "Hungary", "Italy", "Portugal",
                          "Scotland", "England", "Germany", "Holand-Netherlands",
                          "Ireland", "Poland", "Yugoslavia")~"Europe",
  .$native.country %in% c("Columbia", "Dominican-Republic", "El-Salvador", "Haiti",
                          "Honduras", "Mexico", "Outlying-US(Guam-USVI-etc)",
                          "Cuba", "Ecuador", "Guatemala", "Jamaica", "Nicaragua",
                          "Peru", "Puerto-Rico", "Trinadad&Tobago")~"America(no_usa_canada)",
  .$native.country %in% c("Iran", "Japan", "Philippines", "Taiwan", "Vietnam", "Cambodia",
                          "China", "Hong", "India", "Laos", "South", "Thailand")~"Asia",
  .$native.country %in% "United-States" ~ "USA",
  .$native.country %in% "Canada" ~"Canada"))
  #Table:
adults  %>% na.omit() %>%
  group_by(native_continent) %>%
  summarise("less50K" = mean(income== "<=50K"),
            "more50K" = mean(income == ">50K"),
            "total" = n()) %>%
  arrange(desc(more50K))
  #Plot:
adults %>% na.omit() %>% 
  ggplot(aes(native_continent, color= income, fill= income)) +
  geom_bar(position = "fill", alpha = 0.8, width = 0.8) +
  coord_flip() +
  labs(x = "Native continent", y = "Proportion", title = "Incomes by native continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept=0.24, col="blue")


              #METHODS AND MODELS

#TO SELECT THE VARAIBLES CREATED
adults <- adults %>% select(age, workclass, education_level, marital_status,
                             occupation, relationship, race, sex, capital_variation,
                             hours.per.week, native_continent, income)
head(adults)

#TO REMOVE THE NA
sum(is.na(adults)) #Count the n/a
adults <- na.omit(adults) #Remove the n/a
dim(adults) #New dimension of our data

# TO CREATE THE PARTITION BETWEEN TEST AND VALIDATION SETS
set.seed(1)
test_index <- createDataPartition(y = adults$income, times = 1, p = 0.2, list = FALSE)
train <- adults[-test_index,]
test <- adults[test_index,]

#GLM MODEL
set.seed(1)
glm_model <- train(income~., data=train, method= "glm",
                     trControl = trainControl(method = "cv", 
                                              number=5, 
                                              p=0.9)) #Choose crossvalidation with
                                                        #5 k-folds
y_hat_glm <- predict(glm_model, test, type = "raw") #Predict
confusionMatrix(y_hat_glm, test$income) #Show the results

#KNN
knn_model <- train(income~., data=train, method = "knn",
                   tuneGrid = data.frame(k = seq(5, 20, 2)),#test differents k
                   trControl = trainControl(method = "cv", 
                                            number=5, 
                                            p=0.9)) #5-folds crossvalidation
knn_model$bestTune #Best K
y_hat_knn <- predict(knn_model, test, type = "raw") #Predict
confusionMatrix(y_hat_knn, test$income) #show results

#CLASIFICATION TREE
set.seed(1)
rpart_model <- train(income ~., data = train, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.005)),
                   trControl = trainControl(method = "cv", 
                                            number=5, 
                                            p=0.9)) #Choose crossvalidation with
                                                    #5 k-folds)
rpart_model$bestTune #Best complexity parameter
y_hat_rpart <- predict(rpart_model, test)#Predict
confusionMatrix(y_hat_rpart, test$income)#Show results
  #Plotting the tree
plot(rpart_model$finalModel, margin=0.1)
text(rpart_model$finalModel, cex = 0.75)

#RAMDOM FOREST
############DON´T RUN THIS, IT WILL TAKE SEVERAL HOURS###########################
#        rf_mode <- train(income~., data=train, method = "rf")
#        rf_mode$bestTune
#        rf_mode$finalModel

  #Adjusted ramdom forest to reduce the time that it needs to run.
set.seed(1)
rf_fit_mod <- train(income~., data= train, method= "rf",
                    tuneGrid = data.frame(mtry = 27),
                    trControl= trainControl(method="cv", 
                                            number=5), #CV with 5 folds
                    ntree = 50 )
y_hat_rf <- predict(rf_fit_mod, test, type = "raw") #Predict
confusionMatrix(y_hat_rf, test$income) #Show results
varImp(rf_fit_mod) #varaible importance


                #RESULTS
  #To save as objects the confusion matrix
con_glm <- confusionMatrix(y_hat_glm, test$income)
con_knn <- confusionMatrix(y_hat_knn, test$income)
con_rpart <- confusionMatrix(y_hat_rpart, test$income)
con_rf <- confusionMatrix(y_hat_rf, test$income)
  #To build the table with the resoults:
models <- c("glm", "knn", "rpart", "rf")
      #Accuracies
accuracies <- c(con_glm$overall["Accuracy"], con_knn$overall["Accuracy"], 
                con_rpart$overall["Accuracy"], con_rf$overall["Accuracy"])
resoults <- data.frame(models, accuracies)
resoults
    #Acuracies, precision, recall and balanced accuracy.
resoults <- resoults %>% 
  mutate(sensitivity = c(con_glm$byClass["Sensitivity"], con_knn$byClass["Sensitivity"], 
                         con_rpart$byClass["Sensitivity"], con_rf$byClass["Sensitivity"]),
         specificity =  c(con_glm$byClass["Specificity"], con_knn$byClass["Specificity"], 
                          con_rpart$byClass["Specificity"], con_rf$byClass["Specificity"]),
         balanced_accuracy = c(con_glm$byClass["Balanced Accuracy"], con_knn$byClass["Balanced Accuracy"], 
                              con_rpart$byClass["Balanced Accuracy"], con_rf$byClass["Balanced Accuracy"]))
resoults

               #ANNEXED: RAMDOM FOREST JUST WITH EXPLICATIVE VARIABLES
set.seed(1)
rf_exp_mod <- train(income~ age + workclass+ education_level +occupation + race + sex +hours.per.week + native_continent ,
                    data= train, 
                    method= "rf",
                    trControl= trainControl(method="cv", 
                                            number=5), #CV with 5 folds
                    ntree = 100 )
y_hat_rf_exp <- predict(rf_exp_mod, test, type = "raw") #Predict
confusionMatrix(y_hat_rf_exp, test$income) #Show resoults
varImp(rf_exp_mod) #varaible importance
