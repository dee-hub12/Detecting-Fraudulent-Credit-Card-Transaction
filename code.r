
# Loading the dataset and removing the id column
dat <- read.csv("creditcard_2023.csv")
dat1 <- dat[,-1]
head(dat1)


# Checking for missing values in the data set
colSums(is.na(dat1))


# Summary Statistics
print(summary(dat1))
  

# Creating distribution plots for the variables
options(repr.plot.width=12, repr.plot.height=10)
dat1 %>%
  select(-Class) %>%
  gather() %>% 
  ggplot(aes(value, fill = key)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~key, scales = "free") +
  labs(title = "Distribution of Variables") +
  guides(fill = "none")
 

# Creating the correlation  plot to check the correlation  between the variables 
corr.mat <- dat1 %>% 
  select(-Class) %>% 
  cor()
options(repr.plot.width=12, repr.plot.height=8)


# Creating a violin plot distribution by class
options(repr.plot.width=12, repr.plot.height=20)
dat1 %>% gather(key = "Variable", value = "Value", -Class) %>%
  mutate(Class = as.factor(Class)) %>%
  ggplot(aes(x = Class, y = Value, fill = Class)) +
  geom_violin() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution of Variables by Class",
       x = "Class",
       y = "Value") +
  scale_fill_manual(values = c("0" = "blue", "1" = "orange")) +
  theme_minimal() +
  guides(fill = "none")


# Plotting the class distribution
options(repr.plot.width=10, repr.plot.height=7)
ggplot(dat1, aes(x = factor(Class), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Class", y = "Count", fill = "Class", title = "Distribution of Classes") +
  scale_fill_manual(values = c("0" = "blue", "1" = "orange"), labels = c("Genuine",    "Fraudulent")) +
  scale_y_continuous(labels = scales::comma) + 

  
# Normalizing the 'Amount' varaible
pre <- preProcess(dat1["Amount"], method = c("center", "scale"))
dt <- predict(pre, dat1["Amount"])
dat1 <- bind_cols(dat1[, -which(names(dat1) %in% "Amount")], dt)
head(FRAUD)


# Splitting the data into training and test sets (70% training, 30% test)
set.seed(123)
spt <- sample.split(dat1$Class, SplitRatio = 0.7)
train <- subset(dat1, split == TRUE)
test<- subset(dat1, split == FALSE)

nrow(train)
nrow(test)

# Logistic regression model
set.seed(123)
model1 = glm(Class~. ,data = train, family = binomial(link = 'logit'))
summary(model1)
pred.mod1 <- predict(model1, newdata = test, type = 'response')

# ROC and PR Curves for GLM  model
model1.k <- pred_glm[test$Class == 1]
model1.q <- pred_glm[test$Class == 0]

model1.roc <- roc.curve(scores.class0 = model1.k , scores.class1 = model1.q, curve = T)
model1.prd <- pr.curve(scores.class0 = model1.k , scores.class1 = model1.q, curve = T)

options(repr.plot.width=12, repr.plot.height=7)
par(mfrow = c(1, 2))
plot(model1.roc, col = "blue", main = "ROC Curve")
plot(model1.prd, col = "blue", main = "PR Curve")


### 2. Decision Tree
set.seed(123)
model2 <- rpart(Class ~ ., data = train, method = "class")
summary(model2)
pred.model2 <- predict(model2, newdata = test, type = "prob")


# ROC and PR Curves for decision tree  model
model2.k <- pred.model2[test$Class == 1]
model2.q <- pred.model2[test$Class == 0]

model2.roc <- roc.curve(scores.class0 = model2.k , scores.class1 = model2.q, curve = T)
model2.prd <- pr.curve(scores.class0 = model2.k , scores.class1 = model2.q, curve = T)

par(mfrow = c(1, 2))
plot(model2.roc, col = "red", main = "ROC Curve")
plot(model.prd, col = "red", main = "PR Curve")


### 3. Random Forest
set.seed(123)
modedl3 <- randomForest(Class ~ ., data = train, ntree = 10)
summary(model3)
model3.pred <- predict(model3, newdata = test, type = "class")

# ROC and PR Curves for random forest model
model3.k <- model3.pred[test$Class == 1]
model3.q <- model3.pred[test$Class == 0]

model3.roc <- roc.curve(scores.class0 = model3.k , scores.class1 = model3.q, curve = T)
model3.prd <- pr.curve(scores.class0 = model3.k , scores.class1 = model3.q, curve = T)

par(mfrow = c(1, 2))
plot(model3.roc, col = "green", main = "ROC Curve")
plot(model3.prd, col = "green", main = "PR Curve")


### 4. Neural Network
set.seed(123)
model4 <- nnet(Class ~ ., data = train, size = 10, linout = FALSE, maxit = 200)
summary(model4)
model4.prob <- predict(model4, newdata=test[, !names(test) %in% "Class"], type="raw")
model4.pred <- ifelse(model4.prob > 0.5, 1, 0)

# ROC and PR Curves for random forest model
model4.k <- model4.pred[test$Class == 1]
model4.q <- model4.pred[test$Class == 0]

model4.roc <- roc.curve(scores.class0 = model4.k , scores.class1 = model4.q, curve = T)
model4.prd <- pr.curve(scores.class0 = model4.k , scores.class1 = model4.q, curve = T)

par(mfrow = c(1, 2))
plot(model4.roc, col = "violet", main = "ROC Curve")
plot(model4.prd, col = "violet", main = "PR Curve")


## Model Evaluation and Comparison
par(mfrow = c(1, 2))

# Extract ROC data for all models
auc.roc.model1 <- data.frame(FPR = model1.roc$curve[, 1], TPR = model1.roc$curve[, 2])
auc.roc.model2 <- data.frame(FPR = model2.roc$curve[, 1], TPR = model2.roc$curve[, 2])
auc.roc.model3 <- data.frame(FPR = model3.roc$curve[, 1], TPR = model3.roc$curve[, 2])
auc.roc.model4 <- data.frame(FPR = model4.roc$curve[, 1], TPR = model4.roc$curve[, 2])

# Plot ROC curves
plot(auc.roc.model1$FPR, auc.roc.model1$TPR, type = 'l', col = "purple", lwd = 5, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves Comparison", xlim = c(0, 1), ylim = c(0, 1))
lines(auc.roc.model2$FPR, auc.roc.model2$TPR, col = "lightblue", lwd = 5)
lines(auc.roc.model3$FPR, auc.roc.model3$TPR, col = "pink", lwd = 5)
lines(auc.roc.model4$FPR, auc.roc.model4$TPR, col = "yellow", lwd = 5)
legend("bottomright", legend = c("Logistic", "Decision Tree", "Random Forest", "Neural Network"), col = c("purple", "lightblue", "pink","yellow"), lty = 1, lwd = 5)

# Extract PR data for all models
prediction.model1 <- data.frame(Recall = model1.prd$curve[, 1], Precision = model1.prd$curve[, 2])
prediction.model2 <- data.frame(Recall = model2.prd$curve[, 1], Precision = model2.prd$curve[, 2])
prediction.model3 <- data.frame(Recall = model3.prd$curve[, 1], Precision = model3.prd$curve[, 2])
prediction.model3 <- data.frame(Recall = model4.prd$curve[, 1], Precision = model4.prd$curve[, 2])

# Plot PR curves
plot(prediction.model1$Recall, prediction.model1$Precision, type = 'l', col = "purple", , lwd = 5, xlab = "Recall", ylab = "Precision", main = "PR Curves Comparison", xlim = c(0, 1), ylim = c(0, 1))
lines(prediction.model2$Recall, prediction.model2$Precision, col = "lightblue", lwd = 5)
lines(prediction.model13$Recall, prediction.model3$Precision, col = "pink", lwd = 5)
lines(prediction.model14$Recall, prediction.model14$Precision, col = "yellow", lwd = 5)
legend("bottomright", legend = c("Logistic", "Decision Tree", "Random Forest", "Nueral Network"), col = c("purple", "lightblue", "pink", "yellow"), lwd = 5)

# Reset the plotting parameters to default
par(mfrow = c(1, 1))

# Extre AUC ROC values
auc_model1<- model1.roc$auc
auc_model2<- model2.roc$auc
auc_model3<- model3.roc$auc
auc_model4<- model4.roc$auc

# Extre AUC PR values

pr_model1.auc <- model1.prd$auc.integral
pr_model2.auc <- model2.prd$auc.integral
pr_model3.auc <- model3.prd$auc.integral
pr_model4.auc <- model4.prd$auc.integral

# Create a data frame
table <- data.frame(
  Model = c("Logistic", "Decision Tree", "Random Forest", "Neural Network"),
  AUC_ROC = c(auc_model1, auc_model2, auc_model3, auc_model4),
  AUC_PR = c(pr_model1.auc, pr_model2.auc, pr_model3.auc, pr_model4.auc)
)

# Print the table
print(table)
