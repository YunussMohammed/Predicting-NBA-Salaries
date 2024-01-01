NBA<-read.csv("NBASalary.csv")
NBA.norm <- scale(NBA)


NBA.norm <- as.data.frame(NBA)
set.seed(12345)
NBA <- as.data.frame(NBA)
training <- sample(1:nrow(NBA.norm), 0.4*nrow(NBA.norm))
ycol <- match('SALARY',colnames(NBA.norm))  #Replace int_rate with the name of the dependent variable
NBA.norm.training <- NBA.norm[training,-ycol]
NBA.norm.training.results <- NBA.norm[training,ycol]
NBA.norm.test <- NBA.norm[-training,-ycol]
NBA.norm.test.results <- NBA.norm[-training,ycol]

# Linear regression
NBA.norm.reg <- lm(SALARY ~ ., data=NBA.norm[training,])
NBA.reg.predictions <- predict(NBA.reg,NBA)[-training]
(mean((NBA.test.results-NBA.reg.predictions)^2))^0.5

NBA.MLR <- lm(SALARY ~ ., data=NBA)
summary (NBA.norm.reg)


#Logit model
NBA.norm.lr <- glm(as.factor(SALARY) ~ ., family=binomial(link='logit'),data=NBA.norm[training,])
summary (NBA.norm.lr)



NBA.norm.training <- NBA.norm[training,-ycol]
NBA.norm.training.results <- NBA.norm[training,ycol] > 0.5

# The following two commands do the same for the remaining 40% of the data
# Here, again, the second command stores the results as binary variables
NBA.norm.test <- NBA.norm[-training,-ycol]
NBA.norm.test.results <- NBA.norm[-training,ycol] > 0.5

# The following command builds a logistic regression model on the training set
DC.lr <- glm(DEDUCTIBLE ~ ., family=binomial(link='logit'),data=DC[training,])

# The following command shows the logistic regression output
summary(DC.lr)

#Omit variables
is.na(NBA)
sum(is.na(NBA))
NBA<-omit.na(NBA)
na.omit(NBA)










NBA<-lm(SALARY ~ ., data= NBA)
Summary(NBA.reg)
NBA.norm <- scale(NBA)
