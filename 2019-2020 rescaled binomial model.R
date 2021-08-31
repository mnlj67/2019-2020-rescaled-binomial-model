# Set up.
PL19<- read.csv("C:/Users/chenjun/Desktop/2019-2020 Premier League.csv")
View(PL19)

#add rates
PL19$rate = PL19$FTHG/(PL19$FTHG+PL19$FTAG)
PL19$rate[is.nan(PL19$rate)] <- 0.5
View(PL19$rate)

#boxplot
boxplot(rate~HS, data=PL19,
        main = "Boxplot of rate on HS",
        xlab = "HS",
        ylab = "rate")#positive

boxplot(rate~HST, data=PL19,
        main = "Boxplot of rate on HST",
        xlab = "HST",
        ylab = "rate")#positive

boxplot(rate~HY, data=PL19,
        main = "Boxplot of rate on HY",
        xlab = "HY",
        ylab = "rate")#negative

boxplot(rate~HR, data=PL19,
        main = "Boxplot of rate on HR",
        xlab = "HR",
        ylab = "rate")#negative

#fit the model 
glm1 = glm(rate ~ HS + HY, weights = FTHG+FTAG, data = PL19, family = binomial())
##weights = number of trials
summary(glm1)

# Using numbers of each category
glm2 = glm(cbind(FTHG, FTAG) ~ HS + HY, data = PL19, family = binomial())
summary(glm2)

#dispersion parameter
glm2$deviance / glm2$df.residual
#[1] 1.498203

#try to add hometeam
glm3 = glm(rate~HS + HY + HomeTeam, weights = FTHG + FTAG, data = PL19, family = binomial())
summary(glm3)

#predict
predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Liverpool'), type  = "response")

predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Bournemouth'), type  = "response")

predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Man City'), type  = "response")

predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Man United'), type  = "response")

#accuracy
# Split the data up
matchdata2019 <- cbind(rate=PL19$rate, X, FTHG =PL19$FTHG, FTAG=PL19$FTAG)
View(matchdata2019)
training2019 <- matchdata2019[1:150,]
testing2019 <- matchdata2019[-(1:150),]

# Fit the model on the training data only
glm4 <- glm(rate ~ .-FTHG-FTAG, training2019, weights = FTHG + FTAG, family = binomial())
summary(glm4)

# Predict on the testing
pred <- predict(glm4, testing2019, type = "response")

# Now produce a table, where we want to compare the truth to our prediction
res <- table(truth = ifelse(testing2019$rate>0.5, 1, 0),
             prediction = ifelse(pred > 0.5, 1, 0))
res

# Hence overall accuracy (%) is
(res[1,1]+res[2,2])/sum(res)*100

