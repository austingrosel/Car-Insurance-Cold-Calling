library(corrplot)

corrplot(cors, method = "ellipse")

g1 = ggplot(carInsurance, aes(PrevAttemptOutcome, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "PrevAttemptOutcome", fill = "Response")

g2 = ggplot(carInsurance, aes(Education, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Education", fill = "Response")

g3 = ggplot(carInsurance, aes(Default, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Default", fill = "Response")

g4 = ggplot(carInsurance, aes(HHInsurance, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Household Insurance", fill = "Response")

g5 = ggplot(carInsurance, aes(CarLoan, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Car Loan",  fill = "Response")

g6 = ggplot(carInsurance, aes(Communication, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Communication", fill = "Response")

grid.arrange(g2,g3,g4,g5,g6,g1,nrow = 3)

ggplot(carInsurance, aes(Job, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Job", fill = "Response")

h1 = ggplot(carInsurance, aes(Balance)) + geom_density(alpha = 0.5, color = 'black', fill = 'red') + labs(title = "Balance", x = "Balance", y = "Count")

h2 = ggplot(carInsurance, aes(Age)) + geom_density(alpha = 0.5, color = 'black', fill = 'red') + labs(title = "Age", x = "Age", y = "Count")

grid.arrange(h1,h2,nrow = 2)

carInsurance$BalanceBin = factor(carInsurance$BalanceBin, levels = c('Negative', 'Low', 'Normal', 'High', 'Very High'))
g7 = ggplot(carInsurance, aes(BalanceBin, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Balance", fill = "Response")

carInsurance$AgeBin = factor(carInsurance$AgeBin, levels = c('College-Aged', 'Out-of-College', 'Middle-Aged', 'Senior'))
g8 = ggplot(carInsurance, aes(AgeBin, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Age", fill = "Response")

grid.arrange(g7,g8, nrow = 2)

ggplot(carInsurance, aes(Month, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Month", fill = "Response")

g9 = ggplot(carInsurance, aes(MonthBin, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Month", fill = "Response")

g10 = ggplot(carInsurance, aes(Season, ..count..)) + geom_bar(aes(fill = Response), position = "dodge") + labs(title = "Season", fill = "Response")

grid.arrange(g9,g10, nrow = 2)

ggplot(carInsurance) + geom_point(aes(x = Age, y = CarInsurance, fill = CarInsurance)) + labs(title = "Age", x = "Age", y = "Count")

ggplot(carInsurance, aes(Age)) + geom_density(aes(group = CarInsurance, fill = as.factor(CarInsurance)), alpha = 0.3) + labs(title = "Age", x = "Age", y = "Count")


glm.full <- glm(Response~., data = train, family = binomial)
glm.null <- glm(as.factor(Response) ~ 1, data = train, family = binomial)
model.aic.both <- step(glm.null, direction = "both", trace = 1, glm.full)


glm = step(glm(CarInsurance~., data = train))
confMat <- table(test$CarInsurance, ifelse(predict(glm, test) > 0.5, 1, 0))
sum(diag(confMat))/sum(confMat)

levels(train$Response) = c(0, 1)
