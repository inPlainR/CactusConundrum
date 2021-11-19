cactus <- read.csv("ohcrapcacti.csv")

## install.packages("ggplot2")
## install.packages("dplyr")
## install.packages("caret")
## install.packages("e1071")

library(dplyr)
library(ggplot2)
library(caret)
library(e1071)

colnames(cactus)

cactus <- cactus %>%
  rename(cactus = Was.There.A.Cactus, 
         slope = Slope.Degree, 
         distance = Minimum.Distance.From.Another.Cactus..m.)

cactus <- cactus %>%
  mutate(ouch = ifelse(cactus == "No", 0, 1))

ggplot(cactus, aes(x = slope, y = distance, col = cactus))+
  geom_point()

slope.model <- glm(ouch~slope, data =cactus, family=binomial)
summary(slope.model)

distance.model <- glm(ouch~distance, data =cactus, family=binomial)
summary(distance.model)

cactus$slope.predict <- round(predict(slope.model, newdata=cactus, type='response'),0)
cactus$distance.predict <- round(predict(distance.model, newdata=cactus, type='response'),0)

cactus <- cactus %>%
  mutate(ouch1 = as.factor(ouch)) %>%
  mutate(slope.predict1 = as.factor(slope.predict)) %>%
  mutate(distance.predict1 = as.factor(distance.predict))

confusionMatrix(data = cactus$ouch1, reference= cactus$slope.predict1)
confusionMatrix(data = cactus$ouch1, reference= cactus$distance.predict1)

ggplot(cactus, aes(x = slope, y = ouch))+
  geom_point( col = 'blue')+
  geom_smooth(method='glm', method.args=list(family='binomial'))

ggplot(cactus, aes(x = distance, y = ouch))+
  geom_point( col = 'red')+
  geom_smooth(method='glm', method.args=list(family='binomial'))
