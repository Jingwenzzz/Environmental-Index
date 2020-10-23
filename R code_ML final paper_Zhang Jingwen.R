---
  title: "Term Paper" 
subtitle: "Supervised Machine Learning"
author: "Jingwen Zhang(17-742-487) "
date: "17/07/2020"
output: pdf_document
---
  
# data preparation
library(readxl)
dataset <- read_excel("climpolforwb.xlsx")
work <- round(dataset[,2:11], digits = 2)
work <- work[,-7]
rownames(work) <- dataset$country
# replaced with medians
work$gdpgrowth[is.na(work$gdpgrowth)] <- median(work$gdpgrowth, na.rm = T)
work$democracy[is.na(work$democracy)] <- median(work$democracy, na.rm = T)
work$tradeopen[is.na(work$tradeopen)] <- median(work$tradeopen, na.rm = T)
work$CO2cap1990[is.na(work$CO2cap1990)] <- median(work$CO2cap1990, na.rm = T)
# re-scale of trade openess
work$tradeopen <- work$tradeopen*100
work$democracy <- work$democracy*14.28571

# to show the table of descriptive statistics
library(stargazer)
myvar <- as.data.frame(work)
colnames(myvar) <- c("GDP Growth","Democracy Index","Income","Outcome","Output","CCI","Trade openness","CO2cap1990","EPI")
stargazer(myvar,type = "latex",out="descriptivedata.html",digits = 2,title="Descriptive statistics",df=F,header=F)

# GLM & its summary table
model.lin <- glm(EPI ~., data = work)
summary(model.lin)
library(stargazer)
stargazer(model.lin, header=T, caption=T,out.header = FALSE,out="glmresults.tex",type='latex')

# partial least square
library(pls)
set.seed(6180, kind = "Mersenne-Twister", normal.kind = "Inversion") 
pls.fit <- plsr(EPI~., data = work, scale = TRUE, validation = "LOO")
validationplot(pls.fit)
summary(pls.fit)
coefficients(pls.fit)
load <- loadings(pls.fit)
load[,1] # loadings on the first component

# random forest regression
library(randomForest)
set.seed(6180, kind = "Mersenne-Twister", normal.kind = "Inversion") 
# 2 features are selected at each split
rf.fit1 <- randomForest(EPI~., data = work, mtry = 2, validation = "LOO", importance = TRUE)
rf.fit1
rf.fit1$importance
varImpPlot(rf.fit1, main = NULL)

# divide EPI into 4 classes, and subset of important features derived above
quantile(work$EPI, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 6)
work1 <- as.data.frame(work)
work1$EPI.class <- NA
work1$EPI.class[work$EPI < 34.40] <- "1"
work1$EPI.class[work$EPI >= 55.25] <- "4"
work1$EPI.class[work$EPI >= 44.00 & work$EPI < 55.25] <- "3"
work1$EPI.class[work$EPI >= 34.40 & work$EPI < 44.00] <- "2"
work1.1 <- work1[,-(4:7)]
work1.1 <- work1.1[,-1]
work1.1 <- work1.1[,-4]
work1.1$EPI.class <- as.factor(work1.1$EPI.class)
work1.1$democracy <- round(work1.1$democracy, digits = 2)
work1.1 <- as.data.frame(work1.1)

# C5.0 algorithm
library(C50)
set.seed(9034,
         kind = "Mersenne-Twister",
         normal.kind = "Inversion")
indx1 <- createDataPartition(work1.1$EPI.class, p = 0.80, list = FALSE) 
dataTrain1 <- work1.1[indx1,1:3]
dataTest1 <- work1.1[-indx1,1:3]
labelTrain1 <- work1.1[indx1, 4, drop = TRUE] 
labelTest1 <- work1.1[-indx1, 4, drop = TRUE]
control1 <- C5.0Control(minCases = 10)
mytree <- C5.0(dataTrain1, labelTrain1, control = control1) 

pred <- predict(mytree, newdata = dataTest1) 
mytable <- table(labelTest1, pred)
mycon <- confusionMatrix(mytable) 
mycon$overall

plot(mytree)

# 4 classes of EPI and its plot on the world map
work1.2 <- work1.1
work1.2$country <- NA
work1.2$country <- dataset$country
library(maps)
world <- map_data("world")
world$region[world$region=="USA"] <- "United States"
world$region[world$region=="UK"] <- "United Kingdom"
world$region[world$region=="Greenland"] <- "Denmark"
work1.2$country[work1.2$country=="Germany Federal Republic of"] <- "Germany"
mapjoined <- left_join(world, work1.2, by = c('region' = 'country'))
ggplot() +
  geom_polygon(data = mapjoined, aes(x = long, y = lat, group = group, fill=EPI.class, color=EPI.class)) +
  labs(title = "4 classes of Environmental Performance Index 2020", 
       x=NULL, y=NULL) +
  coord_equal() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank())

# 4 classes of income and its plot on the world map
quantile(work$income, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 6)

work1.2$income.class <- NA
work1.2$income.class[work1.2$income < 7.135] <- "1"
work1.2$income.class[work1.2$income >= 8.915] <- "4"
work1.2$income.class[work1.2$income >= 8.120 & work1.2$income < 8.915] <- "3"
work1.2$income.class[work1.2$income >= 7.135 & work1.2$income < 8.120] <- "2"

mapjoined <- left_join(world, work1.2, by = c('region' = 'country'))
ggplot() +
  geom_polygon(data = mapjoined, aes(x = long, y = lat, group = group, fill=income.class, color=income.class)) +
  labs(title = "4 classes of Income (GDP per capita in PPP)", 
       x=NULL, y=NULL) +
  coord_equal() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank())
