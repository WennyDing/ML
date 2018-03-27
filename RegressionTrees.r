
#### 回归树与模型树-------------------


## Example: 评估葡萄酒品级----
## Step : 浏览数据----
wine <- read.csv("whitewines.csv")

str(wine)

# the distribution of quality ratings
hist(wine$quality)

# summary statistics of the wine data
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Step 3: 训练模型----
# 使用rpart生成会归树
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

m.rpart

# 查看树的详细信息
summary(m.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# 可视化数据
rpart.plot(m.rpart, digits = 3)

# 可视化数据

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Step 4: 评估模型性能----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, wine_test)

#  比较预测值与真正值的分布
summary(p.rpart)
summary(wine_test$quality)

# 比较相关性
cor(p.rpart, wine_test$quality)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, wine_test$quality)

# mean absolute error between actual values and mean value
mean(wine_train$quality) # result = 5.87
MAE(5.87, wine_test$quality)

## Step 5: 提升性能----
# 训练 M5' 模型树
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)

# display the tree
m.m5p

# get a summary of the model's performance
summary(m.m5p)

# generate predictions for the model
p.m5p <- predict(m.m5p, wine_test)

# summary statistics about the predictions
summary(p.m5p)

# correlation between the predicted and true values
cor(p.m5p, wine_test$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.m5p)
