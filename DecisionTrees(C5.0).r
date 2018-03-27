## C5.0 决策树识别高风险银行信贷----
## Step 读取浏览数据 ----
credit <- read.csv("credit.csv")
str(credit)

# 查看部分特征
table(credit$checking_balance)
table(credit$savings_balance)

# 观察部分数字特征
summary(credit$months_loan_duration)
summary(credit$amount)

# 观察现有违约情况，YES表示违约 
table(credit$default)

# 创建随机采样的训练和测试数据
# 用 set.seed 保证随机数一样，采样1到1000个数字中的900个数字，用于数据集随机INDEX获取
set.seed(123)
train_sample <- sample(1000, 900)

str(train_sample)

# 基于随机数生成训练和测试数据

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# 检查两个数据集中的频率
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: 创建训练模型----
# 创建简单决策树
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

credit_model

#  显示数的信息
summary(credit_model)

## Step 4: 评估性能----
# 基于测试数据预测，生成为结果为因子向量
credit_pred <- predict(credit_model, credit_test)

# 交叉表评估
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: 性能提升----

##  Adaptive Boosting  算法（自适应增强算法）提升精准度
#   生成10棵决策树
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## 设置部分预测 成本比其他高

# 创建一个成本矩阵维度名称列表
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# 创建这个维度，例如当预测为No，实际是Yes 设置成本为4
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# 应用成本矩阵
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                          costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

