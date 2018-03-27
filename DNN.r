
##### 神经网络-------------------
## 混凝土强度分析  ----

## Step 浏览数据----
# 读取数据
concrete <- read.csv("concrete.csv")
str(concrete)

# 规范化到0 1范围
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# 应用 normalization 到数据框，通过调用lapply
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# 考察强度范围落到0，1之间
summary(concrete_norm$strength)

# 考察原始值
summary(concrete$strength)

# 创建训练 和测试数据
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Step 3: 训练模型----
library(neuralnet)

#  创建单一神经元的神经网络 
set.seed(12345) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                              data = concrete_train)

# 可视化网络
plot(concrete_model)

## Step 4: 评估模型性能 ----
# 获得模型结果
model_results <- compute(concrete_model, concrete_test[1:8])
# 获取结果
predicted_strength <- model_results$net.result
# 考察真实值与预测值的相关性
cor(predicted_strength, concrete_test$strength)

## Step 5: 提高模型性能 ----
#  加入5个神经元
set.seed(12345) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                               data = concrete_train, hidden = 5)

# 查看
plot(concrete_model2)

# 评估模型
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
