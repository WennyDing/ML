##### 关联规则算法-------------------

## 零售商店频繁项集挖掘----
## Step : 浏览数据 ----

# 载入数据到稀疏矩阵
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)

# 检查前5个事务
inspect(groceries[1:5])

# 考察按字母排序的前3个item的购买频率
itemFrequency(groceries[, 1:3])

# 查看支持度为 10% 的商品，以及销售排名前20的商品
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# 可视化前5行交易
image(groceries[1:5])

# 可视化随机100个交易，少数列的点比较密集表示有频繁项
image(sample(groceries, 100))

## Step 3: 训练数据----
library(arules)

# 默认因为support = 0.1 confidence = 0.8，导致0规则学习
apriori(groceries)

# 变换参数
groceryrules <- apriori(groceries, parameter = list(support =
                          0.006, confidence = 0.25, minlen = 2))
groceryrules

## Step 4: 评估模型----
# 查看汇总信息
summary(groceryrules)

# 检查前3条规则
inspect(groceryrules[1:3])

## Step 5: 提升模型性能----

# 根据提升度排序规则取前五条
inspect(sort(groceryrules, by = "lift")[1:5])

# 查询包含浆果的规则
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# 写入规则到CSV 文集
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# 转换规则为数据框
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
