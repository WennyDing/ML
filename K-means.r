##### k-means 聚类 -------------------

## 青少年市场细分 ----
## Step 浏览数据----
teens <- read.csv("snsdata.csv")
str(teens)

# 查看性别频率，已经考察NA频率
table(teens$gender)
table(teens$gender, useNA = "ifany")

# 参考年龄频率
summary(teens$age)

# 处理年龄异常，划分年龄段到13 到 20岁就直接用原来年龄，否则用NA
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                     teens$age, NA)

summary(teens$age)

# 哑变量处理女性列取1，0

teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
# 哑变量处理NA列取1，0

teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# 检查记录数据
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# 求年龄均值
mean(teens$age) # 有NA不能工作
mean(teens$age, na.rm = TRUE) # 删除NA

# age by 基于毕业年龄分组求出各年龄段平均值，用于插值法
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# 调用ave函数，（该函数返回具有重复分组均值的向量，作用与上面aggregate函数相同，但是 它用重复值保证与原始向量长度相同）
ave_age <- ave(teens$age, teens$gradyear,
                 FUN = function(x) mean(x, na.rm = TRUE))

#当NA时用ave_age替代NA，其他不变
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# 检查年龄数据
summary(teens$age)

## Step 3: 基于36个特征训练 经过标准化的数据----
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

## Step 4: 评估木箱----
# 查看clusters的大小
teen_clusters$size

# 查看聚类中心
teen_clusters$centers

## Step 5: 从聚类中分析信息----
# 把cluster IDs 放回原始 data frame
teens$cluster <- teen_clusters$cluster

# 检查前五条数据
teens[1:5, c("cluster", "gender", "age", "friends")]

# 按照cluster求年龄均值
aggregate(data = teens, age ~ cluster, mean)

#  求females比例 按照cluster
aggregate(data = teens, female ~ cluster, mean)

# 求每个类别的朋友数
aggregate(data = teens, friends ~ cluster, mean)
