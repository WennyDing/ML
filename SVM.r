##### Support Vector Machines -------------------
## 光学符号识别----

## 浏览数据----
# 读取数据及考察结构
letters <- read.csv("letterdata.csv")
str(letters)

# 拆分训练及测试数据
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## Step 3: 训练模型----
# 基于线性核训练
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

# 查看模型
letter_classifier

## Step 4: 评估模型性能----
# 预测测试数据及对比
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# 用TRUE FALSE查看模型性能
# 构造矩阵  TRUE/FALSE  对应 正确/错误 预测
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Step 5: 提升模型性能尝试高斯核----
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
