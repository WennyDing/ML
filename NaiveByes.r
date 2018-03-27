##### 贝叶斯分类--------------------

## 垃圾短信过滤----
## Step : 浏览数据---- 

# 读取数据，并且避免把数据作为因子读入，同时指定数据字符集
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE,encoding='UTF-8')
# 检查数据结构
str(sms_raw)

# 中文环境下可能需要重新命名列
names(sms_raw)[1]<-'type'

# 转换 spam/ham 为因子.
sms_raw$type <- factor(sms_raw$type)

# 检查类别变量
str(sms_raw$type)
table(sms_raw$type)

# 创建 corpus 文集 使用 text mining (tm) package 文本挖掘包。
# VectorSource用于读取文本，并调用 VCorpus打包为文集
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# 检查文集的文档数
print(sms_corpus)
# 检查文集的1到3文档
inspect(sms_corpus[1:3])
view1<-inspect(sms_corpus[1:3])

# 检查文集的1到3文档的内容
view1[[1]]$content
view1[[2]]$content
view1[[3]]$content

# 用as.character检查文集的1档的内容等同上面的view1[[1]]$content


as.character(sms_corpus[[1]])

#或者调用lapply 转换为文本检查

lapply(sms_corpus[1:3], as.character)

# 使用 tm_map() 进行ETL的T转换，现实中，中文环境基本全部需要自己开发此处为转换成小写
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# 对比检查转换前后转换后数据
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# 删除数字 ，to, and, but, and or 等STOP词，删除符号
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # remove stop words

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # remove punctuation

# 补充表达式函数，用1个空格替代符号，后续用tm_map完成此操作
removePunctuation("hello...world")
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
replacePunctuation("hello...world")

# Snowball 包可以处理相同词不同形式

library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

#  处理相同词的不同形式
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# 处理空格
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # eliminate unneeded whitespace


# 检查结果
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# 创建稀疏矩阵，提升性能

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)


# 检查结果
sms_dtm

# 创建训练集及测试集
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

# 保持分类标签

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# 检查2个数据集的频率
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# 词云可视化
library(wordcloud)

# wordcloud 用例，传入字母表 26个字母，设置每个字母的频数1到26 ，a为1 Z为26,频数多的居中

wordcloud(c(letters), seq(1, 26),random.order=F)

# wordcloud 用例 ，显示频数最多的3个词
wordcloud(iris$Species, max.words=3)


# 本例中wordcloud 可以直接支持corpus 对象，min.freq 设置 一个词出现50次才出现在词云，如果还是报错，可以加大min.freq 

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# 将训练数据分2组，分别观察词云
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

# 分别观察词云，要求，最多40个词，字体在3到0.5之间

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#低于 99.9% 的稀疏词,本质是降维度操作，或者用后面的直接找出高频词保存
sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train

# 找出频数大于5的词
findFreqTerms(sms_dtm_train, 5)

# 保存高频词
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# 保存这些频繁词
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# 传换频数为 yes or No，判断传入单词如果存在在一条消息，表示YES，否则为NO
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() 基于列 MARGIN = 2 ，应用convert_counts() 到 train/test 数据
# 之前学的lapply是apply函数的简化版
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

## Step 3 训练模型----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

## Step 4: 评估性能
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: 提升性能
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
