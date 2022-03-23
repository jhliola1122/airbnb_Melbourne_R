library(ggplot2)
library(dplyr)
library(MASS)
library(forecast)
library(tidyr)
library(FNN)
library(caret)
library(e1071)
library(leaps)
library(rpart)
library(rpart.plot)
library(factoextra)
library(NbClust)
# Central Business District in Australia
# 读取数据
df_melbourne<- read.csv("melbourne.csv") 
#删除空值
#df_melbourne1<-na.omit(df_melbourne)
# 查看数据概况 [1] 22895    84
dim(df_melbourne)
# 筛选地区
CBD<-subset(df_melbourne,host_neighborhood=="Central Business District")
# 筛选需要的列 
cbd <- subset(CBD, select = c("id","host_id", "host_since", "host_location", "host_response_rate", 
                              "host_is_superhost", "is_location_exact", "property_type", "room_type",  
                              "accommodates", "bathrooms", "bedrooms", "beds", "amenities", "price", 
                              "security_deposit", "cleaning_fee", "guests_included", "extra_people", 
                              "minimum_nights", "maximum_nights", "availability_365", "number_of_reviews", 
                              "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness", 
                              "review_scores_checkin", "review_scores_communication", 
                              "review_scores_location", "review_scores_value", "instant_bookable", 
                              "cancellation_policy","host_identity_verified"))
# 查看数据概况 [1] 3537   33
dim(cbd) 
# Missing Value 
anyNA(cbd) # True

# 删除缺失数据
df_cbd<-na.omit(cbd)
anyNA(df_cbd)
# 查看数据描述,发现price最小为0，bedrooms最小为0
summary(df_cbd) 
# 过滤异常值
df_cbd<-filter(df_cbd,price!=0,bedrooms!=0)
# 处理host_since日期
df_cbd<-separate(df_cbd,host_since,c("host_month","host_date","host_year"),sep="/")
# 删除host_month,hos_date
df_cbd<-df_cbd[,-c(3,4)]
# 添加列price_perbedroom
df_cbd<-mutate(df_cbd,price_perbedroom=price/bedrooms)
dim(df_cbd)
# Summary Statistics 
#1 How the overall performance of price-related variables in Central Business District, Australia? 
# Barplot/Line 
CBDprice_summary<-dplyr::select(df_cbd,price,security_deposit, cleaning_fee, extra_people)
summary(CBDprice_summary)
# 2 What is the main renting property type related with prices in Central Business District? 
# Scatterplot 
Central_GroupBy<-dplyr::select(df_cbd, property_type, price) %>% group_by(property_type)
Central_Summarise<-summarise(Central_GroupBy, Num=n(), TotalPrice=sum(price), MaxPrice=max(price),
                             RangePrice=max(price)-min(price),
                             MinPrice=min(price),AvgPrice=mean(price), MedPrice=median(price),
                             SdPrice=sd(price)) %>%
  arrange(desc(Num))
Central_Summarise
# 3 What is the trend of the number of hosts as times went by? 
# Line/Barplot
df_cbd$host_year<- as.numeric(df_cbd$host_year)
table(df_cbd$host_year)

# 4 What is price difference between superhost and non-superhost? 
# Historgram 
df_superhost<-filter(df_cbd,df_cbd$host_is_superhost=="t")
summary(df_superhost$price_perbedroom)
df_nonsuperhost<-filter(df_cbd,df_cbd$host_is_superhost=="f")
summary(df_nonsuperhost$price_perbedroom)
count(df_nonsuperhost)
count(df_superhost)
# 5 What is Central Business District neighborhood overall review scores rating compare with all neighborhoods'? 
#Boxplot
CBDrating_summary<-dplyr::select(df_cbd,review_scores_rating)
summary(CBDrating_summary)
sd(CBDrating_summary$review_scores_rating)
#删除全墨尔本数据集空值
df_melbourne1<-na.omit(df_melbourne)
summary(df_melbourne1$review_scores_rating)
sd(df_melbourne1$review_scores_rating)

# data visualization
# 1 line
haha <- group_by(df_cbd,host_year)
hehe <- summarize(haha,number_of_registered_host = n())
yiyuan <- ggplot(hehe,aes(x=host_year,y=number_of_registered_host)) +
  geom_line(color="red",arrow = arrow())+
  ggtitle("Distribution of registered host each year") + 
  labs(x ="Year",y ="number of registered host") +
  theme(panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white'),
        plot.title =element_text(colour = "blue"),
        plot.caption=element_text(colour = "blue"),
        axis.line = element_line(colour = "grey80"),
        axis.text = element_text(colour = "blue"),
        axis.title = element_text(colour = "grey80"))
yiyuan

# 2 barplot
xswl <- group_by(df_cbd,property_type)
nsdd <- summarize(xswl,avg_price_perbedroom = mean(price_perbedroom))
chenxi <- ggplot(nsdd,aes(x=reorder(property_type,avg_price_perbedroom),y=avg_price_perbedroom))+geom_bar(stat="identity",color="yellow",fill="purple") + theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x="property_type",y="avg_price") +
  ggtitle("property_type influence on price") +
  theme(panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white'),
        plot.title =element_text(colour = "blue"),
        plot.caption=element_text(colour = "blue"),
        axis.line = element_line(colour = "grey80"),
        axis.text = element_text(colour = "blue"),
        axis.title = element_text(colour = "grey80"))
chenxi

# 3 boxplot
jiahui <- ggplot(df_cbd,aes(x=host_is_superhost,y=price_perbedroom)) + geom_boxplot(fill="red") +
  labs(title = "the distribution of price per bedroom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white'),
        plot.title =element_text(colour = "blue"),
        plot.caption=element_text(colour = "blue"),
        axis.line = element_line(colour = "grey80"),
        axis.text = element_text(colour = "blue"),
        axis.title = element_text(colour = "grey80"))
jiahui

# 4 histogram
df_cbd$cancellation_policy <- droplevels(df_cbd$cancellation_policy)
yusong <- ggplot(df_cbd,aes(x=host_is_superhost,fill=cancellation_policy)) + geom_histogram(stat="count") +
  labs(title = "distribution of superhost and no_superhost") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white'),
        plot.title =element_text(colour = "blue"),
        plot.caption=element_text(colour = "blue"),
        axis.line = element_line(colour = "grey80"),
        axis.text = element_text(colour = "blue"),
        axis.title = element_text(colour = "grey80"))
yusong

# 5 scatterplot
df_pr <- df_cbd[,c(1,15,24)]
row.names(df_pr) <- df_pr[,1]
df_pr <- df_pr[,-1]
df_pr.norm<- sapply(df_pr,scale)
row.names(df_pr.norm)<- row.names(df_pr)
df_pr.norm<- as.data.frame(df_pr.norm)
df_pr.norm<- cbind(df_pr.norm,df_cbd$host_is_superhost)
colnames(df_pr.norm)[colnames(df_pr.norm)=="df_cbd$host_is_superhost"] <- "host_is_superhost"
wuyue <- ggplot(df_pr.norm,aes(x=review_scores_rating,y=price,color=host_is_superhost)) + geom_point(shape=17) +
  labs(title = "scatterplot of price vs review_rating by superhost") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(-7.5,1) + ylim(-1.5,5) +
  theme(panel.background = element_rect(fill='white'),
        plot.background = element_rect(fill='white'),
        plot.title =element_text(colour = "blue"),
        plot.caption=element_text(colour = "blue"),
        axis.line = element_line(colour = "grey80"),
        axis.text = element_text(colour = "blue"),
        axis.title = element_text(colour = "grey80"))
wuyue

# Multiple regression
# data preparation and set the training set and valid set
# "price","host_is_superhost","room_type","accommodates","bathrooms","bedrooms",
#"beds","cleaning_fee","extra_people","availability_365","review_scores_rating"
#reg_selected.var <- c(15,6,9,10,11,12,13,17,19,22,24,31)
reg_selected.var <- c("price","host_is_superhost","accommodates","bathrooms","bedrooms",
                      "beds","cleaning_fee","extra_people","availability_365","review_scores_rating")
newcbd <- df_cbd[reg_selected.var]
newcbd <- newcbd[,-c(3)]
# host_is_superhost 类别变量变哑变量
newcbd$host_is_superhost <- ifelse(newcbd$host_is_superhost=='t',1,0)
str(newcbd)
# sample dataset
set.seed(88)
newcbd <- sample_n(newcbd,dim(newcbd)[1])
train.index <- sample(c(1:dim(newcbd)[1]), dim(newcbd)[1]*0.6)
reg_train.df <- newcbd[train.index, reg_selected.var]
reg_valid.df <- newcbd[-train.index, reg_selected.var]
# 相关性
cor_cbd<- cor(newcbd)
View(cor_cbd)
# 删除相关性低的属性 #host_is_superhost,extra_people,availability_365,review_scores_rating
reg_train.df <- reg_train.df[,-c(2,8,9,10)]
reg_valid.df <- reg_valid.df[,-c(2,8,9,10)]
# regression model
price.lm <- lm(price~.,data=reg_train.df)
summary(price.lm) 
price.step.lm <- step(price.lm,direction = "backward")
summary(price.step.lm)
# accuracy of the model
reg_pred1<-predict(price.step.lm,reg_train.df)
accuracy(reg_pred1,reg_train.df$price)
reg_pred2<-predict(price.step.lm,reg_valid.df)
accuracy(reg_pred2,reg_valid.df$price)

# knn prediction (predict the cancellation fee of a new property)
# data preparation and set the training set and valid set
knn_selected.var <- c(32,10,12,16,17,22,24,34)
knn_train.df <- df_cbd[train.index,knn_selected.var]
knn_valid.df <- df_cbd[-train.index,knn_selected.var]
anyNA(knn_train.df)
# build our apt
df.stairway_to_heaven<-data.frame(accommodates=6,bedrooms=3,
                                  security_deposit=315,cleaning_fee=110,
                                  availability_365=365,review_scores_rating=100,
                                  price_perbedroom=222)
# data nomalization
knn_train.df$cancellation_policy <- as.factor(knn_train.df$cancellation_policy)
knn_valid.df$cancellation_policy <- as.factor(knn_valid.df$cancellation_policy)
knn_train.norm.df <- knn_train.df
knn_valid.norm.df <- knn_valid.df
norm.values <- preProcess(knn_train.df[,-1],method=c("center","scale"))
knn_train.norm.df[,-1] <- predict(norm.values,knn_train.df[,-1]) 
knn_valid.norm.df[,-1] <- predict(norm.values,knn_valid.df[,-1])
df.norm.stairway_to_heaven <- predict(norm.values,df.stairway_to_heaven)
# choose the best k value
accuracy.df <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))
for(i in 1:15) {
  knn.pred <- knn(knn_train.norm.df[,-1], knn_valid.norm.df[,-1],
                  cl = knn_train.norm.df[,1], k=i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, knn_valid.norm.df[, 1])$overall[1]
}
accuracy.df # i=9 accuracy=0.6737

# knn model
# predict
nn <- knn(train=knn_train.norm.df[,-1],test=df.norm.stairway_to_heaven,cl=knn_train.norm.df[,1],k=9)
nn
row.names(knn_train.df)[attr(nn,"nn.index")]
# Levels: moderate

# Naive Bayes （classify and show if new property is instant bookable)
# data preparation
Bayes_selected.var <- c(31,6,9,10,12,15,16,17,19,24,32,33)
Bayes_instant.df <- df_cbd[,Bayes_selected.var]
str(Bayes_instant.df)
# 将连续数字型变量转变为类别型变量
## price
range(Bayes_instant.df$price)
hist(Bayes_instant.df$price)
Bayes_instant.df$price <- cut(Bayes_instant.df$price, breaks= c(-Inf,120,200,Inf),labels = c("low price","mid price","high price"))
anyNA(Bayes_instant.df$price)
table(Bayes_instant.df$price)
## security_deposit
range(Bayes_instant.df$security_deposit)
hist(Bayes_instant.df$security_deposit)
Bayes_instant.df$security_deposit <- cut(Bayes_instant.df$security_deposit, breaks = c(-Inf,200,400,Inf), labels = c("low deposit","mid deposit","high deposit"))
table(Bayes_instant.df$security_deposit)
anyNA(Bayes_instant.df$security_deposit)
## cleaning_fee
range(Bayes_instant.df$cleaning_fee)
hist(Bayes_instant.df$cleaning_fee)
Bayes_instant.df$cleaning_fee <- cut(Bayes_instant.df$cleaning_fee, breaks = c(-Inf,60,100,Inf),labels = c("low cleaningfee","mid cleaningfee","high cleaningfee"))
table(Bayes_instant.df$cleaning_fee)
anyNA(Bayes_instant.df$cleaning_fee)
## extra_people
range(Bayes_instant.df$extra_people)
hist(Bayes_instant.df$extra_people)
Bayes_instant.df$extra_people <- cut(Bayes_instant.df$extra_people, breaks = c(-Inf,20,40,Inf), labels = c("low extra","mid extra","high extra"))
anyNA(Bayes_instant.df$extra_people)
table(Bayes_instant.df$extra_people)
## review_scores_rating
range(Bayes_instant.df$review_scores_rating)
hist(Bayes_instant.df$review_scores_rating)
Bayes_instant.df$review_scores_rating <- cut(Bayes_instant.df$review_scores_rating, breaks = c(-Inf,80,95,Inf), labels = c("low rating","mid rating","high rating"))
anyNA(Bayes_instant.df$review_scores_rating)
table(Bayes_instant.df$review_scores_rating)
# 将非连续型变量类型改为类别变量
str(Bayes_instant.df)
Bayes_instant.df$instant_bookable <- as.factor(Bayes_instant.df$instant_bookable)
Bayes_instant.df$host_is_superhost <- as.factor(Bayes_instant.df$host_is_superhost)
Bayes_instant.df$room_type <- as.factor(Bayes_instant.df$room_type)
Bayes_instant.df$accommodates <- as.factor(Bayes_instant.df$accommodates)
Bayes_instant.df$bedrooms <- as.factor(Bayes_instant.df$bedrooms)
Bayes_instant.df$cancellation_policy <- as.factor(Bayes_instant.df$cancellation_policy)
Bayes_instant.df$host_identity_verified <- as.factor(Bayes_instant.df$host_identity_verified)
# set training set and valid set
Bayes_train.df <- Bayes_instant.df[train.index, ]
Bayes_valid.df <- Bayes_instant.df[-train.index, ]
# naive bayes
instant.nb <- naiveBayes(instant_bookable ~ ., data = Bayes_train.df)
instant.nb
pred_bayes1 <- predict(instant.nb,newdata = Bayes_train.df)
a <- confusionMatrix(pred_bayes1, Bayes_train.df$instant_bookable)$overall[1]
a
pred_bayes2 <- predict(instant.nb, newdata = Bayes_valid.df)
b <- confusionMatrix(pred_bayes2, Bayes_valid.df$instant_bookable)$overall$overall[1]
b
# build a fictional apartment
high_hopes_apt <- data.frame(host_is_superhost = "t",host_identity_verified = "t", 
                             room_type = "Entire home/apt", bedrooms = 3, accommodates = 6,
                             price = "mid price", security_deposit = "mid deposit", 
                             cleaning_fee = "mid cleaningfee", extra_people = "low extra", 
                             review_scores_rating = "high rating", 
                             cancellation_policy = "moderate")
# test new apartment with naive bayes
str(high_hopes_apt)
high_hopes_apt <- as.factor(high_hopes_apt)
high_hopes_apt$bedrooms <- as.factor(high_hopes_apt$bedrooms)
high_hopes_apt$accommodates <- as.factor(high_hopes_apt$accommodates)
pred_bayes3 <- predict(instant.nb, newdata = high_hopes_apt)
pred_bayes3

# classification tree (classify the cleaning fee)
# Data preparation
ct_selected.var <- c(17,6,9,11,12,15,26)
ct.df <- df_cbd[,ct_selected.var]
ct.df$cleaning_fee <- cut(ct.df$cleaning_fee, breaks = c(-Inf,0,60,100,Inf),labels = c("no fee","low cleaningfee","mid cleaningfee","high cleaningfee"))
# Get the training set and the valid set
ct_train.df <- ct.df[train.index,]
ct_valid.df <- ct.df[-train.index,]
# Build the tree
ct.cleaningfee.1 <- rpart(cleaning_fee ~ ., method = "class", xval=5, cp=0.00, data = ct_train.df)
ct.plot1 <- rpart.plot(ct.cleaningfee.1)
# determine the optimal cp value
may <- printcp(ct.cleaningfee.1)
may <- as.data.frame(may)
which.min(may$xerror)
# tree model plot
ct.cleaningfee.2 <- rpart(cleaning_fee ~ ., method = "class", xval=7, cp=0.00435074, data = ct_train.df)
ct.plot2 <- rpart.plot(ct.cleaningfee.2, tweak = 1.3, type = 0)
# test the tree model
ct.model.pred1 <- predict(ct.cleaningfee.2, ct_train.df, type = "class")
confusionMatrix(ct.model.pred1,ct_train.df$cleaning_fee)
ct.model.pred2 <- predict(ct.cleaningfee.2, ct_valid.df, type = "class")
confusionMatrix(ct.model.pred2,ct_valid.df$cleaning_fee)

# clustering
cluster_select.var <- c(1,10,12,15,16,17,24,34)
cluster_cbd <- df_cbd[,cluster_select.var]
row.names(cluster_cbd) <- cluster_cbd[,1]
cluster_cbd <- cluster_cbd[,-1]
cluster_cbd.norm <- sapply(cluster_cbd,scale)
row.names(cluster_cbd.norm) <- row.names(cluster_cbd)
cluster_cbd.norm <- as.data.frame(cluster_cbd.norm)
# Elbow rules
fviz_nbclust(cluster_cbd.norm, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(cluster_cbd.norm, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#  build the k-means model
km<- kmeans(cluster_cbd.norm,6)
km$cluster
km$centers
dist(km$centers)
cluster_cbd.norm<- cbind(cluster_cbd.norm,km$cluster)
colnames(cluster_cbd.norm)[colnames(cluster_cbd.norm)=="km$cluster"]<- "cluster"
# 5 label the cluster
cluster_cbd.norm$cluster[cluster_cbd.norm$cluster==1]<- "apple" 
cluster_cbd.norm$cluster[cluster_cbd.norm$cluster==2]<- "banana" 
cluster_cbd.norm$cluster[cluster_cbd.norm$cluster==3]<- "orange" 
cluster_cbd.norm$cluster[cluster_cbd.norm$cluster==4]<- "peach" 
cluster_cbd.norm$cluster[cluster_cbd.norm$cluster==5]<- "melon" 
cluster_cbd.norm$cluster[cluster_cbd.norm$cluster==6]<- "grape" 

