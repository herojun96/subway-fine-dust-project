library(dplyr)
library(stringr)
d1<-read.csv("data1.csv",header=TRUE)
d1<-tbl_df(d1)
d1
d2<-read.csv("data2.csv",header = T)
d2<-tbl_df(d2)
d2
sum(is.na(d1$X13.30))
d1_1<-d1%>%select(-호선,-역번호)
d1_1$혼잡도=ifelse(!is.na(d1_1$혼잡도),d1_1$혼잡도,0)
sum(is.na(d1_1$혼잡도))
d1_1
d1_1<-d1_1%>%gather(key=time,value = 혼잡도,starts_with("x"))
d1_1[is.na(d1_1$혼잡도),]
d1_1%>%group_by(역명)%>%summarise(평균=mean(혼잡도,na.rm=T))%>%arrange(desc(평균))
d1_1%>%filter(혼잡도>=170)
d1_1%>%group_by(역명)%>%summarise(평균=mean(혼잡도,na.rm=T))
d2_1<-d2%>%select(역사명)

for(i in 1:dim(d2)[1]) {
  a <- utf8ToInt(str_sub(d2$역사명[i], -1, -1))
  if( 47 < a && a < 60 ) {
    d2$역사명[i] <- str_sub(d2$역사명[i], 1, -2)
  }
}
d2


#################데이터 전처리 ########################
library(dplyr)
library(stringr)
d1<-read.csv("data1.csv",header=TRUE)
d1<-tbl_df(d1)
d1
d4<-read.csv("data4.csv",header = T)
d4

d1<-left_join(d1,d4,by='호선')

for(i in 1:dim(d1)[1]) {
  a <- utf8ToInt(str_sub(d1$호선[i], 1, 1))
  if( 47 < a && a < 60 ) {
    d1$호선[i] <- str_sub(d1$호선[i], 1, 1)
  }else{
    d1$호선[i]<-2
  }
}
d1_1<-d1%>%unite(역사명,역명,호선,sep="")
d1_1<-d1_1%>%gather(key=time,value = 혼잡도,starts_with("x"))
d1_1$혼잡도=ifelse(!is.na(d1_1$혼잡도),d1_1$혼잡도,0)
d1_1<-d1_1%>%group_by(역사명)%>%summarise(평균=mean(혼잡도),운행회수=mean(운행회수))
d1_1

d2<-read.csv("data2.csv",header = T)
d2<-tbl_df(d2)
d2
for(i in 1:dim(d2)[1]) {
  a <- utf8ToInt(str_sub(d2$역사명[i], -1, -1))
  if( 47 < a && a < 60 ) {
    d2$역사명[i] <- str_sub(d2$역사명[i], 1, -2)
  }
}
d2_1<-d2%>%unite(역사명,역사명,호선,sep='')

df<-left_join(d2_1,d1_1,by='역사명')
str(df)
df

plot(df$이산화탄소.CO2.,df$평균)
cor(df$이산화탄소.CO2.,df$일산화탄소.CO.,use='complete.obs')

############## 결측값 제거 및 데이터 정규화 ###############
df_1<-na.omit(df)
cor(df_1$미세먼지.PM10.,df_1$운행회수)
df_1_scaled<-scale(df_1[,-1])
write.csv(df,file = "C:/Users/khj/Dropbox/내 PC (DESKTOP-726R6N4)/Desktop/df.csv")

##############k-means 군집분석 ###########################
#elbow method를 통한 최적 군집수 선정 
library(factoextra)
library(fpc)
fviz_nbclust(df_1_scaled, kmeans, method = "wss")


df.kmeans <- kmeans(df_1_scaled, centers = 3, iter.max = 10000)
df.kmeans

# 실루엣 지수 도출  
library("cluster")
sil <- silhouette(df.kmeans$cluster, dist(df_1_scaled))
fviz_silhouette(sil)

#군집 시각화 
fviz_cluster(df.kmeans, data = df_1_scaled)


##################### EM알고리즘을 이용한 혼합분포군집 ########3
install.packages("mclust")
library(mclust)



df.mclust<-Mclust(df_1_scaled,G=2)
summary(df.mclust,parameters = T)

#실루엣 지수로 최적의 군집수 찾기
sil <- silhouette(df.mclust$classification, dist(df_1_scaled))
fviz_silhouette(sil)

#군집 시각화 
fviz_cluster(df.mclust, data = df_1_scaled)


######################계층적 군집분석 ###################
#elbow method
fviz_nbclust(df_1_scaled, hcut, method = "wss")


df.hclust <- hcut(df_1_scaled, k = 3, hc_method = "single")

df.hclust.classification<-cutree(df.hclust,k=3)

df_hclust<-as.data.frame(df_1_scaled)
df_hclust$class<-df.hclust.classification
df_hclust%>%group_by(class)%>%summarise(미세먼지=mean(미세먼지.PM10.),이산화탄소=mean(이산화탄소.CO2.),
                                            포름알데히드=mean(포름알데히드.HCHO.),일산화탄소=mean(일산화탄소.CO.),
                                            혼잡도=mean(평균),운행회수=mean(운행회수))

#실루엣 지수
sil <- silhouette(df.hclust.classification, dist(df_1_scaled))
fviz_silhouette(sil)

#덴드로그램 시각화 
fviz_dend(df.hclust, show_labels = FALSE, rect = TRUE)
#군집 시각화 
fviz_cluster(df.hclust, ellipse.type = "convex")


####################k-mediods 군집분석 ##################
#elbow method
fviz_nbclust(df_1_scaled, pam, method = "wss")


df.kmedoids<-pam(df_1_scaled,7)
df.kmedoids

#실루엣 지수
sil <- silhouette(df.kmedoids$clustering, dist(df_1_scaled))
fviz_silhouette(sil)


fviz_cluster(df.kmedoids, data = df)


#######################voting ####################

km<-which(df.kmeans$cluster=='1')

mc<-which(df.mclust$classification=='2')

hc<-which(df.hclust.classification=='2')

kmed<-which(df.kmedoids$clustering=='4')
#2번이상씹 겹치는 값 찾아내기 
b<-c()
for (i in 1:dim(df_1)[1]) {
  a=0
  a=sum(km==i)+sum(mc==i)+sum(hc==i)+sum(kmed==i)
  if(a >=2){
    b<-c(b,i)
  }
}
b
#2번이상 겹치는 값의 위치 
place<-df_1[b,1]
place  
# 최종적인 선정 위치들의 미세먼지,혼잡도,운행횟수 평균과 전체 데이터의 미세먼지,혼잡도,운행횟수 평균 비교
mean(df_1[b,2]$미세먼지.PM10.)
mean(df_1[,2]$미세먼지.PM10.)

mean(df_1[b,6]$평균)
mean(df_1[,6]$평균)

mean(df_1[b,7]$운행회수)
mean(df_1[,7]$운행회수)   # 모두 최종적인 선정위치들의 평균이 큰것을 확인-> 선정위치에 공기정화시스템 강화 
