#國民幸福指數抽樣分析
#資料來源 : (行政院統計處) https://ws.ndc.gov.tw/Download.ashx?u=LzAwMS9hZG1pbmlzdHJhdG9yLzEwL1JlbEZpbGUvNTU2Ni82MjUxLzAwMTk3NzQucGRm&n=NS4y5pS%2F562W5b%2Br6YGeMS5wZGY%3D&icon


#國民幸福指指數統計架構與內容
#國民幸福指數涵蓋2大面向,包含物質生活條件及生活品質,共11領域(居住條件、所得與財富、工作與收入、社會聯繫、教育與技能、環境品質等)


#國際指標我國與OECD及其夥伴國美好生活指數,top(1~10)
library(ggplot2) 
library(gcookbook)

df <- data.frame(
  country=c("Astralia (澳洲)","Sweden(瑞典)","Norway(挪威)","Switzerland(瑞士)","Denmark(丹麥)","Canada(加拿大)","United States(美國)","New Zealand(紐西蘭)","Iceland(冰島)","Finland芬蘭"),
  score=c(8.02,7.96,7.95,7.88,7.87,7.75,7.67,7.63,7.57,7.53))

#df <- data.frame(country,score)

ggplot(data=df,aes(label=score,y=score,x=country, title="我國與OECD及其夥伴國美好生活指數"))+
  geom_bar(stat="identity",fill=3)+
  geom_text(data=df,aes(label=score),size=3)+
  labs(title="我國與OECD及其夥伴國美好生活指數(2015年,領域等權重,top(1~10)")+
  coord_flip()
 
#國際指標我國與OECD及其夥伴國美好生活指數,top(11~20)
library(ggplot2) 
library(gcookbook)

df <- data.frame(
  country=c("Netherlands (荷蘭)","Ireland(愛爾蘭)","Germany(德國)","Belgium(比利時)","Luxeumbourg(盧森堡)","England(英格蘭)","Austria(奧地利)","Taiwan(中華民國民國)","Feance(法國)","Spain(西班牙)"),
  score=c(7.46,7.35,7.34,7.31,7.26,7.10,6.90,6.76,6.46,6.18))

#df <- data.frame(country,score)

ggplot(data=df,aes(label=score,y=score,x=country, title="我國與OECD及其夥伴國美好生活指數"))+
  geom_bar(stat="identity",fill=4)+
  geom_text(data=df,aes(label=score),size=3)+
  labs(title="我國與OECD及其夥伴國美好生活指數(2015年,領域等權重,top(11~20)")+
  coord_flip()



#物質生活條件面向
#居住條件領域
library(gcookbook)

test3<- data.frame(
  Year = c(2007,2008,2009,2010,2011,2012,2013,2014),
  倍 = c(5.8,5.9,6.7,7.1,7.3,7.8,8.4,8.4)
)
#print(test3)

ggplot(test3, aes(x = Year, y = 倍 )) + geom_line() + 
       geom_point(size = 3, shape = 22, colour = "darkred", fill = "pink") + 
       ylim(5, max(test3$ 倍 )) + ggtitle("2007至2014年房價所得比")

#工作與收入領域
library(ggplot2)
library(gcookbook)

year<-c(2007,2008,2009,2010,2011,2012,2013,2014)
teenager<-c(62.8,62.7,61.3,62,62.7,63,63.3,63.8)
adult<-c(0.55,0.62,0.95,0.96,0.73,0.69,0.67,0.66)
dd <- data.frame(year,teenager,adult)
ggplot()+geom_line(data = dd,aes(x = year,y = teenager,colour = "就業"),size=1)+
  geom_point(data = dd,aes(x = year,y = teenager,colour = "就業"),size=3)+
  ylim(0,64)+
  geom_line(data = dd,aes(x = year,y = adult,colour ="失業"),size=1) + 
  geom_point(data = dd,aes(x = year,y = adult,colour = "失業"),size=3)+
  scale_colour_manual("",values = c("就業" = "blue","失業" = "red"))+
  xlab("Year")+ylab("percentage(%)")+ ggtitle("2007至2014就業及失業率")

#就業率
library(ggplot2) 
library(gcookbook)

test<- data.frame(
  Year = c(2007,2008,2009,2010,2011,2012,2013,2014),
  percentage = c(62.8,62.7,61.3,62,62.7,63,63.3,63.8)
)
print(test)

ggplot(test, aes(x = Year, y =  percentage)) + geom_line() + 
       geom_point(size = 3, shape = 22, colour = "darkred",fill = "blue") +
       ylim(60, max(test$ percentage)) + ggtitle("2007 至 2014 就業率")
labels=c("2007","2008", "2009","2010","2011","2012","2013","2014")

#長期失業率
labels=c("2007","2008", "2009","2010","2011","2012","2013","2014")
percentage=c(0.05,0.12,0.45,0.46,0.23,0.19,0.17,0.16)+0.5

bp=barplot(percentage,
           names.arg =labels,
           ylab = "percentage(%)",
           main="長期失業率",
           xlab="year",
           col="red"
)

text(bp,percentage-0.05,
     labels=format(percentage),
     col="black"
)

#薪資收入
test3<- data.frame(
  Year = c(2007,2008,2009,2010,2011,2012,2013,2014),
  元 = c(46650,45038,43193,44989,45098,44726,44446,45940)
)
print(test3)

ggplot(test3, aes(x = Year, y = 元)) + geom_line() +
       geom_point(size = 3, shape = 22, colour = "darkred",fill = "purple") + 
       ylim(40000, max(test3$元)) + ggtitle("2007至2014年薪資收入")

#生活品質面向
#工作與生活平衡
library(ggplot2)
datn <- read.table(header=TRUE,text="
                   Sex    Years Percentage
                   Male   2008  11.64
                   Male   2009  9.49
                   Male   2010  9.94
                   Male   2011  10.15
                   Male   2012  9.97
                   Male   2013  13.26
                   Male   2014  12.63
                   Female 2008  8.61
                   Female 2009  7.90
                   Female 2010  7.15
                   Female 2011  7.57
                   Female 2012  8.05
                   Female 2013  10.38
                   Female 2014  9.26
                   Total  2008  10.26
                   Total  2009  8.75
                   Total  2010  8.65
                   Total  2011  8.97
                   Total  2012  9.08
                   Total  2013  11.92
                   Total  2014  11.06
                   ")

ggplot(data = datn, aes(x = Years,y = Percentage, group = Sex, colour = Sex))+
       geom_line(size=1)+geom_point(size=3)+labs(title="受雇者工時過長比率")

#健康狀況(平均壽命)
library(ggplot2)
library(gcookbook)

year<-c(2008,2009,2010,2011,2012,2013,2014)
teenager<-c(78.4,78.6,79,79.2,79.1,80,79.8)
adult<-c(70.1,70.2,70.5,70.7,70.1,70.8,71.1)
dd <- data.frame(year,teenager,adult)
ggplot()+geom_line(data = dd,aes(x = year,y = teenager,colour = "平均壽命"),size=1)+
  geom_point(data = dd,aes(x = year,y = teenager,colour = "平均壽命"),size=3)+
  ylim(70,85)+
  geom_line(data = dd,aes(x = year,y = adult,colour ="健康平均餘命"),size=1) + 
  geom_point(data = dd,aes(x = year,y = adult,colour = "健康平均餘命"),size=3)+
  scale_colour_manual("",values = c("平均壽命" = "blue","健康平均餘命" = "green"))+
  xlab("Year")+ylab("age")+ ggtitle("2008至2014平均壽命及健康平均餘命")

