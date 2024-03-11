---
  title: '團體報告'
author: "資二B 09156219 丘宛玉"
output:
  html_document:
  highlight: tango
pdf_document: default
word_document: default
---
  
  
  ```{r dra,class.source = c("numCode", "r", "numberLines")}
labels=c("國中","高中職", "大學以上","研究所以上")

percentage=c(3,3.2778,3.1792,3.4444)+1

bp=barplot(percentage,
           names.arg =labels,
           ylab = "人",
           main="整體滿意度平均數",
           xlab="族群",
           col="light blue",
           
)
text(bp,percentage-0.1,
     labels=format(percentage),
     col="black"
)

```
BaybyGowth <- data.frame( 
  month = c(1, 2, 4, 6, 8, 10, 12), 
  height = c(57, 61, 67, 71, 74, 76, 79)) 
print(BaybyGowth) 

#長條圖(1) 
ggplot(BaybyGowth, aes(x = factor(month), y = height)) + geom_bar(stat = "identity")

```
library(ggplot2)

data <- data.frame(
  name=c("Q","W","E","G","E","H","J","K","L","M") ,  
  value=c(2,14,3,17,50,68,71,64,32,99)
)
ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", width=0.5,fill='light blue')