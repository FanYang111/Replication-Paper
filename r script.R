#This script contains all the code used in the paper.

library(tidyr)
library(ggplot2)
library(stringr)
library(PMCMRplus)
library(kableExtra)
data=read.csv("data.csv")


#generate figure 1
```{r,include=FALSE}
datac=c()
name=c()
year=c(1971, 1972, 1973 ,1974, 1975 ,1976, 1977 ,1978, 1979 ,1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987 ,1988 ,1989 ,1990 ,1991, 1992, 1993 ,1994 ,1995, 1996 ,1997 ,1998 ,1999 ,2000, 2001 ,2002, 2003, 2004 ,2005, 2006, 2007 ,2008, 2009, 2010, 2011 ,2012 ,2013 ,2014 ,2015 ,2016, 2017, 2018 ,2019, 2020)
region=c("South Asia","Sub-Saharan Africa","Europe & Central Asia","Middle East & North Africa","Latin America & Caribbean","High income: OECD","East Asia & Pacific")
for(i in 1:7){
  for(j in 1:50){
    dat=data[which(data$Region==region[i]),]
    dat=dat[which(dat$WBL.Report.Year==year[j]),]
    datt=round(mean(dat$WBL.INDEX),2)
    name1=paste(region[i],",",year[j])
    datac=c(datac,datt)
    name=c(name,name1)
  }}
datac=as.data.frame(cbind(name,datac))
```

```{r,include=FALSE}
datac=separate(datac,col=name,into=c("Region","Year"),sep=",")
```

```{r,include=FALSE}
datt=data[which(data$WBL.Report.Year==2020),]
mean(datt$WBL.INDEX)
```

```{r Fig1,fig.width=6,fig.height=4, fig.cap="Average WBL Index of Different Region Over Time",echo=FALSE}
datac$Year=as.numeric(datac$Year)
datac$datac=as.numeric(datac$datac)
ggplot(data=datac,mapping=aes(x=Year,y=datac,group=Region,color=Region))+geom_line()+ylim(c(0,100))+xlab("Year")+ylab("WBL Index")+theme_minimal()
```

#generate figure2 and table1

```{r,include=FALSE}
data1=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$MOBILITY)
  name1=paste(year[i])
  data1=c(data1,datt)
  name=c(name,name1)
}
data1=cbind(name,data1)
Indicator=rep("Mobility",50)
data1=cbind(Indicator,data1)

data2=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$WORKPLACE)
  name1=paste(year[i])
  data2=c(data2,datt)
  name=c(name,name1)
}
data2=cbind(name,data2)
Indicator=rep("Workplace",50)
data2=cbind(Indicator,data2)

data3=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$PAY)
  name1=paste(year[i])
  data3=c(data3,datt)
  name=c(name,name1)
}
data3=cbind(name,data3)
Indicator=rep("PAY",50)
data3=cbind(Indicator,data3)

data4=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$MARRIAGE)
  name1=paste(year[i])
  data4=c(data4,datt)
  name=c(name,name1)
}
data4=cbind(name,data4)
Indicator=rep("Marriage",50)
data4=cbind(Indicator,data4)

data5=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$PARENTHOOD)
  name1=paste(year[i])
  data5=c(data5,datt)
  name=c(name,name1)
}
data5=cbind(name,data5)
Indicator=rep("Parenthood",50)
data5=cbind(Indicator,data5)

data6=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$ENTREPRENEURSHIP)
  name1=paste(year[i])
  data6=c(data6,datt)
  name=c(name,name1)
}
data6=cbind(name,data6)
Indicator=rep("Entrepreneurship",50)
data6=cbind(Indicator,data6)

data7=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$ASSETS)
  name1=paste(year[i])
  data7=c(data7,datt)
  name=c(name,name1)
}
data7=cbind(name,data7)
Indicator=rep("Asserts",50)
data7=cbind(Indicator,data7)

data8=c()
name=c()
for(i in 1:50){
  dat=data[which(data$WBL.Report.Year==year[i]),]
  datt=mean(dat$PENSION)
  name1=paste(year[i])
  data8=c(data8,datt)
  name=c(name,name1)
}
data8=cbind(name,data8)
Indicator=rep("Pension",50)
data8=cbind(Indicator,data8)

datt1=as.data.frame(rbind(data1,data2,data3,data4,data5,data6,data7,data8))
```


```{r Fig2, fig.width=6, fig.height=4,fig.cap= "Charting the Progress of the Indicators over Time",echo=FALSE}
datt1$name=as.numeric(datt1$name)
datt1$data1=as.numeric(datt1$data1)
ggplot(data=datt1,mapping=aes(x=name,y=data1,group=Indicator,color=Indicator))+geom_line()+ylim(c(0,100))+xlab("Year")+ylab("Score")+theme_minimal()
```



```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data1=c()
data1sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$MOBILITY),2)
  d2=round(sd(datt$MOBILITY),2)
  data1=c(data1,d1)
  data1sd=c(data1sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data1=cbind(name,data1)
data1sd=cbind(name,data1sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data2=c()
data2sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$WORKPLACE),2)
  d2=round(sd(datt$WORKPLACE),2)
  data2=c(data2,d1)
  data2sd=c(data2sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data2=cbind(name,data2)
data2sd=cbind(name,data2sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data3=c()
data3sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$PAY),2)
  d2=round(sd(datt$PAY),2)
  data3=c(data3,d1)
  data3sd=c(data3sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data3=cbind(name,data3)
data3sd=cbind(name,data3sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data4=c()
data4sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$MARRIAGE),2)
  d2=round(sd(datt$MARRIAGE),2)
  data4=c(data4,d1)
  data4sd=c(data4sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data4=cbind(name,data4)
data4sd=cbind(name,data4sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data5=c()
data5sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$PARENTHOOD),2)
  d2=round(sd(datt$PARENTHOOD),2)
  data5=c(data5,d1)
  data5sd=c(data5sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data5=cbind(name,data5)
data5sd=cbind(name,data5sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data6=c()
data6sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$ENTREPRENEURSHIP),2)
  d2=round(sd(datt$ENTREPRENEURSHIP),2)
  data6=c(data6,d1)
  data6sd=c(data6sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data6=cbind(name,data6)
data6sd=cbind(name,data6sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data7=c()
data7sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$ASSETS),2)
  d2=round(sd(datt$ASSETS),2)
  data7=c(data7,d1)
  data7sd=c(data7sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data7=cbind(name,data7)
data7sd=cbind(name,data7sd)
```

```{r,include=FALSE}
dat=data[which(data$WBL.Report.Year==2020),]
data8=c()
data8sd=c()
name=c()
for(i in 1:7){
  datt=dat[which(dat$Region==region[i]),]
  d1=round(mean(datt$PENSION),2)
  d2=round(sd(datt$PENSION),2)
  data8=c(data8,d1)
  data8sd=c(data8sd,d2)
  name1=paste(region[i])
  name=c(name,name1)
}
data8=cbind(name,data8)
data8sd=cbind(name,data8sd)
```

```{r,include=FALSE}
data1=cbind(data1,data1sd)
data2=cbind(data2,data2sd)
data3=cbind(data3,data3sd)
data4=cbind(data4,data4sd)
data5=cbind(data5,data5sd)
data6=cbind(data6,data6sd)
data7=cbind(data7,data7sd)
data8=cbind(data8,data8sd)
```

```{r,include=FALSE}
d1=as.data.frame(cbind(data1,data2,data3,data4,data5,data6,data7,data8))
d1=d1[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)]
d1$Mobility=str_c(d1$data1,",",d1$data1sd)
d1$Workplace=str_c(d1$data2,",",d1$data2sd)
d1$Pay=str_c(d1$data3,",",d1$data3sd)
d1$Marriage=str_c(d1$data4,",",d1$data4sd)
d1$Parenthood=str_c(d1$data5,",",d1$data5sd)
d1$Entrepreneurship=str_c(d1$data6,",",d1$data6sd)
d1$Assets=str_c(d1$data7,",",d1$data7sd)
d1$Pension=str_c(d1$data8,",",d1$data8sd)
d1=d1[,c(1,18:25)]
names(d1)[1]="Region"

da=read.csv("d1.csv")
da=da[,(2:10)]
```




#Figure3 code
```{r Fig3,fig.width=6,fig.height=4,fig.cap="Income Group and WBL Index",echo=FALSE,warning=FALSE}
ggplot(data=data,mapping=aes(x=data$Income.group,y=data$WBL.INDEX,group=data$Income.group,color=data$Income.group))+geom_boxplot()+ylim(c(0,100))+xlab("Group")+ylab("WBL Index")+theme_minimal()+
  scale_colour_discrete("Income Group")+theme(axis.text = element_text(size = 7))  
```

#One-way ANOVA and simple linear model
``{r,echo=FALSE}
fit=aov(data$WBL.INDEX~data$Income.group,data=data)
res=TukeyHSD(fit, conf.level = 0.95)
d1=as.data.frame(res$`data$Income.group`)
names(d1)=c("Mean Difference","Lower Quantile","Upper Quantile","P-Value Adjusted")
kable(d1, booktabs = TRUE,caption = "One Way ANOVA of Income Group and WBL Index") %>%
  kable_styling(font_size = 8)
```

```{r,include=FALSE}
m1=lm(data$WBL.INDEX~data$Income.group,data=data)
m1$coefficients
```

#figure4 and 5 in the appendix

```{r Fig4,fig.cap="Pay and WBL Index of Different Income Groups",message=FALSE,echo=FALSE,warning=FALSE}
ggplot(data=data,mapping=aes(x=data$PAY,y=data$WBL.INDEX,color=data$Income.group))+geom_point()+xlab("PAY")+ylab("WBL Index")+theme_minimal()+
  scale_colour_discrete("Income Group")+
  geom_smooth(method = "lm", se = FALSE)
```

```{r Fig5,fig.cap="Workplace and WBL Index of Different Income Groups",echo=FALSE,message=FALSE,warning=FALSE}
ggplot(data=data,mapping=aes(x=data$WORKPLACE,y=data$WBL.INDEX,color=data$Income.group))+geom_point()+xlab("Workplace")+ylab("WBL Index")+theme_minimal()+
  scale_colour_discrete("Income Group")+
  geom_smooth(method = "lm", se = FALSE)
```





