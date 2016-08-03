library(ggplot2)
library(reshape2)
library(plyr)
library(gtools)
library(grid)
install.packages("gridExtra")
library(gridExtra)
#library(dplyr)
#Load  data
FittsData = read.table("C:/Users/dre/Documents/Matlab/StudyEvaluation/AllPartCombined.csv",sep=",",header=1,skip=0)
ClickTimes= read.table("C:/Users/dre/Documents/Matlab/StudyEvaluation/AllClickTimesCombined.csv",sep=",",header=1,skip=0)
SUS= read.csv("C:/Users/dre/Documents/UserStudy/System Usability Scale - Sheet1.csv",header=1)

tlx_path="C:/Users/dre/Documents/UserStudy"
temp = list.files(tlx_path,pattern="part")
TlxData=TlxData[0,]
for (i in 1:length(temp))
{
  curFile=read.csv(paste(tlx_path,"/",temp[i],sep = ""),header=1,sep=",",na.strings="-")
  TlxData=rbind(TlxData,curFile)
}


#Remove subjects with high error rates(due to tracker problems)
CleanData=subset(FittsData, Subject!=100 & Subject!=105)
ClickTimes=subset(ClickTimes, PartID!=100 & PartID!=105)
#Remove trial data
CleanData=subset(CleanData,Practice_==0)
#Remove outliers(95% quantil)
CleanData95=CleanData[CleanData$MTe<quantile(CleanData$MTe, .95),]
CleanDataReshaped=data.frame(matrix(ncol = 0, nrow = 79))
for(s in 1:length(unique(CleanData$Subject)))
{ 
  CleanDataReshaped=cbind(CleanDataReshaped,data.frame(
  MTe_SPOCK=c(unique(CleanData$Subject)[s],subset(CleanData,Subject==unique(CleanData$Subject)[s] & Button_Type=="SPOCK")$MTe),
  MTe_MSDT=c(unique(CleanData$Subject)[s],subset(CleanData,Subject==unique(CleanData$Subject)[s] & Button_Type=="MSDT")$MTe)))
}

#fix errors caused by projection 
erroCnt=length(CleanData[CleanData$Error_==1,]$Error_)
p=ggplot(subset(CleanData, Error_==1))  + geom_point(aes(x=TargetX-EndX, y=TargetY-EndY, colour="projection errors")) + coord_equal(); print(p)  
CleanData[CleanData$Error_==1 & ((CleanData$TargetY-CleanData$EndY)<(CleanData$W/2)*1.1) &(CleanData$TargetY-CleanData$EndY)>0,]$Error_=0
p + geom_point(data=subset(CleanData, Error_==1),aes(x=TargetX-EndX, y=TargetY-EndY, colour=Button_Type))
  

  
#Plot Clean Data
boxplot(CleanDataReshaped,col=as.double(CleanDataReshaped[1,]))
p=ggplot(CleanData, aes(colour=Button_Type, x=MTe)) + geom_density() + scale_x_log10(); print(p)
p=ggplot(CleanData, aes(colour=Button_Type, x=MTe)) + stat_ecdf() + scale_x_continuous(limits=c(1500, 6000)); print(p)
hist(CleanData$MTe)


#Check for normal distribution
normal_msdt <- shapiro.test(subset(CleanData, Button_Type=="MSDT")$MTe)
print(normal_msdt$p.value)

normal_spock <- shapiro.test(subset(CleanData, Button_Type=="SPOCK")$MTe)
print(normal_spock$p.value)

MTediff=subset(CleanData,Button_Type=="SPOCK")$MTe-subset(CleanData,Button_Type=="MSDT")$MTe
hist(MTediff)


#compare Meantime subject and condtitionwise
df_means=df_means[0,]
for(s in 1:length(unique(CleanData$Subject)))
{ 
  x=subset(CleanData, Subject==unique(CleanData$Subject)[s]);
  a=subset(x, Button_Type=="MSDT")$MTe;
  b=subset(x, Button_Type=="SPOCK")$MTe;
  d=subset(x, Button_Type=="SPOCK_Recalib")$MTe;
  a=a[a<quantile(a, .95)];
  b=b[b<quantile(b, .95)];
  d=d[d<quantile(d, .95)];
  df_means=rbind(df_means, data.frame(Subject=unique(CleanData$Subject)[s], mea_msdt=mean(a), mea_spock=mean(b), mea_spockre=mean(d)))
}
write.csv(df_means,"MeanTimesSubjectwise.csv")

shapiro.test(df_means$mea_msdt)
shapiro.test(df_means$mea_spock)        #<---------normal distribution 
shapiro.test(df_means$mea_spockre)      #<---------normal distribution 
#MT density
wilcox.test(df_means$mea_spock, df_means$mea_msdt, paired=TRUE)      

#T-Test over MTe(prob no normal distribution)
t.test(subset(CleanData, Button_Type=="MSDT")$MTe,subset(CleanData,Button_Type=="SPOCK")$MTe,paired=TRUE)
result <- shapiro.test(df_means$mea_msdt-df_means$mea_spock)
print(result$p.value)

t.test(df_means$mea_msdt,df_means$mea_spock,paired=TRUE)
t.test(df_means$mea_spock,df_means$mea_spockre,paired=TRUE)


#Median stuff
df_medians=data.frame(Subject=numeric(), med_msdt=numeric(), med_spock=numeric(), med_spockre=numeric())
for(s in 1:length(unique(CleanData$Subject)))
{
  x=subset(CleanData, Subject==unique(CleanData$Subject)[s]);
  df_medians=rbind(df_medians, data.frame(Subject=unique(CleanData$Subject)[s], med_msdt=median(subset(x, Button_Type=="MSDT")$MTe), med_spock=median(subset(x, Button_Type=="SPOCK")$MTe), med_spockre=median(subset(x, Button_Type=="SPOCK_Recalib")$MTe)))
}
shapiro.test(df_medians$med_msdt-df_medians$med_spock)
p=ggplot(df_medians, aes(x=med_msdt-med_spock)) + geom_density() + scale_x_log10(); print(p)
wilcox.test(df_medians$med_spock, df_medians$med_msdt, paired=TRUE)
wilcox.test(df_medians$med_spock, df_medians$med_spockre, paired=TRUE)
wilcox.test(df_medians$med_spockre, df_medians$med_msdt, paired=TRUE)


#Error Rate
sum(subset(CleanData, Error_ != 0)$Error_)/length(CleanData$Error_)




#Error rate subjectwise
df_errors=data.frame(Subject=numeric(), MSDT_Errors=numeric(), Spock_Errors=numeric(), SpockRe_Errors=numeric())
for(s in 1:length(unique(CleanData$Subject)))
{
  x=subset(CleanData, Subject==unique(CleanData$Subject)[s]);
  df_errors=rbind(df_errors, data.frame(Subject=unique(CleanData$Subject)[s], MSDT_Errors=sum(subset(x, Button_Type=="MSDT")$Error_), Spock_Errors=sum(subset(x, Button_Type=="SPOCK")$Error_), SpockRe_Errors=sum(subset(x, Button_Type=="SPOCK_Recalib")$Error_)))
}
shapiro.test(df_errors$MSDT_Errors)
shapiro.test(df_errors$Spock_Errors)
shapiro.test(df_errors$MSDT_Errors-df_errors$Spock_Errors)
wilcox.test(df_errors$MSDT_Errors,df_errors$Spock_Errors,paired=TRUE)
wilcox.test(df_errors$SpockRe_Errors,df_errors$Spock_Errors,paired=TRUE)
wilcox.test(df_errors$MSDT_Errors,df_errors$SpockRe_Errors,paired=TRUE)


#Hyp1: Endpoint deviation within target is identical for SPOCK and MSDT
result <- shapiro.test(subset(CleanData,Button_Type=="SPOCK")$dx_2d_)
print(result$p.value)

deviation_mea=data.frame(Subject=numeric(),Condition=numeric(), mea_msdt=numeric(), mea_spock=numeric(), mea_spock_re=numeric())
deviation_mea=deviation_mea[0,]
for(s in 1:length(unique(CleanData$Subject)))
{ 
  x=subset(CleanData, Subject==unique(CleanData$Subject)[s] & Error_==0);
  for(cond in 1:length(unique(CleanData$Condition)))
  {
    y=subset(x,Condition==unique(CleanData$Condition)[cond])
    deviation_mea=rbind(deviation_mea, data.frame(Subject=unique(CleanData$Subject)[s], condition=unique(CleanData$Condition)[cond],mea_msdt=mean(subset(y,Button_Type=="MSDT")$dx_2d_),mea_spock=mean(subset(y,Button_Type=="SPOCK")$dx_2d_),mea_spock_re=mean(subset(y,Button_Type=="SPOCK_Recalib")$dx_2d_)))
  }
}

deviation_mea_subject=aggregate(.~Subject, data=deviation_mea, mean)[,-2]
    
shapiro.test(deviation_mea$mea_spock-deviation_mea$mea_msdt)
wilcox.test(deviation_mea$mea_spock, deviation_mea$mea_msdt,paired=TRUE)

p=ggplot(subset(CleanData, Error_==0), aes(x=TargetX-EndX, y=TargetY-EndY, colour=Button_Type)) + geom_point() + coord_equal(); print(p)

#Throughput
df_TP=data.frame(Subject=numeric(),MSDT_TP=numeric(),SPOCK_TP=numeric(),SPOCK_RE_TP=numeric())
for(s in 1:length(unique(CleanData$Subject)))
{
  x=subset(CleanData, Subject==unique(CleanData$Subject)[s]);
  df_TP=rbind(df_TP, data.frame(Subject=unique(CleanData$Subject)[s], MSDT_TP=unique(subset(x, Button_Type=="MSDT")$Fitts_TP_avg_2d), SPOCK_TP=unique(subset(x, Button_Type=="SPOCK")$Fitts_TP_avg_2d), SPOCK_RE_TP=unique(subset(x, Button_Type=="SPOCK_Recalib")$Fitts_TP_avg_2d)))
}
MeanTP=colMeans(df_TP)
shapiro.test(df_TP$MSDT_TP-df_TP$SPOCK_TP)
shapiro.test(df_TP$MSDT_TP)
shapiro.test(df_TP$SPOCK_TP)
shapiro.test(df_TP$SPOCK_RE_TP)
wilcox.test(df_TP$MSDT_TP, df_TP$SPOCK_TP,paired=TRUE)
wilcox.test(df_TP$MSDT_TP, df_TP$SPOCK_RE_TP,paired=TRUE) #<--significant better SPOCK_RE
wilcox.test(df_TP$SPOCK_TP, df_TP$SPOCK_RE_TP,paired=TRUE)

t.test(df_TP$MSDT_TP, df_TP$SPOCK_TP,paired=TRUE)
t.test(df_TP$MSDT_TP, df_TP$SPOCK_RE_TP,paired=TRUE)#<--significant better 
t.test(df_TP$SPOCK_TP, df_TP$SPOCK_RE_TP,paired=TRUE)

ggplot(melt(df_TP,id.vars="Subject")) + geom_line(aes(x=Subject,y=value,colour=variable))+scale_colour_discrete("Pattern") 
#Manual Throughput calculation(projection error corrected)
for(s in (unique(CleanData$Subject)))
{
  for(but in unique(CleanData$Button_Type))
  {
    for(cond in unique(CleanData$Condition))
    {
      CleanData[CleanData$Condition==cond & CleanData$Subject==s & CleanData$Button_Type==but,"TP_2d_"]= CleanData[CleanData$Condition==cond & CleanData$Subject==s & CleanData$Button_Type==but,"IDe_2d_"]/mean(subset(CleanData,Condition==cond & Subject==s & Button_Type==but & Error_==0)$MTe/1000)
    
    }
    CleanData[CleanData$Subject==s & CleanData$Button_Type==but,"Fitts_TP_avg_2d"]=mean(CleanData[CleanData$Subject==s & CleanData$Button_Type==but,"TP_2d_"])
  }
}
#offset over time
df_mean_error_subjects=data.frame(MSDT_error=numeric(),SPOCK_error=numeric(),SPOCK_RE_error=numeric())
for(cond in unique(CleanData$Condition))
{
  for(samp in unique(CleanData$Trial))
  {
    x=subset(CleanData,Condition==cond & Trial==samp)
    df_mean_error_subjects=rbind(df_mean_error_subjects,data.frame(MSDT_error=median(subset(x,Button_Type=="MSDT")$dx_2d_),SPOCK_error=median(subset(x,Button_Type=="SPOCK")$dx_2d_),SPOCK_RE_error=median(subset(x,Button_Type=="SPOCK_Recalib")$dx_2d_)))
  }
}

p=ggplot(melt(df_mean_error_subjects,measure.vars=c("MSDT_error","SPOCK_error","SPOCK_RE_error"))) + geom_line(aes(x=rep(1:length(df_mean_error_subjects[,2]),3),y=value,colour=variable))+scale_colour_discrete("Pattern") ; print(p)
test <- 1:(length(df_mean_error_subjects$SPOCK_error))
spock_fit <- lm(SPOCK_error ~ test,data=df_mean_error_subjects)
abline(spock_fit)


#Mt by target distance
df_dist_MT=data.frame(A=numeric(),MT_MSDT=numeric(),MT_SPOCK=numeric(),MT_SPOCK_RE=numeric())
for(dist in unique(CleanData$A))
{
  s=subset(CleanData,A==dist)
  df_dist_MT=rbind(df_dist_MT,data.frame(A=dist,MT_MSDT=median(subset(s,Button_Type=="MSDT")$MTe),MT_SPOCK=median(subset(s,Button_Type=="SPOCK")$MTe),MT_SPOCK_RE=median(subset(s,Button_Type=="SPOCK_Recalib")$MTe)))
}

meltedDist=melt(df_dist_MT,id.vars="A")
distFit<-lm(data=meltedDist,value~A)
abline(distFit)
p=ggplot(meltedDist) + geom_jitter(aes(x=A,y=value,colour=variable))+scale_colour_discrete("Pattern") ; print(p)
#precision by target distance
df_dist_precision=data.frame(A=numeric(),dx_MSDT=numeric(),dx_SPOCK=numeric(),dx_SPOCK_RE=numeric())
for(dist in unique(CleanData$A))
{
  s=subset(CleanData,A==dist & Error_==0)
  df_dist_precision=rbind(df_dist_precision,data.frame(A=dist,dx_MSDT=median(subset(s,Button_Type=="MSDT")$dx_2d_),
  dx_SPOCK=median(subset(s,Button_Type=="SPOCK")$dx_2d_),dx_SPOCK_RE=median(subset(s,Button_Type=="SPOCK_Recalib")$dx_2d_)))
}
df_dist_precision=arrange(df_dist_precision,A )
mea_df_dist_precision=rbind(colMeans(df_dist_precision[1:2,]),colMeans(df_dist_precision[3:4,]),colMeans(df_dist_precision[5:6,]))


#Click Time comparison
df_meansClicktime=data.frame(PartID=numeric(),mea_msdt=numeric(),mea_spock=numeric(),mea_spockre=numeric())
df_meansClicktime=df_meansClicktime[0,]
for(s in 1:length(unique(ClickTimes$PartID)))
{ 
  x=subset(ClickTimes, PartID==unique(ClickTimes$PartID)[s]);
  a=subset(x, Button_Type=="MSDT")$ClickTime;
  b=subset(x, Button_Type=="SPOCK")$ClickTime;
  d=subset(x, Button_Type=="SPOCK_Recalib")$ClickTime;
  a=a[a<quantile(a, .95)];
  b=b[b<quantile(b, .95)];
  d=d[d<quantile(d, .95)];
  df_meansClicktime=rbind(df_meansClicktime, data.frame(PartID=unique(ClickTimes$PartID)[s], mea_msdt=mean(a), mea_spock=mean(b), mea_spockre=mean(d)))
}
wilcox.test(df_meansClicktime$mea_msdt, df_meansClicktime$mea_spock,paired=TRUE) #<-significant
wilcox.test(df_meansClicktime$mea_msdt, df_meansClicktime$mea_spockre,paired=TRUE) #<-significant
wilcox.test(df_meansClicktime$mea_spockre, df_meansClicktime$mea_spock,paired=TRUE)

p=ggplot(ClickTimes, aes(colour=Button_Type, x=ClickTime)) + geom_density() + scale_x_log10(); print(p)

#Check for learning effects
ggplot(melt(df_meansClicktime,id.vars="PartID")) + geom_line(aes(x=PartID,y=value,colour=variable))
ggplot(ClickTimes)+geom_line(aes(x=Timestamp,y=ClickTime,colour=Button_Type))

#Add Button-Type and Subject to SUS
buttons=c("SPOCK","SPOCK_Recalib","MSDT")
Subject=rep(100:111,each=3)
SUS=cbind(subjects,Button_Type=buttons[c(2,3,1,3,1,2,1,2,3,2,3,1,3,1,2,1,2,3,2,3,1,3,1,2,1,2,3,2,3,1,3,1,2,1,2,3)],SUS)

#compute sus score
score=(rowSums(SUS[,c(4,6,8,10,12)]-1)+rowSums(5-SUS[,c(5,7,9,11,13)]))*2.5
SUS=cbind(SUS,score)

p=ggplot(SUS, aes(colour=Button_Type, x=score)) + geom_density(); print(p)
ggplot(SUS,aes(colour=Button_Type, x=score))+geom_boxplot()
meaSUS=c(mea_SPOCK=mean(subset(SUS,Button_Type=="SPOCK")$score),mea_SPOCK_RE=mean(subset(SUS,Button_Type=="SPOCK_Recalib")$score),mea_MSDT=mean(subset(SUS,Button_Type=="MSDT")$score))
p=ggplot(SUS,aes(colour=Button_Type, x=score))  +stat_ecdf() +labs(x="SUS-Score",y="Percentile")   ; print(p)

#only use valid participants
p=ggplot(subset(SUS,subjects!=100 & subjects!=103 & subjects!=105), aes(colour=Button_Type, x=score)) + geom_density(); print(p)

#TLX evaluation
p1=ggplot(TlxData,aes(colour=expID,fill=expID, x=parID,y=tlx_Score))+geom_bar(stat = "identity",position=position_dodge())
p2=ggplot(TlxData,aes(colour=expID,fill=expID, x=parID,y=scale_mental))+geom_bar(stat = "identity",position=position_dodge())
multiplot(p1, p2,  cols=2)
grid.arrange(p1, p2, ncol = 2, main = "Main title")
