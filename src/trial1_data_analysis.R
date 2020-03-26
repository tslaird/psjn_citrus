library(stringr)
setwd('/Users/tslaird/Box/psjn_citrus/Trial1')
#import time0 data

time0_counts<-read.csv('raw_machine_output/psjn_trial1_time5_counted012520.csv', sep=';')
time0_masses<-read.csv('time5_masses.csv')
time0_masses$complete_id<-paste0(time0_masses$plant_id,'-',time0_masses$time_id,'_',time0_masses$leaf_or_petiole)
time0_counts$complete_id<- str_extract(time0_counts$PlateId, '.+(?=-)')


time0_merged<-merge(time0_counts,time0_masses, by= 'complete_id',all=T )

# add plate ids missing because they had no colonies and were not counted
for(r in 1:nrow(time0_merged)){
  while(sum(time0_merged[,1] == time0_merged[r,1]) <3){
    time0_merged<-rbind.data.frame(time0_merged,time0_merged[r,])
  }
}

#make NA counts equal to 0.5 and the volumes of those counts equal to 50µL
time0_merged$Counted[is.na(time0_merged$Counted)]<-0.5
time0_merged$CountedVolume[is.na(time0_merged$CountedVolume)]<-0.050
time0_merged$DilutionFactor[is.na(time0_merged$DilutionFactor)]<-10
time0_merged$Result[is.na(time0_merged$Result)]<- (time0_merged$Counted[is.na(time0_merged$Result)]/time0_merged$CountedVolume[is.na(time0_merged$Result)]) * 
  time0_merged$DilutionFactor[is.na(time0_merged$Result)]

time0_merged$bacteria_per_leaf<- apply(time0_merged,1, function(x) 
  if(x['leaf_or_petiole']=='L'){
    as.numeric(x['Result'])*10
    }
  else{
    as.numeric(x['Result'])*2
  }
  )
time0_merged$bacteria_per_gram_leaf<-time0_merged$bacteria_per_leaf/time0_merged$mass
time0_merged$log_cfu_per_gram<-log10(time0_merged$bacteria_per_leaf/time0_merged$mass)




#function for processing data

process_data<- function(countfile, mass_file){
data_counts<-read.csv(countfile, sep=';')
data_masses<-read.csv(mass_file)
data_masses$complete_id<-paste0(data_masses$plant_id,'-',data_masses$time_id,'_',data_masses$leaf_or_petiole)
data_counts$complete_id<- str_extract(data_counts$PlateId, '.+(?=-)')
data_merged<-merge(data_counts,data_masses, by= 'complete_id',all=T )
# add plate ids missing because they had no colonies and were not counted
for(r in 1:nrow(data_merged)){
  while(sum(data_merged[,1] == data_merged[r,1]) <3){
    data_merged<-rbind.data.frame(data_merged,data_merged[r,])
  }
}
#make NA counts equal to 0.5 and the volumes of those counts equal to 50µL
data_merged$Counted[(is.na(data_merged$Counted)) | (data_merged$Counted==0)]<-0.5
data_merged$CountedVolume[is.na(data_merged$CountedVolume)]<-0.050
data_merged$DilutionFactor[is.na(data_merged$DilutionFactor)]<-10
data_merged$Result[(is.na(data_merged$Result) | (data_merged$Result ==0))]<- (data_merged$Counted[(is.na(data_merged$Result) | (data_merged$Result ==0))]/data_merged$CountedVolume[(is.na(data_merged$Result) | (data_merged$Result ==0))]) * 
  data_merged$DilutionFactor[(is.na(data_merged$Result) | (data_merged$Result ==0))]
data_merged$bacteria_per_leaf<- apply(data_merged,1, function(x) 
  if(x['leaf_or_petiole']=='L'){
    as.numeric(x['Result'])*10
  }
  else{
    as.numeric(x['Result'])*2
  }
)
data_merged$bacteria_per_gram_leaf<-data_merged$bacteria_per_leaf/data_merged$mass
data_merged$log_cfu_per_gram<-log10(data_merged$bacteria_per_leaf/data_merged$mass)
data_merged<-data_merged[order(data_merged$complete_id),]
rownames(data_merged)<-NULL
return(data_merged)
}

time0_df<-process_data('raw_machine_output/psjn_trial1_time0_counted012120.csv','time0_masses.csv')
time5_df<-process_data('raw_machine_output/psjn_trial1_time5_counted012520.csv','time5_masses.csv')
time10_df<-process_data('raw_machine_output/psjn_trial1_time10.csv','time10_masses.csv')

write.csv(time0_df, file='processed_data/time0.csv')
write.csv(time5_df, file='processed_data/time5.csv')
write.csv(time10_df, file='processed_data/time10.csv')



## get processed data ----

process_data2<- function(countfile, mass_file){
  data_counts<-read.csv(countfile, sep=',')
  data_masses<-read.csv(mass_file)
  data_masses$complete_id<-paste0(data_masses$plant_id,'-',data_masses$time_id,'_',data_masses$leaf_or_petiole)
  data_counts$complete_id<- str_extract(data_counts$PlateId, '.+(?=-)')
  data_merged<-merge(data_counts,data_masses, by= 'complete_id',all=T )
  data_merged$Counted[(is.na(data_merged$Counted)) | (data_merged$Counted==0)]<-0.5
  data_merged$CountedVolume[is.na(data_merged$CountedVolume)]<-0.050
  data_merged$Result[data_merged$Result==0]<-(0.5/0.05)*10
  data_merged$bacteria_per_leaf<- apply(data_merged,1, function(x) 
    if(x['leaf_or_petiole']=='L'){
      as.numeric(x['Result'])*10
    }
    else{
      as.numeric(x['Result'])*2
    }
  )
  data_merged$bacteria_per_gram_leaf<-data_merged$bacteria_per_leaf/data_merged$mass
  data_merged$log_cfu_per_gram<-log10(data_merged$bacteria_per_leaf/data_merged$mass)
  data_merged$log_cfu_per_leaf<-log10(data_merged$bacteria_per_leaf)
  data_merged<-data_merged[order(data_merged$complete_id),]
  rownames(data_merged)<-NULL
  return(data_merged)
  
}


time0_df<-process_data2('processed_data/psjn_trial1_time0.csv','time0_masses.csv')
time5_df<-process_data2('processed_data/psjn_trial1_time5.csv','time5_masses.csv')
time10_df<-process_data2('processed_data/psjn_trial1_time10.csv','time10_masses.csv')
time40_df<-process_data2('processed_data/psjn_trial1_time40.csv','time40_masses.csv')


write.csv(time0_df, file='processed_data/time0.csv')
write.csv(time5_df, file='processed_data/time5.csv')
write.csv(time10_df, file='processed_data/time10.csv')
write.csv(time40_df, file='processed_data/time40.csv')

aggregated<-cbind.data.frame( aggregate(time0_df$log_cfu_per_gram, by=list(time0_df$complete_id), FUN=mean),
                  aggregate(time5_df$log_cfu_per_gram, by=list(time5_df$complete_id), FUN=mean),
                  aggregate(time10_df$log_cfu_per_gram, by=list(time10_df$complete_id), FUN=mean),
                  aggregate(time40_df$log_cfu_per_gram, by=list(time40_df$complete_id), FUN=mean))
colnames(aggregated)<-c('t_4_labels','t_4_log_cfu_per_gram','t_9_labels','t_9_log_cfu_per_gram','t_14_labels','t_14_log_cfu_per_gram')
write.csv(aggregated,'processed_data/aggregated_log_cfus_per_gram.csv',row.names = F)



time0_leaves<-time0_df[(time0_df$leaf_or_petiole=='L') & !(time0_df$plant_id %in% c('X','Z','H')),]
time5_leaves<-time5_df[(time5_df$leaf_or_petiole=='L') & !(time5_df$plant_id %in% c('X','Z','H')),]
time10_leaves<-time10_df[(time10_df$leaf_or_petiole=='L') & !(time10_df$plant_id %in% c('X','Z','H')),]
time40_leaves<-time40_df[(time40_df$leaf_or_petiole=='L') & !(time40_df$plant_id %in% c('X','Z','H')),]


#we assume that all plants of X,Y,Z and H(somehow not innoculated) have no bacteria whatsoever
# the petioles and leaves with zero values may have bacteria below the detection limit 
#the limit of detection for leaves is 2000 bacteria per leaf
# 1 bacteria/50µL * 10 = 10 bacteria/50µL = 2000 bacteria/10mL(the buffer added to the leaves)
#the limit of detection for petioles is 400 bacteria per petiole
# 1 bacteria/50µL * 10 = 10 bacteria/50µL =  400 bacteria/2mL(the buffer added to the leaves)


library(dplyr)
#including 0 values changed to 0.5
all_leaves<-dplyr::bind_rows(list('4'=time0_leaves,'9'= time5_leaves, '14'=time10_leaves, '29'=time40_leaves), .id = 'source')
all_leaves$day <- factor(all_leaves$source,levels = c("4", "9", "14","29"))
all_leaves_summary<- all_leaves %>% 
  group_by(complete_id,day) %>%
  summarise_all("mean")
library(ggplot2)
pdf("Trial_1_boxplot_cfu_per_gram_leaf.pdf",width = 5,height = 5) 
p <- ggplot(all_leaves_summary, aes(day, log_cfu_per_gram))
p + geom_boxplot(fill = "white", colour = "black") +
  geom_jitter(width = 0.1, color='grey2')+ ylim(c(1,7))
  labs(y="log CFU/gram of leaf", x = "Days Post Innoculation",title = "Trial 1")+
  theme_bw()
dev.off()
pdf("Trial_1_boxplot_cfu_per_leaf.pdf",width = 5,height = 5) 
p <- ggplot(all_leaves_summary, aes(day, log_cfu_per_leaf))
p + geom_boxplot(fill = "white", colour = "black") +
  geom_jitter(width = 0.1, color='grey2')+
  labs(y="log CFU/leaf", x = "Days Post Innoculation",title = "Trial 1")+
  theme_bw()
dev.off()
p <- ggplot(all_leaves_summary, aes(day,log_cfu_per_gram))
p + geom_bar(stat="identity")+
  theme_bw()

cor(all_leaves_summary$log_cfu_per_leaf,all_leaves_summary$time_id)
linearMod <- lm(log_cfu_per_leaf ~ age_status, data=all_leaves)
summary(linearMod)

#censoring 0 values
all_leaves<-dplyr::bind_rows(list('4'=time0_leaves,'9'= time5_leaves, '14'=time10_leaves, '29'=time40_leaves), .id = 'source')
all_leaves<-all_leaves[all_leaves$Counted!=0.5,]
all_leaves$day <- factor(all_leaves$source,levels = c("4", "9", "14","29"))
all_leaves_summary<- all_leaves %>% 
  group_by(complete_id,day) %>%
  summarise_all("mean")
library(ggplot2)
p <- ggplot(all_leaves_summary, aes(day, log_cfu_per_gram))
p + geom_boxplot(fill = "white", colour = "black" , aes(middle=mean(log_cfu_per_gram))) +
  geom_jitter(width = 0.1, color='grey2')+
  theme_bw()

#line plot of the data cfu/gram leaf
all_leaves_summary2<- all_leaves_summary %>% 
  group_by(day) %>%
  summarize(mean_log_cfu_per_gram=mean(log_cfu_per_gram), sdev_log_cfu_per_gram= sd(log_cfu_per_gram))
pdf("Trial_1_lineplot_cfu_per_gram_leaf.pdf",width = 5,height = 5) 
p <- ggplot(all_leaves_summary2, aes(x= as.numeric(as.character(day)), y= mean_log_cfu_per_gram)) + 
  geom_line() + geom_point()+ scale_x_continuous(breaks = c(0,4,9,14,29))+ theme_bw() +  scale_y_continuous(limits = c(1,6), breaks=c(1,2,3,4,5,6))+
  geom_errorbar(aes(ymin = mean_log_cfu_per_gram-sdev_log_cfu_per_gram, ymax= mean_log_cfu_per_gram + sdev_log_cfu_per_gram), 
                width=.2, lty="longdash")+
  labs(y="average log CFU/gram of leaf", x = "Days Post Innoculation",title = "Trial 1")
p
dev.off()

#line plot of the data cfu/leaf
all_leaves_summary2<- all_leaves_summary %>% 
  group_by(day) %>%
  summarize(mean_log_cfu_per_leaf=mean(log_cfu_per_leaf), sdev_log_cfu_per_leaf= sd(log_cfu_per_leaf))
pdf("Trial_1_lineplot_cfu_per_leaf.pdf",width = 5,height = 5) 
p <- ggplot(all_leaves_summary2, aes(x= as.numeric(as.character(day)), y= mean_log_cfu_per_leaf)) + 
  geom_line() + geom_point()+ scale_x_continuous(breaks = c(0,4,9,14,29))+ theme_bw() + scale_y_continuous(limits = c(1,6), breaks=c(1,2,3,4,5,6))+
  geom_errorbar(aes(ymin = mean_log_cfu_per_leaf-sdev_log_cfu_per_leaf, ymax= mean_log_cfu_per_leaf + sdev_log_cfu_per_leaf), 
                width=.2, lty="longdash")+
  labs(y="average log CFU/leaf", x = "Days Post Innoculation",title = "Trial 1")
p
dev.off()

#does person innoculating leaves show a difference
all_leaves<-dplyr::bind_rows(list('4'=time0_leaves,'9'= time5_leaves, '14'=time10_leaves, '29'=time40_leaves), .id = 'source')
#all_leaves<-all_leaves[all_leaves$Counted!=0.5,]
all_leaves$day <- as.numeric(as.character(factor(all_leaves$source,levels = c("4", "9", "14","29"))))
all_leaves$person<-ifelse(grepl('A|E|C|G', all_leaves$complete_id),'Tyler','Abby')
all_leaves_summary<- all_leaves %>% 
  group_by(complete_id,person,day,age_status, person) %>%
  summarise_all("mean")


#leaf size and bacteria
library(ggpubr)
plot(all_leaves$mass, all_leaves$log_cfu_per_leaf)
ggqqplot(all_leaves$Result)
shapiro.test(all_leaves$)
ggdensity(all_leaves$)
model<-lm(log_cfu_per_leaf ~ age_status, data = all_leaves_summary)
summary(model)
model1<-lm(Result ~ day + age_status +person ,data = all_leaves_summary)
all_leaves_summary$Result
summary(model1)
model2<-lm(Counted ~ day+person+age_status+mass, data = all_leaves_summary)
summary(model2)
plot(all_leaves_summary$Result,all_leaves_summary$age_status)
plot(model1)
model2$coefficients[2]
(model1$coefficients[2] - model2$coefficients[2])/model1$coefficients[2]*100
t.test(all_leaves_summary$log_cfu_per_leaf[all_leaves_summary$day==4],all_leaves_summary$log_cfu_per_leaf[all_leaves_summary$day==14])

#Trial 2----
setwd('/Users/tslaird/Box/psjn_citrus/Trial2')
trial2_time0_df<-process_data2('processed_data/psjn_trial2_time0.csv','time0_masses.csv')
trial2_time10_df<-process_data2('processed_data/psjn_trial2_time10.csv','time10_masses.csv')
trial2_time20_df<-process_data2('processed_data/psjn_trial2_time20.csv','time20_masses.csv')

trial2_time0_leaves<-trial2_time0_df[(trial2_time0_df$leaf_or_petiole=='L') & !(trial2_time0_df$plant_id %in% c('X','Z','H')),]
trial2_time10_leaves<-trial2_time10_df[(trial2_time10_df$leaf_or_petiole=='L') & !(trial2_time10_df$plant_id %in% c('X','Z','H')),]
trial2_time20_leaves<-trial2_time20_df[(trial2_time20_df$leaf_or_petiole=='L') & !(trial2_time20_df$plant_id %in% c('X','Z','H')),]

all_leaves<-dplyr::bind_rows(list('0'=trial2_time0_leaves,'10'=trial2_time10_leaves, '20'=trial2_time20_leaves), .id = 'source')
all_leaves$day <- factor(all_leaves$source,levels = c("0", "10", "20"))
all_leaves_summary<- all_leaves %>% 
  group_by(complete_id,day) %>%
  summarise_all("mean")

#cfu/gram leaf
all_leaves_summary2<- all_leaves_summary %>% 
  group_by(day) %>%
  summarize(mean_log_cfu_per_gram=mean(log_cfu_per_gram), sdev_log_cfu_per_gram= sd(log_cfu_per_gram))
pdf("Trial_2_lineplot_cfu_per_gram_leaf.pdf",width = 5,height = 5) 
p <- ggplot(all_leaves_summary2, aes(x= as.numeric(as.character(day)), y= mean_log_cfu_per_gram)) + 
  geom_line() + geom_point()+ scale_x_continuous(breaks = c(0,10,20))+ theme_bw() +  scale_y_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  geom_errorbar(aes(ymin = mean_log_cfu_per_gram-sdev_log_cfu_per_gram, ymax= mean_log_cfu_per_gram + sdev_log_cfu_per_gram), 
                width=.2, lty="longdash")+
  labs(y="average log CFU/gram of leaf", x = "Days Post Innoculation",title = "Trial 2")
p
dev.off()

#cfu / leaf
all_leaves_summary2<- all_leaves_summary %>% 
  group_by(day) %>%
  summarize(mean_log_cfu_per_leaf=mean(log_cfu_per_leaf), sdev_log_cfu_per_leaf= sd(log_cfu_per_leaf))
pdf("Trial_2_lineplot_cfu_per_leaf.pdf",width = 5,height = 5) 
p <- ggplot(all_leaves_summary2, aes(x= as.numeric(as.character(day)), y= mean_log_cfu_per_leaf)) + 
  geom_line() + geom_point()+ scale_x_continuous(breaks = c(0,10,20))+ theme_bw() +  scale_y_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  geom_errorbar(aes(ymin = mean_log_cfu_per_leaf-sdev_log_cfu_per_leaf, ymax= mean_log_cfu_per_leaf + sdev_log_cfu_per_leaf), 
                width=.2, lty="longdash")+
  labs(y="average log CFU/leaf", x = "Days Post Innoculation",title = "Trial 2")
p
dev.off()







library(NADA)

censummary(ShePyrene)
data(ShePyrene)
attach(ShePyrene)
names(ShePyrene) 
censummary(Pyrene, PyreneCen)
ShePyrene$Pyrene<-0
ShePyrene$PyreneCen<-T
pykm = cenfit(Pyrene, PyreneCen) 
plot(pykm) 
pykm@survfit
pyros = cenros(Pyrene, PyreneCen) 
plot(pyros) 
pymle <-cenmle(Pyrene, PyreneCen)
pymle
plot(pymle)
censtats(Pyrene, PyreneCen)
