gtd_12to15=read.csv('c:/Users/Administrator/Documents/EDA/gtd_12to15_52134.csv')
gtd_70to91=read.csv('c:/Users/Administrator/Documents/EDA/gtd_70to91_49566.csv')
gtd_92to11=read.csv('c:/Users/Administrator/Documents/EDA/gtd_92to11_no 93_55072.csv')
gtd1993=read.csv('c:/Users/Administrator/Documents/EDA/gtd1993_748.csv')
a=merge(gtd_12to15,gtd_70to91,all=TRUE)
b=merge(gtd_92to11,gtd1993,all=TRUE)
c=merge(a,b,all=TRUE)

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(tidyr)
library(graphics)
##########################################################################################
####1. Number of Attacks per year ?

Q1=c%>%group_by(iyear)%>%summarise(`No Of Attacks`=sum(iyear=n()))
View(Q1)

aa=ggplot(Q1,aes(x=iyear,y=`No Of Attacks`))+
  geom_area(fill='steelblue2',col='black',size=1)+
 geom_point(col="red",size=1)+
    scale_x_continuous(breaks = seq(1970,2015,5))+
  scale_y_continuous(breaks = seq(500,20000,3000))+
  labs(subtitle='From 1970 to 2015',y='No Of Attacks', x='Years', title='NO OF ATTACKS PER YEAR',caption ='www.GTD.com')+
  theme_bw() 

ggplotly(aa)
##########################################################################################
####2. Number of bombing per year ?

Q2=c%>%filter(attacktype1_txt=='Bombing/Explosion')%>%group_by(iyear)%>%summarise(`no of bombing`=sum(attacktype1_txt=n()))
View(Q2)


bb=ggplot(Q2,aes(x=iyear,y=`no of bombing`))+
  geom_area(fill='seagreen4',col='black',size=1)+
  #geom_line(aes(y=`no of bombing`),size=(1), col = "Blue")+
  geom_point(col="Red",size=1,y=Q2$`no of bombing`)+
  labs(subtitle='Red=No Of Attacks, Blue=No Of Bombing' ,y='No Of Attacks and Bombings', x='Years', title='NO OF ATTACKS AND BOMBING PER YEAR',caption ='www.GTD.com')+
  theme_bw()+
  scale_x_continuous(breaks = seq(1970,2015,5))+
  scale_y_continuous(breaks = seq(100,20000,1000))
ggplotly(bb)

bbb=ggplot(Q2,aes(x=iyear))+
  geom_line(aes(y=Q2$`no of bombing`),size=(1), col = "Blue")+
  geom_point(col="black",size=2,y=Q2$`no of bombing`)+geom_point(col="black",size=2,y=Q1$`No Of Attacks`)+
  geom_line(aes(y=Q1$`No Of Attacks`),size=(1), col = "red")+
  labs(subtitle='Red=No Of Attacks, Blue=No Of Bombing' ,y='No Of Attacks and Bombings', x='Years', title='NO OF ATTACKS AND BOMBING PER YEAR',caption ='www.GTD.com')+
  theme_bw()+
  scale_x_continuous(breaks = seq(1970,2015,5))+
  scale_y_continuous(breaks = seq(100,20000,1000))

ggplotly(bbb)
#  theme(legend.position ='right', legend.key.width = unit(.2, "cm"))+


##########################################################################################
####3. Terrorist attacks region wise per year ?

Q3=c%>%filter(doubtterr==0)%>% group_by(iyear,region_txt)%>%summarise(`No Of Attack`=n())
View(Q3)


zz=ggplot(Q3,aes(x=iyear,y=`No Of Attack`))+
  scale_y_continuous(breaks=seq(0,6000,1000))+
  theme(legend.title = element_blank())+
  labs(subtitle='From 1970 to 2015',y='No Of Attacks', x='Year', title='Terrorist attacks region wise per year',caption ='www.GTD.com')+
  geom_point(aes(fill = region_txt,frame=iyear,size=(`No Of Attack`)*10))
ggplotly(zz)
  
##########################################################################################
####4. Top 5 type of terror attacks per region ?

Q4=c%>%filter(doubtterr==0)%>%group_by(region_txt,attacktype1_txt)%>%summarise(no_of_attacks=sum(attacktype1_txt=n()))%>%arrange(region_txt,-no_of_attacks)%>%top_n(5)
View(Q4)


dd=ggplot(Q4,aes(x=attacktype1_txt,y=no_of_attacks))+
  geom_bar(stat = "Identity",width = .5, aes(fill =attacktype1_txt))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom", legend.key.width = unit(.2, "cm"))+
  labs(subtitle='From 1970 to 2015',y='No Of Attacks', x='Region', title='Top 5 Terrorist attacks region wise per year',caption ='www.GTD.com')+
  theme_bw() + facet_wrap(~region_txt, scales="free",ncol=3)+
  theme(legend.title = element_blank())+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

ggplotly(dd)
 #   geom_text(aes(label = no_of_attacks),position = position_stack(vjust = 0.5))
##########################################################################################
####5. Heaviest hit Target types (Based on both Killed and wounded)?

Q5=c%>% group_by(targtype1_txt)%>%
  summarise(`Heaviest hit Target types`=sum(round(sum(nkill, na.rm = TRUE)),round(sum(nwound, na.rm=TRUE))))%>%
arrange(-`Heaviest hit Target types`)%>%head(10)
View(Q5)

ee=ggplot(Q5,aes(x=reorder(targtype1_txt,-`Heaviest hit Target types`),y=`Heaviest hit Target types`))+
  geom_bar(aes(fill=targtype1_txt),stat = "Identity",width=.9)+
  geom_text(aes(label=`Heaviest hit Target types`),position = position_stack(vjust = 0.5))+
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = seq(10500,255000,50000))+
  theme(axis.text.x=element_blank(),axis.ticks.x = element_blank())+
  labs(subtitle='From 1970 to 2015',y='Total Casualities', x='Target Type', title='Heaviest hit Target types (Based on both Killed and wounded)',caption ='www.GTD.com')

ggplotly(ee)
  ##########################################################################################
####6. Terrorist attack in India and Pakistan in last 45 years?

Q6=c%>%filter(doubtterr==0 & (country_txt=="India" | country_txt=="Pakistan"))%>%group_by(country_txt,iyear)%>%
  summarise(`No Of Attacks`=sum(iyear=n()))%>%arrange(iyear)
View(Q6)

ff=ggplot(Q6,aes(x=iyear,y=`No Of Attacks`))+
  geom_bar(stat = 'identity',aes(fill=country_txt,width = .5))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom", legend.key.width = unit(.5, "cm"))+
theme(axis.text.x=element_blank(),axis.ticks.x = element_blank())+
  labs(subtitle='From 1970 to 2015',y='No Of Attacks', x='Year', title='Terrorist attack in India and Pakistan in last 45 years',caption ='www.GTD.com')

ggplotly(ff)
##########################################################################################
####7. Terror attack in United States vs Russian Federation/USSR in last 45 years?
###########correct it

Q7=c%>%filter(doubtterr==0 & (country_txt=="United States" | country_txt=="Russia"|country_txt=="Soviet Union"))%>%group_by(iyear,country_txt)%>%summarise(`No Of Attacks`=sum(iyear=n()))%>%arrange(`No Of Attacks`)
Q7$country_txt <- as.character(Q7$country_txt)
Q7$country_txt[Q7$country_txt=="Soviet Union"] = "Russia/Soviet Union"
Q7$country_txt[Q7$country_txt=="Russia"] = "Russia/Soviet Union"
Q7$country_txt <- as.character(Q7$country_txt)

Q7_1=Q7%>%group_by(iyear,country_txt)%>%summarise(total=sum(`No Of Attacks`))
View(Q7_1)


hh=ggplot(Q7_1,aes(x=iyear,y=total))+
  geom_bar(stat = 'identity',aes(fill=country_txt,width = .5))+
  theme_bw() +
  labs(subtitle='From 1970 to 2015',y='No Of Attacks', x='Country', title='Terror attack in United States vs Russian Federation/USSR in last 45 years',caption ='www.GTD.com')
ggplotly(hh)
###########################################################################################
####8. Where are there the most casualties ?

Q8=c%>%group_by(country_txt)%>%summarise(`No Of Casuality`=round(sum(sum(nkill,na.rm=T),sum(nwound,na.rm=T))))%>%arrange(-`No Of Casuality`)%>%head(10)
View(Q8)


hh=ggplot(Q8,aes(x=reorder(country_txt,-`No Of Casuality`),y=`No Of Casuality`))+
geom_bar(stat="Identity",aes(fill =country_txt),width = .40)+
geom_text(aes(label = `No Of Casuality`),vjust = 0.25)+
theme_bw()+  
theme(axis.text.x = element_blank())+
theme(legend.position = "bottom",legend.title = element_blank())+
theme(legend.text = element_text(colour = 'black', size = 8,face = 'bold'))+
  labs(subtitle='From 1970 to 2015',y='No Of Casualities', x='Year', title='Where are there the most casualties',caption ='www.GTD.com')

ggplotly(hh)
###########################################################################################
####9.How have casualties evolved throughout the years?
#############
Q9=c%>%group_by(iyear)%>%summarise(`Total Kills`=round(sum(nkill,na.rm=T)),`Total Wounded`=round(sum(nwound,na.rm=T)),`Total Casualities`=`Total Kills`+`Total Wounded`)
View(Q9)

ii=ggplot(Q9,aes(x=iyear))+
  #geom_line(stat="Identity")+
  geom_line(aes(y=Q9$`Total Kills`),col='red',size=1)+
  geom_line(aes(y=Q9$`Total Wounded`),col='blue',size=1)+
  labs(subtitle='Kills=Red  Wounded=Blue',y='No Of Casualities', x='Year', title='How have casualties evolved throughout the years',caption ='www.GTD.com')+
  theme_bw()+  
  theme(axis.text.x = element_text())+
  theme(legend.position = " ")+
  scale_x_continuous(breaks = seq(1970,2015,5))+
  scale_y_continuous(breaks = seq(80,44000,5000))+
  geom_point(aes(y=Q9$`Total Kills`),col='Black',size=1)+
  geom_point(aes(y=Q9$`Total Wounded`),col='black',size=1)+
  theme(legend.text = element_text(colour = 'black', size = 8,face = 'bold'))

ggplotly(ii)
###########################################################################################
####Q10. What are the casualties by weapon type?

Q10=c%>%group_by(weaptype1_txt)%>%
  summarise(`No Of Casuality`=round(sum(sum(nkill,na.rm=T),sum(nwound,na.rm=T))))
View(Q10)


jj=ggplot(Q10,aes(x=reorder(weaptype1_txt,-`No Of Casuality`),y=`No Of Casuality`))+
  geom_bar(aes(fill=weaptype1_txt),stat="Identity",width=(.9))+
  geom_text(aes(label=`No Of Casuality`),vjust = -0.25)+
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"),legend.title = element_blank())+
  scale_y_continuous(breaks = seq(0,500000,50000))+
  theme(axis.text.x = element_blank())+
  labs(subtitle='From 1970 to 2015',y='Total Casualities', x='Target Type', title='Heaviest hit Target types (Based on both Killed and wounded)',caption ='www.GTD.com')


ggplotly(jj)
###########################################################################################
####11. Are certain nationalities more targeted? If yes, which one ?

Q11=c%>%filter(natlty1_txt!='.')%>% group_by(natlty1_txt)%>%summarise(most.targated=n())%>%arrange(-most.targated)%>%head(10)
View(Q11)


kk=ggplot(Q11,aes(x=reorder(natlty1_txt,-most.targated),y=most.targated))+
  geom_bar(aes(fill=natlty1_txt),stat="Identity",width=(.9))+
  theme(legend.position = "bottom", legend.key.width = unit(.7, "cm"))+
  scale_y_continuous(breaks = seq(3000,18000,5000))+
  theme(axis.text.x = element_text(angle = -45))+
  geom_text(aes(label=most.targated),vjust = -0.25)+
  theme(axis.text.x = element_blank(),legend.title = element_blank())+
  labs(subtitle='From 1970 to 2015',y='Most Targeted', x='Nationality', title='nationalities more targeted',caption ='www.GTD.com')

ggplotly(kk)
###########################################################################################
####Q12. Are some countries better at defending themselves against terrorist attacks?If yes, which is the safest country to live

total_casualities = c %>% group_by(country_txt) %>% 
  summarise(totalcas = (round(sum(nkill,na.rm = TRUE) +sum(nwound,na.rm = TRUE))))

total1 = sum(total_casualities$totalcas)

proportion_casualities = total_casualities%>% group_by(country_txt) %>% 
  summarise(casualprop = round(totalcas/total1,3))%>%
  arrange(-casualprop)

total_attack = c %>% group_by(country_txt) %>% summarise(totalattack = n())%>%arrange(-totalattack)%>% head(30)
View(total_attack)

total2 = sum(total_attack$totalattack)

proportion_attack = total_attack %>% group_by(country_txt) %>% 
  summarise(attackprop = round(totalattack/total2,3))%>% arrange(-attackprop)

attack_cas_prop = merge(x = proportion_attack,y = proportion_casualities,
                        by.x = "country_txt",by.y = "country_txt",all.x = TRUE)
#countries with less proportion_casualities than proportional_attack
attack_cas_prop1  = attack_cas_prop %>% filter(attackprop > casualprop) %>% group_by(country_txt)%>% 
  summarise(diff = attackprop - casualprop)%>% arrange(-diff) %>% head(10)

mergetab4 = merge(total_attack,attack_cas_prop1)%>% arrange(-diff)####
View(mergetab4)

ggplotly(ggplot(mergetab4,aes(x=reorder(country_txt,-diff),y=diff))+
  geom_bar(stat="identity",aes(fill=country_txt))+
    labs(title='Attack Casuality Prop difference')+
  theme(axis.text.x = element_blank(),legend.title = element_blank()))

## looking for low number of extended attacks
## looking for the proportion between total attacks and extended attackz

extended_attack = c %>%  filter(extended == 1) %>% group_by(country_txt) %>% summarise(count = n()) %>%
  arrange(-count)

merge_table1 = merge(total_attack,extended_attack)

prop_extended_attack = merge_table1%>% group_by(country_txt) %>%
  summarise(prop = round(count/totalattack,3)) %>% arrange(-prop) %>% tail(10)
mergetab3 = merge(prop_extended_attack,total_attack) %>% arrange(-totalattack)####
View(mergetab3)

ggplotly(ggplot(mergetab3,aes(x=reorder(country_txt,prop),y=prop))+geom_bar(stat="identity",aes(fill=country_txt))+
           labs(title='Less Extended Attacks')+
           theme(axis.text.x = element_blank(),legend.title = element_blank()))
##Comparing the proportion of kidnap and rescued:

number_kidnap = c %>% filter(ishostkid == 1) %>% filter(nhostkid != -99)%>% filter(nreleased != -99)%>%
  group_by(country_txt) %>% summarise(total_kidnap = sum(nhostkid)) %>% arrange(-total_kidnap)

View(number_kidnap)

number_release = c %>% filter(ishostkid == 1) %>% filter(nhostkid != -99) %>% filter(nreleased != -99) %>%
  group_by(country_txt)%>%summarise(total_release = sum(nreleased)) 

merge_table2 = merge(x = number_kidnap,y= number_release) %>% group_by(country_txt) %>% 
  summarise(prop.rescued = round(total_release/total_kidnap,3)) %>%  arrange(-prop.rescued)%>% 
  head(10)

View(merge_table2)

mergetable2=merge(total_attack,merge_table2)####
View(mergetable2)
ggplotly(ggplot(mergetable2,aes(x=reorder(country_txt,-prop.rescued),y=prop.rescued))+
           geom_bar(stat="identity",aes(fill=country_txt))+
           labs(title='Kidnapped and Rescued prop')+
           theme(axis.text.x = element_blank(),legend.title = element_blank()))

##success proportion 
number_defend = c %>%filter(success == 0) %>%  group_by(country_txt) %>% summarise(count = n())

merge_table3 = merge(x = number_defend,y = total_attack)

defend_prop = merge_table3 %>% group_by(country_txt) %>% summarise(prop = round(count/totalattack,3))%>% 
  arrange(-prop)

View(defend_prop)

temp1tab=merge(total_attack,defend_prop)%>%arrange(-prop)%>%head(10)
View(temp1tab)####4
ggplotly(ggplot(temp1tab,aes(x=reorder(country_txt,-prop),y=prop))+
           geom_bar(stat="identity",aes(fill=country_txt))+
           labs(title='Sucesses rate for defending terrorist attacks')+
           theme(axis.text.x = element_blank(),legend.title = element_blank()))


