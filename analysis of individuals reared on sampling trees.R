######### load packages ##########
library(emmeans)
library(nlme)
library(plyr)
library(ggplot2)
######### read the morph data #########
morph.data<-read.csv("B. treatae morphological measurements.csv")
head(morph.data)
morph.data<-morph.data[,c(6,7,8,12,13,14)]
summary(morph.data)
morph.data$native[morph.data$Original.Host==morph.data$Tree.Species]<-"native"
morph.data$native[morph.data$Original.Host!=morph.data$Tree.Species]<-"non-native"
morph.data<-morph.data[morph.data$Original.Host %in% c("Qv","Qg"),]
morph.data$Original.Host<-as.factor(morph.data$Original.Host)
morph.data$Tree.Species<-as.factor(morph.data$Tree.Species)

morph.data$Abdomen.length1<-scale(morph.data$Abdomen.length)
morph.data$Tibia.length1<-scale(morph.data$Tibia.length)
morph.data$Wing.length1<-scale(morph.data$Wing.length)

morph.data$type[morph.data$Original.Host=="Qv" & morph.data$native=="native"]<-"Bt on native host"
morph.data$type[morph.data$Original.Host=="Qv" & morph.data$native=="non-native"]<-"Bt on non-native host"
morph.data$type[morph.data$Original.Host=="Qg" & morph.data$native=="native"]<-"Bf on native host"
morph.data$type[morph.data$Original.Host=="Qg" & morph.data$native=="non-native"]<-"Bf on non-native host"

### summary of sample size ###
morph.data$count<-1
ddply(morph.data,c("type"),summarize,N=sum(count))
morph.datanew<-morph.data[morph.data$Site!="RU",]
#########################################################################################################
AL.TL.cm1<-lme(Abdomen.length1~Tibia.length1*Original.Host*native,
               random=~1|Site,data=morph.datanew,na.action=na.omit)
summary(AL.TL.cm1)

POV(Abdomen.length~Tibia.length*Original.Host*native, morph.datanew[!is.na(morph.datanew$Abdomen.length),],
    Complete = TRUE)

POV(Abdomen.length~Original.Host*native, morph.datanew[!is.na(morph.datanew$Abdomen.length),],
    Complete = TRUE)

POV(Abdomen.length~Original.Host, morph.datanew[!is.na(morph.datanew$Abdomen.length),],
    Complete = TRUE)

AL.TL.Splot<-ggplot(data = morph.datanew, aes(y=Abdomen.length,x=Tibia.length,fill=factor(type),shape=type)) +
  geom_point(size=3)+
  geom_smooth(aes(linetype=type),method = "lm", se = FALSE,color="black",size=0.5,fullrange=TRUE,show.legend = FALSE)+
  scale_shape_manual(values=c(21,22,21,22))+
  scale_fill_manual(name="",values=c("black","black","white","white"),
                    labels=c(expression(italic(Bf)~on~native~host),expression(italic(Bf)~on~non-native~host),
                        expression(italic(Bt)~on~native~host),expression(italic(Bt)~on~non-native~host)))+
  scale_linetype_manual(values=c(1,1,5,5))+
  guides(fill= guide_legend(override.aes = list(
    shape =c(21,22,21,22)),byrow=TRUE),shape="none")+
  ylab("Abdomen length (mm)")+
  xlab("Body size (mm)")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.2,1.4,by=0.2))+
  scale_y_continuous(expand=c(0,0),limits=c(0,2),breaks=seq(0,2,by=0.5))+
  theme_bw()+
  theme(
    plot.margin=unit(c(0.6,0.9,0.5,1),"cm"),
    legend.position = c(0.76,0.15),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=22),
    legend.spacing.y = unit(0.14, 'cm'),
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.46, y=1.9,label="B",size=16)

AL.TL.Splot

summary(AL.TL.cm1)$terms

POV(Wing.length~ Species, data.new[!is.na(data.new$Wing.length),], 
    Complete = TRUE)
#### wing length body size on sampling trees ######################################################
WL.TL.cm1<-lme(Wing.length1~Tibia.length1*Original.Host*native,random=~1|Site,data=morph.data[morph.data$Site!="RU",],na.action=na.omit)
summary(WL.TL.cm1)
WL.TL.Splot<-ggplot(data = morph.data[morph.data$Site!="RU",], aes(y=Wing.length,x=Tibia.length,fill=factor(type),
                                                      shape=type)) +
  geom_point(size=3)+
  geom_smooth(aes(linetype=type),method = "lm", color="black",se = FALSE,size=0.5,fullrange=TRUE,show.legend = FALSE)+
  scale_shape_manual(values=c(21,22,21,22))+
  scale_fill_manual(name="",values=c("black","black","white","white"),
                    labels=c(expression(italic(Bf)~on~native~host),expression(italic(Bf)~on~non-native~host),
                             expression(italic(Bt)~on~native~host),expression(italic(Bt)~on~non-native~host)))+
  guides(fill= guide_legend(override.aes = list(shape =c(21,22,21,22)),byrow=TRUE),shape="none")+
  scale_linetype_manual(values=c(1,1,5,5))+
  ylab("Wing length (mm)")+
  xlab("Body size (mm)")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.2,1.4,by=0.2))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4),breaks=seq(0,4,by=0.5))+
  theme_bw()+
  theme(
    plot.margin=unit(c(0.5,0.9,0.5,1),"cm"),
    legend.position = c(0.76,0.15),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=24),
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.46, y=3.8,label="A",size=16)


WL.TL.Splot|AL.TL.Splot ### save 




############################################################################
########### wing comparison between allopatric and sympatric species #####
###########################################################################
morph.data$wasp.species<-"B. treatae"
morph.data$wasp.species[morph.data$Original.Host=="Qg"]<-"B. fossoria"
morph.data$wasp.species[morph.data$Site=="RU"]<-"B. kinseyi"

morph.data$wasp.species<-factor(morph.data$wasp.species,levels=c("B. kinseyi","B. fossoria","B. treatae"))

AL.TL.cm2<-lme(Abdomen.length1~Tibia.length1*wasp.species,random=~1|Site,data=morph.data,na.action=na.omit)
summary(AL.TL.cm2)

WL.TL.cm3<-lme(Wing.length1~Tibia.length1*wasp.species,random=~1|Site,data=morph.data,na.action=na.omit)
summary(WL.TL.cm3)

m3<-lm(Wing.length1~Tibia.length1,data=morph.data[morph.data$Site=="RU",])
residuals(m3)
m4<-lm(Abdomen.length1~Tibia.length1,data=morph.data[morph.data$Site=="RU",])

residuals(m4)

plot(residuals(m3),residuals(m4))
cor.test(residuals(m3),residuals(m4))

ggplot(data = morph.data, aes(y=Wing.length,x=Tibia.length,fill=wasp.species,
                              shape=wasp.species)) +
  geom_point(size=2.5)+
  geom_smooth(aes(color=wasp.species,linetype=wasp.species),method = "lm", 
              se = FALSE,size=0.5,fullrange=TRUE,show.legend = TRUE)+
  scale_shape_manual(values=c(21,22,24))+
  scale_fill_manual(values=c("black","white","grey"))+
  scale_color_manual(values=c("black","black","black"))+
  scale_linetype_manual(values=c(1,5,3))+
  ylab("Wing length (mm)")+
  xlab("Body size (mm)")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.2,1.4,by=0.2))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4),breaks=seq(0,4,by=0.5))+
  theme_bw()+
  theme(
    plot.margin=unit(c(0.5,0.9,0.5,1),"cm"),
    legend.position = c(0.76,0.15),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=20,face="italic"),
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
