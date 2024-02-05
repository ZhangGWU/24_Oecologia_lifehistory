###################### April 2 2022#######################
################## by Linyi Zhang ####################
######################################################
library(ggplot2)
library(readr)
library(tidyr)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(cowplot)
library(dplyr)
library(devtools)
library(nlme)
library(patchwork)
library(emmeans)
library(POV)
packages <- c("ggplot2", "dplyr", "lavaan", "plyr", "cowplot", "rmarkdown", 
              "readr", "caTools", "bitops")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots@main')
library(raincloudplots)
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}
###############################################################################
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)
####################################################################################
########### read dataset ###########
data<-read.csv("Reproductive.Strategy.Data.csv")
head(data)
str(data)
summary(data$Egg.Count)

#Compute additional fields

#egg shape (L-W)
data$Egg.Shape <- data$Avg.egg.length-data$Avg.egg.width

#compute egg volume (in mm^3) using ellisoid formula
data$Egg.Volume <- (4/3)*pi*(1/8)*data$Avg.egg.width*data$Avg.egg.length^2

#Compute total volume of eggs using individual egg volume and number of eggs
data$Rep.Effort <- data$Egg.Volume*as.numeric(data$Egg.Count)

#Compute total abdomen volume using ellipsoid formula: a reasonable estimate
data$Abdomen.Volume <- (4/3)*pi*(1/8)*data$Abdomen.length*data$Abdomen.width*data$Abdomen.height

##### only including individuals from natural collection #####
unique(data$Collection.Type)
data.new<-data[data$Collection.Type=="Nat. Col.",]
#####################################################################################
####### comparison of tibia length, abdomen volume, wing length, potential fecundity, individual egg size, reproductive effort #######
TL.m1<-lme(Tibia.length~Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(TL.m1)
emmeans(TL.m1, list(pairwise~Species),adjust="tukey")

AV.m1<-lme(Abdomen.Volume~Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(AV.m1) ### t= -2.873, p= 0.0283 ###
emmeans(AV.m1, list(pairwise~Species),adjust="tukey")

WL.m1<-lme(Wing.length~Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(WL.m1) ### t= 6.240, p= 0.0004 ###
emmeans(WL.m1, list(pairwise~Species),adjust="tukey")

EN.m1<-lme(Egg.Count~Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(EN.m1) ### t= -1.826, p= 0.111 ###
emmeans(EN.m1, list(pairwise~Species),adjust="tukey")

EV.m1<-lme(Egg.Volume~Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(EV.m1) ### t= -4.148, p= 0.0043 ###
emmeans(EV.m1, list(pairwise~Species),adjust="tukey")

RE.m1<-lme(Rep.Effort~Species,random=~1|Location,data=data.new,na.action=na.omit)

summary(RE.m1) ### t= -3.643, p= 0.0083 ###
emmeans(RE.m1, list(pairwise~Species),adjust="tukey")
####################################################################################
#### making plot of tibia length, abdomen volume, wing length, potential fecundity, individual egg size, reproductive effort #######
TL.plot<-ggplot(data = data.new, aes(y =Tibia.length,color=Species,fill=Species)) +
  geom_boxplot(aes(x=Species),width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5)+
  geom_point(aes(x=Species), position = position_jitter(width = 0.15),  size = .5, alpha = 0.8) +
  geom_flat_violin(aes(x=2),position = position_nudge(x =0.3, y = 0),adjust = 1.5,trim = FALSE, alpha = .5, colour = NA)+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_y_continuous(limits=c(0.5,1.6),breaks=seq(0.5,1.5,by=0.25))+
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey")) +
  ylab("Body size (mm)")+
  xlab("")+
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 22),
    axis.text.y = element_text(colour = 'black',size=18),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.6, y=1.6,label="A",size=16)+
  annotate("text",x=c(1,2),y=c(1.42,1.38),label=c("n = 105","n = 97"),size=9)
TL.plot
#########################################################################
WL.plot<-ggplot(data = data.new, aes(y =Wing.length,color=Species,fill=Species)) +
  geom_boxplot(aes(x=Species),width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5)+
  geom_point(aes(x=Species), position = position_jitter(width = 0.15),  size = .5, alpha = 0.8) +
  geom_flat_violin(aes(x=2),position = position_nudge(x =0.3, y = 0),adjust = 1.5,trim = FALSE, alpha = .5, colour = NA)+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_y_continuous(limits=c(0.9,4.2),breaks=seq(1,4,0.5))+
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey")) +
  ylab("Wing length (mm)")+
  xlab("")+
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 22),
    axis.text.y = element_text(colour = 'black',size=18),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.6, y=4.2,label="B",size=16)+
  annotate("text",x=1.5, y=3.9,label="***",size=13)+
  annotate("text",x=c(1,2),y=c(3.0,3.7),label=c("n = 94","n = 95"),size=9)
WL.plot

### annotate("text",x=1.5, y=4,label=expression(paste(italic("t"),"= 6.240, ",italic("P"),"< 0.001")),size=10)+ #####

AV.plot<-ggplot(data = data.new, aes(y =Abdomen.Volume,color=Species,fill=Species)) +
  geom_boxplot(aes(x=Species),width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5)+
  geom_point(aes(x=Species), position = position_jitter(width = 0.15),  size = .5, alpha = 0.8) +
  geom_flat_violin(aes(x=2),position = position_nudge(x =0.3, y = 0),adjust = 1.5,trim = FALSE, alpha = .5, colour = NA)+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey")) +
  scale_y_continuous(limits=c(-0.2,3.6),breaks=seq(0,3.5,by=0.5))+
  ylab(expression(Abdomen~volume~(mm^3)))+
  xlab("")+
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 22),
    axis.text.y = element_text(colour = 'black',size=18),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.6, y=3.6,label="C",size=16)+
  annotate("text",x=1.5, y=3.4,label="*",size=13)+
  annotate("text",x=c(1,2),y=c(3.2,1.6),label=c("n = 105","n = 97"),size=9)

AV.plot
### annotate("text",x=1.5, y=3.48,label=expression(paste(italic("t"),"= -2.873, ",italic("P"),"= 0.0283")),size=10)+ ####

EN.plot<-ggplot(data = data.new, aes(y=Egg.Count,color=Species,fill=Species)) +
  geom_boxplot(aes(x=Species),width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5)+
  geom_point(aes(x=Species), position = position_jitter(width = 0.15),  size = .5, alpha = 0.8) +
  geom_flat_violin(aes(x=2),position = position_nudge(x =0.3, y = 0),adjust = 1.5,trim = FALSE, alpha = .5, colour = NA)+
  guides(fill = FALSE)+
  guides(color = FALSE)+
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey")) +
  scale_y_continuous(limits = c(0,620),breaks = seq(0,600,100))+
  ylab("Potential fecundity")+
  xlab("")+
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 22),
    axis.text.y = element_text(colour = 'black',size=18),
    axis.text.x = element_text(colour = 'black',size=26,face="italic",vjust = 0.5),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.6, y=600,label="D",size=16)+
  annotate("text",x=c(1,2),y=c(500,400),label=c("n = 102","n = 92"),size=9)

EN.plot
### annotate("text",x=1.5, y=580,label=expression(paste(italic("t"),"= -1.826, ",italic("P"),"= 0.111")),size=10)+ ###
EV.plot<-ggplot(data = data.new, aes(y=Egg.Volume*10000,color=Species,fill=Species)) +
  geom_boxplot(aes(x=Species),width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5)+
  geom_point(aes(x=Species), position = position_jitter(width = 0.15),  size = .5, alpha = 0.8) +
  geom_flat_violin(aes(x=2),position = position_nudge(x =0.3, y = 0),adjust = 1.5,trim = FALSE, alpha = .5, colour = NA)+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey")) +
  scale_y_continuous(expand=c(0,0),limits=c(2,12),breaks=seq(0,12,by=2))+
  ylab(expression(Egg~volume~(10^-4~mm^3)))+
  xlab("")+
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 22),
    axis.text.y = element_text(colour = 'black',size=18),
    axis.text.x = element_text(colour = 'black',size=26,face="italic",vjust = 0.5),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.6, y=11.5,label="E",size=16)+
  annotate("text",x=1.5, y=10.5,label="**",size=13)+
  annotate("text",x=c(1,2),y=c(10,8),label=c("n = 102","n = 92"),size=9)

EV.plot
##  annotate("text",x=1.5, y=11.6,label=expression(paste(italic("t"),"= -4.148, ",italic("P"),"= 0.0043")),size=10)+ ###

RE.plot<-ggplot(data = data, aes(y=Rep.Effort,color=Species,fill=Species))+
  geom_boxplot(aes(x=Species),width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5)+
  geom_point(aes(x=Species), position = position_jitter(width = 0.15),  size = .5, alpha = 0.8)+
  geom_flat_violin(aes(x=2),position = position_nudge(x =0.3, y = 0),adjust = 1.5,trim = FALSE, alpha = .5, colour = NA)+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey")) +
  ylab(expression(Reproductive~effort~(mm^3)))+
  xlab("")+
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 22),
    axis.text.y = element_text(colour = 'black',size=18),
    axis.text.x = element_text(colour = 'black',size=26,face="italic",vjust = 0.5),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.6, y=0.5,label="F",size=16)+
  annotate("text",x=1.5, y=0.46,label="**",size=13)+
  annotate("text",x=c(1,2),y=c(0.4,0.25),label=c("n = 102","n = 92"),size=9)

RE.plot
####  annotate("text",x=1.5, y=0.45,label=expression(paste(italic("t"),"= -3.643, ",italic("P"),"= 0.0083")),size=10)+ ########

Blank.TL.plot<-ggplot(data = data, aes(y=Rep.Effort,color=Species,fill=Species))+
  ylab("")+
  xlab("")+
  theme_bw() +
  theme(
    text = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_blank())

Blank.TL.plot
### combine six plots, figure 4#####
(TL.plot|WL.plot|AV.plot)/(EN.plot|EV.plot|RE.plot)

### save image 21*13 ####

#### correlation bettween different morphological traits ###################
cor.test(data$Egg.Count[data$Species=="B. treatae"],data$Abdomen.length[data$Species=="B. treatae"])
cor.test(data$Egg.Count[data$Species=="B. fossoria"],data$Abdomen.length[data$Species=="B. fossoria"])
cor.test(data$Egg.Count,data$Abdomen.length)
cor.test(data$Rep.Effort,data$Abdomen.length)
cor.test(data$Abdomen.Volume,data$Abdomen.length)
data1<-data
data1$Col[data1$Species=="B. fossoria"]<-"black"
data1$Col[data1$Species=="B. treatae"]<-"white"
length(data1$Col[data1$Species=="B. fossoria"])
length(data1$Col[data1$Species=="B. treatae"])
########## make supplemental figure ############
par(mfrow=c(1,2),mar=c(5,5,2,2))
plot(data=data1,Egg.Count~Abdomen.length,xlim=c(0.5,2.5),ylim=c(0,500),cex.lab=1.8,
     cex.axis=1.4,pch=21,cex=0.8,bg=Col,
     xlab="Abdomen length (mm)", ylab="Potential fecundity")
text(1,420,labels=expression(paste(italic("r"), "= 0.823, ",italic("P")," < 0.001")),cex=1.8)
text(0.55,480,labels="A",cex=2.5)
abline(lm(data=data1,Egg.Count~Abdomen.length))
plot(data=data1,Rep.Effort~Abdomen.length,xlim=c(0.5,2.5),ylim=c(0,0.4),cex.lab=1.8,
     cex.axis=1.4,pch=21,cex=0.8,bg=Col,
     xlab="Abdomen length (mm)",ylab="Reproductive effort")
text(1,0.32,labels=expression(paste(italic("r"), "= 0.844, ",italic("P"), " < 0.001")),cex = 1.8)
text(0.55,0.385,labels="B",cex=2.5)
abline(lm(data=data1,Rep.Effort~Abdomen.length))
## save as 14*8 ####
###################################################################
TL.result<-list()
### center variables ###
data.new$Tibia.length1<-scale(data.new$Tibia.length)
data.new$Wing.length1<-scale(data.new$Wing.length)
data.new$Abdomen.Volume1<-scale(data.new$Abdomen.Volume)
data.new$Abdomen.length1<-scale(data.new$Abdomen.length)
data.new$Egg.Volume1<-scale(data.new$Egg.Volume)
data.new$Egg.Count1<-scale(data.new$Egg.Count)
data.new$Rep.Effort1<-scale(data.new$Rep.Effort)
###### correlation between tibia length and abdomen ######
AV.TL.m1<-lme(Abdomen.length1~Tibia.length1*Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(AV.TL.m1)

summary(anova(AV.TL.m1,type = ))
POV(Abdomen.length~Tibia.length*Species, data.new[!is.na(data.new$Abdomen.length),],
    Complete = TRUE)

POV(Abdomen.length~Species, data.new[!is.na(data.new$Abdomen.length),],
    Complete = TRUE)

AV.TL.plot<-ggplot(data = data.new, aes(y=Abdomen.Volume,x=Tibia.length,fill=Species,shape=Species)) +
  geom_point(size=3)+
  geom_smooth(aes(color=Species,linetype=Species),method = "lm", se = FALSE,size=0.5,fullrange=TRUE)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  scale_color_manual(values=c("black","black"))+
  scale_linetype_manual(values=c(1,5))+
  ylab(expression(Abdomen~volume~(mm^3)))+
  xlab("")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.2,1.4,by=0.2))+
  scale_y_continuous(limits=c(-0.2,3.6),breaks=seq(0,3.5,by=0.5))+
  theme_bw() +
  theme(
    plot.margin=unit(c(0.5,0.9,0.5,1),"cm"),
    legend.position = "none",
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.46, y=3.5,label="B",size=16)

AV.TL.plot
##### annotate("text",x=0.7, y=3.5,label="Body size x Species: ",size=9)+
##### annotate("text",x=0.7, y=3.2,label=expression(paste(italic("t"),"= -4.607, ",italic("p"),"< 0.001")),size=9)+
##### annotate("text",x=0.7, y=2.82,label="***",size=16)
###### correlation between tibia length and wing length ######

WL.TL.m1<-lme(Wing.length1~Tibia.length1*Species,random=~1|Location,data=data.new,na.action=na.omit,
              method="ML")
summary(WL.TL.m1)

WL.TL.plot<-ggplot(data = data.new, aes(y=Wing.length,x=Tibia.length,fill=Species,shape=Species)) +
  geom_point(size=3)+
  geom_smooth(aes(color=Species,linetype=Species),method = "lm", se = FALSE,size=0.5,fullrange=TRUE)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  scale_color_manual(values=c("black","black"))+
  scale_linetype_manual(values=c(1,5))+
  ylab("Wing length (mm)")+
  xlab("")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.2,1.4,by=0.2))+
  scale_y_continuous(limits=c(0.9,4.2),breaks=seq(1,4,0.5))+
  theme_bw()+
  theme(
    plot.margin=unit(c(0.5,0.9,0.5,1),"cm"),
    legend.position = c(0.5,0.95),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=32, face="italic"),
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.46, y=4,label="A",size=16)

WL.TL.plot
#### annotate("text",x=0.7, y=0.38,label="body size x species: ",size=9)+
#### annotate("text",x=0.7, y=0.34,label=expression(paste(italic("t"),"= -4.308, ",italic("p"),"< 0.001")),size=9)+
#### annotate("text",x=0.7, y=0.30,label="***",size=16)+
###### correlation between tibia length and egg count ######
EN.TL.m1<-lme(Egg.Count1~Tibia.length1*Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(EN.TL.m1)
TL.result[[3]]<-data.frame(summary(EN.TL.m1)[[4]])
names(TL.result)[3]<-"Potential fecundity"

EN.TL.plot<-ggplot(data = data.new, aes(y=Egg.Count,x=Tibia.length,fill=Species,shape=Species)) +
  geom_point(size=3)+
  geom_smooth(aes(color=Species,linetype=Species),method = "lm", se = FALSE,size=0.5,fullrange=TRUE)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  scale_color_manual(values=c("black","black"))+
  scale_linetype_manual(values=c(1,5))+
  ylab("Potential fecundity")+
  xlab("Body size (mm)")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.4,1.4,by=0.2))+
  scale_y_continuous(limits = c(0,620),breaks = seq(0,600,100))+
  theme_bw() +
  theme(
    plot.margin=unit(c(0.6,0.9,0.5,1),"cm"),
    legend.position = "none",
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.48, y=600,label="C",size=16)

EN.TL.plot
#########  annotate("text",x=0.7, y=370,label="body size x species: ",size=9)+
######### annotate("text",x=0.7, y=330,label=expression(paste(italic("t"),"= -2.959, ",italic("p"),"= 0.0035")),size=9)+
######### annotate("text",x=0.7, y=290,label="**",size=16)
###### correlation between tibia length and egg volume ######
EV.TL.m1<-lme(Egg.Volume1~Tibia.length1*Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(EV.TL.m1)

EV.TL.plot<-ggplot(data = data.new, aes(y=Egg.Volume *10000,x=Tibia.length,fill=Species,shape=Species)) +
  geom_point(size=3)+
  geom_smooth(aes(color=Species,linetype=Species),method = "lm", se = FALSE,size=0.5,fullrange=TRUE)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  scale_color_manual(values=c("black","black"))+
  scale_linetype_manual(values=c(1,5))+
  ylab(expression(Egg~volume~(10^-4~mm^3)))+
  xlab("Body size (mm)")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.4,1.4,by=0.2))+
  scale_y_continuous(expand=c(0,0),limits=c(0,12),breaks=seq(2,12,by=2))+
  theme_bw() +
  theme(
    plot.margin=unit(c(0.6,0.9,0.5,1),"cm"),
    legend.position = "none",
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.48, y=11.5,label="D",size=16)

EV.TL.plot
###### correlation between tibia length and reproductive effort ######
RE.TL.m1<-lme(Rep.Effort1~Tibia.length1*Species,random=~1|Location,data=data.new,na.action=na.omit)
summary(RE.TL.m1)

RE.TL.plot<-ggplot(data = data.new, aes(y=Rep.Effort,x=Tibia.length,fill=Species,shape=Species)) +
  geom_point(size=3)+
  geom_smooth(aes(color=Species,linetype=Species),method = "lm", se = FALSE,size=0.5,fullrange=TRUE)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  scale_color_manual(values=c("black","black"))+
  scale_linetype_manual(values=c(1,5))+
  ylab(expression(Reproductive~effort~(mm^3)))+
  xlab("Body size (mm)")+
  scale_x_continuous(expand=c(0,0),limits=c(0.4,1.4),breaks=seq(0.2,1.4,by=0.2))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.06,0.5),breaks=seq(0,0.5,by=0.1))+
  theme_bw()+
  theme(
    plot.margin=unit(c(0.6,0.9,0.5,1),"cm"),
    legend.position = "none",
    text = element_text(size = 10),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  annotate("text",x=0.46, y=0.475,label="E",size=16)

RE.TL.plot
###  annotate("text",x=0.7, y=0.38,label="body size x species: ",size=9)+
### annotate("text",x=0.7, y=0.34,label=expression(paste(italic("t"),"= -4.308, ",italic("p"),"< 0.001")),size=9)+
### annotate("text",x=0.7, y=0.30,label="***",size=16)+


Blank.TL.plot<-ggplot(data = data.new, aes(y=Rep.Effort,x=Tibia.length,fill=Species,shape=Species)) +
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(
    plot.margin=unit(c(0.6,0.9,0.5,1),"cm"),
    legend.position = "none",
    text = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_blank())
Blank.TL.plot
### combine six plots, make figure 5 #####
(WL.TL.plot|AV.TL.plot|Blank.TL.plot)/(EN.TL.plot|EV.TL.plot|RE.TL.plot)
### save image 21*13 ####

##################################################################
##### test the trade-off between fecundity and wing length #######
data.new2<-data.new[!is.na(data.new$Tibia.length),]
data.new2<-data.new2[!is.na(data.new2$Abdomen.Volume),]
data.new2<-data.new2[!is.na(data.new2$Wing.length),]

#### get the residual of wing length against body size ##########
## for Qv wasps ##
cor.WlTl.Qv<-lm(Wing.length~Tibia.length,data=data.new2[data.new2$Species=="B. treatae",])
res.WlTl.Qv<-residuals(cor.WlTl.Qv)
ind.ID.Qv<-as.numeric(names(residuals(cor.WlTl.Qv)))
## for Qg wasps ##
cor.WlTl.Qg<-lm(Wing.length~Tibia.length,data=data.new2[data.new2$Species=="B. fossoria",])
res.WlTl.Qg<-residuals(cor.WlTl.Qg)
ind.ID.Qg<-as.numeric(names(residuals(cor.WlTl.Qg)))
## check the model ###
par(mfrow=c(2,2))
plot(cor.WlTl.Qv)
plot(cor.WlTl.Qg)
### combine Qv and Qg data ###
res.WlTl<-c(res.WlTl.Qv,res.WlTl.Qg)
ind.ID<-c(ind.ID.Qv,ind.ID.Qg)

data.new3<-data.frame(data[ind.ID,],res.WlTl)
data.new3$Abdomen.Volume1<-scale(data.new3$Abdomen.Volume)
data.new3$Egg.Count1<-scale(data.new3$Egg.Count)
data.new3$Egg.Volume1<-scale(data.new3$Egg.Volume)
data.new3$res.AvTl1<-scale(data.new3$res.AvTl)
##################################################################################
AvTL.WL.m1<-lme(Abdomen.Volume1~res.WlTl*Species,random=~1|Location,data=data.new3,na.action=na.omit)
summary(AvTL.WL.m1)

AvTL.WL.Qg<-lme(Abdomen.Volume1~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. fossoria",],na.action=na.omit)
summary(AvTL.WL.Qg)
cor.test(data.new3[data.new3$Species=="B. fossoria","Abdomen.Volume1"],
         data.new3[data.new3$Species=="B. fossoria","res.WlTl"]   )
AvTL.WL.Qv<-lme(Abdomen.Volume1~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. treatae",],na.action=na.omit)
summary(AvTL.WL.Qv)
cor.test(data.new3[data.new3$Species=="B. treatae","Abdomen.Volume1"],
         data.new3[data.new3$Species=="B. treatae","res.WlTl"]   )


AvTL.EC.Qg<-lme(Egg.Count1~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. fossoria",],na.action=na.omit)
summary(AvTL.EC.Qg)
AvTL.EC.Qv<-lme(Egg.Count1~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. treatae",],na.action=na.omit)
summary(AvTL.EC.Qv)

cor.test(data.new3[data.new3$Species=="B. fossoria","Egg.Count1"],
         data.new3[data.new3$Species=="B. fossoria","res.WlTl"]   )
cor.test(data.new3[data.new3$Species=="B. treatae","Egg.Count1"],
         data.new3[data.new3$Species=="B. treatae","res.WlTl"]   )

AvTL.EV.Qg<-lme(Egg.Volume1~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. fossoria",],na.action=na.omit)
summary(AvTL.EC.Qg)
AvTL.EV.Qv<-lme(Egg.Volume1~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. treatae",],na.action=na.omit)
summary(AvTL.EC.Qv)

cor.test(data.new3[data.new3$Species=="B. fossoria","Egg.Volume1"],
         data.new3[data.new3$Species=="B. fossoria","res.WlTl"]   )
cor.test(data.new3[data.new3$Species=="B. treatae","Egg.Volume1"],
         data.new3[data.new3$Species=="B. treatae","res.WlTl"]   )

AvTL.RE.Qg<-lme(Rep.Effort~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. fossoria",],na.action=na.omit)
summary(AvTL.EC.Qg)
AvTL.EV.Qv<-lme(Rep.Effort~res.WlTl,random=~1|Location,data=data.new3[data.new3$Species=="B. treatae",],na.action=na.omit)
summary(AvTL.EC.Qv)

cor.test(data.new3[data.new3$Species=="B. fossoria","Rep.Effort"],
         data.new3[data.new3$Species=="B. fossoria","res.WlTl"]   )
cor.test(data.new3[data.new3$Species=="B. treatae","Rep.Effort"],
         data.new3[data.new3$Species=="B. treatae","res.WlTl"]   )

################## make figure 7 ########
WLTL.AV.plot<-ggplot(data=data.new3,aes(y=Abdomen.Volume,x=res.WlTl,fill=Species,shape=Species)) +
  geom_point(size=3)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  xlab("Residuals of wing length against body size")+
  ylab("Abdomen volume (mm3)")+
  scale_x_continuous(expand=c(0,0),limits=c(-2,1),breaks=seq(-2,1,by=0.5))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4),breaks=seq(0,4,by=1))+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.margin=unit(c(0.5,1,1,1),"cm"),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

WLEC.AV.plot<-ggplot(data=data.new3,aes(y=Egg.Count,x=res.WlTl,fill=Species,shape=Species)) +
  geom_point(size=3)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  xlab("Residuals of wing length against body size")+
  ylab("Egg number")+
  scale_x_continuous(expand=c(0,0),limits=c(-2,1),breaks=seq(-2,1,by=0.5))+
  scale_y_continuous(expand=c(0,0),limits=c(0,500),breaks=seq(0,500,by=100))+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.margin=unit(c(0.5,1,1,1),"cm"),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

WLEV.AV.plot<-ggplot(data=data.new3,aes(y=Egg.Volume*10000,x=res.WlTl,fill=Species,shape=Species)) +
  geom_point(size=3)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  xlab("Residuals of wing length against body size")+
  ylab("Egg volume (mm3 * 10-04)")+
  scale_x_continuous(expand=c(0,0),limits=c(-2,1),breaks=seq(-2,1,by=0.5))+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.margin=unit(c(0.5,1,1,1),"cm"),
    text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

WLRE.AV.plot<-ggplot(data=data.new3,aes(y=Rep.Effort,x=res.WlTl,fill=Species,shape=Species)) +
  geom_point(size=3)+
  scale_shape_manual(values=c(21,21))+
  scale_fill_manual(values=c("black","white"))+
  xlab("Residuals of wing length against body size")+
  ylab("Reproductive effort (mm3)")+
  scale_x_continuous(expand=c(0,0),limits=c(-2,1),breaks=seq(-2,1,by=0.5))+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.margin=unit(c(0.5,1,1,1),"cm"),
    text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 22),
    axis.title.y = element_text(size = 22),
    axis.text = element_text(colour = 'black',size=18),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

plot_grid(WLTL.AV.plot,WLEC.AV.plot,
          WLEV.AV.plot,WLRE.AV.plot,nrow=2,align="v",
          labels=c("A","B","C","D"),label_size=36) ######## figure 7 #######

