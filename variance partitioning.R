POV(Tibia.length ~ Species, data.new[!is.na(tibianew$Tibia.length),], Complete = TRUE)

POV(Abdomen.Volume ~ Species, data.new[!is.na(data.new$Abdomen.Volume),], Complete = TRUE)

POV(Wing.length~ Species, data.new[!is.na(data.new$Wing.length),], Complete = TRUE)

POV(Egg.Count~ Species, data.new[!is.na(data.new$Egg.Count),], Complete = TRUE)

POV(Egg.Volume~ Species, data.new[!is.na(data.new$Egg.Volume),], Complete = TRUE)

POV(Rep.Effort~ Species, data.new[!is.na(data.new$Rep.Effort),], Complete = TRUE)

morph.datanew<-morph.data[morph.data$Site!="RU",] #### RU population is neither "B. treatae" nor "B. fossoria" species
morph.datanew<-morph.datanew[!is.na(morph.datanew$Abdomen.length1),]
morph.datanew<-morph.datanew[!is.na(morph.datanew$Wing.length1),]
morph.datanew$Wasp<-"B. treatae"
morph.datanew$Wasp[morph.datanew$Original.Host=="Qg"]<-"B. fossoria"
morph.datanew2<-morph.datanew[,c(1,13,4,5,6,7)]
head(morph.datanew2)
colnames(morph.datanew2)[6]<-"Plant"


data.new2<-data.new[!is.na(data.new$Tibia.length1),]
data.new2<-data.new2[!is.na(data.new2$Abdomen.width),]
data.new2<-data.new2[!is.na(data.new2$Wing.length1),]
data.new2<-data.new2[!is.na(data.new2$Abdomen.length1),]
data.new3<-data.new2[,c(5,6,10,13,14)]
head(data.new3)
data.new3$Plant<-"natal"
colnames(data.new3)[c(1,2)]<-c("Site","Wasp")

datacomb<-rbind(data.new3,morph.datanew2)

Ab.var<-POV(Abdomen.length~Wasp*Plant*Tibia.length, datacomb, Complete = TRUE)

POV(Abdomen.length~Wasp*Plant, datacomb, Complete = TRUE)
POV(Abdomen.length~Wasp, datacomb, Complete = TRUE)
POV(Abdomen.length~Plant, datacomb, Complete = TRUE)

write.csv(Ab.var,"Ab.var.csv")

Wing.var<-POV(Wing.length~Wasp*Plant*Tibia.length, datacomb, Complete = TRUE)
write.csv(Wing.var,"Wing.var.csv")
POV(Wing.length~Wasp*Plant, datacomb, Complete = TRUE)


  ggplot(data=datacomb,aes(x=Wing.length, fill=Wasp)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

Tib.var<-POV(Tibia.length~Wasp*Plant, datacomb[!is.na(datacomb$Tibia.length),], Complete = TRUE)
Tib.var
write.csv(Tib.var,"Tib.var.csv")
