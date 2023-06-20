R.Version()

rm(list=ls()) # Borra todos los objetos
setwd("") # Choosing dir
dir()

#DATOS USADOS
datos<-read.table(file="seeds.csv", header=T, sep=",", check.names=T, dec=".")
datos$individuo<-as.factor(datos$individuo)
datos$TRAT<-as.factor(datos$TRAT)
datos_pseudo<-datos[c(-24,-79),] # remove individuals with less of 3 flowers treated
str(datos_pseudo)

summary(datos_pseudo$individuo) # to verify the number of flowers per individual
datos_pseudo$individuo

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = ((sd(x[[col]], na.rm=TRUE))/(sqrt(length(x[[col]])))),
      N = length(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}


###############
#SEED NUMBER#
###############
library(lme4)
library(lmerTest)
library(report)

model_numero_sr<-lmerTest::lmer(num_semillas ~ TRAT + (1|individuo), 
                                data = datos_pseudo, REML=T) # TRAT + (1|individuo)-1
model_numero_sr
coef(summary(model_numero_sr,ddf = c("Satterthwaite")))
anova(model_numero_sr, ddf = c("Satterthwaite"))
difflsmeans(model_numero_sr) # A posteriori analysis
ranova(model_numero_sr,reduce.terms = F) #random effect (individuals)

report_performance(model_numero_sr)
report_table(model_numero_sr)

#FIGURE Seed number
# Statistical descriptive
names(datos)
df_numero<-data_summary(datos_pseudo, varname="num_semillas", 
                          groupnames=c("TRAT"))
df_numero

#"Anova" plot
library(ggplot2)
ggplot(df_numero, aes(x = TRAT, y = num_semillas, fill=TRAT)) +
  theme(legend.position="none", 
        axis.title=element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=20),
        axis.text.y = element_text(color="black",size=20),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"))+
  xlab("")+ylab("Seed number")+labs(fill = "Tratamientos")+
  scale_y_continuous(expand=c(0,0),limits=c(0,275))+
  geom_errorbar(aes(ymin=num_semillas-se, 
                    ymax=num_semillas+se), 
                width=.15,
                position=position_dodge(.9), stat="identity")+
  geom_bar(stat="identity", position=position_dodge(.9))+
  annotate(geom="text", x=4, y=260, label="***",
           color="black",size=20)+
  scale_x_discrete(labels=c("control" = "Unmanipulated \n (Automatic selfing)", 
                            "mix" = "Pollen-plastic mix",
                            "plastic" = "Pure plastic", 
                            "plastic_and_pollen" = "Plastic-then-pollen",
                            "self"="Hand self-pollinated"), limits=c("plastic",
                                                                     "plastic_and_pollen",
                                                                     "mix","self","control"))+
  scale_fill_brewer(palette="Accent")
  #this is for a grayscale palette scale_fill_grey(start = 0.2,
                  #end = 0.8,
                  #na.value = "red",
                  #aesthetics = "fill")


#Number of seeds: plot per individual (Supplementary)
names(df_numero)
individual_numero<- data_summary(datos, varname=c("num_semillas"), 
                                 groupnames=c("individuo","TRAT"))
str(individual_numero)

ggplot(data=individual_numero,aes(x=individuo, y=num_semillas)) + 
  geom_point(data=individual_numero, aes(colour=TRAT),size=6)+
  geom_pointrange(aes(ymin=num_semillas-se, ymax=num_semillas+se, colour=TRAT))+
  geom_errorbar(aes(ymin=num_semillas-se, ymax=num_semillas+se,group=TRAT,
                    width=0.1), width=0.3)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"),
        legend.background = element_rect(),
         axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=25),
        axis.text.y = element_text(color="black",size=25))+
  xlab("Individuals")+ylab("Seed number")+
  labs(color="Treatments")+
  scale_fill_brewer(palette="Accent")
#    scale_colour_grey(start = 0.2,end = 0.8,labels=c("Unmanipulated","Mix","Pure plastic",
 #                                                    "Plastic-then-pollen", "Hand self-pollinated")
  #                                                   )###
  

###############
#SEED MASS#
###############
#lmer function omits NA's 
model_mass_sr<-lmerTest::lmer(masa_num~ TRAT + (1|individuo), 
                                data = datos_pseudo, REML=T) # TRAT + (1|individuo)-1
model_mass_sr
coef(summary(model_mass_sr,ddf = c("Satterthwaite")))
anova(model_mass_sr, ddf = c("Satterthwaite"))
difflsmeans(model_mass_sr) # A posteriori analysis
ranova(model_mass_sr,reduce.terms = F)#random effects (individuals)

report_performance(model_mass_sr)
report_table(model_numero_sr)

#FIGURE: Seed mass
names(datos)
df_masa_por_seed <- data_summary(datos, varname="masa_num", 
                          groupnames=c("TRAT"))
summary(df_masa_por_seed)
df_masa_por_seed

# "Anova" plot
library(ggplot2)
names(df_masa_por_seed)
ggplot(df_masa_por_seed, aes(x = TRAT, y = masa_num, fill=TRAT)) +
  theme(legend.position="none", 
        axis.title=element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=20),
        axis.text.y = element_text(color="black",size=20),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"))+
  xlab("")+ylab("Mass per seed [mg]")+labs(fill = "Tratamientos")+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.05))+
  geom_errorbar(aes(ymin=masa_num-se, 
                    ymax=masa_num+se), 
                width=.15,
                position=position_dodge(.9), stat="identity")+
  geom_bar(stat="identity", position=position_dodge(.9))+
  annotate(geom="text", x=4, y=260, label="***",
           color="black",size=20)+
  scale_x_discrete(labels=c("control" = "Unmanipulated \n (Automatic selfing)", 
                            "mix" = "Pollen-plastic mix",
                            "plastic" = "Pure plastic", 
                            "plastic_and_pollen" = "Plastic-then-pollen",
                            "self"="Hand self-pollinated"), limits=c("plastic",
                                                                     "plastic_and_pollen",
                                                                     "mix","self","control"))+
  scale_fill_brewer(palette="Accent")
#scale_fill_grey(start = 0.2,
 #                 end = 0.8,
  #                na.value = "red",
   #               aesthetics = "fill")
  
  #scale_fill_brewer(palette="Dark2")

#####
##Seed mass per individual
####
names(datos)
individual_masa<- data_summary(datos, varname=c("masa_num"), 
                                 groupnames=c("individuo","TRAT"))
str(individual_masa)

ggplot(data=individual_masa,aes(x=individuo, y=masa_num)) + 
  geom_point(data=individual_masa, aes(colour=TRAT),size=6)+
  geom_pointrange(aes(ymin=masa_num-se, ymax=masa_num+se, colour=TRAT))+
  geom_errorbar(aes(ymin=masa_num-se, ymax=masa_num+se,group=TRAT,
                    width=0.1), width=0.3)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"),
        legend.background = element_rect(),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=25),
        axis.text.y = element_text(color="black",size=25))+
  xlab("Individuals")+ylab("Seed mass [mg]")+
  labs(color="Treatments")+
  scale_colour_grey(start = 0.2,end = 0.8,labels=c("Unmanipulated","Mix","Pure plastic",
                                                   "Plastic-then-pollen", "Hand self-pollinated")
  )###


############################
#####GERMINATION##############
##############################
#library(devtools)
#install_github("OnofriAndreaPG/drcte")
#install_github("OnofriAndreaPG/drcSeedGerm")
library(drcte)

germinacion<-read.table(file="germination.csv", header=T, sep=",", check.names=T, dec=".")
germinacion
head(germinacion)

library(plyr)
describeBy(germinacion, group=c("days","TRAT"))

#########
#non-parametric model
#########
modNP <-drmte(count ~ timeBef + timeAf, fct = NPMLE(),
              data = germinacion, 
              curveid = TRAT)
summary(modNP)
compCDF(modNP)
class(modNP)
par(cex.axis=1.5, mgp=c(4.5,1.5,0),mar=c(5.5,7,1,1)+0.1)
plot(modNP, log = "", legendPos = c(30, 0.6), 
     xlab = "Days", ylab= "Cummulative distribution function",cex.lab=2,
     cex.legend = 1.)


######
#Parametric model (not included in the manuscript)
######
mod1 <- drmte(count ~ timeBef + timeAf, fct = loglogistic(),
              data = germinacion, 
              curveid = TRAT,
              upperl = c(NA, NA, NA, NA, 1, 1, 1, 1, NA, NA, NA, NA))
#Parameters meaning: 𝑏,𝑑and 𝑒.These parameters represent, respectively, 
#the slope at inflection point, 
#the higher asymptote (i.e. the maximum proportion of germinated seeds) and 
#the median germination time
# https://www.r-bloggers.com/2021/12/analysing-seed-germination-and-emergence-data-with-r-a-tutorial-part-5/
# el parámetro upperl permite establecer la proporción máxima de germinación (para´metro d)

plot(mod1, log = "", legendPos = c(25, 0.6), xlab = "Days",
     ylab = "Cumulative probability of germination")
compCDF(mod1)
compCDF(mod1, type = "permutation", units = germinacion$dish)


####
###Total germination
#####GLM analysis
prop<-germinacion$germ/10
germinacion<-cbind(germinacion,prop)
names(germinacion)
str(germinacion)
germinacion$TRAT<-as.factor(germinacion$TRAT)
germinacion$INDIVIDUO<-as.factor(germinacion$INDIVIDUO)

final<-germinacion[germinacion$days=="30", ]
final
str(final)

model_germ<-lmerTest::lmer(germ ~ TRAT + (1|INDIVIDUO), 
                                data = final, REML=T) # TRAT + (1|individuo)-1
model_germ
coef(summary(model_germ,ddf = c("Satterthwaite")))
anova(model_germ, ddf = c("Satterthwaite"))
difflsmeans(model_germ) # A posteriori analysis
ranova(model_germ,reduce.terms = F) #random effect (individuals)

report_performance(model_germ)
report_table(model_germ)


#Germination curves
head(germinacion)
count<-data_summary(germinacion, varname="germ", 
                                 groupnames=c("TRAT","days"))
count
cumm<-data_summary(germinacion, varname="count", 
                   groupnames=c("TRAT","days"))
head(cumm)
prop<-data_summary(germinacion, varname="prop", 
               groupnames=c("TRAT","days"))
prop

#Plot curves based on number (not published)
ggplot(count, aes(x=days, y=germ, group=TRAT, color=TRAT)) + 
  geom_errorbar(aes(ymin=germ-se, ymax=germ+se), width=.1,
                position=position_dodge(0.1)) +
  geom_line() + geom_point()+
  xlab("Days")+
  ylab("Cummulative number of germinated seeds")+
  scale_y_continuous(expand=c(0,0), limits=c(0,10))+
  scale_x_continuous(expand=c(0,0),limits=c(c(10,32)))+
  scale_color_brewer(palette="Dark2")+theme_minimal()

#Plot curves based on proportion (not published)
ggplot(prop, aes(x=days, y=prop, group=TRAT, color=TRAT)) + 
  geom_errorbar(aes(ymin=prop-se, ymax=prop+se), width=.1,
                position=position_dodge(0.1)) +
  geom_line() + geom_point()+
  xlab("Days")+
  ylab("Commulative seed germination [proportion]")+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  scale_x_continuous(expand=c(0,0),limits=c(c(10,32)))+
  scale_color_brewer(palette="Dark2")+theme_minimal()

###"Anova" plot for germination
final_plot<-data_summary(final,varname="prop",groupnames="TRAT")
summary(final_plot$TRAT)

ggplot(final_plot, aes(x = TRAT, y = prop, fill=TRAT)) +
  theme(legend.position="none", 
        axis.title=element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=20),
        axis.text.y = element_text(color="black",size=20),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"))+
  xlab("")+ylab("Proportion of germinated seeds")+labs(fill = "Tratamientos")+
  scale_y_continuous(expand=c(0,0),limits=c(0,1))+
  geom_errorbar(aes(ymin=prop-se, 
                    ymax=prop+se), 
                width=.15,
                position=position_dodge(.9), stat="identity")+
  geom_bar(stat="identity", position=position_dodge(.9))+
  annotate(geom="text", x=2, y=0.9, label="*",
           color="black",size=15)+
  annotate(geom="text", x=4, y=0.8, label="*",
           color="black",size=15)+
  scale_x_discrete(labels=c("Control" = "Unmanipulated \n (Automatic selfing)", 
                            "Mixed" = "Pollen-plastic mix",
                            "PtP" = "Plastic-then-pollen",
                            "Self"="Hand self-pollinated"), limits=c("PtP",
                                                                     "Mixed","Self","Control"))+
  scale_fill_manual(values=c("#7FC97F", "#BEAED4", "#FFFF99", "#386CB0"),aesthetics = "fill")

#RColorBrewer::brewer.pal(5,"Accent") #to pick colors of used baras
# barplot(c(2,5), col=c("#386CB0")) #to print colors
#verde:"#7FC97F" violeta:"#BEAED4" naranjo:"#FDC086" amarillo:"#FFFF99" azul:"#386CB0"

###############
#ULTRASTRUCTURE#
###############
ust<-read.table(file="ultrastructure.csv", header=T, sep=",", check.names=T, dec=".")
ust$TRAT<-as.factor(ust$TRAT)
ust$Individuo<-as.factor(ust$Individuo)
head(ust)


#POLLEN TUBES
#lmer function omits NA's
names(ust)
model_tubes<-lmerTest::lmer(pollen_tubes~ TRAT + (1|Individuo), 
                             data = ust, REML=T) # TRAT + (1|individuo)-1
model_tubes
coef(summary(model_tubes,ddf = c("Satterthwaite")))
anova(model_tubes, ddf = c("Satterthwaite"))
difflsmeans(model_tubes) # A posteriori analysis
ranova(model_tubes,reduce.terms = F)#random effects (individuals)

report_performance(model_tubes)
report_table(model_tubes)

#FIGURE: Pollen_grains
head(ust)
tubes<-data_summary(ust, varname="pollen_tubes", 
                     groupnames=c("TRAT"))
summary(tubes)
names(tubes)
tubes

# "Anova" plot
library(ggplot2)
ggplot(tubes, aes(x = TRAT, y = pollen_tubes, fill=TRAT)) +
  theme(legend.position="none", 
        axis.title=element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=20),
        axis.text.y = element_text(color="black",size=20),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"),
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  xlab("")+ylab("Number of pollen tubes")+
  scale_y_continuous(expand=c(0,0),limits=c(0,320))+
  geom_errorbar(aes(ymin=pollen_tubes-se, 
                    ymax=pollen_tubes+se), 
                width=.15,
                position=position_dodge(.9), stat="identity")+
  geom_bar(stat="identity", position=position_dodge(.9))+
  scale_x_discrete(labels=c("C" = "Unmanipulated \n (Automatic selfing)", 
                            "M" = "Pollen-plastic mix",
                            "P" = "Pure plastic", 
                            "Ex" = "Plastic-then-pollen",
                            "A"="Hand self-pollinated"), limits=c("P",
                                                                  "Ex",
                                                                  "M","A","C"))+
  scale_fill_manual(values=c("#386CB0", "#BEAED4", "#FFFF99", "#BEAED4", "#386CB0"),aesthetics = "fill")

#verde:"#7FC97F" violeta:"#BEAED4" naranjo:"#FDC086" amarillo:"#FFFF99" azul:"#386CB0"
tubes2<-tubes[c(1,3,4),]
tubes2

# This plot include only three treatments
ggplot(tubes2, aes(x = TRAT, y = pollen_tubes, fill=TRAT)) +
  theme(legend.position="none", 
        axis.title=element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color="black",size=20),
        axis.text.y = element_text(color="black",size=20),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "gray"),
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black", 
                                 size = 0.4, linetype = "solid"),
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  xlab("")+ylab("Number of pollen tubes")+
  scale_y_continuous(expand=c(0,0),limits=c(0,320))+
  geom_errorbar(aes(ymin=pollen_tubes-se, 
                    ymax=pollen_tubes+se), 
                width=.15,
                position=position_dodge(.9), stat="identity")+
  geom_bar(stat="identity", position=position_dodge(.9))+
  scale_x_discrete(labels=c("M" = "Pollen-plastic mix",
                            "Ex" = "Plastic-then-pollen",
                            "A"="Hand self-pollinated"), limits=c("M","Ex","A"))+
  scale_fill_manual(values=c("#386CB0", "#BEAED4","#FFFF99" ),aesthetics = "fill")
