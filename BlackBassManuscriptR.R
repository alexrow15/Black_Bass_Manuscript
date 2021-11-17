#Tournament Paper R Analysis w/ Alex

library(ggplot2)
library(dplyr)
library(emmeans)
library(car)
install.packages("emmeans")
#Data Import
setwd("C:/Users/Patwo/OneDrive/Queens School Work/Fisheries Conservation Lab/2021/Tournament Mortality - Life History Papers")

MyData <- read.csv(file.choose()) #"Alex Processed Fish - Aug13 2021.csv"
summary(MyData)
MyData <- filter(MyData, Species!= "Northern Pike")
ModelData <- MyData

ModelData <- ModelData %>% 
  filter(Age >= 3, Age <= 16)

names(MyData)

FFCLTheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 12, colour = "black"),
  axis.text.y = element_text(size = 12, colour = "black"),
  axis.title = element_text(size = 16, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1.5),
  legend.key = element_rect(colour = "transparent", fill = "transparent"))

Theme2 <- theme(
  axis.ticks = element_line(size = 2, colour = "black"),
  axis.text.x = element_text(size = 20, colour = "black"),
  axis.text.y = element_text(size = 20, colour = "black"),
  axis.title = element_text(size = 25, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1.5),
  legend.key = element_rect(colour = "transparent", fill = "transparent"))





OtolithData <- MyData %>% 
  select(Age, Fork.Length..mm., Species, Weight..kg.) %>% 
  na.omit(OtolithData)

ggplot(OtolithData)+
  geom_point(aes(x = Age, y = Fork.Length..mm.))

agedata <- OtolithData %>% group_by(Species, Age) %>% 
  summarise(n = length(Fork.Length..mm.))




OtolithData <- OtolithData %>% 
  group_by(Species, Age) %>% 
  summarise(mean.FL = mean(Fork.Length..mm.), nl=length(Fork.Length..mm.), sdl = sd(Fork.Length..mm.), sel = sdl/sqrt(nl), mean.W = mean(Weight..kg.), nw = length(Weight..kg.), sdw = sd(Weight..kg.), sew = sdw/sqrt(nw)) %>% 
  filter(nl >= 3) %>% 
  mutate(UCLL = mean.FL + qt(.975, nl - 1)*sel, LCLL = mean.FL - qt(0.975, nl - 1)*sel, UCLW = mean.W + qt(.975, nw - 1)*sew, LCLW = mean.W - qt(0.975, nw - 1)*sew) %>% 
  filter(Age <= 16 | Age >5)

position = position_dodge(width = 0.5)

png("Length distribution of LMB and SMB", units = "in", width = 10, height = 5, res = 300)
ggplot(OtolithData)+
  geom_point(aes(x = Age, y = mean.FL, color = Species), size = 2.5, position = position)+
  geom_errorbar(aes(x = Age, ymin = LCLL, ymax = UCLL, color = Species),
                width = 0.25, lwd = 0.25, position = position)+
  scale_x_continuous(name = "Otolith age", limits = c(5.75,15.25), breaks = seq(6,15,1))+
  scale_y_continuous(name = "Fork length (mm)")+
  scale_color_manual(values = c("grey", "black"), labels = c("LMB", "SMB"))+
  Theme2+
  theme(legend.position = c(0.9,0.2), legend.title = element_blank(), legend.text = element_text(size = 18))
dev.off()

png("Weight distribution of LMB and SMB", units = "in", width = 10, height = 5, res = 300)
ggplot(OtolithData)+
  geom_point(aes(x = Age, y = mean.W, color = Species), size = 3, position = position)+
  geom_errorbar(aes(x = Age, ymin = LCLW, ymax = UCLW, color = Species),
                width = 0.25, lwd = 0.25, position = position)+
  scale_x_continuous(name = "Otolith age", limits = c(5.75,15.25), breaks = seq(6,15,1))+
  scale_y_continuous(name = "Weight (kg)", limits = c(0,3.1), breaks = seq(0,3,0.5))+
  scale_color_manual(values = c("#999999", "black"), labels = c("LMB", "SMB"))+
  Theme2+
  theme(legend.position = c(0.8,0.2), legend.title = element_blank(), legend.text = element_text(size = 18))
dev.off()

ModelData$Age <- as.factor(ModelData$Age)
ModelData$Species <- as.factor(ModelData$Species)

model <- lm(Fork.Length..mm. ~ Age + Species, data = ModelData)

AOV <- Anova(model)

AOV <- aov(Fork.Length..mm. ~ Age*Species, data = ModelData)


emm1 = emmeans(model, specs = pairwise ~ Age|Species)
emm1$emmeans

emm1$contrasts

emm1_contrasts <- emm1$contrasts %>% 
  summary() %>% 
  as.data.frame()

emm1_contrasts

contrastlist <- data.frame(Contrasts = c("3 Largemouth Bass - 3 Smallmouth Bass",
                                         "4 Largemouth Bass - 4 Smallmouth Bass",
                                         "5 Largemouth Bass - 5 Smallmouth Bass",
                                         "6 Largemouth Bass - 6 Smallmouth Bass",
                                         "7 Largemouth Bass - 7 Smallmouth Bass",
                                         "8 Largemouth Bass - 8 Smallmouth Bass",
                                         "9 Largemouth Bass - 9 Smallmouth Bass",
                                         "10 Largemouth Bass - 10 Smallmouth Bass",
                                         "11 Largemouth Bass - 11 Smallmouth Bass",
                                         "12 Largemouth Bass - 12 Smallmouth Bass",
                                         "13 Largemouth Bass - 13 Smallmouth Bass",
                                         "14 Largemouth Bass - 14 Smallmouth Bass",
                                         "15 Largemouth Bass - 15 Smallmouth Bass",
                                         "16 Largemouth Bass - 16 Smallmouth Bass"))

emm1_contrasts <- emm1_contrasts %>% 
  select(contrast == contrastlist$Contrasts)

em.M <- emmeans(model, c("Species", "Age"))

em.M
options(max.print=100000)

emmeans(model, specs = pairwise ~ Species|Age, adjust = "tukey")

pairs(em.M, adjust = "tukey")

TukeyHSD(AOV)









Age <- seq(3,16,1)
i <- 1
TT.Res <- data.frame(Age, "Result", "Significance")
names(TT.Res) <- c("Age", "Result", "Significance")

for(i in 1:length(Age)){
  
  x <- Age[i]
  
  TTestData <- MyData %>% 
    filter(Age == x) %>% 
    select(Age, Species, Fork.Length..mm.)
  
  d <- ggplot(TTestData)+
    geom_histogram(aes(x = Fork.Length..mm., y = ..density..))+
    stat_function(fun = dnorm, args = list(mean = mean(TTestData$Fork.Length..mm.), sd = sd(TTestData$Fork.Length..mm.)), color = "red", lwd = 2)
  
  print(d)
  
  LMBData <- TTestData %>% 
    filter(Species == "Largemouth Bass") %>% 
    pull(Fork.Length..mm.)
  
  SMBData <- TTestData %>% 
    filter(Species == "Smallmouth Bass") %>% 
    pull(Fork.Length..mm.)
  
  
  
  T.Test.DF <- t.test(LMBData, SMBData)
  
  P.Value <- T.Test.DF$p.value
  
  TT.Res$Result[i] <- P.Value
  
  
  if(TT.Res$Result[i] <= 0.05){
    
    TT.Res$Significance[i] <- c("Significant")
    
  } else{
    
    TT.Res$Significance[i] <- c("Insignificant")
    
  }
  
  i <- i + 1
  
}




#YCS

YCS.Data <- MyData %>% 
  filter(Species == "Smallmouth Bass") %>% 
  select(Age, Weight..kg., Year.Caught, Fork.Length..mm.) %>% 
  na.omit() %>% 
  filter(Age > 6)


resid.strength <- data.frame(Resid.Stren = c(), yearclass = c(), year.caught = c())

for(year in min(YCS.Data$Year.Caught):max(YCS.Data$Year.Caught)){
  
  catch <- YCS.Data %>% 
    filter(Year.Caught == year) %>% 
    select(Age) %>% 
    group_by(Age) %>% 
    summarise(Number = n()) %>% 
    mutate(year.class = year-Age, weight = row_number()+6, year.caught = year)
  
  weighted_fit <- lm(log(Number)~year.class, weights = weight, data = catch)
  Resid.Stren <- ((log(catch$Number)-weighted_fit$fitted.values))
  yearclass <- catch$year.class
  year.caught <- catch$year.caught
  resid.strength.loop <- data.frame(Resid.Stren, yearclass, year.caught)
  resid.strength <- bind_rows(resid.strength, resid.strength.loop)
}

resid.strength$yearclass <- as.factor(resid.strength$yearclass)
resid.strength$year.caught <- as.factor(resid.strength$year.caught)

ggplot(resid.strength)+
  geom_point(aes(x = yearclass, y = Resid.Stren, color = year.caught))

install.packages("EnvStats")
library(EnvStats)
install.packages("agricolae")
library(agricolae)
hsd <- HSD.test(aov(Resid.Stren~yearclass, data = resid.strength), "yearclass", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)), ]

SigTestYCS <- resid.strength %>% 
  group_by(yearclass) %>% 
  summarise(maxResidStr = max(Resid.Stren))

tiff("YCS Figure", units="in", width=10, height=5, res=300)

ggplot(resid.strength)+
  stat_boxplot(aes(x = yearclass, y = Resid.Stren), geom = 'errorbar') +
  geom_boxplot(aes(x = yearclass, y = Resid.Stren)) +
  scale_x_discrete(name = "Year-class") +
  scale_y_continuous(name = "Year-class Strength") +
  Theme2 +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5), 
        axis.ticks.length=unit(.25, "cm")) +
  stat_n_text(aes(x = yearclass, y = Resid.Stren)) +
  geom_text(data = SigTestYCS, aes(x = yearclass, y = 0.2 + maxResidStr, label = sig.letters$groups), vjust = 0)
dev.off()
##### Testing YCS residual 

YCS.test <- aov(Resid.Stren~yearclass, data = resid.strength)
summary(YCS.test)

TukeyHSD(YCS.test)
plot(YCS.test)


##################################################################################################################################################

###Comparison of L from processed fish to dead fish \
library(dplyr)
library(ggplot2)


data <- read.csv(file.choose()) #processedANDfloytagfish
summary(data)

data <- select(data, Species, Status, Fork.Length..mm.)

SMBdat <- filter(data, Species == "Smallmouth Bass" & Fork.Length..mm. < 600)
LMBdat <- filter(data, Species == "Largemouth Bass" & Fork.Length..mm. < 600)



ggplot(SMBdat, aes(x = Fork.Length..mm., colour = Status)) +
  geom_histogram()
ggplot(LMBdat, aes(x = Fork.Length..mm., colour = Status)) +
  geom_histogram()

test <- t.test(Fork.Length..mm.~Status, data = SMBdat)
test

test <- t.test(Fork.Length..mm.~Status, data = LMBdat)
test


################################################################################################################################################

dat <- read.csv(file.choose())

str(dat)
names(dat)
library(dplyr)
library(car)
dat <- filter(dat, Location!="Cornwall")
dat <- dat[1:47,]
dat %>% 
  group_by(LakeO) %>% 
  summarise(num = n(),
            sumAng = sum(Anglers),
            sumFish = sum(FishCaught))
sum(dat$FishCaught, na.rm = TRUE)

#man efficiency 
meanEffic <- dat %>% 
  summarise(meanEFF <- mean(Efficeicy, na.rm = TRUE))
meanEffic

#remove deadfish = Unknown \
#create linear model predicting dead fish with all the variables --- Num Anglers, limits, weigh-in....

#anglers vs limits graph 
library(ggplot2)

dat$DeadFish <- as.numeric(dat$DeadFish)

ggplot(dat, aes(x= WeighIn.Type, y = DeadFish)) +
  geom_boxplot()

ggplot(dat, aes(x= LakeO, y = DeadFish)) +
  geom_boxplot()


#Explore data 
head(dat)
tail(dat)
str(dat) #change number of fish in livewell to categorical 
dat$Number.Fish.In.Livewell <- as.factor(dat$Number.Fish.In.Livewell)
str(dat)
summary(dat)

#check histogram for normality 
y <- dat$Anglers
ggplot(dat, aes(x= y)) +
  geom_histogram()

hist(y, breaks = 10)
par(mfrow=c(1,1))
shapiro.test(log(y))

#can i make a linmod to see if deadfish can be predicted by any other factors
test <- lm(data = dat, DeadFish~Anglers)
test
summary(test)
anova(test)
cor.test(x=dat$Anglers, y=dat$DeadFish, na.rm = TRUE)


#create a column to use for graphs instead of tourny circuit
dat <- dat %>%
  arrange(FishCaught) 
dat$Circuit <- c(1:47)

ggplot(dat, aes(x=Circuit, y = FishCaught, col = Dead.Fish.Penalty)) +
  geom_point()

ggplot(dat, aes(x=Circuit, y = DeadFish, col = WeighIn.Type)) +
  geom_point()

library(tidyverse)
library(dplyr)


#####################Work With 

# fish caught vs each tournament to highlight weigh in type

themeElite <- theme(
  axis.ticks = element_line(size = 2, colour = "black"),
  axis.text.x = element_text(size = 18, colour = "black"),
  axis.text.y = element_text(size = 18, colour = "black"),
  axis.title = element_text(size = 30, colour = "black", vjust = -0.5),
  panel.background = element_blank(), 
  axis.line = element_line(colour = 'black', size = 1.5))
ColourBlindPalette <- c("", "#000000", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dat$WeighIn.Type <- factor(dat$WeighIn.Type, levels = c("Air", "Water", "Onboard", "Unknown"))

ggplot(dat, aes(x=Circuit, y = FishCaught)) +
  geom_point(aes(shape = WeighIn.Type, col = WeighIn.Type), size = 2)+
  scale_y_continuous(breaks = seq(0,1400,250)) +
  scale_x_continuous(name = "Tournament") +
  themeElite +
  scale_colour_discrete(name  ="Weigh-In Type",
                        breaks=c("Air", "Onboard", "U", "Water"),
                        labels=c("Air", "Onboard", "Unknown", "Water")) +
  scale_shape_discrete(name  ="Weigh-In Type",
                       breaks=c("Air", "Onboard", "U", "Water"),
                       labels=c("Air", "Onboard", "Unknown", "Water")) +
  theme(legend.background = element_blank()) 

###Test with facet wrap
#ggplot(dat, aes(x=Circuit, y = FishCaught)) +
geom_point(aes(shape = WeighIn.Type), size = 2)+
  scale_y_continuous(breaks = seq(0,1400,250)) +
  scale_x_continuous(name = "Tournament") +
  themeElite +
  scale_colour_discrete(name  ="Weigh-In Type",
                        breaks=c("Air", "Onboard", "U", "Water"),
                        labels=c("Air", "Onboard", "Unknown", "Water")) +
  scale_shape_discrete(name  ="Weigh-In Type",
                       breaks=c("Air", "Onboard", "U", "Water"),
                       labels=c("Air", "Onboard", "Unknown", "Water")) +
  theme(legend.background = element_blank()) +
  facet_wrap(~WeighIn.Type)


#####Fish Caugth vs Tournament 
tiff("Fish caught per tourny", units="in", width=9, height=5, res=300)

ggplot(dat, aes(x=Circuit, y = FishCaught)) +
  geom_point(aes(shape = WeighIn.Type, col = WeighIn.Type), size = 2)+
  scale_shape_manual(values = c(15, 16, 17,4)) +
  scale_colour_manual(values = c("darkgrey", "black", "#E69F00", "#0072B2")) +
  scale_y_continuous(breaks = seq(0,1400,250), name = "Fish Caught") +
  scale_x_continuous(name = "Tournament") +
  themeElite +
  theme(legend.background = element_blank()) 
dev.off()

#Number of Anglers vs number of limits Caught 
tiff("Limits vs number of anglers2", units="in", width=9, height=5, res=300)


ggplot(dat, aes(x = Anglers, y = Limits)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,300,100), limits = c(0,300), expand = c(0,0)) +
  geom_abline(slope = 1, intercept = 0)+
  themeElite 
dev.off()
#number of potential fish vs number of fish actually caught 
tiff("Potential Fish #fish caught 2", units="in", width=9, height=5, res=300)

ggplot(dat, aes(x = PotentialFish, y = FishCaught)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0) +
  scale_y_continuous(breaks = seq(0,2000,500), limits = c(0,2000), name = "Number of Fish Caught") +
  scale_x_continuous(breaks = seq(0,2000,500), limits = c(0,2000), name = "Potential Fish Caught") +
  themeElite

dev.off()

#number of fish caught  and coloured with # of fish in Livewell 
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

tiff("Fish caught by tourny and # fish in livewell", units="in", width=9, height=5, res=300)

ggplot(dat, aes(x=Circuit, y = FishCaught, col = factor(Number.Fish.In.Livewell))) +
  geom_point() +
  scale_colour_manual(values = cbp2, name = "Number of Fish \n   in Livewell") +
  themeElite +
  scale_y_continuous(breaks = seq(0,1500,500), limits = c(0,1500), name = "Fish Caught") +
  scale_x_continuous(name = "Tournament")

dev.off()




#################### SHOW BUT DONT SEND###########


#need to test this in the linear model -- just to see if the relationship is linear 
ggplot(dat, aes(x = Anglers, y = DeadFish)) +
  geom_point(size = 2.5) +
  scale_y_continuous(breaks = seq(0,15,5), limits = c(0,15)) +
  scale_x_continuous(breaks = seq(0,150,50), limits = c(0,150)) +
  themeElite +
  geom_smooth(method = "lm", se = FALSE, lwd = 1, fullrange = TRUE, formula = y ~ x)

#number of fish in the livewell 
####this is the same as the boxplot but has mean with error bars 

ggplot(dat, aes(x= as.factor(Number.Fish.In.Livewell), y = DeadFish)) +
  stat_summary(fun.y=mean, geom="point", size = 4) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar", width = 0.5, size = 1.3) +
  themeElite +
  scale_x_discrete(name = "Number of fish in Livewell") +
  scale_y_continuous(name = "Dead Fish")

#boxplot of number of fish in the livewell and relationship to dead fish
ggplot(dat, aes(x= as.factor(Number.Fish.In.Livewell), y = DeadFish)) +
  geom_boxplot() +
  themeElite +
  scale_x_discrete(name = "Number of fish in Livewell") +
  scale_y_continuous(name = "Dead Fish")



#############
TIOdat <- read.csv(file.choose())
library(ggplot2)
library(dplyr)

TIOdat <- TIOdat %>% 
  mutate(Effic =(DeadFish/Anglers))




#TIO Number of Dead Fish

ggplot(TIOdat, aes(x=Year, y = DeadFish)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2018.5, linetype="dashed", 
             size=1.5) +
  geom_line(size =1.5) +
  scale_x_continuous(breaks = seq(2014,2021,1)) +
  scale_y_continuous(name = "Dead Fish", expand = c(0,0), breaks = seq(0,150,30), limits = c(0,150)) +
  themeElite +
  annotate(geom="text", x=2020, y=15, label="*",
           color="black", size = 10)


#TIo dead fish per angler
TIOdat[6,5] <- 0


ggplot(TIOdat, aes(x=Year, y = Effic)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2018.5, linetype="dashed", 
             size=1.5) +
  geom_line(size =1.5) +
  scale_x_continuous(breaks = seq(2014,2021,1)) +
  scale_y_continuous(name = "Dead Fish", expand = c(0,0), breaks = seq(0,2,0.5), limits = c(0,2)) +
  themeElite +
  annotate(geom="text", x=2020, y=0.1, label="*",
           color="black", size = 10)
######add switch to 4 fish angler days ------ this is oer 3 days

###############################################################################################################################################


####Updating figures for Paper 
#Theme
Theme2 <- theme(
  axis.ticks = element_line(size = 2, colour = "black"),
  axis.text.x = element_text(size = 30, colour = "black"),
  axis.text.y = element_text(size = 30, colour = "black"),
  axis.title = element_text(size = 40, colour = "black", vjust = -0.5),
  panel.background = element_blank(), 
  axis.line = element_line(colour = 'black', size = 1.5))
#Packages 
library(ggplot2)
library(dplyr)
library(cowplot)

#import Dataset 
MyData <- read.csv("~/Desktop/School/school - Previous classes /537thesis/Data Analysis/Excel and csv files/Processed Fish Data - AlexUpdated.csv")
summary(MyData)
MyData <- filter(MyData, Species!= "Northern Pike")

summ <- MyData %>% 
  group_by(Species) %>% 
  summarise(meanFL = mean(Fork.Length..mm., na.rm = TRUE), 
            meanWeight = mean(Weight..kg., na.rm = TRUE))



#distribution of smaples for LMB and SMB --- Count
tiff("Age Distribution of Both species - Count", units="in", width=10, height=5, res=300)
ggplot(filter(MyData, Age!= "NA"), aes(x = factor(Age), y = ..count.., fill = Species)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(name = "Otolith Age") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual(values = c('#999999',"#333333"), label = c("LMB", "SMB")) +
  Theme2 +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 25, colour = "black", vjust = -0.5, face="bold"))
dev.off()


#Distributiion of samples for LMB and SMB but wigh proportion 
tiff("Age Distribution of Both species - proportion", units="in", width=10, height=5, res=300)
Fig3Comp <- ggplot(filter(MyData, Age!= "NA"), aes(x = factor(Age), y = ..count../(sum(..count..)), fill = Species)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(name = "Otolith Age") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c('#999999',"#333333"), label = c("LMB (n=203)", "SMB (n=1088)")) +
  theme(legend.position = c(0.85,0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 18, colour = "black", vjust = -0.5), 
        axis.ticks.length=unit(0.25, "cm"))
dev.off()

#weight vs proportion of smb 
tiff("length Distribution of Smallmouth - proportion", units="in", width=10, height=5, res=300)

ggplot(filter(MyData, Species=="Smallmouth Bass"), aes(x = Fork.Length..mm., y = ..count../sum(..count..))) +
  geom_histogram(fill = "black", binwidth = 5) +
  scale_x_continuous(name = "\nFork Length (mm)", limits = c(260,550), breaks = seq(280,520,40)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0)) +
  Theme2 +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"), 
        axis.ticks.length=unit(0.25, "cm")) +
  theme(axis.text.x = element_text(angle = 315)) 
dev.off()

#weight vs proportion of LMB 
tiff("FL Distribution of Largemouth  - proportion", units="in", width=10, height=5, res=300)

ggplot(filter(MyData, Species == "Largemouth Bass"), aes(x = Fork.Length..mm., y = ..count../sum(..count..))) +
  geom_histogram(binwidth = 4, fill = "black") +
  scale_x_continuous(name = "\nFork Length (mm)", limits = c(260,520), breaks = seq(280,520,40)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0), limit = c(0,0.075)) +
  scale_fill_manual(values = c('#999999',"#333333"), label = c("LMB", "SMB")) +
  Theme2 +
  theme(legend.position = c(0.25,0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 315)) 
dev.off()

#combined LMB and SMB FL distribution 

tiff("FL Distribution of both species  - proportion", units="in", width=10, height=5, res=300)

Fig2Comp <- ggplot(filter(MyData), aes(x = Fork.Length..mm., y = ..count../sum(..count..), fill = Species)) +
  geom_histogram(binwidth = 6, position = "dodge") +
  scale_x_continuous(name = "Fork Length (mm)", limits = c(260,550), breaks = seq(300,550,100)) +
  scale_y_continuous(name = "", expand = c(0,0), limit = c(0,0.1), breaks = seq(0,0.09,0.03)) +
  scale_fill_manual(values = c('#999999',"#333333"), label = c("LMB", "SMB")) +
  Theme2 +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 18, colour = "black", vjust = -0.5), 
        axis.ticks.length=unit(0.25, "cm")) 
dev.off()


tiff("FL Between SMB and LMB", units="in", width=10, height=5, res=300)
plot_grid(fig1, fig2, labels = "AUTO", label_x = 0.05, label_y = 0, align = "v", label_size = 14, vjust = -6, hjust = -1, nrow = 2)
dev.off()


#lengh vs age scatterplot BOTH species 


tiff("Age vs FL in both species Scatterplot", units="in", width=10, height=5, res=300)

ggplot(MyData, aes(x = Age, y = Fork.Length..mm., colour = Species)) +
  stat_summary(fun.y=mean, geom="point", position = position_dodge(width = 0.5), size =4) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar", width = 0.4, position = position_dodge(width = 0.5)) +
  scale_y_continuous(name = "Fork Length (mm)") +
  scale_x_continuous(name = "Otolith Age", limits = c(0,22), breaks = seq(0,20,2)) +
  scale_colour_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"),
        legend.text=element_text(size=18),
        legend.title = element_blank(), 
        legend.position = c(0.9,0.2), 
        legend.key = element_rect(fill = "white", color = NA),) 
dev.off()

#weight vs age between LM and SMB
tiff("Age vs weight in both species Scatterplot", units="in", width=10, height=5, res=300)

MyDataPlotAge <- filter(MyData, Age>=3 & Age<=16)
ggplot(MyDataPlotAge, aes(x = Age, y = Weight..kg., colour = Species)) +
  stat_summary(fun.y=mean, geom="point", position = position_dodge(width = 0.5), size =4) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar", width = 0.4, position = position_dodge(width = 0.5)) +
  scale_y_continuous(name = "Weight (Kg)", limits = c(0,3), breaks = seq(0,3,0.5)) +
  scale_x_continuous(name = "Otolith Age", limits = c(2,17), breaks = seq(3,16,1)) +
  scale_colour_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"),
        legend.text=element_text(size=18),
        legend.title = element_blank(), 
        legend.position = c(0.9,0.2), 
        legend.key = element_rect(fill = "white", color = NA),) 
dev.off()


##### nonlinmod for weihght to lengh  

nonlinmod <- nls(Weight..kg.~A*Fork.Length..mm.^(B), data = (filter(MyData, Species=="Smallmouth Bass")), start = list(A=0.000000005, B = 3))
summary(nonlinmod)


tiff("Fork L vs Weight for SMB", units="in", width=10, height=5, res=300)

ggplot(filter(MyData, Species=="Smallmouth Bass"), aes(x = Fork.Length..mm., y = Weight..kg.)) +
  geom_point() +
  scale_x_continuous(name = "Fork Length (mm)", limits = c(250,550), breaks = seq(250,600,50)) +
  scale_y_continuous(name = "Weight (Kg)", limits = c(0,3.5), breaks = seq(0,3.5,0.5)) +
  stat_smooth(method = "nls",
              formula = y ~ a*x^(b),
              method.args = list(start = list(a=0.000000005, b = 3)),
              se = FALSE, colour = "grey", 
              fullrange = TRUE, 
              size = 2) +
  annotate(geom="text", x=315, y=3, label = expression("y = 4.08E-08x"^{"2.90"} ), parse = TRUE, size = 10) + #fomrula found from the nonlinmod above
  Theme2 +
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"),
        axis.ticks.length=unit(0.25, "cm"),
        legend.text=element_text(size=18),
        legend.title = element_blank(), 
        legend.position = c(0.9,0.2), 
        legend.key = element_rect(fill = "white", color = NA)) 

dev.off()

#weight vs FOrk length in Largies 

nonlinmod <- nls(Weight..kg.~A*Fork.Length..mm.^(B), data = (filter(MyData, Species=="Largemouth Bass")), start = list(A=0.000000005, B = 3))
summary(nonlinmod)


tiff("Fork L vs Weight for LMB", units="in", width=10, height=5, res=300)

ggplot(filter(MyData, Species=="Largemouth Bass"), aes(x = Fork.Length..mm., y = Weight..kg.)) +
  geom_point() +
  scale_x_continuous(name = "Fork Length (mm)", limits = c(250,550), breaks = seq(250,600,50)) +
  scale_y_continuous(name = "Weight (Kg)", limits = c(0,3.5), breaks = seq(0,3.5,0.5)) +
  stat_smooth(method = "nls",
              formula = y ~ a*x^(b),
              method.args = list(start = list(a=0.000000005, b = 3)),
              se = FALSE, colour = "grey", 
              fullrange = TRUE, 
              size = 2) +
  annotate(geom="text", x=315, y=3, label = expression("y = 1.56E-08x"^{"3.03"} ), parse = TRUE, size = 10) + #fomrula found from the nonlinmod above
  Theme2 +
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"),
        axis.ticks.length=unit(0.25, "cm"),
        legend.text=element_text(size=18),
        legend.title = element_blank(), 
        legend.position = c(0.9,0.2), 
        legend.key = element_rect(fill = "white", color = NA)) 

dev.off()

#year class vs count in said year class ---- ALL FISH 
tiff("Year class All Fish 2017", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2017), aes(x=as.factor(Year.Class), y = ..count..)) +
  geom_bar(position = "dodge", fill = "black") +
  Theme2  +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(name = "Year Class") +
  scale_y_continuous(name = "Frequency", expand = c(0,0))

dev.off()

#2018 YEar class
tiff("Year class All Fish 2018", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2018), aes(x=as.factor(Year.Class), y = ..count..)) +
  geom_bar(position = "dodge", fill = "black") +
  Theme2  +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(name = "Year Class", breaks = seq(1998,2020,2)) +
  scale_y_continuous(name = "Frequency", expand = c(0,0))

dev.off()

#2019 
tiff("Year class All Fish 2019", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2019, Year.Class!= "NA"), aes(x=(as.factor(Year.Class)), y = ..count.., na.rm = TRUE)) +
  geom_bar(position = "dodge", fill = "black") +
  Theme2  +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(name = "Year Class", breaks = seq(1998,2020,2)) +
  scale_y_continuous(name = "Frequency", expand = c(0,0))

dev.off()

##all samples combined (2012-2020)
tiff("Year class All Fish ", units="in", width=5, height=5, res=300)

ggplot(filter(MyData,Year.Class!= "NA"), aes(x=(as.factor(Year.Class)), y = ..count.., na.rm = TRUE)) +
  geom_bar(position = "dodge", fill = "black") +
  Theme2  +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(name = "Year Class", breaks = seq(1998,2020,2)) +
  scale_y_continuous(name = "Frequency", expand = c(0,0))

dev.off()

######Filtered and showing LMB and SMB 2013

tiff("YC both Species 2013", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2013, Year.Class!="NA"), aes(x=as.factor(Year.Class), y = ..count.., fill = Species)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  scale_x_discrete(name = "Year Class") +
  scale_y_continuous(name = "Frequency", expand = c(0,0)) +
  Theme2  +
  theme(legend.position = c(0.85,0.7)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
dev.off()

#2014 
tiff("YC both Species 2014", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2014, Year.Class!="NA"), aes(x=as.factor(Year.Class), y = ..count.., fill = Species)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  scale_x_discrete(name = "Year Class") +
  scale_y_continuous(name = "Frequency", expand = c(0,0)) +
  Theme2  +
  theme(legend.position = c(0.85,0.7)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
dev.off()

#2015
tiff("YC both Species 2015", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2015, Year.Class!="NA"), aes(x=as.factor(Year.Class), y = ..count.., fill = Species)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  scale_x_discrete(name = "Year Class") +
  scale_y_continuous(name = "Frequency", expand = c(0,0)) +
  Theme2  +
  theme(legend.position = c(0.25,0.7)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
dev.off()

#2016
tiff("YC both Species 2016", units="in", width=5, height=5, res=300)

ggplot(filter(MyData, Year.Caught==2016, Year.Class!="NA"), aes(x=as.factor(Year.Class), y = ..count.., fill = Species)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  scale_x_discrete(name = "Year Class") +
  scale_y_continuous(name = "Frequency", expand = c(0,0)) +
  Theme2  +
  theme(legend.position = c(0.25,0.7)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
dev.off()


#catch curve for LM 
LMCatchCurve <- read.csv("~/Desktop/LM CC June 15.csv")
lmCCLMB <- lm(LNage~Age,data = LMCatchCurve)

LMCatchCurve$predicted <- predict(lmCCLMB)
LMCatchCurve$residuals <- residuals(lmCCLMB)
LMCatchCurve <- LMCatchCurve %>% 
  mutate(NumbAge = exp(LNage))
lmCCLMB <- lm(LNage~Age, weights = NumbAge,data = filter(LMCatchCurve, Age>8))
summary(lmCCLMB)

### testing below the ues of weihgting using sample size 
shapiro.test(residuals(lmCCLMB))
compmod.lmod <- LMCatchCurve %>% 
  mutate(resid = residuals(lmCCLMB),
         pred = fitted(lmCCLMB))
summary(lm(abs(resid) ~ pred, data = compmod.lmod))

tiff("CC LMB all fish", units="in", width=8, height=5, res=300)

ggplot(LMCatchCurve, aes(x = Age, y = LNage)) +
  geom_point(size = 4) +
  scale_y_continuous(name = "Log Number at Age", limits = c(1,4),breaks = seq(1,4,0.5), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(3,16,1), name = "Age") +
  geom_abline(slope = -.29, intercept = 6.1, col = "black", size = 1.5) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  geom_vline(xintercept=6.5, linetype="dashed", size =1.5)
dev.off()

###SMB CC 
SMBCC <- read.csv("~/Desktop/SMB CC June 15 2021.csv")

LMSMB <- lm(LNage~Age, weights = NumbAge,data = filter(SMBCC, Age>6))
summary(LMSMB)

### testing below the ues of weihgting using sample size 
shapiro.test(residuals(LMSMB))
compmod.lmod <- LMCatchCurve %>% 
  mutate(resid = residuals(lmCCLMB),
         pred = fitted(lmCCLMB))
summary(lm(abs(resid) ~ pred, data = compmod.lmod))

tiff("CC SMB all fish", units="in", width=8, height=5, res=300)

ggplot(filter(SMBCC,Age<17), aes(x = Age, y = LNage)) +
  geom_point(size = 4) +
  scale_y_continuous(name = "Log Number at Age", limits = c(1,7),breaks = seq(1,7,1), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(3,16,1), name = "Age") +
  geom_abline(slope = -.32, intercept = 7.64, col = "black", size = 1.5) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.ticks=element_blank(),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  geom_vline(xintercept=6.5, linetype="dashed", size =1.5)
dev.off()


#figure of lengh vs proportion from externally tagged fish 

Floy <- read.csv("~/Desktop/Black Bass Manuscript/Data/Working Tourny Tag for Rwork.csv")
summary(Floy)

summ <- Floy %>% 
  group_by(Species) %>% 
  summarise(meanFL = mean(Forked.Length, na.rm = TRUE))

tuke


Floy <- select(Floy, Species, Forked.Length)
Floy <- filter(Floy, Forked.Length !="NA",Forked.Length<600 )

tiff("FL vs proportion ALL TAGGED fish", units="in", width=6, height=5, res=300)
ggplot(filter(Floy, Species=="SMB"), aes(x = as.numeric(Forked.Length), y = ..count../sum(..count..))) +
  geom_histogram(binwidth = 4, fill = "black") +
  scale_x_continuous(name = "\nFork Length (mm)", limits = c(290,550), breaks = seq(320,520,40)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0)) +
  scale_fill_manual(values = c('#999999',"#333333")) +
  Theme2 +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 315)) 
dev.off()

#LMB length vs proportion 
tiff("FL vs LMB proportion ALL TAGGED fish", units="in", width=6, height=5, res=300)
ggplot(filter(Floy, Species=="LMB"), aes(x = as.numeric(Forked.Length), y = ..count../sum(..count..))) +
  geom_histogram(binwidth = 4, fill = "black") +
  scale_x_continuous(name = "\nFork Length (mm)", limits = c(290,550), breaks = seq(320,520,40)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0)) +
  Theme2 +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold")) +
  theme(axis.text.x = element_text(angle = 315)) 
dev.off()


##proportion with weight
tiff("weight proportion for SMB", units="in", width=6, height=5, res=300)
ggplot(filter(MyData, Species=="Smallmouth Bass"), aes(x = Weight..kg., y = ..count../sum(..count..))) +
  geom_histogram(binwidth = 0.14, fill = "black")+
  scale_x_continuous(name = "Weight (Kg)", limits = c(0,3.5), breaks = seq(0,5,1)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0)) +
  Theme2 +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"), 
        axis.ticks.length=unit(0.25, "cm")) 
dev.off()


#weight proportion for LMB 
tiff("weight proportion for LMB", units="in", width=6, height=5, res=300)
ggplot(filter(MyData, Species=="Largemouth Bass"), aes(x = Weight..kg., y = ..count../sum(..count..))) +
  geom_histogram(binwidth = 0.1, fill = "black")+
  scale_x_continuous(name = "Weight (Kg)", limits = c(0,3.5), breaks = seq(0,5,1)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0), limit = c(0,0.15), breaks = seq(0,0.15,0.05)) +
  Theme2 +
  theme(legend.position = c(0.75,0.5)) +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 17, colour = "black"),
        axis.text.y = element_text(size = 17, colour = "black"),
        axis.title = element_text(size = 22, colour = "black", vjust = -0.5, face="bold"), 
        axis.ticks.length=unit(0.25, "cm")) 
dev.off()

#weight proportion for both species combined 
tiff("weight proportion for both species combined ", units="in", width=10, height=5, res=300)

Fig1Comp <- ggplot(MyData, aes(x = Weight..kg., y = ..count../sum(..count..), fill = Species)) +
  geom_histogram(binwidth = 0.15, position = "dodge") +
  scale_x_continuous(name = "Weight (Kg)", limits = c(0,3.5), breaks = seq(0,5,1)) +
  scale_y_continuous(name = "", expand = c(0,0)) +
  scale_fill_manual(values = c('#999999',"#333333"), labels = c("LMB", "SMB")) +
  Theme2 +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  Theme2 +
  theme(axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 18, colour = "black", vjust = -0.5), 
        axis.ticks.length=unit(0.25, "cm")) 
dev.off()



###Composite plot for age , weight, and length distribnution 

library(cowplot)
library(gridExtra)
library(grid)

plot_grid(Fig2Comp, Fig1Comp, Fig3Comp, nrow = 2, rel_widths = c(1,1,2), align = "h")

grid.arrange(Fig2Comp, Fig1Comp, Fig3Comp, nrow = 2, layout_matrix = rbind(c(1,2), c(3,3)), left = textGrob("Proportion", rot = 90,  gp=gpar(fontsize=25, angle = 45)))

################################################################################################################################################
