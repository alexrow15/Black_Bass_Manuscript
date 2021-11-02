#Tournament Paper R Analysis w/ Alex

library(ggplot2)
library(dplyr)
library(emmeans)
library(car)
install.packages("emmeans")
#Data Import
setwd("C:/Users/Patwo/OneDrive/Queens School Work/Fisheries Conservation Lab/2021/Tournament Mortality - Life History Papers")

MyData <- read.csv(file.choose()) #"Alex Processed Fish - Aug13 2021.csv"
summary(Mydata)
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
  filter(Age <= 16)

position = position_dodge(width = 0.5)

png("Length distribution of LMB and SMB", units = "in", width = 10, height = 5, res = 300)
ggplot(OtolithData)+
  geom_point(aes(x = Age, y = mean.FL, color = Species), size = 2.5, position = position)+
  geom_errorbar(aes(x = Age, ymin = LCLL, ymax = UCLL, color = Species),
                width = 0.25, lwd = 0.25, position = position)+
  scale_x_continuous(name = "Otolith age", limits = c(2.75,16.25), breaks = seq(3,16,1))+
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
  scale_x_continuous(name = "Otolith age", limits = c(2.75,16.5), breaks = seq(3,16,1))+
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
