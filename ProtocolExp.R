library(dplyr)
library(tidyr)
#install.packages("readr")
library(readr)
library(ggplot2)
library(ggpubr)
library(cowplot)

(files <- fs::dir_ls("Exp1_2022-09-21_at_11-08-48am/", glob="*.CSV"))
dat <- read_csv(files, id="path")
head(dat)
View(dat)
dat3 <- dat %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(dat3)
dat4<-dat3

dat4$dilution<-replace(dat4$well, dat4$well == "A01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H01", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A07", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B07", 50)

head(dat4)
test <- subset(dat4, dilution == "H06")
nrow(test)

dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C07", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D07", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E07", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F07", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G07", 50)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H07", 50)

#20
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H02", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G08", 20)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H08", 20)

#10
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H03", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G09", 10)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H09", 10)

#5
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H04", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G10", 5)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H10", 5)

#2
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H05", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G11", 2)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H11", 2)

#1
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H06", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "A12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "B12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "C12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "D12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "E12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "F12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "G12", 1)
dat4$dilution<-replace(dat4$dilution, dat4$dilution == "H12", 1)

nrow(dat4)
head(dat4)

dat4$dilution <- as.character(dat4$dilution)
names(dat4) <- gsub("-", ".", names(dat4), fixed=TRUE)

dat4$RED.B.HLog <- as.numeric(dat4$RED.B.HLog)

ggdensity(dat4, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(dat4$RED.B.HLog)$y
DensityX <- density(dat4$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
which(DensityY == MinYDensity)
DensityX[334]

ggdensity(dat4, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(dat4$RED.B.HLog)$x[334])

#3.005898

dat4$order<-replace(dat4$well, dat4$well == "A01", 1)
dat4$order<-replace(dat4$order, dat4$order == "B01", 3)
dat4$order<-replace(dat4$order, dat4$order == "C01", 5)
dat4$order<-replace(dat4$order, dat4$order == "D01", 7)
dat4$order<-replace(dat4$order, dat4$order == "E01", 9)
dat4$order<-replace(dat4$order, dat4$order == "F01", 11)
dat4$order<-replace(dat4$order, dat4$order == "G01", 13)
dat4$order<-replace(dat4$order, dat4$order == "H01", 15)
dat4$order<-replace(dat4$order, dat4$order == "A07", 2)
dat4$order<-replace(dat4$order, dat4$order == "B07", 4)
dat4$order<-replace(dat4$order, dat4$order == "C07", 6)
dat4$order<-replace(dat4$order, dat4$order == "D07", 8)
dat4$order<-replace(dat4$order, dat4$order == "E07", 10)
dat4$order<-replace(dat4$order, dat4$order == "F07", 12)
dat4$order<-replace(dat4$order, dat4$order == "G07", 14)
dat4$order<-replace(dat4$order, dat4$order == "H07", 16)

#20
dat4$order<-replace(dat4$order, dat4$order == "A02", 1)
dat4$order<-replace(dat4$order, dat4$order == "B02", 3)
dat4$order<-replace(dat4$order, dat4$order == "C02", 5)
dat4$order<-replace(dat4$order, dat4$order == "D02", 7)
dat4$order<-replace(dat4$order, dat4$order == "E02", 9)
dat4$order<-replace(dat4$order, dat4$order == "F02", 11)
dat4$order<-replace(dat4$order, dat4$order == "G02", 13)
dat4$order<-replace(dat4$order, dat4$order == "H02", 15)
dat4$order<-replace(dat4$order, dat4$order == "A08", 2)
dat4$order<-replace(dat4$order, dat4$order == "B08", 4)
dat4$order<-replace(dat4$order, dat4$order == "C08", 6)
dat4$order<-replace(dat4$order, dat4$order == "D08", 8)
dat4$order<-replace(dat4$order, dat4$order == "E08", 10)
dat4$order<-replace(dat4$order, dat4$order == "F08", 12)
dat4$order<-replace(dat4$order, dat4$order == "G08", 14)
dat4$order<-replace(dat4$order, dat4$order == "H08", 16)

#10
dat4$order<-replace(dat4$order, dat4$order == "A03", 1)
dat4$order<-replace(dat4$order, dat4$order == "B03", 3)
dat4$order<-replace(dat4$order, dat4$order == "C03", 5)
dat4$order<-replace(dat4$order, dat4$order == "D03", 7)
dat4$order<-replace(dat4$order, dat4$order == "E03", 9)
dat4$order<-replace(dat4$order, dat4$order == "F03", 11)
dat4$order<-replace(dat4$order, dat4$order == "G03", 13)
dat4$order<-replace(dat4$order, dat4$order == "H03", 15)
dat4$order<-replace(dat4$order, dat4$order == "A09", 2)
dat4$order<-replace(dat4$order, dat4$order == "B09", 4)
dat4$order<-replace(dat4$order, dat4$order == "C09", 6)
dat4$order<-replace(dat4$order, dat4$order == "D09", 8)
dat4$order<-replace(dat4$order, dat4$order == "E09", 10)
dat4$order<-replace(dat4$order, dat4$order == "F09", 12)
dat4$order<-replace(dat4$order, dat4$order == "G09", 14)
dat4$order<-replace(dat4$order, dat4$order == "H09", 16)

#5
dat4$order<-replace(dat4$order, dat4$order == "A04", 1)
dat4$order<-replace(dat4$order, dat4$order == "B04", 3)
dat4$order<-replace(dat4$order, dat4$order == "C04", 5)
dat4$order<-replace(dat4$order, dat4$order == "D04", 7)
dat4$order<-replace(dat4$order, dat4$order == "E04", 9)
dat4$order<-replace(dat4$order, dat4$order == "F04", 11)
dat4$order<-replace(dat4$order, dat4$order == "G04", 13)
dat4$order<-replace(dat4$order, dat4$order == "H04", 15)
dat4$order<-replace(dat4$order, dat4$order == "A10", 2)
dat4$order<-replace(dat4$order, dat4$order == "B10", 4)
dat4$order<-replace(dat4$order, dat4$order == "C10", 6)
dat4$order<-replace(dat4$order, dat4$order == "D10", 8)
dat4$order<-replace(dat4$order, dat4$order == "E10", 10)
dat4$order<-replace(dat4$order, dat4$order == "F10", 12)
dat4$order<-replace(dat4$order, dat4$order == "G10", 14)
dat4$order<-replace(dat4$order, dat4$order == "H10", 16)

#2
dat4$order<-replace(dat4$order, dat4$order == "A05", 1)
dat4$order<-replace(dat4$order, dat4$order == "B05", 3)
dat4$order<-replace(dat4$order, dat4$order == "C05", 5)
dat4$order<-replace(dat4$order, dat4$order == "D05", 7)
dat4$order<-replace(dat4$order, dat4$order == "E05", 9)
dat4$order<-replace(dat4$order, dat4$order == "F05", 11)
dat4$order<-replace(dat4$order, dat4$order == "G05", 13)
dat4$order<-replace(dat4$order, dat4$order == "H05", 15)
dat4$order<-replace(dat4$order, dat4$order == "A11", 2)
dat4$order<-replace(dat4$order, dat4$order == "B11", 4)
dat4$order<-replace(dat4$order, dat4$order == "C11", 6)
dat4$order<-replace(dat4$order, dat4$order == "D11", 8)
dat4$order<-replace(dat4$order, dat4$order == "E11", 10)
dat4$order<-replace(dat4$order, dat4$order == "F11", 12)
dat4$order<-replace(dat4$order, dat4$order == "G11", 14)
dat4$order<-replace(dat4$order, dat4$order == "H11", 16)

#1
dat4$order<-replace(dat4$order, dat4$order == "A06", 1)
dat4$order<-replace(dat4$order, dat4$order == "B06", 3)
dat4$order<-replace(dat4$order, dat4$order == "C06", 5)
dat4$order<-replace(dat4$order, dat4$order == "D06", 7)
dat4$order<-replace(dat4$order, dat4$order == "E06", 9)
dat4$order<-replace(dat4$order, dat4$order == "F06", 11)
dat4$order<-replace(dat4$order, dat4$order == "G06", 13)
dat4$order<-replace(dat4$order, dat4$order == "H06", 15)
dat4$order<-replace(dat4$order, dat4$order == "A12", 2)
dat4$order<-replace(dat4$order, dat4$order == "B12", 4)
dat4$order<-replace(dat4$order, dat4$order == "C12", 6)
dat4$order<-replace(dat4$order, dat4$order == "D12", 8)
dat4$order<-replace(dat4$order, dat4$order == "E12", 10)
dat4$order<-replace(dat4$order, dat4$order == "F12", 12)
dat4$order<-replace(dat4$order, dat4$order == "G12", 14)
dat4$order<-replace(dat4$order, dat4$order == "H12", 16)

dat4sym <- subset(dat4, RED.B.HLog>=3.005898)
head(dat4sym)


### Taking Curated Dataset
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggalt)
library(ggthemes)

#Exp1CuratedDataset <- read_csv("Exp1CuratedDataset.csv", 
#                               col_types = cols(dilution = col_character(), 
#                                                order = col_character()))
#View(Exp1CuratedDataset)
#dat4sym <- Exp1CuratedDataset
dat4sym$dilution <- factor(dat4sym$dilution, levels = c("50","20","10","5","2","1"))

#install.packages("viridis")
library(viridis)

dat4sym$order <- as.numeric(dat4sym$order)

RED <- ggplot(dat4sym, aes(order, RED.B.HLog))+
  geom_smooth(aes(color = dilution), se = FALSE)+
  geom_boxplot(aes(group = well, color = dilution), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(2.8,4.6))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")

GRN <- ggplot(dat4sym, aes(order, GRN.B.HLog))+
  geom_smooth(aes(color = dilution), se = FALSE)+
  geom_boxplot(aes(group = well, color = dilution), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(0,4))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
GRN


SSC <- ggplot(dat4sym, aes(order, SSC.HLog))+
  geom_smooth(aes(color = dilution), se = FALSE)+
  geom_boxplot(aes(group = well, color = dilution), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(1.5,5))+
  xlab("")+
  theme_classic()
SSC

FSC <- ggplot(dat4sym, aes(order, FSC.HLog))+
  geom_smooth(aes(color = dilution), se = FALSE)+
  geom_boxplot(aes(group = well, color = dilution), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(0.5,4.5))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
FSC
#install.packages("gridExtra")
#library(gridExtra)

grid.arrange(RED,GRN,FSC,SSC, ncol = 2)

dat4sym$orderfactor <- as.factor(dat4sym$order)

RED <- ggplot(dat4sym, aes(dilution, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  geom_tufteboxplot(aes(group = well, color = orderfactor), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
RED

GRN <- ggplot(dat4sym, aes(dilution, GRN.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  geom_tufteboxplot(aes(group = well, color = orderfactor), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(0,3.5))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
GRN

SSC <- ggplot(dat4sym, aes(dilution, SSC.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  geom_tufteboxplot(aes(group = well, color = orderfactor), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(1.5,5))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
SSC

FSC <- ggplot(dat4sym, aes(dilution, FSC.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  geom_tufteboxplot(aes(group = well, color = orderfactor), outlier.alpha = 0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  scale_y_continuous(limits = c(0.8,4.5))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
FSC

Fluor <- grid.arrange(RED,GRN,FSC,SSC, ncol = 2)
Fluor
library(readr)
Exp1_2022_09_21_at_11_08_48am <- read_csv("Exp1_2022-09-21_at_11-08-48am.CSV", 
                                          col_types = cols(Dilution = col_character(), 
                                                           Time = col_character()))
View(Exp1_2022_09_21_at_11_08_48am)

Concentration <- Exp1_2022_09_21_at_11_08_48am

Concentration$Time <- factor(Concentration$Time, levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))
Concentration$Row <- factor(Concentration$Row, levels = c("1","2","3","4","5","6","7","8"))
Concentration$Dilution <- factor(Concentration$Dilution, levels = c("50","20","10","5","2","1"))

Concentration2 <- subset(Concentration, Row != "8")
Concentration2 <- subset(Concentration2, Row != "7")

Conc <-  ggplot(Concentration, aes(Dilution, Corrected.Concentration))+
  #geom_smooth(aes(group = Dilution), se = FALSE)+
  geom_tufteboxplot(aes(color = Time), outlier.alpha = 1, size = 1.5, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  coord_cartesian(ylim=c(0, 3e+05))+
  #scale_y_continuous(limits = c(0,4e+05))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "right")+
  guides(color=guide_legend(ncol=2))
Conc
grid.arrange(Fluor, Conc, nrow = 2, heights = c(3,1))

###Exp 2
Exp2 <- read_csv("Exp2_2022-09-27.csv",)
head(Exp2)

Exp2$MinutesFactor <- as.character(Exp2$MinutesSinceSpray)

Exp2$MinutesFactor <- factor(Exp2$MinutesFactor, levels = c("43","136","236","344","470","1459"))

Conc2 <- ggplot(Exp2, aes(Condition, Corrected.Concentration))+
  #geom_smooth(aes(color = Condition), se = FALSE)+
  #geom_boxplot(aes(group = ID, color = MinutesFactor), outlier.alpha = 0, alpha = 0.8)+
  geom_tufteboxplot(aes(color = MinutesFactor), outlier.alpha = 1, size = 2.0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  #scale_y_continuous(limits = c(0.8,4.5))+
  #facet_grid(rows = vars(Light), cols = vars (Ice))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "bottom")
Conc2

(files <- fs::dir_ls("Exp2/2022-09-26_at_09-43-31am/", glob="*.CSV"))
nineam <- read_csv(files, id="path")

(files <- fs::dir_ls("Exp2/2022-09-26_at_11-16-28am/", glob="*.CSV"))
elevenam <- read_csv(files, id="path")

(files <- fs::dir_ls("Exp2/2022-09-26_at_12-56-05pm/", glob="*.CSV"))
onepm <- read_csv(files, id="path")

(files <- fs::dir_ls("Exp2/2022-09-26_at_02-44-32pm/", glob="*.CSV"))
threepm <- read_csv(files, id="path")

(files <- fs::dir_ls("Exp2/2022-09-26_at_04-50-00pm/", glob="*.CSV"))
fivepm <- read_csv(files, id="path")

(files <- fs::dir_ls("Exp2/2022-09-27_at_10-02-49am//", glob="*.CSV"))
nextday <- read_csv(files, id="path")
head(nextday)
exp2 <- rbind(nineam,elevenam)
exp2 <- rbind(exp2, threepm)  
exp2 <- rbind(exp2, fivepm)  
exp2 <- rbind(exp2, nextday)  

exp2 <- dplyr::bind_rows(exp2, onepm)
class(exp2)
exp2 <- as.data.frame(exp2)
exp23 <- exp2 %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(exp23)

exp24 <-  exp23
metadata <- Exp2
metadata <- metadata[c(1,2,3,4,5,6,7,8,9)]
head(metadata)

exp25 <- merge(exp24, metadata, by.x = "well", 
                   by.y = "Well", all.x = TRUE, all.y = FALSE)
head(exp25)
nrow(exp25)

names(exp25) <- gsub("-", ".", names(exp25), fixed=TRUE)

ggdensity(exp25, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(exp25$RED.B.HLog)$y
DensityX <- density(exp25$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
which(DensityY == MinYDensity)
DensityX[329]

ggdensity(exp25, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(dat4$RED.B.HLog)$x[329])

exp26 <- subset(exp25, RED.B.HLog>=2.99355)

ggdensity(exp26, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(dat4$RED.B.HLog)$x[329])

exp26$MinutesFactor <- as.factor(exp26$MinutesSinceSpray)
RED2 <- ggplot(exp26, aes(Condition, RED.B.HLog))+
  #geom_smooth(aes(color = Condition), se = FALSE)+
  #geom_boxplot(aes(group = ID, color = MinutesFactor), outlier.alpha = 0, alpha = 0.8)+
  geom_tufteboxplot(aes(color = MinutesFactor), size = 2.0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  #scale_y_continuous(limits = c(0.8,4.5))+
  #facet_grid(rows = vars(Light), cols = vars (Ice))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
RED2

GRN2 <- ggplot(exp26, aes(Condition, GRN.B.HLog))+
  #geom_smooth(aes(color = Condition), se = FALSE)+
  #geom_boxplot(aes(group = ID, color = MinutesFactor), outlier.alpha = 0, alpha = 0.8)+
  geom_tufteboxplot(aes(color = MinutesFactor), size = 2.0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  #scale_y_continuous(limits = c(0.8,4.5))+
  #facet_grid(rows = vars(Light), cols = vars (Ice))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
GRN2

FSC2 <- ggplot(exp26, aes(Condition, FSC.HLog))+
  #geom_smooth(aes(color = Condition), se = FALSE)+
  #geom_boxplot(aes(group = ID, color = MinutesFactor), outlier.alpha = 0, alpha = 0.8)+
  geom_tufteboxplot(aes(color = MinutesFactor), size = 2.0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  #scale_y_continuous(limits = c(0.8,4.5))+
  #facet_grid(rows = vars(Light), cols = vars (Ice))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
FSC2

SSC2 <- ggplot(exp26, aes(Condition, SSC.HLog))+
  #geom_smooth(aes(color = Condition), se = FALSE)+
  #geom_boxplot(aes(group = ID, color = MinutesFactor), outlier.alpha = 0, alpha = 0.8)+
  geom_tufteboxplot(aes(color = MinutesFactor), size = 2.0, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = -1)+
  #scale_y_continuous(limits = c(0.8,4.5))+
  #facet_grid(rows = vars(Light), cols = vars (Ice))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
SSC2

pheno2 <- grid.arrange(RED2, GRN2, FSC2, SSC2, nrow = 2)
pheno2 <- grid.arrange(pheno2, Conc2, nrow=2,  heights=c(1.5, 1.3))

library(FSA)
library(rcompanion)
redstats <- dunnTest(exp26$RED.B.HLog ~ interaction(exp26$Condition, exp26$MinutesFactor))
redstats = redstats$res
redstatsletters <- cldList(comparison = redstats$Comparison, p.value = redstats$P.adj, threshold = 0.05)
redstatsletters

greenstats <- dunnTest(exp26$GRN.B.HLog ~ interaction(exp26$Condition, exp26$MinutesFactor))
greenstats = greenstats$res
greenstatsletters <- cldList(comparison = greenstats$Comparison, p.value = greenstats$P.adj, threshold = 0.05)
greenstatsletters

fscstats <- dunnTest(exp26$FSC.HLog ~ interaction(exp26$Condition, exp26$MinutesFactor))
fscstats = fscstats$res
fscstatsletters <- cldList(comparison = fscstats$Comparison, p.value = fscstats$P.adj, threshold = 0.05)
fscstatsletters

sscstats <- dunnTest(exp26$SSC.HLog ~ interaction(exp26$Condition, exp26$MinutesFactor))
sscstats = sscstats$res
sscstatsletters <- cldList(comparison = sscstats$Comparison, p.value = sscstats$P.adj, threshold = 0.05)
sscstatsletters

cdstats <- dunnTest(Exp2$Corrected.Concentration ~ interaction(Exp2$Condition, Exp2$MinutesFactor))
cdstats = cdstats$res
cdstatsletters <- cldList(comparison = cdstats$Comparison, p.value = cdstats$P.adj, threshold = 0.05)
cdstatsletters
library(gridExtra)
