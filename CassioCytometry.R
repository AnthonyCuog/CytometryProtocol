library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(cowplot)

(files <- fs::dir_ls("CassioListMode/", 
                     glob="*.CSV"))
df <- read_csv(files, id="path")
head(df)

names(df) <- gsub("-", ".", names(df), fixed=TRUE)

df$RED.B.HLog <- as.numeric(df$RED.B.HLog)

ggdensity(df, x = "RED.B.HLog", fill = "lightgray", rug = FALSE)+
  scale_x_continuous(limits = c(1.5, 5))
head(df)
DensityY <- density(df$RED.B.HLog)$y
DensityX <- density(df$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.0033001299
which(DensityY == MinYDensity)
#329
DensityX[329]

#Visualize your threshold here
ggdensity(df, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(df$RED.B.HLog)$x[329])

#X Minimum = 3.04001
#3.005898 is your binning threshold. Go back in to InCyte, 
#adjust your "Symbiont" bin on the RED-B-HLog histogram appropriately if

#If desired, subset your symbionts from the noise, and use this data to plot
#fluorescent signatures for each seperately counted cell 

dfsym <- subset(df, RED.B.HLog>=3.04001)
dfsym <- subset(dfsym, SSC.HLog>=1)
head(dfsym)
nrow(dfsym)
ggdensity(dfsym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(df$RED.B.HLog)$x[329])

#you will need to map metadata to this dataset in most cases
#You are not able to get a cells/mL concentration with this dataset
#In most cases, it is easier to reaccess InCyte with your new binning threshold
#And export summarized data from InCyte as a .csv file 
class(dfsym$path)
head(dfsym$path)
df2<-tidyr::separate(dfsym, 'path', into = c(NA, NA,'well',NA))
unique(df2$well)
class(df2$well)
dat4<-df2

dat4$condition<-replace(dat4$well, dat4$well == "A01", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A02", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A03", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A04", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A05", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A06", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A07", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A08", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A09", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A10", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A11", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "A12", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B01", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B02", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B03", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B04", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B05", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B06", "Light")
dat4$condition<-replace(dat4$condition, dat4$condition == "B07", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "B08", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "B09", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "B10", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "B11", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "B12", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "C01", "Dark")
dat4$condition<-replace(dat4$condition, dat4$condition == "C02", "Dark")

dat4$individual<-replace(dat4$well, dat4$well == "A01", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "A02", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "A03", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "A04", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "A05", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "A06", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "A07", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "A08", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "A09", "5")
dat4$individual<-replace(dat4$individual, dat4$individual == "A10", "5")
dat4$individual<-replace(dat4$individual, dat4$individual == "A11", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "A12", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "B01", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B02", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B03", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "B04", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "B05", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "B06", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "B07", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "B08", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "B09", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B10", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B11", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "B12", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "C01", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "C02", "4")
head(dat4)

write.csv(dat4, "CassiopeaExperimentalDataset.csv", row.names=FALSE)

library(ggbiplot)
library(viridis)
dat4<- as.data.frame(dat4)
head(dat4)
LDCass <- dat4
head(LDCass)
CassRED <- ggplot(LDCass, aes(condition, RED.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassRED

CassGRN <- ggplot(LDCass, aes(condition, GRN.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassGRN

CassFSC <- ggplot(LDCass, aes(condition, FSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassFSC

CassSSC <- ggplot(LDCass, aes(condition, SSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassSSC

library(gridExtra)
grid.arrange(CassRED, CassGRN, CassFSC, CassSSC, ncol = 4)

library(lattice)
library(FSA)
library(rstatix)
library(car)

kruskal.test(RED.B.HLog ~ condition, data = LDCass)
# p < 0.001
kruskal.test(GRN.B.HLog ~ condition, data = LDCass)
# p < 0.001
kruskal.test(FSC.HLog ~ condition, data = LDCass)
# p < 0.001
kruskal.test(SSC.HLog ~ condition, data = LDCass)
# p < 0.001

leveneTest(RED.B.HLog ~ condition, data = LDCass)
# F = 19.782, p = 0.4711
leveneTest(GRN.B.HLog ~ condition, data = LDCass)
# F = 79.675, p < 0.001
leveneTest(FSC.HLog ~ condition, data = LDCass)
# F = 66.731, p < 0.001
leveneTest(SSC.HLog ~ condition, data = LDCass)
# F = 89.699, p < 0.001

#pairwise
dunn_test(RED.B.HLog ~ condition, data = LDCass)
#.y.        group1 group2      n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>      <chr>  <chr>    <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#  1 RED.B.HLog Dark   Light    16002 18226    -14.2  6.71e-46 1.34e-45 ****        
#  2 RED.B.HLog Dark   Variable 16002 19755      2.36 1.85e- 2 1.85e- 2 *           
#  3 RED.B.HLog Light  Variable 18226 19755     17.4  4.04e-68 1.21e-67 **** 
dunn_test(GRN.B.HLog ~ condition, data = LDCass)
#.y.        group1 group2      n1    n2 statistic         p     p.adj p.adj.signif
#* <chr>      <chr>  <chr>    <int> <int>     <dbl>     <dbl>     <dbl> <chr>       
#  1 GRN.B.HLog Dark   Light    16002 18226     18.8  8.20e- 79 1.64e- 78 ****        
#  2 GRN.B.HLog Dark   Variable 16002 19755     21.6  3.87e-103 1.16e-102 ****        
#  3 GRN.B.HLog Light  Variable 18226 19755      2.51 1.22e-  2 1.22e-  2 *     
dunn_test(FSC.HLog ~ condition, data = LDCass)
#.y.      group1 group2      n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>    <chr>  <chr>    <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#  1 FSC.HLog Dark   Light    16002 18226    -8.93  4.35e-19 8.73e-19 ****        
#  2 FSC.HLog Dark   Variable 16002 19755    -0.429 6.68e- 1 6.68e- 1 ns          
#  3 FSC.HLog Light  Variable 18226 19755     8.97  2.91e-19 8.73e-19 **** 
dunn_test(SSC.HLog ~ condition, data = LDCass)
#.y.      group1 group2      n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>    <chr>  <chr>    <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#  1 SSC.HLog Dark   Light    16002 18226     -4.86 1.18e- 6 2.35e- 6 ****        
#  2 SSC.HLog Dark   Variable 16002 19755      2.28 2.26e- 2 2.26e- 2 *           
#  3 SSC.HLog Light  Variable 18226 19755      7.49 7.03e-14 2.11e-13 ****  

LDHost <- LDCass %>% 
  dplyr::group_by(condition, individual) %>%
  dplyr::summarize(RED.B.HLog = mean(RED.B.HLog), 
                   GRN.B.HLog = mean(GRN.B.HLog),
                   FSC.HLog = mean(FSC.HLog),
                   SSC.HLog = mean(SSC.HLog)
  )
View(LDHost)
HostRED <- ggplot(LDHost, aes(condition, RED.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostRED

HostGRN <- ggplot(LDHost, aes(condition, GRN.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostGRN

HostFSC <- ggplot(LDHost, aes(condition, FSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostFSC

HostSSC <- ggplot(LDHost, aes(condition, SSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostSSC

CassPheno <- grid.arrange(CassRED, CassGRN, CassFSC, CassSSC, HostRED, HostGRN,HostFSC,HostSSC, ncol = 4)

wilcox.test(RED.B.HLog ~ condition, data = LDHost)
# p < 0.2
wilcox.test(GRN.B.HLog ~ condition, data = LDHost)
# p < 0.02857
wilcox.test(FSC.HLog ~ condition, data = LDHost)
# p < 0.05714
wilcox.test(SSC.HLog ~ condition, data = LDHost)
# p < 0.3429

leveneTest(RED.B.HLog ~ condition, data = LDHost)
# F = 0.4665, p = 0.6402
leveneTest(GRN.B.HLog ~ condition, data = LDHost)
# F = 0.4568, p = 0.6459
leveneTest(FSC.HLog ~ condition, data = LDHost)
# F = 0.1749, p = 0.8421
leveneTest(SSC.HLog ~ condition, data = LDHost)
# F = 10127, p = 0.3619

#pairwise comparisons
LDHost <- as.data.frame(LDHost)
dunn_test(RED.B.HLog ~ condition, data = LDHost)
# .y.        group1 group2      n1    n2 statistic      p p.adj p.adj.signif
#  * <chr>      <chr>  <chr>    <int> <int>     <dbl>  <dbl> <dbl> <chr>       
#  1 RED.B.HLog Dark   Light        4     4    -1.27  0.204  0.407 ns          
#  2 RED.B.HLog Dark   Variable     4     5     0.574 0.566  0.566 ns          
#  3 RED.B.HLog Light  Variable     4     5     1.91  0.0556 0.167 ns     
dunn_test(GRN.B.HLog ~ condition, data = LDHost)
#  .y.        group1 group2      n1    n2 statistic       p   p.adj p.adj.signif
#  * <chr>      <chr>  <chr>    <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#  1 GRN.B.HLog Dark   Light        4     4      1.82 0.0694  0.139   ns          
#  2 GRN.B.HLog Dark   Variable     4     5      2.95 0.00320 0.00961 **          
#  3 GRN.B.HLog Light  Variable     4     5      1.03 0.301   0.301   ns     
dunn_test(FSC.HLog ~ condition, data = LDHost)
#  .y.      group1 group2      n1    n2 statistic      p  p.adj p.adj.signif
#  * <chr>    <chr>  <chr>    <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#  1 FSC.HLog Dark   Light        4     4    -2.18  0.0293 0.0880 ns          
#  2 FSC.HLog Dark   Variable     4     5    -0.402 0.688  0.688  ns          
#  3 FSC.HLog Light  Variable     4     5     1.89  0.0581 0.116  ns 
dunn_test(SSC.HLog ~ condition, data = LDHost)
#  .y.      group1 group2      n1    n2 statistic      p p.adj p.adj.signif
#  * <chr>    <chr>  <chr>    <int> <int>     <dbl>  <dbl> <dbl> <chr>       
#  1 SSC.HLog Dark   Light        4     4    -1.36  0.173  0.347 ns          
#  2 SSC.HLog Dark   Variable     4     5     0.402 0.688  0.688 ns          
#  3 SSC.HLog Light  Variable     4     5     1.84  0.0662 0.198 ns  

head(LDHost)
theopca1 <- prcomp(LDHost[, c(3,4,5,6)], center = TRUE, scale. = TRUE)
summary(theopca1)
citation("stats")
casspca <- ggbiplot(theopca1, alpha = 1, size = 2, ellipse = TRUE, groups = LDHost$condition)+
  scale_color_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  scale_fill_viridis(discrete = TRUE, option = "B", end = 0.8, direction = 1)+
  theme_classic2()
casspca
grid.arrange(casspca, CassPheno, ncol = 2)
