# compare CEA
library(broom)
library(tibble)

folder<-"C:/Data/GitHub/ETC_ICM/CEA_20190503/"

file1<- paste0(folder,"03_impact_additive_mean1")
file2<- paste0(folder,"03_impact_additive_mean2")
file3<- paste0(folder,"03_impact_additive_mean3")
file4<- paste0(folder,"03_impact_additive_mean4")

df1<-read.table(file=file1,quote="",sep=",",header=T,stringsAsFactors=F)
df2<-read.table(file=file2,quote="",sep=",",header=T,stringsAsFactors=F)
df3<-read.table(file=file3,quote="",sep=",",header=T,stringsAsFactors=F)
df4<-read.table(file=file4,quote="",sep=",",header=T,stringsAsFactors=F)

df <- bind_rows(df1,df2,df3)

areafile<-"data/Grid_sea_area.csv"
dfarea<-read.table(areafile, quote="",sep=",", header=TRUE, fileEncoding="UTF-8", stringsAsFactors=FALSE) %>%
  rename(GridID=GRIDCODE,Area_km2=SEA_KM2,x=XCoord,y=YCoord) %>%
  mutate(x=x+5000,y=y+5000)

dfCEA <- dfarea %>%
  left_join(df,by=c("x","y")) %>%
  select(GridID,Impact=value)

Class <- c("High","Good","Moderate","Poor","Bad")

dfCEA <- df_MESH %>% 
  left_join(dfCEA,by="GridID") %>%
  filter(!is.na(EQR),!is.na(Impact)) %>%
  mutate(Class=Class[Cat_MESH])
  
dfCEA$Class <- factor(dfCEA$Class,levels=Class)


p <- ggplot(dfCEA, aes(x=Class, y=Impact)) + 
  geom_boxplot() +
  facet_grid(REGION~CoastOff)
  
p
ggsave(filename="boxplot_impact_REGION.png",plot=p,width=15,height=10,units="cm")


p<-ggplot(dfCEA, aes(x=Impact, y=EQR)) + 
  geom_point(aes(colour=REGION))+
  geom_smooth(method=lm) + 
  theme_classic()

ggsave(filename="EQR_vs_impact.png",plot=p,width=15,height=10,units="cm")

p<-ggplot(dfCEA, aes(x=Impact, y=EQR)) + 
  geom_point()+
  geom_smooth(method=lm) + 
  theme_classic() +
  facet_grid(REGION~CoastOff)

ggsave(filename="EQR_vs_impact_REGION.png",plot=p,width=20,height=20,units="cm")


dfCEA <- as_tibble(dfCEA)

regressions <- dfCEA %>%
  nest(-c(REGION,CoastOff)) %>% 
  mutate(
    fit = map(data, ~ lm(EQR ~ Impact, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

res <- regressions %>% 
  unnest(tidied)
res %>% arrange(REGION,CoastOff)
