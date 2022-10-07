getwd()
rm(list=ls()) 
#可视化
sub_merge <- read.table('RA_upload_seed_bulk.txt', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
library(ggplot2)
sub_merge$Comparts<-factor(sub_merge$Comparts,levels=c("Bulk_soil", "Seed_endosphere"), 
                         labels = c("Bulk_soil", "Seed_endosphere"))
sub_merge$Phylum<-factor(sub_merge$Phylum,levels=c("Gammaproteobacteria",
                                                   "Betaproteobacteria",
                                                   "Actinobacteria",
                                                   "Alphaproteobacteria",
                                                   "Deltaproteobacteria",
                                                   "Gemmatimonadetes",
                                                   "Firmicutes",
                                                   "Bacteroidetes",
                                                   "Acidobacteria",
                                                   "Ignavibacteriae",
                                                   "Nitrospirae",
                                                   "Low_Abundance"))
phy.cols <- c("#FFFF00", "#FF8247", 
              "#FFE7BA", "#87CEFA",
              "#B0E0E6", "#48D1CC",
              "#5F9EA0", "#66CDAA",
              "#458B00", "#BCEE68",
              "#FFF68F", "#EEEE00") 
p=ggplot(sub_merge, aes(x = TreatmentID, y = RA, fill=Phylum)) +
  geom_bar(stat='identity', position = "fill")+  
  labs(x="Treatment",y="Relative abundance")+
  facet_grid(rows = Comparts~Treatment,scales= "free" ,space= "free")+
  theme_bw()+
  scale_fill_manual(values=phy.cols) 
p=p+theme(axis.text.x = element_blank())
p
ggsave("RA_seed_bulk.pdf", p, width = 5, height = 4)
ggsave("RA_seed_bulk.png", p, width = 5, height = 4)