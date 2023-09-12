# Setup Environment ####
library(rFVS) # see 'setup.r' for installation of the package
#library(fvsOL) # not used here. don't know how to setup run using this
library(readr) # use readr::read_file() to load .txt strings for keyfile making
library(dplyr) # general data handling. ** note that rFVS loads plyr package that may mess up with the group_by() function of dplyr**
library(stringr) # for string handling
library(DBI) # SQLite
library(tidyr) # data tidy for outputs
library(ggplot2) ; library(grafify) ; library(gridExtra) ; library(ggpubr) ; library(cowplot) # visualization
library(parallel) # multi-core 
# set project dir
setwd("/Users/weierliu/Dropbox (YSE)/YASSP/PROJECTS/DOE_Road to Removal/case_studies/new_england_forests/FVS_simulation/rFVS/FVSProjects/new_run_index")

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Data.db')
stdids <- dbReadTable(db_conn,'FVS_StandInit') %>% select(c(STAND_ID,GROUPS))
dbDisconnect(db_conn)
spp_grp <- read.csv('spp_grp.csv')

dbid <- c('./NMND.db','./NMLD.db','./NMHD.db',
          './SLND.db','./SLLD.db','./SLHD.db',
          './SWND.db','./SWLD.db','./SWHD.db')
################################################################################

############################## Effects of Cutting ##############################
################################################################################
# Carbon pools NO DISTURBANCE ####

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
cmp_carbon <- dbReadTable(db_conn,'cmp_carbon')
dbDisconnect(db_conn)

c <- cmp_carbon %>% filter(grepl("ND",cmp_carbon$MgmtID,fixed = TRUE)==TRUE)
c$MgmtID <- ifelse(grepl("NM",c$MgmtID,fixed = TRUE)==TRUE,"A) No Cut",
                   ifelse(grepl("SL",c$MgmtID,fixed = TRUE)==TRUE,
                          "B) Diameter-Limit Timber Cut", "C) Shelterwood Regeneration Cut"))
c$Year <- c$Year-2025
c$pool <- factor(c$pool, levels = c("Substitution Energy", "Substitution Material",
                                    "Harvested Wood Product", "Landfill",
                                    "Aboveground Live","Standing Dead", "Down Woody Material",
                                    "Belowground Live","Belowground Dead"),
                 labels = c("Avoided Emission - Energy", "Avoided Emission - Product",
                            "Wood Product", "Landfill",
                            "Aboveground Live","Standing Dead", "Down Woody Material",
                            "Belowground Live","Belowground Dead"))

sc <- c %>% filter(MgmtID=="A) No Cut") %>% 
  group_by(Year) %>% summarise(carbon=sum(carbon))

p_p <- ggplot() +
  geom_area(data=c,aes(x=Year,y=carbon,fill=pool),linewidth=0.1,color="White") + 
  scale_fill_manual(values = c('#5498CC','#F9BC80','#DC835F','#A0CFEF','#46AA64','#BCD1A3','#AF80B2','#A08B74','#A7AA99'),
                    name=element_blank()) + 
  geom_line(data=sc,aes(x=Year,y=carbon, linetype = "Baseline"), linewidth = 0.4) +
  scale_linetype_manual(values = c("Baseline"="solid"))+
  ylim(0,350) + facet_wrap( ~ MgmtID, labeller = label_wrap_gen(width = 35, multi_line = TRUE)) +
  labs(y = paste("Carbon Storage", "\n", "(tonnes carbon per hectare)"),
       x = "Years After Cutting", linetype = element_blank()) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE),linetype="none") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text=element_text(size=10), legend.text=element_text(size=8), legend.title=element_text(size=6),
                     legend.position = "bottom", legend.key.size = unit(0.5, "lines"),
                     legend.background = element_rect(fill='transparent'))
ggsave('./Figure/1_pools_ND.jpg',p_p,dpi=300,width = 6.2, height = 3.1, units="in")

# BA increment & C sequestration rates ####

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
p <- dbReadTable(db_conn,'growth_rate') %>% filter(grepl("ND",MgmtID,fixed = TRUE)==TRUE)
dbDisconnect(db_conn)

# basal area increament
p_ba <-ggplot(data=p,aes(x=Year,y=ba_rate,color=MgmtID)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_point() + geom_line() + xlim(0,100) +
  ylab(paste("Basal Area Increment","\n","(square foot per acre per year)")) + scale_y_continuous(position = "right") +
  xlab("Years After Cutting") +
  geom_errorbar(aes(ymin=ba_rate-ba_sd,ymax=ba_rate+ba_sd),alpha=0.2,width=0,position=position_dodge(1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "bottom", legend.key.size = unit(1, "lines"),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=10))
p_ba
# Carbon sequestration
p_c <-ggplot(data=p,aes(x=Year,y=c_rate,color=MgmtID)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_point() + geom_line() + xlim(0,100) +
  geom_hline(yintercept=0, linewidth=0.25) +
  ylab(paste("Forest Carbon Sequestration","\n","(ton C per hectare per year)")) +
  xlab("Years After Cutting") + 
  geom_errorbar(aes(ymin=c_rate-c_sd,ymax=c_rate+c_sd),alpha=0.2,width=0,position=position_dodge(1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "bottom",legend.key.size = unit(1, "lines"),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=10)) +
  guides(color = guide_legend(byrow = TRUE))
p_c
# combine
pr <- ggarrange(p_c,p_ba,ncol=2,labels="AUTO",hjust = -1, vjust = 1.6,common.legend = TRUE,legend = "bottom")
pr
ggsave('./Figure/2_growth_rate.jpg',pr,dpi=300,width = 6, height = 2.8, units="in")

# H indexes NO DISTURBANCE ####

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
h <- dbReadTable(db_conn,'cmp_h_index')
dbDisconnect(db_conn)

h$Year <- as.numeric(h$Year)-2025
h <- h %>% filter(grepl("ND",MgmtID,fixed = TRUE)==TRUE)

p_spp <-ggplot(data=h,aes(x=Year,group=MgmtID)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_line(aes(y=h_spp,color=MgmtID)) + 
  geom_ribbon(aes(y=h_spp,ymin=h_spp-sd_hspp,ymax=h_spp+sd_hspp,fill=MgmtID),alpha = 0.1) +
  scale_fill_manual(values=c('#5498CC','#DC835F','#46AA64')) +
  geom_point(aes(y=n_spp/5,color=MgmtID)) + 
  geom_errorbar(aes(ymin=(n_spp-sd_nspp)/5,ymax=(n_spp+sd_nspp)/5,color=MgmtID),alpha=0.2,width=0,position=position_dodge(1)) +
  scale_y_continuous(name = "H Index - Species", sec.axis = sec_axis(trans = ~.*5, name = "Number of Species")) +
  xlab("Years After Cutting") + guides(fill="none") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "bottom", legend.key.size = unit(0.5, "lines"),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=10))
p_spp

p_dbh <-ggplot(data=h,aes(x=Year,group=MgmtID)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_line(aes(y=h_dbh,color=MgmtID)) + 
  geom_ribbon(aes(y=h_dbh,ymin=h_dbh-sd_hdbh,ymax=h_dbh+sd_hdbh,fill=MgmtID),alpha = 0.1) +
  scale_fill_manual(values=c('#5498CC','#DC835F','#46AA64')) +
  geom_point(aes(y=n_dbh/5,color=MgmtID)) + 
  geom_errorbar(aes(ymin=(n_dbh-sd_ndbh)/5,ymax=(n_dbh+sd_ndbh)/5,color=MgmtID),alpha=0.2,width=0,position=position_dodge(1)) +
  scale_y_continuous(name = "H Index - DBH Class", sec.axis = sec_axis(trans = ~.*5, name = "Number of DBH Classes")) +
  xlab("Years After Cutting") + guides(fill="none") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "bottom", text=element_text(size=10),
                     legend.background = element_rect(fill='transparent'))
p_dbh

ph <- ggarrange(p_dbh,p_spp,ncol=2,labels="AUTO",common.legend = TRUE,legend = "bottom")
ph
ggsave('./Figure/3_H_index.jpg',ph,dpi=300,width = 6, height = 2.8, units="in")

# arrange plots ####
p_all <- ggarrange(pc,pr,ph,nrow = 3,heights = c(1.5,1,1.2),align = "v")
p_all
ggsave('./Figure/Figure1.jpg',p_all,dpi=300,width = 6, height = 9, units="in")



################################################################################

############################ Effects of Disturbance ############################
################################################################################
# Data prep
prep <- function(data) {
  data$Mgmt=NA;data$Dist=NA;data$MgmtID=as.character(data$MgmtID)
  for (i in 1:nrow(data)) {
    if (grepl("NM",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="No Cut"}
    if (grepl("SL",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="Diameter-Limit Timber Cut"}
    if (grepl("SW",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="Shelterwood Regeneration Cut"}
    if (grepl("ND",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="No Disturbance"}
    if (grepl("LD",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="Extreme Weather"}
    if (grepl("HD",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="Extreme Weather & Pests & Diseases"}
  }
  data$Dist <- factor(data$Dist, levels = c("No Disturbance", "Extreme Weather", "Extreme Weather & Pests & Diseases"))
  data$Mgmt <- factor(data$Mgmt, levels = c("No Cut", "Diameter-Limit Timber Cut", "Shelterwood Regeneration Cut"))
  return(data)
}
# Carbon pools DISTURBANCE ####

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
cmp_carbon <- dbReadTable(db_conn,'cmp_carbon')
dbDisconnect(db_conn)

c <- cmp_carbon %>% filter(grepl("ND",cmp_carbon$MgmtID,fixed = TRUE)==FALSE)
c$Dist <- ifelse(grepl("LD",c$MgmtID,fixed = TRUE)==TRUE,"D) Extreme Weather",
                   "E) Extreme Weather & Pests & Diseases")
c$MgmtID <- ifelse(grepl("NM",c$MgmtID,fixed = TRUE)==TRUE,"A) No Cut",
                   ifelse(grepl("SL",c$MgmtID,fixed = TRUE)==TRUE,
                          "B) Diameter-Limit Timber Cut", "C) Shelterwood Regeneration Cut"))
c$Year <- c$Year-2025
c$pool <- factor(c$pool, levels = c("Substitution Energy", "Substitution Material",
                                    "Harvested Wood Product", "Landfill",
                                    "Aboveground Live","Standing Dead", "Down Woody Material",
                                    "Belowground Live","Belowground Dead"),
                 labels = c("Avoided Emission - Energy", "Avoided Emission - Product",
                            "Wood Product", "Landfill",
                            "Aboveground Live","Standing Dead", "Down Woody Material",
                            "Belowground Live","Belowground Dead"))

sc <- c %>% filter(MgmtID=="A) No Cut") %>% 
  group_by(Dist,Year) %>% summarise(carbon=sum(carbon))

p_p <- ggplot() +
  geom_area(data=c,aes(x=Year,y=carbon,fill=pool),linewidth=0.1,color="White") + 
  scale_fill_manual(values = c('#5498CC','#F9BC80','#DC835F','#A0CFEF','#46AA64','#BCD1A3','#AF80B2','#A08B74','#A7AA99'),
                    name=element_blank()) + 
  geom_line(data=sc,aes(x=Year,y=carbon, linetype = "Baseline"), linewidth = 0.4) +
  scale_linetype_manual(values = c("Baseline"="solid"))+
  ylim(0,350) + facet_grid(Dist ~ MgmtID, labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
  labs(y = paste("Carbon Storage", "\n", "(tonnes carbon per hectare)"),
       x = "Years After Cutting", linetype = element_blank()) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE),linetype="none") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     strip.text = element_text(face="bold"),
                     text=element_text(size=10), legend.text=element_text(size=8), legend.title=element_text(size=6),
                     legend.position = "bottom", legend.key.size = unit(0.5, "lines"),
                     legend.background = element_rect(fill='transparent')) ;p_p
ggsave('./Figure/4_pools_D.jpg',p_p,dpi=300,width = 6, height = 4, units="in")
# BA increment & C sequestration rates DISTURBANCE ####

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
p <- dbReadTable(db_conn,'growth_rate') %>% filter(grepl("ND",MgmtID,fixed = TRUE)==FALSE) %>% prep() 
dbDisconnect(db_conn)

# basal area increament
p_ba <-ggplot(data=p,aes(x=Year,y=ba_rate,color=Mgmt)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank()) + 
  geom_line(aes(linetype=Dist)) +
  scale_linetype_manual(values=c('solid','dashed'),name=element_blank()) +
  geom_point() + xlim(0,100) +
  ylab(paste("Basal Area Increment","\n","(square foot per acre per year)")) + scale_y_continuous(position = "right") +
  xlab("Years After Cutting") +
  geom_errorbar(aes(ymin=ba_rate-ba_sd,ymax=ba_rate+ba_sd),alpha=0.2,width=0,position=position_dodge(1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none", legend.key.size = unit(0.5, "lines"),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=10))+
  guides(color = guide_legend(nrow=3,byrow = TRUE),linetype = guide_legend(nrow=3,byrow = TRUE))
p_ba
#ggsave("ba_rate.jpg",p_ba,dpi=300)

# Carbon sequestration
p_c <-ggplot(data=p,aes(x=Year,y=c_rate,color=Mgmt)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_point() + geom_line(aes(linetype=Dist)) + xlim(0,100) +
  scale_linetype_manual(values=c('solid','dashed'),name=element_blank()) +
  ylab(paste("Forest Carbon Sequestration","\n","(ton per hectare per year)")) +
  xlab("Years After Cutting") + 
  geom_errorbar(aes(ymin=c_rate-c_sd,ymax=c_rate+c_sd),alpha=0.2,width=0,position=position_dodge(1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none",legend.key.size = unit(0.5, "lines"),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=10)) +
  guides(color = guide_legend(nrow=3,byrow = TRUE),linetype = guide_legend(nrow=3,byrow = TRUE))
p_c
#ggsave("c_rate.jpg",p_c,dpi=300)

# combine
pr <- ggarrange(p_c,p_ba,ncol=2,labels="AUTO",common.legend = TRUE, legend = "bottom")
pr
ggsave('./Figure/5_growth_rate_D.jpg',pr,dpi=300,width = 6, height = 3, units="in")

# H indexes DISTURBANCE ####

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
h <- dbReadTable(db_conn,'cmp_h_index')
dbDisconnect(db_conn)

h$Year <- as.numeric(h$Year)-2025
h <- h %>% filter(grepl("LD",MgmtID,fixed = TRUE)==TRUE)

p_spp <-ggplot(data=h,aes(x=Year,group=MgmtID)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_line(aes(y=h_spp,color=MgmtID)) + 
  geom_ribbon(aes(y=h_spp,ymin=h_spp-sd_hspp,ymax=h_spp+sd_hspp,fill=MgmtID),alpha = 0.1) +
  scale_fill_manual(values=c('#5498CC','#DC835F','#46AA64')) +
  geom_point(aes(y=n_spp/5,color=MgmtID)) + 
  geom_errorbar(aes(ymin=(n_spp-sd_nspp)/5,ymax=(n_spp+sd_nspp)/5,color=MgmtID),alpha=0.2,width=0,position=position_dodge(1)) +
  scale_y_continuous(name = "H Index - Species", sec.axis = sec_axis(trans = ~.*5, name = "Number of Species")) +
  xlab("Years After Cutting") + guides(fill="none") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "bottom", legend.key.size = unit(0.5, "lines"),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=10))
p_spp

p_dbh <-ggplot(data=h,aes(x=Year,group=MgmtID)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name=element_blank(),
                     labels=c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut")) + 
  geom_line(aes(y=h_dbh,color=MgmtID)) + 
  geom_ribbon(aes(y=h_dbh,ymin=h_dbh-sd_hdbh,ymax=h_dbh+sd_hdbh,fill=MgmtID),alpha = 0.1) +
  scale_fill_manual(values=c('#5498CC','#DC835F','#46AA64')) +
  geom_point(aes(y=n_dbh/5,color=MgmtID)) + 
  geom_errorbar(aes(ymin=(n_dbh-sd_ndbh)/5,ymax=(n_dbh+sd_ndbh)/5,color=MgmtID),alpha=0.2,width=0,position=position_dodge(1)) +
  scale_y_continuous(name = "H Index - DBH Class", sec.axis = sec_axis(trans = ~.*5, name = "Number of DBH Classes")) +
  xlab("Years After Cutting") + guides(fill="none") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "bottom", text=element_text(size=10),
                     legend.background = element_rect(fill='transparent'))
p_dbh

ph1 <- ggarrange(p_dbh,p_spp,ncol=2,labels=c("A","B"),common.legend = TRUE,legend = "none")
ph1<-annotate_figure(ph1, top = text_grob("Extreme Weather"))
ph2 <- ggarrange(p_dbh,p_spp,ncol=2,labels=c("C","D"),common.legend = TRUE,legend = "bottom")
ph2<-annotate_figure(ph2, top = text_grob("Extreme Weather & Pests & Diseases"))
ph <- ggarrange(ph1,ph2,nrow=2)
ph
ggsave('./Figure/6_H_index_D.jpg',ph,dpi=300,width = 6, height = 6, units="in")


################################################################################

############################ Overall Summary Figure ############################
################################################################################

# Total carbon all scenario ####
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
cmp_carbon <- dbReadTable(db_conn,'cmp_carbon')
dbDisconnect(db_conn)
# Data prep
prep <- function(data) {
  data$Mgmt=NA;data$Dist=NA;data$MgmtID=as.character(data$MgmtID)
  for (i in 1:nrow(data)) {
    if (grepl("NM",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="No Cut"}
    if (grepl("SL",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="Diameter-Limit Timber Cut"}
    if (grepl("SW",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="Shelterwood Regeneration Cut"}
    if (grepl("ND",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="No Disturbance"}
    if (grepl("LD",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="Extreme Weather"}
    if (grepl("HD",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="Extreme Weather & Pests & Diseases"}
  }
  data$Dist <- factor(data$Dist, levels = c("No Disturbance", "Extreme Weather", "Extreme Weather & Pests & Diseases"))
  data$Mgmt <- factor(data$Mgmt, levels = c("No Cut", "Diameter-Limit Timber Cut", "Shelterwood Regeneration Cut"))
  data$Year <- as.numeric(data$Year)-2025
  return(data)
}
# with substitution
tc_ws <- prep(cmp_carbon) %>% group_by(Mgmt,Dist,Year) %>% summarise(carbon=sum(carbon))
# without substitution
tc_ns <- prep(cmp_carbon) %>% filter(pool!="Substitution Energy" & pool!="Substitution Material") %>%
  group_by(Mgmt,Dist,Year) %>% summarise(carbon=sum(carbon))
# plot
p_tc_ws <- ggplot(data = tc_ws,aes(x=Year,y=carbon)) +
  geom_line(aes(color=Mgmt,linetype=Dist)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name="Forest Cutting Practice",
                     labels = function(x) str_wrap(x, width = 15))+
  scale_linetype_manual(values=c('solid','longdash','twodash'),name="Natural Disturbance Regime",
                        labels = function(x) str_wrap(x, width = 15))+
  ylim(0,350) +
  ylab(paste("Total Climate Benefit","\n","(ton C per hectare)")) + xlab("Years After Cutting") +
  guides(color=guide_legend(nrow=3,byrow=TRUE),linetype=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "right", legend.key.size = unit(0.5, "lines"),
                     legend.text = element_text(size=6),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=8))
p_tc_ws
p_tc_ns <- ggplot(data = tc_ns,aes(x=Year,y=carbon)) +
  geom_line(aes(color=Mgmt,linetype=Dist)) +
  scale_color_manual(values=c('#5498CC','#DC835F','#46AA64'),name="Forest Cutting Practice")+
  scale_linetype_manual(values=c('solid','longdash','twodash'),name="Natural Disturbance Regime")+
  ylim(0,350) +
  ylab(paste("Total Forest Carbon","\n","(ton C per hectare)")) + xlab("Years After Cutting") +
  guides(color=guide_legend(nrow=3,byrow=TRUE),linetype=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none", legend.key.size = unit(0.5, "lines"),
                     legend.text = element_text(size=6),
                     legend.background = element_rect(fill='transparent'),
                     text=element_text(size=8))
p_tc_ns
# combine
p_tc <- ggarrange(p_tc_ns,p_tc_ws,nrow=1,labels=c("A","B"),align = "h",
                  common.legend = TRUE, legend = "bottom")
p_tc
# save 
ggsave('./Figure/total_carbon.jpg',p_tc,dpi=300,width = 6, height = 3, units="in")

p_tc1 <- ggarrange(p_tc_ns,p_tc_ws,nrow=1,labels=c("A","B"),align = "h",
                   common.legend = TRUE, legend = "none") %>%
  annotate_figure(top = text_grob("Main Analysis"))
p_tc_all <- ggarrange(p_tc1,p_tc2,p_tc3, nrow=3, align="h",common.legend = TRUE, legend="bottom")
ggsave('/Users/weierliu/Dropbox (YSE)/YASSP/PROJECTS/DOE_Road to Removal/case_studies/new_england_forests/FVS_simulation/rFVS/FVSProjects/new_run_index/Figure/total_carbon_all.jpg',p_tc_all,dpi=300,width = 6, height = 9, units="in")

 ################################################################################

################################## Supplement ##################################
################################################################################

# Stand Dynamics ####
# Data prep
prep <- function(data) {
  data$Mgmt=NA;data$Dist=NA;data$MgmtID=as.character(data$MgmtID)
  data$ForTyp<-as.character(data$ForTyp)
  for (i in 1:nrow(data)) {
    if (grepl("NM",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="No Cut"}
    if (grepl("SL",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="Diameter-Limit Timber Cut"}
    if (grepl("SW",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Mgmt[i]="Shelterwood Regeneration Cut"}
    if (grepl("ND",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="No Disturbance"}
    if (grepl("LD",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="Extreme Weather"}
    if (grepl("HD",data$MgmtID[i],fixed = TRUE)==TRUE) {data$Dist[i]="Extreme Weather & Pests & Diseases"}
  }
  data$Dist <- factor(data$Dist, levels = c("No Disturbance", "Extreme Weather", "Extreme Weather & Pests & Diseases"))
  data$Mgmt <- factor(data$Mgmt, levels = c("No Cut", "Diameter-Limit Timber Cut", "Shelterwood Regeneration Cut"))
  data$ft <- factor(paste(data$ForTyp, data$Fertility, sep = " "), levels = c("Oak Fertile", "Oak Infertile", "NHW Fertile", "NHW Infertile"),
                    labels = c("Oak-Mixed Hardwood - 'Mesic'", "Oak-Mixed Hardwood - 'Xeric'", "Northern Hardwood - 'Mesic'", "Northern Hardwood - 'Xeric'"))
  data$Year <- as.numeric(data$Year)-2025
  return(data)
}
# read table
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
cmp_stdstk <- dbReadTable(db_conn,'cmp_stdstk')
dbDisconnect(db_conn)
# prepare data
data <- prep(cmp_stdstk)
data <- data %>% mutate(DBHclass=recode(DBHclass, '1'="< 12.5 cm",'2'="< 12.5 cm",'3'="< 12.5 cm",
                                        '4'="12.5 ~ 27.5 cm",'5'="12.5 ~ 27.5 cm",'6'="12.5 ~ 27.5 cm",
                                        '7'="27.5 ~ 52.5 cm",'8'="27.5 ~ 52.5 cm",'9'="27.5 ~ 52.5 cm",'10'="27.5 ~ 52.5 cm",'11'="27.5 ~ 52.5 cm",
                                        '12'="> 52.5 cm",'13'="> 52.5 cm",'14'="> 52.5 cm",'15'="> 52.5 cm",'16'="> 52.5 cm",
                                        '17'="> 52.5 cm",'18'="> 52.5 cm",'19'="> 52.5 cm",'20'="> 52.5 cm",'21'="> 52.5 cm"))
data0 <- data
# species
data <- data0 %>% group_by(Mgmt,Dist,ft,Year,spp) %>% summarise(BA=sum(BA)) %>% rename(group=spp)
# DBH
data <- data0 %>% group_by(Mgmt,Dist,ft,Year,DBHclass) %>% summarise(BA=sum(BA)) %>% rename(group=DBHclass)
# function 
p_s <- function(data=data,dt="No Disturbance") {
  # read data
  lvs <- levels(as.factor(data$group))
  nr <- ifelse(nlevels(as.factor(lvs))>4,2,1)
  sdata <- data %>% filter(Dist==dt) %>%
    # add 0 to empty levels
    spread(group, BA, fill = 0) %>%
    pivot_longer(cols = all_of(lvs),
                 names_to = "group",
                 values_to = "BA",
                 values_drop_na = TRUE)
  if(nlevels(as.factor(lvs))>4) {
    sdata$group <- factor(sdata$group, levels = c("Other","Hickory","Hemlock","Pine","Ash","Birch","Beech","Red Maple","Sugar Maple","Oak"))
  } else {sdata$group <- factor(sdata$group, levels=c("> 52.5 cm","27.5 ~ 52.5 cm","12.5 ~ 27.5 cm","< 12.5 cm"))}
  rdata <- sdata %>% filter(Mgmt=="No Cut") %>% group_by(ft,Year) %>% summarise(BA=sum(BA))
  # plot
  p <- ggplot() +
    geom_area(data=sdata,aes(x=Year,y=BA,fill=group),linewidth=0.1,color="White") + 
    scale_fill_grafify(palette = "fishy",name=element_blank()) +
    # add reference line (NMND)
    geom_line(data=rdata,aes(x=Year,y=BA, linetype = "Baseline"), linewidth = 0.4) +
    scale_linetype_manual(values = c("Baseline"="solid"))+
    # wrap figures
    facet_grid(Mgmt ~ ft,labeller = label_wrap_gen(width = 19, multi_line = TRUE)) +
    # add labels
    labs(title = paste(dt),
         y = expression(paste("Total Basal Area (",ft^2,acre^-1,")")),
         x = "Year",
         linetype = element_blank()) +
    guides(fill=guide_legend(nrow=nr,byrow=TRUE),linetype="none") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                       text=element_text(size=10), legend.text=element_text(size=8), legend.title=element_text(size=6),
                       legend.position = "bottom", legend.key.size = unit(0.5, "lines"),
                       legend.background = element_rect(fill='transparent'))
  return(p)
}
# plots
p1 <- p_s(data = data,"No Disturbance") ; print(p1) 
p2 <- p_s(data = data,"Extreme Weather") ; print(p2)
p3 <- p_s(data = data,"Extreme Weather & Pests & Diseases") ; print(p3)
# save spp
ggsave('./Figure/SI/spp_nd.jpg',p1,dpi=300,width = 5.5, height = 4.5, units="in") ; ggsave(
  './Figure/SI/spp_ld.jpg',p2,dpi=300,width = 5.5, height = 4.5, units="in") ; ggsave(
    './Figure/SI/spp_hd.jpg',p3,dpi=300,width = 5.5, height = 4.5, units="in")
# save dbh
ggsave('./Figure/SI/dbh_nd.jpg',p1,dpi=300,width = 5.5, height = 4.5, units="in");ggsave(
  './Figure/SI/dbh_ld.jpg',p2,dpi=300,width = 5.5, height = 4.5, units="in");ggsave(
    './Figure/SI/dbh_hd.jpg',p3,dpi=300,width = 5.5, height = 4.5, units="in")
