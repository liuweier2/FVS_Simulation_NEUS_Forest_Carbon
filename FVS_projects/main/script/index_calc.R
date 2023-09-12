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

mgmtids <- c("NMND","NMLD","NMHD","SLND","SLLD","SLHD","SWND","SWLD","SWHD")
dbid <- c('./DBout/NMND.db','./DBout/NMLD.db','./DBout/NMHD.db',
          './DBout/SLND.db','./DBout/SLLD.db','./DBout/SLHD.db',
          './DBout/SWND.db','./DBout/SWLD.db','./DBout/SWHD.db')
stdstkids <- c('stdstk_NMND','stdstk_NMLD','stdstk_NMHD',
               'stdstk_SLND','stdstk_SLLD','stdstk_SLHD',
               'stdstk_SWND','stdstk_SWLD','stdstk_SWHD')


# Stand Stock table (DBH class) ####
stdstk_calc <- function(n) {
db_conn <- dbConnect(RSQLite::SQLite(), dbid[n])
ids <- dbReadTable(db_conn,'FVS_Cases') %>% select(c(CaseID,StandID,MgmtID)) %>% left_join(stdids,by=c('StandID'='STAND_ID'))
ids$ForTyp <- NA ; ids$Fertility <- NA
for (i in 1:nrow(ids)) {
  if (grepl("Oak",ids$GROUPS[i])==TRUE) {ids$ForTyp[i]<-"Oak"}
  if (grepl("NHW",ids$GROUPS[i])==TRUE) {ids$ForTyp[i]<-"NHW"}
  if (grepl("Fertile",ids$GROUPS[i])==TRUE) {ids$Fertility[i]<-"Fertile"}
  if (grepl("Infertile",ids$GROUPS[i])==TRUE) {ids$Fertility[i]<-"Infertile"}
}
ids <- ids %>% select(-c(GROUPS))
treelist <- dbReadTable(db_conn,'FVS_TreeList_East') 
if (n>3) {
  cutlist <- dbReadTable(db_conn,'FVS_CutList_East') %>% select(CaseID,Year,TreeId,TreeIndex)
  cutlist$cut <- 1
  treelist <- treelist %>% left_join(cutlist) %>% filter(is.na(cut)==TRUE)
}
treelist <- treelist %>% left_join(ids) %>% 
  select(c(MgmtID,ForTyp,Fertility,StandID,Year,SpeciesPLANTS,SpeciesFVS,SpeciesFIA,TPA,DBH))
stdstk <- treelist %>% filter(Year>=2023) %>%
  mutate(DBHclass=findInterval(DBH,c(0,seq(0.98,40,by=1.97)))) %>% #compute DBH classes in 5cm interval (2.5-7.5;7.5-12.5;...cm)
  group_by(MgmtID,ForTyp,Fertility,StandID,Year,SpeciesPLANTS,SpeciesFVS,SpeciesFIA,DBHclass) %>% 
  summarise(liveTPA=sum(TPA),liveBA=sum(DBH*DBH*.005454154*TPA)) %>%
  left_join(spp_grp)
stdstk$spp_grp <- replace_na(stdstk$spp_grp,"Other")
#return(stdstk)
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,paste("stdstk_",mgmtids[n],sep=""),stdstk,overwrite=TRUE)
dbDisconnect(db_conn)
rm(treelist)
rm(cutlist)
}

# compute per mgmtID
start_time <- Sys.time() ; r <- lapply(1:9, function(n) {
  stdstk_calc(n)
  gc()
}) ; print(Sys.time() - start_time)
# n_species total = 67

# Combine stdstk
tb <- c("stdstk_NMND","stdstk_NMLD","stdstk_NMHD",
        "stdstk_SLND","stdstk_SLLD","stdstk_SLHD",
        "stdstk_SWND","stdstk_SWLD","stdstk_SWHD")
r <- lapply(1:9, function(n){
  db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
  d <- dbReadTable(db_conn,tb[n])
  dbDisconnect(db_conn)
  return(d)})
r <- bind_rows(r)
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,'stdstk',r,overwrite=TRUE)
dbDisconnect(db_conn)

# Composite Stand and Stock Table ####
cmp_stdstk_calc <- function(stdstk) {
  stdstk_all = stdstk %>%
    group_by(MgmtID,ForTyp,Fertility,StandID,Year,spp_grp,DBHclass) %>%
    summarise(liveTPA=sum(liveTPA),liveBA=sum(liveBA)) 
  stdstk_all = stdstk_all %>% group_by(MgmtID,ForTyp,Fertility,Year) %>%
    mutate(nstand=nlevels(as.factor(StandID)))
  cmp_stdstk = stdstk_all %>% group_by(MgmtID,ForTyp,Fertility,Year,spp_grp,DBHclass) %>%
    summarise(TPA=sum(liveTPA)/mean(nstand),BA=sum(liveBA)/mean(nstand)) %>%
    rename(spp=spp_grp)
  return(cmp_stdstk)
}


start_time <- Sys.time() ; r <- mclapply(1:9, function(n) {
  db_conn = dbConnect(RSQLite::SQLite(), './FVS_Out.db')
  stdstk_temp = dbReadTable(db_conn,stdstkids[n])
  dbDisconnect(db_conn)
  cmp_stdstk = cmp_stdstk_calc(stdstk_temp)
  rm(stdstk_temp) 
  return(cmp_stdstk)
  gc()            
}, mc.cores = 5) ; print(Sys.time() - start_time)

r <- bind_rows(r)
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,'cmp_stdstk',r,overwrite=TRUE)
dbDisconnect(db_conn)

# Diversity index calculation ####
# Shannon Index Species 
hspp_calc <- function(stdstk) {
  stdstk_spp = stdstk %>%
    filter(liveBA!=0) %>%
    group_by(MgmtID,ForTyp,Fertility,StandID,Year,SpeciesFVS) %>%
    summarise(BA=sum(liveBA)) %>%
    ungroup() %>% group_by(MgmtID,ForTyp,Fertility,StandID,Year) %>%
    mutate(totalBA=sum(BA),n_spp=nlevels(as.factor(SpeciesFVS)))
  stdstk_spp$percBA = stdstk_spp$BA/stdstk_spp$totalBA
  hspp = stdstk_spp %>%
    group_by(MgmtID,ForTyp,Fertility,StandID,Year) %>%
    summarise(hspp = -sum(percBA*log(percBA)), n_spp=mean(n_spp))
  t1 <- hspp %>% select(-c(n_spp)) %>% spread(Year, hspp, fill = 0) %>% 
    pivot_longer(cols = c("2023":"2130"), names_to = "Year", values_to = "hspp", values_drop_na = TRUE)
  t2 <- hspp %>% select(-c(hspp)) %>% spread(Year, n_spp, fill = 0) %>% 
    pivot_longer(cols = c("2023":"2130"), names_to = "Year", values_to = "n_spp", values_drop_na = TRUE)
  hspp <- left_join(t1,t2)
  db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
  dbWriteTable(db_conn,'H_spp',hspp,overwrite=FALSE,append=TRUE)
  dbDisconnect(db_conn)
}
# Shannon Index DBH Class
hdbh_calc <- function(stdstk) {
  stdstk_dbh = stdstk %>%
    filter(liveBA!=0) %>%
    group_by(MgmtID,ForTyp,Fertility,StandID,Year,DBHclass) %>%
    summarise(BA=sum(liveBA)) %>%
    ungroup() %>% group_by(MgmtID,ForTyp,Fertility,StandID,Year) %>%
    mutate(totalBA=sum(BA),n_dbh=nlevels(as.factor(DBHclass)))
  stdstk_dbh$percBA = stdstk_dbh$BA/stdstk_dbh$totalBA
  hdbh = stdstk_dbh %>%
    group_by(MgmtID,ForTyp,Fertility,StandID,Year) %>%
    summarise(hdbh = -sum(percBA*log(percBA)),n_dbh = mean(n_dbh))
  t1 <- hdbh %>% select(-c(n_dbh)) %>% spread(Year, hdbh, fill = 0) %>% 
    pivot_longer(cols = c("2023":"2130"), names_to = "Year", values_to = "hdbh", values_drop_na = TRUE)
  t2 <- hdbh %>% select(-c(hdbh)) %>% spread(Year, n_dbh, fill = 0) %>% 
    pivot_longer(cols = c("2023":"2130"), names_to = "Year", values_to = "n_dbh", values_drop_na = TRUE)
  hdbh <- left_join(t1,t2)
  db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
  dbWriteTable(db_conn,'H_dbh',hdbh,overwrite=FALSE,append=TRUE)
  dbDisconnect(db_conn)
}
# Calculate per stand
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbRemoveTable(db_conn,'H_spp',c,overwrite=FALSE,append=TRUE)
dbRemoveTable(db_conn,'H_dbh',c,overwrite=FALSE,append=TRUE)
dbDisconnect(db_conn)

start_time <- Sys.time() ; r <- lapply(1:9, function(n) {
  db_conn = dbConnect(RSQLite::SQLite(), './FVS_Out.db')
  stdstk_temp = dbReadTable(db_conn,stdstkids[n])
  dbDisconnect(db_conn)
  hspp_calc(stdstk_temp)
  gc()
  hdbh_calc(stdstk_temp)
  gc()                       # clear RAM (TreeList & StdStk tables are massive, have to compute one at a time)
  rm(stdstk_temp)            # (this function is tested applicable on a Mac with 32Gb RAM)
}) ; print(Sys.time() - start_time)

# Composite Diversity Index ####
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
hspp <- dbReadTable(db_conn,'H_spp') 
hdbh <- dbReadTable(db_conn,'H_dbh') 
dbDisconnect(db_conn)
cmp_h <- left_join(hspp,hdbh) %>%
  #group_by(MgmtID,ForTyp,Fertility,Year) %>%
  group_by(MgmtID,Year) %>%
  summarise(sd_nspp=sd(n_spp),sd_ndbh=sd(n_dbh),sd_hdbh=sd(hdbh),sd_hspp=sd(hspp),
            h_spp=mean(hspp),h_dbh=mean(hdbh),n_spp=mean(n_spp),n_dbh=mean(n_dbh))
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,'cmp_h_index',cmp_h,overwrite=TRUE)
dbDisconnect(db_conn)

# Composite Harvest Table ####
harv_calc <- function(n) {
  db_conn <- dbConnect(RSQLite::SQLite(), dbid[n])
  ids <- dbReadTable(db_conn,'FVS_Cases') %>% select(c(CaseID,StandID,MgmtID)) %>% left_join(stdids,by=c('StandID'='STAND_ID'))
  ids$ForTyp <- NA ; ids$Fertility <- NA
  for (i in 1:nrow(ids)) {
    if (grepl("Oak",ids$GROUPS[i])==TRUE) {ids$ForTyp[i]<-"Oak"}
    if (grepl("NHW",ids$GROUPS[i])==TRUE) {ids$ForTyp[i]<-"NHW"}
    if (grepl("Fertile",ids$GROUPS[i])==TRUE) {ids$Fertility[i]<-"Fertile"}
    if (grepl("Infertile",ids$GROUPS[i])==TRUE) {ids$Fertility[i]<-"Infertile"}
  }
  ids <- ids %>% select(-c(GROUPS))
  cutbf <- dbReadTable(db_conn,'FVS_Compute') %>% left_join(ids) %>% select(c(StandID,Year,MgmtID,ForTyp,Fertility,CUTBF))
  cutbf <- subset(cutbf,cutbf$Year>=2023&is.na(cutbf$CUTBF)!=TRUE)
  cmp_cutbf <- cutbf %>% select(-c(StandID)) %>% 
    group_by(MgmtID,ForTyp,Year) %>% summarize(volume_mbf=mean(CUTBF),sd_mbf=sd(CUTBF),n=n())
  cmp_cutbf$volume_m3 <- 0.0833333*cmp_cutbf$volume_mbf
  cmp_cutbf$sd_m3 <- 0.0833333*cmp_cutbf$sd_mbf
  return(cmp_cutbf)
}

start_time <- Sys.time() ; cmp_harv_list <- lapply(1:9, function(n) {
  harv_calc(n)
}) ; print(Sys.time() - start_time)

cmp_harv <- bind_rows(cmp_harv_list)

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,'cmp_harv',cmp_harv,overwrite=TRUE)
dbDisconnect(db_conn)

# Carbon Table ####
carbon_calc <- function(n) {
  db_conn <- dbConnect(RSQLite::SQLite(), dbid[n])
  ids <- dbReadTable(db_conn,'FVS_Cases') %>% select(c(CaseID,StandID,MgmtID)) %>% left_join(stdids,by=c('StandID'='STAND_ID'))
  ids$ForTyp <- NA ; ids$Fertility <- NA
  for (i in 1:nrow(ids)) {
    if (grepl("Oak",ids$GROUPS[i])==TRUE) {ids$ForTyp[i]<-"Oak"}
    if (grepl("NHW",ids$GROUPS[i])==TRUE) {ids$ForTyp[i]<-"NHW"}
    if (grepl("Fertile",ids$GROUPS[i])==TRUE) {ids$Fertility[i]<-"Fertile"}
    if (grepl("Infertile",ids$GROUPS[i])==TRUE) {ids$Fertility[i]<-"Infertile"}
  }
  ids <- ids %>% select(-c(GROUPS))
  carbon <- dbReadTable(db_conn,'FVS_Carbon') %>% left_join(ids) %>% select(-c(CaseID))
  hrvc <- dbReadTable(db_conn,'FVS_Hrv_Carbon') %>% left_join(ids) %>% select(-c(CaseID))
  carbon <- subset(carbon,carbon$Year>=2023)
  carbon$Aboveground_Live <- carbon$Aboveground_Total_Live
  carbon$Belowground_Live <- carbon$Belowground_Live
  carbon$Belowground_Dead <- carbon$Belowground_Dead
  carbon$Standing_Dead <- carbon$Standing_Dead
  carbon$Down_Woody_Material <- carbon$Forest_Shrub_Herb+carbon$Forest_Down_Dead_Wood+carbon$Forest_Floor
  carbon <- carbon %>% select(-c(Total_Removed_Carbon,Aboveground_Total_Live,Forest_Down_Dead_Wood,Forest_Floor,Forest_Shrub_Herb,
                                         Total_Stand_Carbon,Carbon_Released_From_Fire,Aboveground_Merch_Live))
  # calculate harvested carbon in categories 
  hrvc <- subset(hrvc,hrvc$Year>=2023)
  colnames(hrvc)
  hrvc$Harvested_Wood_Product <- hrvc$Products
  hrvc$Substitution_Energy <- 0.87*hrvc$Energy
  hrvc$Substitution_Material <- 1.6*hrvc$Merch_Carbon_Stored
  hrvc <- hrvc %>% select(-c(Products,Emissions,Merch_Carbon_Stored,Merch_Carbon_Removed,Energy))
  c <- left_join(carbon,hrvc)
  db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
  dbWriteTable(db_conn,'carbon',c,overwrite=FALSE,append=TRUE)
  dbDisconnect(db_conn)
}

db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbRemoveTable(db_conn,'carbon',c,overwrite=FALSE,append=TRUE)
dbDisconnect(db_conn)

start_time <- Sys.time() ; r <- lapply(1:9, function(n) {
  carbon_calc(n)
  gc()            
}) ; print(Sys.time() - start_time)

# Composite
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
carbon <- dbReadTable(db_conn,'carbon')
dbDisconnect(db_conn)

cmp_carbon <- carbon %>% 
  #select(-c(StandID)) %>%
  #group_by(MgmtID,ForTyp,Fertility,Year) %>%
  select(-c(StandID,ForTyp,Fertility)) %>%
  group_by(MgmtID,Year) %>%
  summarize(across(everything(),.fns=mean))
# transform Cartesian table to indexed table using tidyr::pivot_longer
cmp_carbon <- cmp_carbon %>% 
  pivot_longer(
    cols = c(Belowground_Live:Substitution_Material),
    names_to = "pool",
    values_to = "carbon",
    values_drop_na = TRUE)
cmp_carbon$pool <- gsub("_"," ",cmp_carbon$pool)
levels(as.factor(cmp_carbon$pool))
cmp_carbon$carbon <- cmp_carbon$carbon * 2.2417 #from us ton/acre to t/ha
# write table to db
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,'cmp_carbon',cmp_carbon,overwrite=TRUE)
dbDisconnect(db_conn)

# Growth rates ####
# Basal Area Increment
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
d <- dbReadTable(db_conn,'stdstk')
dbDisconnect(db_conn)
c_d <- d %>% 
  group_by(MgmtID,ForTyp,Fertility,StandID,Year) %>%
  summarise(BA=sum(liveBA)) %>% 
  #filter(MgmtID=="NMND" | MgmtID=="SLND" | MgmtID=="SWND") %>%
  spread(Year, BA, fill = 0) %>%
  pivot_longer(cols = c("2023":"2130"),
               names_to = "Year",
               values_to = "BA",
               values_drop_na = TRUE) %>%
  mutate(BA_rate=(BA-lag(BA))/(as.numeric(Year)-lag(as.numeric(Year)))) %>% filter(is.na(BA_rate)==FALSE)
ba_rate <- c_d %>% filter(Year>2023 & Year<2130) %>%
  group_by(MgmtID,Year) %>%
  summarise(ba_rate=mean(BA_rate),ba_sd=sd(BA_rate))
ba_rate$Year <- as.numeric(ba_rate$Year)-2025
# Carbon Sequestration 
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
d <- dbReadTable(db_conn,'carbon')
dbDisconnect(db_conn)
c_d <- d %>% 
  group_by(MgmtID,ForTyp,Fertility,StandID,Year) %>%
  summarise(live_c=Belowground_Live+Aboveground_Live +
              Belowground_Dead+Standing_Dead+Down_Woody_Material
              ) %>% 
  #filter(MgmtID=="NMND" | MgmtID=="SLND" | MgmtID=="SWND") %>%
  spread(Year, live_c, fill = 0) %>%
  pivot_longer(cols = c("2023":"2125"),
               names_to = "Year",
               values_to = "live_c",
               values_drop_na = TRUE) %>%
  mutate(C_rate=(live_c-lag(live_c))/(as.numeric(Year)-lag(as.numeric(Year)))) #%>% filter(is.na(C_rate)==FALSE)
c_rate <- c_d %>% filter(Year>2023) %>%
  group_by(MgmtID,Year) %>%
  summarise(c_rate=mean(C_rate),c_sd=sd(C_rate))
c_rate$Year <- as.numeric(c_rate$Year) - 2025

p <- left_join(c_rate,ba_rate) 
for (i in 1:nrow(p)) {
  if (p$MgmtID[i]=="SLND") {
    if (p$Year[i]==0|p$Year[i]==25|p$Year[i]==50|p$Year[i]==75|p$Year[i]==100) {
      p$c_rate[i]=NA;p$c_sd[i]=NA;p$ba_rate[i]=NA;p$ba_sd[i]=NA}
  } else if (p$MgmtID[i]=="SWND") {
    if (p$Year[i]==0|p$Year[i]==55|p$Year[i]==80) {
      p$c_rate[i]=NA;p$c_sd[i]=NA;p$ba_rate[i]=NA;p$ba_sd[i]=NA}
  }
}



db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbWriteTable(db_conn,'growth_rate',p,overwrite=TRUE)
dbDisconnect(db_conn)

# Synthesize indices (radar plot) ####
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Out.db')
dbListTables(db_conn)
h <- dbReadTable(db_conn,'cmp_h_index')
c <- dbReadTable(db_conn,'cmp_carbon')

colnames(h)
h <- h %>% select(c(MgmtID, Year, h_spp,n_spp,h_dbh,n_dbh)) %>%
  filter(Year==2050|Year==2125)

colnames(c)
c <- c %>% filter(Year==2050|Year==2125) %>%
  mutate(pool=recode(pool,"Harvested Wood Product"="HWP","Landfill"="HWP","Substitution Energy"="HWP","Substitution Material"="HWP",
                     "Aboveground Live"="FC","Belowground Dead"="FC","Belowground Live"="FC",
                     "Down Woody Material"="FC","Standing Dead"="FC")) %>%
  group_by(MgmtID,Year,pool) %>% summarise(carbon=sum(carbon))
c <- c %>% pivot_wider(names_from = pool,values_from = carbon)

h$Year<-as.numeric(h$Year)
c$Year<-as.numeric(c$Year)

s <- left_join(c,h)

s$Year<-s$Year-2025
s$Mgmt <- substr(s$MgmtID,1,2)
s$Dist <- substr(s$MgmtID,3,4)
s <- s %>% ungroup() %>% select(-c(MgmtID))
s0 <- s

library(ggradar)
library(scales)

s <- s0 %>% mutate_at(vars(FC,HWP,h_spp,n_spp,h_dbh,n_dbh),rescale) %>%
  mutate(Mgmt=recode(Mgmt,"NM"="No Cut","SL"="Diameter-Limit Timber Cut","SW"="Shelterwood Regeneration Cut")) %>%
  rename(group=Mgmt,"Forest Carbon"=FC,"Product"=HWP,"H-spp"=h_spp,
         "N-spp"=n_spp,"H-DBH"=h_dbh,"N-DBH"=n_dbh) %>% relocate(group,.before="Forest Carbon")
s$group <- factor(s$group,levels = c("No Cut","Diameter-Limit Timber Cut","Shelterwood Regeneration Cut"))

radar <- function(ss) {
p<-ggradar(ss, 
        grid.min = 0, grid.mid = 0.5, grid.max = 1,
        group.line.width = 1, 
        group.point.size = 0,
        group.colours = c('#5498CC','#DC835F','#46AA64'),
        # Background and grid lines
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        legend.position = "bottom",
        fill=TRUE,fill.alpha=0.2,
        base.size = 4,
        grid.label.size = 0)
return(p)
}

ss <- s %>% filter(Year==25 & Dist=="ND") %>%
  select(-c(Year,Dist)) 
p1 <- radar(ss) 

ss <- s %>% filter(Year==25 & Dist=="LD") %>%
  select(-c(Year,Dist)) 
p2 <- radar(ss) 

ss <- s %>% filter(Year==25 & Dist=="HD") %>%
  select(-c(Year,Dist)) 
p3 <- radar(ss)

ss <- s %>% filter(Year==100 & Dist=="ND") %>%
  select(-c(Year,Dist)) 
p4 <- radar(ss)

ss <- s %>% filter(Year==100 & Dist=="LD") %>%
  select(-c(Year,Dist)) 
p5 <- radar(ss)

ss <- s %>% filter(Year==100 & Dist=="HD") %>%
  select(-c(Year,Dist)) 
p6 <- radar(ss)

p_radar<-ggarrange(p1,p2,p3,p4,p5,p6,nrow = 2, ncol = 3,
                   labels = "AUTO",common.legend = TRUE,legend = "bottom")
p_radar
ggsave('radar.jpg',p_radar,dpi=300,width = 6, height = 6, units="in")
