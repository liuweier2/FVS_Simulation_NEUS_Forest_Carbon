# Setup environment ####
library(rFVS) # see 'setup.r' for installation of the package
#library(fvsOL) # not used here. don't know how to setup run using this
library(readr) # use readr::read_file() to load .txt strings for keyfile making
library(dplyr) # general data handling. ** note that rFVS loads plyr package that may mess up with the group_by() function of dplyr**
library(stringr) # for string handling
library(DBI) # SQLite
library(tidyr) # data tidy for outputs
library(ggplot2) ; library(grafify) ; library(gridExtra) ; library(ggpubr) ; library(cowplot) # visualization
library(parallel)  # multi-core 
library(parallelly)
# set project dir
setwd("/Users/weierliu/Dropbox (YSE)/YASSP/PROJECTS/DOE_Road to Removal/case_studies/new_england_forests/FVS_simulation/rFVS/FVSProjects/sensitivity_regen")
setwd("C:/Users/liuwe/Dropbox (YSE)/YASSP/PROJECTS/DOE_Road to Removal/case_studies/new_england_forests/FVS_simulation/rFVS/FVSProjects/sensitivity_regen")


# load keys ####
db <- read_file('./key/db.txt') # DATABASE keyword (input/output and SQLite query)
sl <- read_file('./key/treat_SL.txt') # treatments
sw_irr <- read_file('./key/treat_irrSW.txt')
sw_one <- read_file('./key/treat_oneSW.txt')
regen_oak_f <- read_file('./key/regen_oak_f.txt') # regeneration tally
regen_oak_if <- read_file('./key/regen_oak_if.txt')
regen_nhw_f <- read_file('./key/regen_nhw_f.txt')
regen_nhw_if <- read_file('./key/regen_nhw_if.txt')
bgregen_oak_f <- read_file('./key/bgregen_oak_f.txt') # background regeneration tally
bgregen_oak_if <- read_file('./key/bgregen_oak_if.txt')
bgregen_nhw_f <- read_file('./key/bgregen_nhw_f.txt')
bgregen_nhw_if <- read_file('./key/bgregen_nhw_if.txt')
dist_bio <- read_file('./key/dist_bio.txt') # mortality rates from disturbance
dist_drought <- read_file('./key/dist_drought.txt')
dist_wind <- read_file('./key/dist_wind.txt')
dist_low <- "low" ; dist_high <- "high"
cmp <- read_file('./key/COMPUTE.txt') # COMPUTE keyword (calculate output indices)


# function - build keyfile ####
makekey <- function(stdid="0009201807060100500444",inv_year=2018,mgmtid="NMND",treatment="!!!!!!!!",rotation=125,regen="!!!!!!!!",disturbance="!!!!!!!!",bgregen="!!!!!!!") {
  InvYear = paste("InvYear",str_pad(inv_year,11),sep = "")
  nyr1 <- 2023-as.numeric(inv_year) # calculate cycle length - cycle 1 simulate to 2023
  TimeInt = paste("TimeInt",str_pad("1",8),str_pad(nyr1,8),sep = "") #incert the exact number of spaces using stringr::str_pad() 
  if (disturbance == "low") {
    disturbance = paste(dist_drought,"ENDIF",
                        dist_wind,"ESTAB",bgregen,"END","ENDIF",
                        sep = "\n")}
  if (disturbance == "high") {
    disturbance = paste(dist_drought,"ENDIF",
                        dist_wind,"ESTAB",bgregen,"END","ENDIF",
                        dist_bio,"ENDIF",
                        sep = "\n")}
  keyfile = paste(
                  # paste Stand ID
                  "STDIDENT",
                  stdid,
                  "MgmtId",
                  mgmtid,
                  # paste Inventory Year and cycle length
                  InvYear,
                  paste("TimeInt",str_pad("5",16),sep = ""),
                  TimeInt,
                  paste("TimeInt",str_pad("2",8),str_pad("2",8),sep = ""), # cycle 2 from 2023 - 2025 (2 years)
                  paste("NumCycle",str_pad("23",7)), # 5-year cycles thereafter
                  # database keyword
                  "DATABASE",
                  "DSNOut",
                  paste("./DBout/",mgmtid,".db",sep=""), # output database per mgmtID
                  "Summary",
                  "Computdb",
                  "END",
                  db,
                  # treatment keyword
                  treatment,
                  # regeneration keyword 
                  paste("If",str_pad(rotation,18),sep = ""),
                  "year ge 2025", "Then",
                  "ESTAB", # call ESTAB - establishment model - to start; 2025 is treatment year
                  paste("SPROUT",str_pad("0",14),sep = ""), # set sprout to every cycle (0)
                  regen, # regeneration tally (per treatment per site type)
                  "END", "ENDIF", # end establishment model
                  # disturbance input
                  disturbance,
                  # COMPUTE keyword
                  cmp,
                  # end one stand
                  "PROCESS",
                  sep = "\n"
                  )
  return(keyfile)
}

# make all keys ####
# read (and filter) stand table from database 
db_conn <- dbConnect(RSQLite::SQLite(), './FVS_Data.db')
all_std <- dbReadTable(db_conn,'FVS_StandInit') %>% select(c(STAND_ID,INV_YEAR,GROUPS))
dbDisconnect(db_conn)

ids <- c("NMND","NMLD","NMHD","SLND","SLLD","SLHD","SWND","SWLD","SWHD")
mgmtid <- c('./keyfile/NMND.key','./keyfile/NMLD.key','./keyfile/NMHD.key',
            './keyfile/SLND.key','./keyfile/SLLD.key','./keyfile/SLHD.key',
            './keyfile/SWND.key','./keyfile/SWLD.key','./keyfile/SWHD.key')
treat <- c("!!!!","!!!!","!!!!",sl,sl,sl,sw_irr,sw_irr,sw_irr,"!!!!","!!!!","!!!!",sl,sl,sl,sw_one,sw_one,sw_one)
rt <- c(125,125,125,25,25,25,125,125,125,125,125,125,25,25,25,125,125,125)
dis <- c("!!!!","low","high","!!!!","low","high","!!!!","low","high")
rg <- c()

all_key <- function(n) {
  stdlist <- all_std %>% filter(str_detect(GROUPS,"Oak"))
  if (grepl("SW", ids[n], fixed = TRUE)==FALSE) {
    rg=c(bgregen_oak_if,bgregen_oak_f,bgregen_nhw_if,bgregen_nhw_f)
  } else {rg=c(regen_oak_if,regen_oak_f,regen_nhw_if,regen_nhw_f)}
  keyfile_all <- "!!!!START!!!!"
  for (i in 1:nrow(stdlist)) {
    if (str_detect(stdlist$GROUPS[i],"Infertile")==TRUE) {
      keyfile_temp <- makekey(stdid = stdlist$STAND_ID[i],inv_year = stdlist$INV_YEAR[i],mgmtid = ids[n],regen = rg[1],bgregen = bgregen_oak_if,treatment=treat[n],rotation=rt[n],disturbance=dis[n])
    }
    else {keyfile_temp <- makekey(stdid = stdlist$STAND_ID[i],inv_year = stdlist$INV_YEAR[i],mgmtid = ids[n],regen = rg[2],bgregen = bgregen_oak_f,treatment=treat[n],rotation=rt[n],disturbance=dis[n])}
    keyfile_all <- paste(keyfile_all,keyfile_temp,sep = "\n")
  }
  stdlist <- all_std %>% filter(str_detect(GROUPS,"NHW"))
  for (i in 1:nrow(stdlist)) {
    if (str_detect(stdlist$GROUPS[i],"Infertile")==TRUE) {
      keyfile_temp <- makekey(stdid = stdlist$STAND_ID[i],inv_year = stdlist$INV_YEAR[i],mgmtid = ids[n],regen = rg[3],bgregen = bgregen_nhw_if,treatment=treat[n+9],rotation=rt[n+9],disturbance=dis[n])
    }
    else {keyfile_temp <- makekey(stdid = stdlist$STAND_ID[i],inv_year = stdlist$INV_YEAR[i],mgmtid = ids[n],regen = rg[4],bgregen = bgregen_nhw_f,treatment=treat[n+9],rotation=rt[n+9],disturbance=dis[n])}
    keyfile_all <- paste(keyfile_all,keyfile_temp,sep = "\n")
  }
  keyfile_all <- paste(keyfile_all,"STOP",sep = "\n")
  writeLines(keyfile_all,mgmtid[n])
}

start_time <- Sys.time()
r <- mclapply(1:9,function(n) {
  all_key(n)
}, mc.cores = 6)
print(Sys.time() - start_time)

# Setup variant and functions #### 
fvsLoad("FVSne",bin="/Users/weierliu/Documents/GitHub/ForestVegetationSimulator/bin/")

stdno <- seq(1:nrow(all_std))
runFVS <- function(stdno) {
  fvsRun()
}

mgmtid <- c('--keywordfile=./keyfile/NMND.key','--keywordfile=./keyfile/NMLD.key','--keywordfile=./keyfile/NMHD.key',
            '--keywordfile=./keyfile/SLND.key','--keywordfile=./keyfile/SLLD.key','--keywordfile=./keyfile/SLHD.key',
            '--keywordfile=./keyfile/SWND.key','--keywordfile=./keyfile/SWLD.key','--keywordfile=./keyfile/SWHD.key')

# Parallel processing runs ####
start_time <- Sys.time()
r <- mclapply(1:9, function(n) {    
  fvsSetCmdLine(mgmtid[n])
  results <- lapply(stdno,runFVS)
  }, mc.cores=4)    
print(Sys.time() - start_time)

#####################################################################################################################################
#Windows
lapply(1:9, function(n){all_key(n)})

fvsLoad("FVSne",bin="C:/FVS/FVSSoftware/FVSbin")

availableCores()
cl <- makeCluster(12)
clusterEvalQ(cl, c(library(rFVS),fvsLoad("FVSne",bin="C:/FVS/FVSSoftware/FVSbin")))
clusterExport(cl, c("stdno", "runFVS","mgmtid",'.FVSLOADEDLIBRARY'), 
              envir=environment())

start_time <- Sys.time()
parLapply(cl,c(2,4,8,9),function(n) {
  fvsSetCmdLine(mgmtid[n])
  results <- lapply(stdno,runFVS)
})
print(Sys.time() - start_time)

stopCluster(cl)
