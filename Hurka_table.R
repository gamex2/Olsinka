source(here::here('packages.R'))
source(here::here('functions.R'))
 
#Data from the database#####
# con <- dbConnect(PostgreSQL(), dbname = "fishecu_complex-survey_db", user = "fishecuuser", host = "172.21.3.20", password = "f1sh3cuus3r!")

#Connecting to the database to extract info about Lipno
#Latin names
# specs <- data.table(dbGetQuery(conn = con, statement =  "SELECT * FROM fishecu.species;"))#to see the whole table (reservoir). Important to see the reservoir ID number.
# specs <- specs[,c(1,2,10)]
# write.xlsx(specs, here::here('specs.xlsx'))
specs <- setDT(readxl::read_xlsx(here::here('specs.xlsx')))

#Gillnet deployments####
# all_gill_depl <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.gillnet_sampling_merged_wide
# WHERE reservoirid IN (2);"))
# write.xlsx(all_gill_depl, here::here('all_gill_depl.xlsx'))
all_gill_depl <- setDT(readxl::read_xlsx(here::here('all_gill_depl.xlsx')))

#Vilém's function renaming
setnames(x = all_gill_depl,old = 'gearsize', new = 'Effort')#changing the name of effort to Effort to allow the function to run
all_gill_depl[, year := year(date_start)]

#localities
all_gill_depl_hurka <- all_gill_depl[localityid == 10]#selecting the localities
all_gill_depl_hurka$dl_layertype[all_gill_depl_hurka$dl_layertype == "benthic"] <- "Benthic"
all_gill_depl_hurka$dl_layertype[all_gill_depl_hurka$dl_layertype == "pelagic"] <- "Pelagic"

#Catches ####
# catches_db <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch_merged
#                                                                   WHERE  sa_samplingid IN ('",paste(all_gill_depl$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_db, here::here('catches_db.xlsx'))
catches_db <- setDT(readxl::read_xlsx(here::here('catches_db.xlsx')))

# dbDisconnect(con)

#separating the target species
catches_db <- merge(catches_db, specs[, .(sp_speciesid, sp_scientificname, sp_taxonomicorder)], by='sp_speciesid')
catches_db <- catches_db[!sp_speciesid == 'EA']
catches_db <- catches_db[!ct_agegroup == 'YOY']
# catches_db <- catches_db[!sp_scientificname %in% c("Abramis sp.", "Abramis x Blicca", "Abramis brama x Blicca bjoerkna", "Abramis brama x Rutilus rutilus",
#                                                    "Rutilus rutilus x Blicca bjoerkna", "Tinca tinca", "Gobio gobio", "Anguilla anguilla", "Carassius gibelio",
#                                                    "Hypophthalmichthys nobilis", "Coregonus", "Oncorhynchus mykiss", "Hypophthalmichthys molitrix")]

#insert impact
catches_hurka <- merge(catches_db, all_gill_depl_hurka[, .(sa_samplingid, year, locality, dl_layertype, depthlayerid)], by = "sa_samplingid")
catches_hurka <- catches_hurka[!is.na(catches_hurka$ct_weightstar),]
catches_hurka[, sp_grouped := fct_lump(f = sp_scientificname, prop = 0.01, w = ct_weightstar, other_level = "Others")]
#Calcule BPUE mean
bpue_hurk <- getVPUE(samplings = all_gill_depl_hurka, catch = catches_hurka, 
                      split.factors.samplings = c("locality", "year", "dl_layertype"),
                      split.factors.catch = c("sp_grouped"),
                      id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE
cpue_hurk <- getVPUE(samplings = all_gill_depl_hurka, catch = catches_hurka, 
                      split.factors.samplings = c("locality", "year", "dl_layertype"),
                      split.factors.catch = c("sp_grouped"),
                      id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

vpue_hurk <- merge(cpue_hurk, bpue_hurk)
#changing the name of variables
setnames(x = vpue_hurk, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),
         new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
vpue_hurk[, ':='(cpue_mean = cpue_mean*1000)]
dcast_vpue_hurk <- dcast(data = vpue_hurk, formula = locality + year + dl_layertype ~ sp_grouped,
                            value.var = "bpue_mean", )
write.xlsx(dcast_vpue_hurk, here::here('dcast_vpue_hurk.xlsx'))
