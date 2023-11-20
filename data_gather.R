source(here::here('packages.R'))
source(here::here('functions.R'))

#Data from the database#####
con <- dbConnect(PostgreSQL(), dbname = "fishecu_complex-survey_db", user = "fishecuuser", host = "172.21.3.20", password = "f1sh3cuus3r!")

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
all_gill_depl <- all_gill_depl[localityid %in% c(9, 10, 12, 13)]#selecting the localities
all_gill_depl <- all_gill_depl[year %in% c(2008, 2009, 2010, 2012, 2016, 2017)]#year that olsinka was sampled
all_gill_depl$dl_layertype[all_gill_depl$dl_layertype == "benthic"] <- "Benthic"
all_gill_depl$dl_layertype[all_gill_depl$dl_layertype == "pelagic"] <- "Pelagic"

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
# catches_db <- catches_db[!sp_scientificname %in% c("Abramis sp.", "Abramis x Blicca", "Abramis brama x Blicca bjoerkna", "Abramis brama x Rutilus rutilus",
#                                                    "Rutilus rutilus x Blicca bjoerkna", "Tinca tinca", "Gobio gobio", "Anguilla anguilla", "Carassius gibelio",
#                                                    "Hypophthalmichthys nobilis", "Coregonus", "Oncorhynchus mykiss", "Hypophthalmichthys molitrix")]

#insert impact
catches_olsinka <- merge(catches_db, all_gill_depl[, .(sa_samplingid, year, locality, dl_layertype)], by = "sa_samplingid")

#VPUE filter####
#Calcule BPUE
bpue_lip <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
                    split.factors.samplings = c("locality", "year"),
                    split.factors.catch = c("sa_samplingid"),
                    id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE
cpue_lip <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
                    split.factors.samplings = c("locality", "year"),
                    split.factors.catch = c("sa_samplingid"),
                    id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

olsinka_vpue <- merge(cpue_lip, bpue_lip)
#changing the name of variables
setnames(x = olsinka_vpue, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),
         new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
olsinka_vpue[, ':='(cpue_mean = cpue_mean*1000, bpue_se = bpue_se*1000)]

#Calcule BPUE mean
bpue_lipm <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
                    split.factors.samplings = c("sa_samplingid", "locality", "year"),
                    split.factors.catch = c("sp_scientificname"),
                    id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE
cpue_lipm <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
                    split.factors.samplings = c("sa_samplingid", "locality", "year"),
                    split.factors.catch = c("sp_scientificname"),
                    id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

olsinka_vpuem <- merge(cpue_lipm, bpue_lipm)
#changing the name of variables
setnames(x = olsinka_vpuem, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),
         new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
olsinka_vpuem[, ':='(cpue_mean = cpue_mean*1000, bpue_se = bpue_se*1000)]

#
# olsinka_t <- dcast(data = olsinka_vpuem, formula = sp_scientificname ~ locality + year,
#                    value.var = "bpue_mean")
# write.xlsx(olsinka_t, here::here('olsinka_tb.xlsx'))

#Mean####
mean_size_olsinka <- catches_olsinka[!ct_sl == 0,.(Mean = round(mean(ct_sl, na.rm = T), 2),
                                                     SE = round(plotrix::std.error(ct_sl), 2),
                                                     Max = max(ct_sl),
                                                     Min = min(ct_sl)),
                                       by =.(year, locality, sp_scientificname)]
mean_size_olsinka[is.na(mean_size_olsinka)] <- 0
olsinka_mt <- dcast(data = mean_size_olsinka, formula = sp_scientificname ~ locality + year,
                   value.var = "Mean")
# write.xlsx(olsinka_mt, here::here('olsinka_mt.xlsx'))

mean_weight_olsinka <- catches_olsinka[!ct_weight == 0,.(Mean = round(mean(ct_weight, na.rm = T), 2),
                                             SE = round(plotrix::std.error(ct_weight), 2),
                                             Max = max(ct_weight),
                                             Min = min(ct_weight)),
                                       by =.(year, locality, sp_scientificname)]
mean_weight_olsinka[is.na(mean_weight_olsinka)] <- 0
olsinka_wt <- dcast(data = mean_weight_olsinka, formula = sp_scientificname ~ locality + year,
                    value.var = "Mean")
# write.xlsx(olsinka_wt, here::here('olsinka_wt.xlsx'))



