source(here::here('packages.R'))
source(here::here('functions.R'))

#Data from the database#####
con <- dbConnect(PostgreSQL(), dbname = "fishecu_complex-survey_db", user = "fishecuuser", host = "172.21.3.20", password = "f1sh3cuus3r!")
# dbDisconnect(con)
#Connecting to the database to extract info about Lipno
#Latin names
# specs <- data.table(dbGetQuery(conn = con, statement =  "SELECT * FROM fishecu.species;"))#to see the whole table (reservoir). Important to see the reservoir ID number.
# specs <- specs[,c(1,2,10)]
# write.xlsx(specs, here::here('specs.xlsx'))
specs <- setDT(readxl::read_xlsx(here::here('specs.xlsx')))

# Gillnet deployments####
# all_gill_sto <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.gillnet_sampling;"))
# all_gill_sto <- all_gill_sto[grep("LIP", all_gill_sto$sa_samplingid), ]
# write.xlsx(all_gill_sto, here::here('all_gill_sto.xlsx'))
all_gill_sto <- setDT(readxl::read_xlsx(here::here('all_gill_sto.xlsx')))
# merge_map <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.gillnet_merge_map;"))
#beachsein
# all_seining <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.beachsein_sampling;"))
# all_trawling <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.trawl_sampling;")))
# write.xlsx(all_trawling, here::here('all_trawling.xlsx'))
all_trawling <- setDT(readxl::read_xlsx(here::here('all_trawling.xlsx')))
irisSubset_tra <- all_trawling[grep("F2010LIP", all_trawling$sa_samplingid), ]
all_locality <- data.table(dbGetQuery(conn = con, statement = "SELECT *
FROM fishecu.locality;"))
sein_sampl <- merge(all_seining, all_sampling, by = 'sa_samplingid')
sein_sampl[, year := year(sa_date_start)]
sein_sampl <- sein_sampl[year == 2016]
sein_sampl <- merge(sein_sampl, all_locality, by = 'lo_localityid')
sein_sampl <- sein_sampl[lo_localityid == 154]
# 
# catches_db <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_gill_sto$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
write.xlsx(catches_db, here::here('all_catch.xlsx'))
all_catch <- setDT(readxl::read_xlsx(here::here('all_catch.xlsx')))
all_catch <- merge(all_catch, specs[, .(sp_speciesid, sp_scientificname, sp_taxonomicorder)], by='sp_speciesid')
irisSubset_sa <- all_gill_sto[grep("F2010LIP", all_gill_sto$sa_samplingid), ]
irisSubset_ca <- all_catch[grep("F2010LIP", all_catch$sa_samplingid), ]
irisSubset_ca_sa <- merge(irisSubset_ca, irisSubset_sa[, .(sa_samplingid, date_start, locality)], by = "sa_samplingid")
fish_diet_2019_2021 <- setDT(read_excel("fish_diet_2019_2021.xlsx"))
fish_diet_2019_2021_new <- merge(fish_diet_2019_2021, all_catch[, .(sa_samplingid, ct_catchid, sp_speciesid, ct_abundance, ct_sl, ct_tl,
                                                                    ct_weight, ct_diet, ct_envelope, ct_envelope_fishid, ct_agegroup)], 
                                 by = c("sp_speciesid", "ct_sl", "ct_weight"), all.x = T)

Stomach_content <- setDT(read_excel("Stomach content.xlsx", sheet = "all"))
Stomach_content <- merge(Stomach_content, all_catch[, .(ct_catchid, sa_samplingid, sp_scientificname, ct_tl, ct_weight, ct_sex, ct_agegroup)], by = 'ct_catchid')
write.xlsx(Stomach_content, here::here('Stomach_content.xlsx'))
catches_sei <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
                                                                  WHERE  sa_samplingid IN ('",paste(sein_sampl$sa_samplingid, collapse = "','"), "')
                                                                  ;", sep = "")))

#Vilém's function renaming
setnames(x = all_gill_depl,old = 'gearsize', new = 'Effort')#changing the name of effort to Effort to allow the function to run
all_sampling[, year := year(date_start)]
