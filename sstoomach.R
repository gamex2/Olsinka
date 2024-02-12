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

#Localities
# all_locality <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.locality;"))
# all_locality <- all_locality[,c(1,3)]
# write.xlsx(all_locality, here::here('all_locality.xlsx'))
all_locality <- setDT(readxl::read_xlsx(here::here('all_locality.xlsx')))

#Sampling
# all_sampling <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.sampling;"))
# all_sampling <- all_sampling[grep("LIP", all_sampling$sa_samplingid), ]
# write.xlsx(all_sampling, here::here('all_sampling.xlsx'))
all_sampling <- setDT(readxl::read_xlsx(here::here('all_sampling.xlsx')))


# Gillnet deployments####
# all_gill_sto <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.gillnet_sampling;"))
# all_gill_sto <- all_gill_sto[grep("LIP", all_gill_sto$sa_samplingid), ]
# write.xlsx(all_gill_sto, here::here('all_gill_sto.xlsx'))
all_gill_sto <- setDT(readxl::read_xlsx(here::here('all_gill_sto.xlsx')))

#Catches from gillnet####
# catches_gillnet <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_gill_sto$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_gillnet, here::here('catches_gillnet.xlsx'))
# catches_gillnet <- setDT(readxl::read_xlsx(here::here('catches_gillnet.xlsx')))
catches_gillnet <- setDT(readxl::read_xlsx(here::here('catches_gillnet.xlsx')))


#beachsein####
# all_seining <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.beachsein_sampling;"))
# all_seining <- all_seining[grep("LIP", all_seining$sa_samplingid), ]
# write.xlsx(all_seining, here::here('all_seining.xlsx'))
all_seining <- setDT(readxl::read_xlsx(here::here('all_seining.xlsx')))

#Catches from beachsein
# catches_seining <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_seining$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_seining, here::here('catches_seining.xlsx'))
catches_seining <- setDT(readxl::read_xlsx(here::here('catches_seining.xlsx')))



#trawling####
# all_trawling <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.trawl_sampling;")))
# all_trawling <- all_trawling[grep("LIP", all_trawling$sa_samplingid), ]
# write.xlsx(all_trawling, here::here('all_trawling.xlsx'))
all_trawling <- setDT(readxl::read_xlsx(here::here('all_trawling.xlsx')))

#Catches from trawling
# catches_trawling <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_trawling$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_trawling, here::here('catches_trawling.xlsx'))
catches_trawling <- setDT(readxl::read_xlsx(here::here('catches_trawling.xlsx')))

#pas####
# all_pas <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.pas_sampling;")))
# all_pas <- all_pas[grep("LIP", all_pas$sa_samplingid), ]
# write.xlsx(all_pas, here::here('all_pas.xlsx'))
all_pas <- setDT(readxl::read_xlsx(here::here('all_pas.xlsx')))

#Catches from pas
# catches_pas <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_pas$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_pas, here::here('catches_pas.xlsx'))
catches_pas <- setDT(readxl::read_xlsx(here::here('catches_pas.xlsx')))





irisSubset_tra <- all_trawling[grep("F2010LIP", all_trawling$sa_samplingid), ]

sein_sampl <- merge(all_seining, all_sampling, by = 'sa_samplingid')
sein_sampl[, year := year(sa_date_start)]
sein_sampl <- sein_sampl[year == 2016]
sein_sampl <- merge(sein_sampl, all_locality, by = 'lo_localityid')
sein_sampl <- sein_sampl[lo_localityid == 154]
# 
# catches_db <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_gill_sto$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_db, here::here('all_catch.xlsx'))
all_catch <- setDT(readxl::read_xlsx(here::here('all_catch.xlsx')))
all_catch <- merge(all_catch, specs[, .(sp_speciesid, sp_scientificname, sp_taxonomicorder)], by='sp_speciesid')
irisSubset_sa <- all_gill_sto[grep("F2016LIP", all_gill_sto$sa_samplingid), ]
irisSubset_ca <- all_catch[grep("F2016LIP", all_catch$sa_samplingid), ]
irisSubset_ca_sa <- merge(irisSubset_ca, irisSubset_sa[, .(sa_samplingid, gi_deployment_date_start)], by = "sa_samplingid")

Stomach_content <- setDT(read_excel("Stomach content.xlsx", sheet = "all"))
Stomach_content <- merge(Stomach_content, all_catch[, .(ct_catchid, sa_samplingid, sp_scientificname, ct_tl, ct_weight, ct_sex, ct_agegroup)], by = 'ct_catchid')
write.xlsx(Stomach_content, here::here('Stomach_content.xlsx'))
catches_sei <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
                                                                  WHERE  sa_samplingid IN ('",paste(sein_sampl$sa_samplingid, collapse = "','"), "')
                                                                  ;", sep = "")))

all_sampling[, year := year(date_start)]
#finding fish####
irisSubset_ca_sa[sp_speciesid == "bolen" & ct_sl == 200]


