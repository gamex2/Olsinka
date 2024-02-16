source(here::here('packages.R'))
source(here::here('functions.R'))

#Data from the database#####
# con <- dbConnect(PostgreSQL(), dbname = "fishecu_complex-survey_db", user = "fishecuuser", host = "172.21.3.20", password = "f1sh3cuus3r!")
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
all_sampling <- merge(all_sampling, all_locality, by = "lo_localityid")
all_sampling[, year := year(sa_date_start)]

# Gillnet deployments####
# all_gill_sto <- data.table(dbGetQuery(conn = con, statement = "SELECT *
# FROM fishecu.gillnet_sampling;"))
# all_gill_sto <- all_gill_sto[grep("LIP", all_gill_sto$sa_samplingid), ]
# write.xlsx(all_gill_sto, here::here('all_gill_sto.xlsx'))
all_gill_sto <- setDT(readxl::read_xlsx(here::here('all_gill_sto.xlsx')))
all_gill_sto <- merge(all_gill_sto, all_sampling[, .(sa_samplingid, sa_date_start, lo_nameczech)], by = "sa_samplingid")

#Catches from gillnet####
# catches_gillnet <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch
#                                                                   WHERE  sa_samplingid IN ('",paste(all_gill_sto$sa_samplingid, collapse = "','"), "')
#                                                                   ;", sep = "")))
# write.xlsx(catches_gillnet, here::here('catches_gillnet.xlsx'))
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

#All catch####
all_catch <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch;")))
write.xlsx(all_catch, here::here('all_catch.xlsx'))
all_catch_lip <- all_catch[grep("LIP", all_catch$sa_samplingid), ]
# write.xlsx(all_catch_lip, here::here('all_catch_lip.xlsx'))
all_catch_lip <- setDT(readxl::read_xlsx(here::here('all_catch_lip.xlsx')))
all_gill_sto[, year := year(sa_date_start)]
irisSubset_ca <- merge(all_catch_lip[, .(sa_samplingid, ct_catchid, sp_speciesid, ct_sl, ct_weight, ct_diet)], 
                       all_sampling[, .(sa_samplingid, lo_nameczech, sa_date_start, year)],
                       by = "sa_samplingid")
irisSubset_ca_sa <- irisSubset_ca[year == 2021]


#finding fish####
irisSubset_ca_sa[sp_speciesid == "candat" & ct_sl == 69]


