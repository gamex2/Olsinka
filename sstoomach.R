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
all_seining_lo <- merge(all_seining, all_sampling[, .(sa_samplingid, lo_nameczech, sa_date_start, year)], by = "sa_samplingid")
  
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
all_trawling <- merge(all_trawling, all_sampling[, .(sa_samplingid, lo_nameczech, sa_date_start, year)], by = "sa_samplingid")

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
# all_catch <- data.table(dbGetQuery(conn = con, statement = paste("SELECT * FROM fishecu.catch;")))
# write.xlsx(all_catch, here::here('all_catch.xlsx'))
# all_catch_lip <- all_catch[grep("LIP", all_catch$sa_samplingid), ]
# write.xlsx(all_catch_lip, here::here('all_catch_lip.xlsx'))
all_catch_lip <- setDT(readxl::read_xlsx(here::here('all_catch_lip.xlsx')))
all_gill_sto[, year := year(sa_date_start)]
irisSubset_ca <- merge(all_catch_lip[, .(sa_samplingid, ct_catchid, sp_speciesid, ct_sl, ct_weight, ct_diet, ct_sex, ct_envelope, ct_envelope_fishid)], 
                       all_sampling[, .(sa_samplingid, lo_nameczech, sa_date_start, year)],
                       by = "sa_samplingid")
irisSubset_at <- merge(irisSubset_ca, all_trawling[, .(sa_samplingid, dl_depthlayerid)],
                       by = "sa_samplingid", all.x = T)
irisSubset_ca_sa <- irisSubset_ca[year == 2021]
all_gill_sto[sa_samplingid == "F2012LIP-AT9"]
all_trawling[sa_samplingid == "F2012LIP-AT9"]
all_seining_lo[sa_samplingid == "F2012LIP-AT9"]

#finding fish####
irisSubset_ca_sa[sp_speciesid == "candat" & ct_sl == 69]
fish_diet_2019_2021 <- setDT(readxl::read_xlsx(here::here('fish_diet_2019_2021.xlsx')))
fish_diet_2019_2021 <- setDT(fish_diet_2019_2021)
fish_diet_2019_2021_2 <- merge(fish_diet_2019_2021, irisSubset_ca[, .(sa_samplingid, ct_catchid, year, lo_nameczech, ct_sex)], by = "ct_catchid")
fish_diet_2019_2021_2 <- merge(fish_diet_2019_2021_2, all_seining[, .(sa_samplingid, bsg_gearid)], by = "sa_samplingid", all.x = T)
fish_diet_2019_2021_2 <- merge(fish_diet_2019_2021_2, all_gill_sto[, .(sa_samplingid, dl_depthlayerid, gg_gearid)], by = "sa_samplingid", all.x = T)
write.xlsx(fish_diet_2019_2021_2, here::here('fish_diet_2019_2021_2.xlsx'))

Stomach_content_km <- setDT(read_excel("Stomach_content_km.xlsx", sheet = "all_stom"))
Stomach_content_km <- merge(Stomach_content_km, specs[, .(sp_scientificname, sp_speciesid)], by = "sp_scientificname", all.x = T)
Stomach_content_km <- merge(Stomach_content_km, all_seining[, .(sa_samplingid, bsg_gearid)], by = "sa_samplingid", all.x = T)
Stomach_content_km <- merge(Stomach_content_km, all_gill_sto[, .(sa_samplingid, dl_depthlayerid, gg_gearid, lo_nameczech, sa_date_start)], by = "sa_samplingid", all.x = T)
write.xlsx(Stomach_content_km, here::here('Stomach_content_km2.xlsx'))

Fish_diet_Bydz <- setDT(read_excel("Fish_diet_Bydz.xlsx"))
Fish_diet_Bydz <- merge(Fish_diet_Bydz, all_catch_lip[, .(ct_catchid, ct_sex, sa_samplingid)], by = "ct_catchid")
Fish_diet_Bydz <- merge(Fish_diet_Bydz, all_seining[, .(sa_samplingid, bsg_gearid)], by = "sa_samplingid", all.x = T)
Fish_diet_Bydz <- merge(Fish_diet_Bydz, all_gill_sto[, .(sa_samplingid, dl_depthlayerid, gg_gearid, lo_nameczech, sa_date_start)], by = "sa_samplingid", all.x = T)
Fish_diet_Bydz[, year := year(sa_date_start)]
write.xlsx(Fish_diet_Bydz, here::here('Fish_diet_Bydz2.xlsx'))

#Protected area graph
MPA <- setDT(read.delim(here::here('MPA.txt')))
setnames(x = MPA, old = c('Record.Count', "Publication.Years"), new = c('MPA', "Year"))
MPA <- MPA[,-3]
FPA <- setDT(read.delim(here::here('FPA.txt')))
setnames(x = FPA, old = c('Record.Count', "Publication.Years"), new = c('FPA', "Year"))
FPA <- FPA[,-3]
PA.pubc <- merge(MPA, FPA, all.x = T, by = "Year")
PA.pubc[is.na(PA.pubc)] <- 0
PA.pubc <- PA.pubc[-35,]
PA.melt <- melt(PA.pubc, id = "Year")
setnames(x = PA.melt, old = 'value', new = 'Publications')
PA.melt <- setDT(PA.melt)
PA.melt <- mutate(PA.melt, log_var = log(Publications+1))
ggplot(data = PA.melt, 
       aes(x = Year, y = log_var, fill = variable)) + 
  geom_bar(stat="identity", position = position_dodge()) + 
  geom_text(aes(label=Publications, y = log_var + 0.25), position = position_dodge(width = 0.9), size = 6.5, angle = 90) +
  labs(y = 'Log(n) publications per year', fill = "Area type")+
  scale_fill_viridis_d(option = 'E')+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        legend.title = element_text(size=28),
        legend.text = element_text(size = 26, face = "italic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#area graph#area graphlog10()
Fresh <- setDT(read.delim(here::here('Fresh.txt')))
setnames(x = Fresh, old = c('Record.Count', "Publication.Years"), new = c('Freshwater', "Year"))
Fresh <- Fresh[,-3]
Marine <- setDT(read.delim(here::here('Marine.txt')))
setnames(x = Marine, old = c('Record.Count', "Publication.Years"), new = c('Marine', "Year"))
Marine <- Marine[,-3]
PA.pubc <- merge(Marine, Fresh, all = T, by = "Year")
PA.pubc[is.na(PA.pubc)] <- 0
PA.pubc <- PA.pubc[!Year == 2024]
PA.melt <- melt(PA.pubc, id = "Year")
setnames(x = PA.melt, old = 'value', new = 'Publications')
ggplot(data = PA.melt, 
       aes(x = Year, y = Publications, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(y = 'Publications per year', fill = "Environment")+
  scale_fill_viridis_d(option = 'E')+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        legend.title = element_text(size=28),
        legend.text = element_text(size = 26, face = "italic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#data analises stomach####
Stomach_content_all <- setDT(readxl::read_xlsx(here::here('Stomach.content.all.xlsx')))
Stomach_content_all[, 12:68][is.na(Stomach_content_all[, 12:68])] <- 0
Stomach_content_all[["Sex"]][is.na(Stomach_content_all[["Sex"]])] <- "X"
Stomach_content_all$Sex[Stomach_content_all$Sex == "Male"] <- "M"
Stomach_content_all$Sex[Stomach_content_all$Sex == "Female"] <- "F"
Stomach_content_all$Sex[Stomach_content_all$Sex == "?"] <- "X"
colnames(Stomach_content_all)[which(colnames(Stomach_content_all) %in% c("SL (mm)","Weight (g)", "Stomach content") )] <- c("SL","Wg", "Stomach_content")
Stomach_content_all$Stomach_content[Stomach_content_all$Stomach_content == 0] <- "no"
Stomach_content_all$Stomach_content[Stomach_content_all$Stomach_content == 1] <- "yes"

ggplot(Stomach_content_all, aes(Species, fill = Gear)) + 
  geom_histogram(position = "stack", stat = "count")+
  scale_fill_viridis_d(option = 'C')+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        legend.title = element_text(size=28),
        legend.text = element_text(size = 26, face = "italic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(Stomach_content_all, aes(Species, fill = Stomach_content)) + 
  geom_histogram(position = "stack", stat = "count") +
  scale_fill_viridis_d(option = 'C') +
  labs(fill = "Stomach content")+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        legend.title = element_text(size=28),
        legend.text = element_text(size = 26, face = "italic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  

#Fish####
Stomach_content_fish <- Stomach_content_all[Stomach_content_all$Fish==1, ]
n_na <- sum(is.na(Stomach_content_fish$ct_catchid))
Stomach_content_fish <- merge(Stomach_content_fish, all_catch_lip[,.(ct_catchid,ct_agegroup)], by = "ct_catchid", all.x = T)
Stomach_content_fish$ct_catchid[is.na(Stomach_content_fish$ct_catchid)] <- paste0("Fish_", seq_len(n_na))
Stomach_content_fish2 <- Stomach_content_fish %>% 
  mutate(cannibal = case_when(Species == "candat" & candat != 0 ~ 1, #condition 1
                         Species == "okoun" & okoun != 0 ~ 1,
                         TRUE ~ 0)) #all other cases
Stomach_content_fish2$cannibal[Stomach_content_fish2$cannibal == 0] <- "no"
Stomach_content_fish2$cannibal[Stomach_content_fish2$cannibal == 1] <- "yes"
Stomach_melt_fish <- setDT(melt(Stomach_content_fish2[, .(ct_catchid, SL, Year, Gear, Species, candat, ouklej, okoun, plotice, jezdik, cejn, cejnek, pstruh, kaprovitka, okounovitá, Unknown, ct_agegroup)], 
                          id.vars = c("ct_catchid", "Year", "Gear", "Species", "SL", "ct_agegroup"), variable.name = "fish_sto"))

ggplot(Stomach_melt_fish[!value == 0], aes(fill=fish_sto, y=value, x=Species)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Year, scales = "free") +
  labs(fill = "Sp stomach")+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        legend.title = element_text(size=28),
        legend.text = element_text(size = 26, face = "italic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


ggplot(Stomach_content_fish2[Species %in% c ("candat", "okoun")], aes(Species, fill = forcats::fct_rev(cannibal))) + 
  geom_histogram(position = "stack", stat = "count")+
 facet_wrap(~Year, scales = "free") +
scale_fill_viridis_d(option = 'E') +
  labs(fill = "Cannibalism")+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
        axis.text.y = element_text(size = 28),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        legend.title = element_text(size=28),
        legend.text = element_text(size = 26, face = "italic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(checking[Species == "candat"], aes(x = SL, fill = check1)) + 
  geom_histogram(bins = 300)+
  facet_grid(rows = "ct_agegroup")

new_tb <- merge(Stomach_content_all[,.(ct_catchid, Species, Year, SL, Wg)], 
                irisSubset_ca[,.(ct_catchid, sp_speciesid, year, ct_sl, ct_weight)], by = "ct_catchid")
checking <- new_tb %>%
mutate(check1 = case_when(Wg == ct_weight ~ "ok_checked", 
                          Wg != ct_weight ~ "danger", 
                        TRUE ~ NA_character_))   
 unique(checking$check1) 
hist(Stomach_content_all$Stomach_content)
count(Stomach_content_all$Stomach_content)
table(data.frame(Stomach_content_all[1], value = unlist(Stomach_content_all[-1])))
all_sampling <- all_sampling[, -c(5:9)]
#Weigth calculation####
#candat 2010
c10 <- irisSubset_ca%>%
  dplyr::filter(sp_speciesid == "sumec" & year == 2010 & !is.na(ct_weight))
c10$logL <- log(c10$ct_sl)
c10$logW <- log(c10$ct_weight)
models <- lapply(split(c10, c10$sp_speciesid), 'lm', formula = logW ~ logL)

#Sander Lucioperca
mydata <- setDT(structure(list(ct_sl = c(580, 246, 2)), 
                          class = "data.table"))
mydata[, ct_wg_comp := predict.lm(object = models$`sumec`,
                                       newdata = data.frame(logL = log(mydata$ct_sl)))]
mydata$ct_wg_comp <- round(exp(mydata$ct_wg_comp), 2)


