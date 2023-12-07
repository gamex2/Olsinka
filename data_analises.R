
olsinka_vpue$locality <- factor(olsinka_vpue$locality, levels = c("Olsinska zatoka", "Pritok", "Hurka", "Hraz"))
set.seed(3333)
lip_glm_bpue <- glm.nb(data = olsinka_vpue, formula = bpue_mean ~ locality)
summary(lip_glm_bpue)

pair_bpue_lip <- emmeans(lip_glm_bpue,~ locality, adjust="sidak", weights = "flat")
CLD_bpue_lip <- multcomp::cld(pair_bpue_lip, 
                              alpha=0.05,
                              Letters=letters)
setDT(CLD_bpue_lip)
setnames(x = CLD_bpue_lip, old = c('.group'),
         new = c('locality_g'))
olsinka_vpue3 <- merge(olsinka_vpue2, CLD_bpue_lip[, .(locality, locality_g)], by = c("locality"))

set.seed(3333)
lip_glm_bpue2 <- glm.nb(data = olsinka_vpue, formula = bpue_mean ~ locality + depthlayerid)
summary(lip_glm_bpue2)
pair_bpue_lip2 <- emmeans(lip_glm_bpue2,~ locality * depthlayerid, adjust="sidak", weights = "flat")
CLD_bpue_lip2 <- multcomp::cld(pair_bpue_lip2, 
                               alpha=0.05,
                               Letters=letters)
setDT(CLD_bpue_lip2)
setnames(x = CLD_bpue_lip2, old = c('.group'),
         new = c('depht_g'))
olsinka_vpue3 <- merge(olsinka_vpue3, CLD_bpue_lip2[, .(locality, depthlayerid, depht_g)], by = c("locality", "depthlayerid"))

ggplot(olsinka_vpue3, #bpue 
       aes(x = locality, y = bpue_mean, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinka_vpue3, #bpue depht 
       aes(x = locality, y = bpue_mean, fill = depht_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'E')+
  facet_grid(~depthlayerid) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#sp####
olsinka_vpuesp <- olsinka_vpuem[sp_scientificname %in% c("Cyprinus carpio", "Perca fluviatilis", "Sander lucioperca")]
olsinka_vpuesp$locality <- factor(olsinka_vpuesp$locality, levels = c("Olsinska zatoka", "Pritok", "Hurka", "Hraz"))
#Dcasting cpue####
olsinka_vpuesp <- setDT(olsinka_vpuesp)
dcast_olsinka_bpue <- dcast(data = olsinka_vpuesp, formula = locality + sa_samplingid + year + depthlayerid + dl_layertype ~ sp_scientificname,
                            value.var = "bpue_mean")
dcast_olsinka_bpue <- setDT(dcast_olsinka_bpue)
dcast_olsinka_bpue[is.na(dcast_olsinka_bpue)] <- 0
lapply(dcast_olsinka_bpue[,c(6:8)], function(x) shapiro.test(x))

set.seed(3333)
ols_glm_bpuesp <- lapply(dcast_olsinka_bpue[,c(6:8)], function(x) glm.nb(data = dcast_olsinka_bpue, formula = x ~ locality))
summary(ols_glm_bpuesp$`Cyprinus carpio`)
summary(ols_glm_bpuesp$`Perca fluviatilis`)
summary(ols_glm_bpuesp$`Sander lucioperca`)

pair_steep_cpue_rimovcc <- emmeans(ols_glm_bpuesp$`Cyprinus carpio`, pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_steep_cpue_rimovcc <- multcomp::cld(pair_steep_cpue_rimovcc,
                                      Letters=letters, alpha = 0.05)
setDT(CLD_steep_cpue_rimovcc)
setnames(x = CLD_steep_cpue_rimovcc, old = c('.group'),
         new = c('locality_g'))
CLD_steep_cpue_rimovcc$sp_scientificname <- "Cyprinus carpio"

pair_steep_cpue_rimovpf <- emmeans(ols_glm_bpuesp$`Perca fluviatilis`,pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_steep_cpue_rimovpf <- multcomp::cld(pair_steep_cpue_rimovpf,
                                        alpha=0.05,
                                        Letters=letters)
setDT(CLD_steep_cpue_rimovpf)
setnames(x = CLD_steep_cpue_rimovpf, old = c('.group'),
         new = c('locality_g'))
CLD_steep_cpue_rimovpf$sp_scientificname <- "Perca fluviatilis"

pair_steep_cpue_rimovsl <- emmeans(ols_glm_bpuesp$`Sander lucioperca`,pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_steep_cpue_rimovsl <- multcomp::cld(pair_steep_cpue_rimovsl,
                                        alpha=0.05,
                                        Letters=letters)
setDT(CLD_steep_cpue_rimovsl)
setnames(x = CLD_steep_cpue_rimovsl, old = c('.group'),
         new = c('locality_g'))
CLD_steep_cpue_rimovsl$sp_scientificname <- "Sander lucioperca"

test <- rbind(CLD_steep_cpue_rimovpf, CLD_steep_cpue_rimovcc)
test <- rbind(test, CLD_steep_cpue_rimovsl)
olsinka_vpuesp3 <- olsinka_vpuem2[sp_scientificname %in% c("Cyprinus carpio", "Perca fluviatilis", "Sander lucioperca")]
olsinka_vpuesp3 <- merge(olsinka_vpuesp3, test[, .(sp_scientificname, locality, locality_g)], by = c("locality", "sp_scientificname"))

ggplot(olsinka_vpuesp3[sp_scientificname == "Sander lucioperca"], #bpue 
       aes(x = locality, y = bpue_mean, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinka_vpuesp3[sp_scientificname == "Cyprinus carpio"], #bpue 
       aes(x = locality, y = bpue_mean, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinka_vpuesp3[sp_scientificname == "Perca fluviatilis"], #bpue 
       aes(x = locality, y = bpue_mean, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#sp with depthlayer####
set.seed(3333)
ols_glm_bpuesp2 <- lapply(dcast_olsinka_bpue[,c(6:8)], function(x) glm.nb(data = dcast_olsinka_bpue, formula = x ~ locality + depthlayerid))
summary(ols_glm_bpuesp2$`Cyprinus carpio`)
summary(ols_glm_bpuesp2$`Perca fluviatilis`)
summary(ols_glm_bpuesp2$`Sander lucioperca`)

pair_steep_cpue_rimovcc2 <- emmeans(ols_glm_bpuesp2$`Cyprinus carpio`, ~ locality * depthlayerid, weights = "cell")
CLD_steep_cpue_rimovcc2 <- multcomp::cld(pair_steep_cpue_rimovcc2,
                                        Letters=letters, alpha = 0.05)
setDT(CLD_steep_cpue_rimovcc2)
setnames(x = CLD_steep_cpue_rimovcc2, old = c('.group'),
         new = c('depht_g'))
CLD_steep_cpue_rimovcc2$sp_scientificname <- "Cyprinus carpio"

pair_steep_cpue_rimovpf2 <- emmeans(ols_glm_bpuesp2$`Perca fluviatilis`, ~ locality * depthlayerid, adjust="sidak", weights = "flat")
CLD_steep_cpue_rimovpf2 <- multcomp::cld(pair_steep_cpue_rimovpf2,
                                        alpha=0.05,
                                        Letters=letters)
setDT(CLD_steep_cpue_rimovpf2)
setnames(x = CLD_steep_cpue_rimovpf2, old = c('.group'),
         new = c('depht_g'))
CLD_steep_cpue_rimovpf2$sp_scientificname <- "Perca fluviatilis"

pair_steep_cpue_rimovsl2 <- emmeans(ols_glm_bpuesp2$`Sander lucioperca`, ~ locality * depthlayerid, adjust="sidak", weights = "flat")
CLD_steep_cpue_rimovsl2 <- multcomp::cld(pair_steep_cpue_rimovsl2,
                                        alpha=0.05,
                                        Letters=letters)
setDT(CLD_steep_cpue_rimovsl2)
setnames(x = CLD_steep_cpue_rimovsl2, old = c('.group'),
         new = c('depht_g'))
CLD_steep_cpue_rimovsl2$sp_scientificname <- "Sander lucioperca"

test2 <- rbind(CLD_steep_cpue_rimovpf2, CLD_steep_cpue_rimovcc2)
test2 <- rbind(test2, CLD_steep_cpue_rimovsl2)
olsinka_vpuesp3 <- merge(olsinka_vpuesp3, test2[, .(sp_scientificname, locality, depthlayerid, depht_g)], by = c("locality", "depthlayerid", "sp_scientificname"))

ggplot(olsinka_vpuesp3[sp_scientificname == "Sander lucioperca"], #bpue 
       aes(x = locality, y = bpue_mean, fill = depht_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(depthlayerid~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinka_vpuesp3[sp_scientificname == "Cyprinus carpio"], #bpue 
       aes(x = locality, y = bpue_mean, fill = depht_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(depthlayerid~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinka_vpuesp3[sp_scientificname == "Perca fluviatilis"], #bpue 
       aes(x = locality, y = bpue_mean, fill = depht_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(depthlayerid~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#True bpue####
olsinska_trbpue$locality <- factor(olsinska_trbpue$locality, levels = c("Olsinska zatoka", "Pritok", "Hurka", "Hraz"))
shapiro.test(olsinska_trbpue$truebpue)
set.seed(3333)
lip_glm_tbpue <- glm.nb(data = olsinska_trbpue, formula = truebpue ~ locality)
summary(lip_glm_tbpue)

pair_tbpue_lip <- emmeans(lip_glm_tbpue,pairwise ~ locality, adjust="tukey")
CLD_tbpue_lip <- multcomp::cld(pair_tbpue_lip, 
                              alpha=0.05,
                              Letters=letters)
setDT(CLD_tbpue_lip)
setnames(x = CLD_tbpue_lip, old = c('.group'),
         new = c('locality_g'))
olsinska_trbpue3 <- merge(olsinska_trbpue, CLD_tbpue_lip[, .(locality, locality_g)], by = c("locality"))
olsinska_trbpue3$locality <- factor(olsinska_trbpue3$locality, levels = c("Pritok", "Hurka", "Olsinska zatoka", "Hraz"))
ggplot(olsinska_trbpue3, #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#TRUE BPUE sp####
olsinska_trbpuesp2 <- olsinska_trbpuesp[sp_scientificname %in% c("Cyprinus carpio", "Perca fluviatilis", "Sander lucioperca", "Blicca bjoerkna", "Abramis brama")]
olsinska_trbpuesp2$locality <- factor(olsinska_trbpuesp2$locality, levels = c("Olsinska zatoka", "Pritok", "Hurka", "Hraz"))
#Dcasting cpue####
olsinska_trbpuesp2 <- setDT(olsinska_trbpuesp2)
dcast_olsinka_tbpue <- dcast(data = olsinska_trbpuesp2, formula = locality + year ~ sp_scientificname,
                            value.var = "truebpue")
dcast_olsinka_tbpue <- setDT(dcast_olsinka_tbpue)
dcast_olsinka_tbpue[is.na(dcast_olsinka_tbpue)] <- 0
lapply(dcast_olsinka_tbpue[,c(3:7)], function(x) shapiro.test(x))

set.seed(3333)
ols_glm_tbpuesp <- lapply(dcast_olsinka_tbpue[,c(3:7)], function(x) glm.nb(data = dcast_olsinka_tbpue, formula = x ~ locality))
summary(ols_glm_tbpuesp$`Cyprinus carpio`)
summary(ols_glm_tbpuesp$`Perca fluviatilis`)
summary(ols_glm_tbpuesp$`Sander lucioperca`)
summary(ols_glm_tbpuesp$`Blicca bjoerkna`)
summary(ols_glm_tbpuesp$`Abramis brama`)

pair_tbpue_cc <- emmeans(ols_glm_tbpuesp$`Cyprinus carpio`, pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_tbpue_cc <- multcomp::cld(pair_tbpue_cc,
                                        Letters = letters, alpha = 0.05)
setDT(CLD_tbpue_cc)
setnames(x = CLD_tbpue_cc, old = c('.group'),
         new = c('locality_g'))
CLD_tbpue_cc$sp_scientificname <- "Cyprinus carpio"

pair_tbpue_pf <- emmeans(ols_glm_tbpuesp$`Perca fluviatilis`,pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_tbpue_pf <- multcomp::cld(pair_tbpue_pf,
                                        alpha = 0.05,
                                        Letters = letters)
setDT(CLD_tbpue_pf)
setnames(x = CLD_tbpue_pf, old = c('.group'),
         new = c('locality_g'))
CLD_tbpue_pf$sp_scientificname <- "Perca fluviatilis"

pair_tbpue_sl <- emmeans(ols_glm_tbpuesp$`Sander lucioperca`,pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_tbpue_sl <- multcomp::cld(pair_tbpue_sl,
                                        alpha=0.05,
                                        Letters=letters)
setDT(CLD_tbpue_sl)
setnames(x = CLD_tbpue_sl, old = c('.group'),
         new = c('locality_g'))
CLD_tbpue_sl$sp_scientificname <- "Sander lucioperca"

pair_tbpue_bj <- emmeans(ols_glm_tbpuesp$`Blicca bjoerkna`,pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_tbpue_bj <- multcomp::cld(pair_tbpue_bj,
                              alpha=0.05,
                              Letters=letters)
setDT(CLD_tbpue_bj)
setnames(x = CLD_tbpue_bj, old = c('.group'),
         new = c('locality_g'))
CLD_tbpue_bj$sp_scientificname <- "Blicca bjoerkna"

pair_tbpue_ab <- emmeans(ols_glm_tbpuesp$`Abramis brama`,pairwise ~ locality, adjust="sidak", weights = "flat")
CLD_tbpue_ab <- multcomp::cld(pair_tbpue_ab,
                              alpha=0.05,
                              Letters=letters)
setDT(CLD_tbpue_ab)
setnames(x = CLD_tbpue_ab, old = c('.group'),
         new = c('locality_g'))
CLD_tbpue_ab$sp_scientificname <- "Abramis brama"

test_tb <- rbind(CLD_tbpue_cc, CLD_tbpue_pf)
test_tb <- rbind(test_tb, CLD_tbpue_sl)
test_tb <- rbind(test_tb, CLD_tbpue_bj)
test_tb <- rbind(test_tb, CLD_tbpue_ab)
olsinska_trbpuesp3 <- merge(olsinska_trbpuesp2, test_tb[, .(sp_scientificname, locality, locality_g)], by = c("locality", "sp_scientificname"))
olsinska_trbpuesp3$locality <- factor(olsinska_trbpuesp3$locality, levels = c("Pritok", "Hurka", "Olsinska zatoka", "Hraz"))

ggplot(olsinska_trbpuesp3[sp_scientificname == "Sander lucioperca"], #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinska_trbpuesp3[sp_scientificname == "Cyprinus carpio"], #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinska_trbpuesp3[sp_scientificname == "Perca fluviatilis"], #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinska_trbpuesp3[sp_scientificname == "Blicca bjoerkna"], #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinska_trbpuesp3[sp_scientificname == "Abramis brama"], #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(olsinska_trbpuesp3[sp_scientificname == "Abramis brama"], #bpue 
       aes(x = locality, y = truebpue, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = NULL, y = 'BPUE in Kg per 1000m² net night')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 18,angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 22), 
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 18, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# #True bpue per sp####
# #Calcule BPUE mean
# tbpue_lipsp <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
#                      split.factors.samplings = c("depthlayerid", "locality", "dl_layertype"),
#                      split.factors.catch = c("sp_scientificname"),
#                      id.colname = 'sa_samplingid', value.var = "ct_weightstar")
# olsinska_btbpuesp <- tbpue_lipsp[dl_layertype == 'Benthic',.(mbenthic = round(mean(ct_weightstar.mean, na.rm = T), 2)),
#                                by =.(locality, sp_scientificname)]
# olsinska_plbpuesp <- tbpue_lipsp[dl_layertype == 'Pelagic',.(spelagic = round(sum(ct_weightstar.mean, na.rm = T), 2)),
#                                by =.(locality, sp_scientificname)]
# olsinska_trbpuesp <- merge(olsinska_btbpuesp, olsinska_plbpuesp, by = c("locality", 'sp_scientificname'))
# olsinska_trbpuesp$alpha[olsinska_trbpuesp$locality == "Hraz"] <- 0.9
# olsinska_trbpuesp$alpha[olsinska_trbpuesp$locality == "Hurka"] <- 0.7
# olsinska_trbpuesp$alpha[olsinska_trbpuesp$locality == "Pritok"] <- 0.5
# olsinska_trbpuesp$alpha[olsinska_trbpuesp$locality == "Olsinska zatoka"] <- 0.5
# olsinska_trbpuesp$bpue <- olsinska_trbpuesp$mbenthic + olsinska_trbpuesp$spelagic * olsinska_trbpuesp$alpha
# olsinska_trbpuesp$truebpue <- olsinska_trbpuesp$bpue*2
# olsinska_trbpuesp[, sp_grouped := fct_lump(f = sp_scientificname, prop = 0.05, w = truebpue, other_level = "Others")]
# 
# ggplot(data = olsinska_trbpuesp, 
#        aes(x = locality, y = truebpue, fill = sp_grouped)) + 
#   geom_bar(stat="identity") + 
#   labs(x = "Locality", y = 'Biomass in Kg per 1000m² net night', fill = "Species")+
#   # facet_wrap(~year)+
#   scale_fill_viridis_d(option = 'D')+
#   theme(plot.title = element_text(size = 32, face = "bold"),
#         axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
#         axis.text.y = element_text(size = 28),
#         strip.text = element_text(size = 14),
#         axis.title.x = element_text(size = 20),
#         axis.title.y = element_text(size = 26),
#         legend.title = element_text(size=28),
#         legend.text = element_text(size = 26, face = "italic"))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Calcule BPUE year
tbpue_liysp <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
                       split.factors.samplings = c("depthlayerid", "locality", "dl_layertype", "year"),
                       split.factors.catch = c("sp_scientificname"),
                       id.colname = 'sa_samplingid', value.var = "ct_weightstar")

olsinska_btbpuespy <- tbpue_liysp[dl_layertype == 'Benthic',.(mbenthic = round(mean(ct_weightstar.mean, na.rm = T), 2)),
                                 by =.(locality, sp_scientificname, year)]
olsinska_plbpuespy <- tbpue_liysp[dl_layertype == 'Pelagic',.(spelagic = round(sum(ct_weightstar.mean, na.rm = T), 2)),
                                 by =.(locality, sp_scientificname, year)]
olsinska_trbpuespy <- merge(olsinska_btbpuespy, olsinska_plbpuespy, by = c("locality", 'sp_scientificname', "year"))
olsinska_trbpuespy$alpha[olsinska_trbpuespy$locality == "Hraz"] <- 0.9
olsinska_trbpuespy$alpha[olsinska_trbpuespy$locality == "Hurka"] <- 0.7
olsinska_trbpuespy$alpha[olsinska_trbpuespy$locality == "Pritok"] <- 0.5
olsinska_trbpuespy$alpha[olsinska_trbpuespy$locality == "Olsinska zatoka"] <- 0.5
olsinska_trbpuespy$bpue <- olsinska_trbpuespy$mbenthic + olsinska_trbpuespy$spelagic * olsinska_trbpuespy$alpha
olsinska_trbpuespy$truebpue <- olsinska_trbpuespy$bpue*2
olsinska_trbpuespy[, sp_grouped := fct_lump(f = sp_scientificname, prop = 0.05, w = truebpue, other_level = "Others")]

ggplot(data = olsinska_trbpuespy, 
       aes(x = locality, y = truebpue, fill = sp_grouped)) + 
  geom_bar(stat="identity") + 
  labs(x = "Locality", y = 'Biomass in Kg per 1000m² net night', fill = "Species")+
  facet_wrap(~year)+
  scale_fill_viridis_d(option = 'D')+
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

# #True bpue per locality####
# #Calcule BPUE mean
# tbpue_lip <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
#                        split.factors.samplings = c("depthlayerid", "locality", "dl_layertype"),
#                        split.factors.catch = c("ct_agegroup"),
#                        id.colname = 'sa_samplingid', value.var = "ct_weightstar")
# # write.xlsx(tbpue_lip, here::here('tbpue_lip.xlsx'))
# olsinska_btbpue <- tbpue_lip[dl_layertype == 'Benthic',.(mbenthic = round(mean(ct_weightstar.mean, na.rm = T), 2)),
#                                  by =.(locality)]
# olsinska_plbpue <- tbpue_lip[dl_layertype == 'Pelagic',.(spelagic = round(sum(ct_weightstar.mean, na.rm = T), 2)),
#                                  by =.(locality)]
# olsinska_trbpue <- merge(olsinska_btbpue, olsinska_plbpue, by = c("locality"))
# olsinska_trbpue$alpha[olsinska_trbpue$locality == "Hraz"] <- 0.9
# olsinska_trbpue$alpha[olsinska_trbpue$locality == "Hurka"] <- 0.7
# olsinska_trbpue$alpha[olsinska_trbpue$locality == "Pritok"] <- 0.5
# olsinska_trbpue$alpha[olsinska_trbpue$locality == "Olsinska zatoka"] <- 0.5
# olsinska_trbpue$bpue <- olsinska_trbpue$mbenthic + olsinska_trbpue$spelagic * olsinska_trbpue$alpha
# olsinska_trbpue$truebpue <- olsinska_trbpue$bpue*2
# 
# ggplot(data = olsinska_trbpue, 
#        aes(x = locality, y = truebpue, fill = locality)) + 
#   geom_bar(stat="identity") + 
#   labs(x = "Locality", y = 'Biomass in Kg per 1000m² net night')+
#   # facet_wrap(~year)+
#   scale_fill_viridis_d(option = 'C')+
#   theme(plot.title = element_text(size = 32, face = "bold"),
#         axis.text.x = element_text(size = 28,angle = 45, hjust = 1.05, vjust = 1.05),
#         axis.text.y = element_text(size = 28),
#         strip.text = element_text(size = 14),
#         axis.title.x = element_text(size = 20),
#         axis.title.y = element_text(size = 26),
#         legend.title = element_text(size=28),
#         legend.text = element_text(size = 26, face = "italic"))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   theme(legend.position = "none")

#Calcule BPUE year
tbpue_liy <- getVPUE(samplings = all_gill_depl, catch = catches_olsinka, 
                       split.factors.samplings = c("depthlayerid", "locality", "dl_layertype", "year"),
                       split.factors.catch = c("ct_agegroup"),
                       id.colname = 'sa_samplingid', value.var = "ct_weightstar")
# write.xlsx(tbpue_liy, here::here('tbpue_liy.xlsx'))
olsinska_btbpuey <- tbpue_liy[dl_layertype == 'Benthic',.(mbenthic = round(mean(ct_weightstar.mean, na.rm = T), 2)),
                                  by =.(locality, year)]
olsinska_plbpuey <- tbpue_liy[dl_layertype == 'Pelagic',.(spelagic = round(sum(ct_weightstar.mean, na.rm = T), 2)),
                                  by =.(locality, year)]
olsinska_trbpuey <- merge(olsinska_btbpuey, olsinska_plbpuey, by = c("locality", "year"))
olsinska_trbpuey$alpha[olsinska_trbpuey$locality == "Hraz"] <- 0.9
olsinska_trbpuey$alpha[olsinska_trbpuey$locality == "Hurka"] <- 0.7
olsinska_trbpuey$alpha[olsinska_trbpuey$locality == "Pritok"] <- 0.5
olsinska_trbpuey$alpha[olsinska_trbpuey$locality == "Olsinska zatoka"] <- 0.5
olsinska_trbpuey$bpue <- olsinska_trbpuey$mbenthic + olsinska_trbpuey$spelagic * olsinska_trbpuey$alpha
olsinska_trbpuey$truebpue <- olsinska_trbpuey$bpue*2

ggplot(data = olsinska_trbpuey, 
       aes(x = locality, y = truebpue, fill = locality)) + 
  geom_bar(stat="identity") + 
  labs(x = "Locality", y = 'Biomass in Kg per 1000m² net night')+
  facet_wrap(~year)+
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
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")
