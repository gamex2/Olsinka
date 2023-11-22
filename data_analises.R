
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

