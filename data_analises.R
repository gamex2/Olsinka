
olsinka_vpue$locality <- factor(olsinka_vpue$locality, levels = c("Olsinska zatoka", "Pritok", "Hurka", "Hraz"))
set.seed(3333)
lip_glm_bpue <- glm.nb(data = olsinka_vpue, formula = bpue_mean ~ locality)
summary(lip_glm_bpue)

pair_bpue_lip <- lsmeans(lip_glm_bpue, ~ locality)
CLD_bpue_lip <- multcomp::cld(pair_bpue_lip,
                               alpha=0.05,
                               Letters=letters,
                               adjust="sidak")
setDT(CLD_bpue_lip)
setnames(x = CLD_bpue_lip, old = c('.group'),
         new = c('locality_g'))
olsinka_vpue2 <- merge(olsinka_vpue2, CLD_bpue_lip[, .(locality, locality_g)], by = "locality")

ggplot(olsinka_vpue2, #bpue 
       aes(x = locality, y = bpue_mean, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  # coord_cartesian(ylim = c(0, 5)) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in g per 1000m² net night')+
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
dcast_olsinka_bpue <- dcast(data = olsinka_vpuesp, formula = locality + sa_samplingid + year ~ sp_scientificname,
                            value.var = "bpue_mean")
dcast_olsinka_bpue <- setDT(dcast_olsinka_bpue)
dcast_olsinka_bpue[is.na(dcast_olsinka_bpue)] <- 0
lapply(dcast_olsinka_bpue[,c(3:5)], function(x) shapiro.test(x))

set.seed(3333)
ols_glm_bpuesp <- lapply(dcast_olsinka_bpue[,c(4:6)], function(x) glmer.nb(data = dcast_olsinka_bpue, formula = x ~ locality + (1|year)))
summary(ols_glm_bpuesp$`Cyprinus carpio`)
summary(ols_glm_bpuesp$`Perca fluviatilis`)
summary(ols_glm_bpuesp$`Sander lucioperca`)

pair_steep_cpue_rimovcc <- emmeans(ols_glm_bpuesp$`Cyprinus carpio`,pairwise ~ locality, adjust="sidak", weights = "cells")
CLD_steep_cpue_rimovcc <- multcomp::cld(pair_steep_cpue_rimovcc,
                                      Letters=letters)
setDT(CLD_steep_cpue_rimovcc)
setnames(x = CLD_steep_cpue_rimovcc, old = c('.group'),
         new = c('locality_g'))
CLD_steep_cpue_rimovcc$sp_scientificname <- "Cyprinus carpio"

pair_steep_cpue_rimovpf <- emmeans(ols_glm_bpuesp$`Perca fluviatilis`,pairwise ~ locality, adjust="sidak", weights = "cells")
CLD_steep_cpue_rimovpf <- multcomp::cld(pair_steep_cpue_rimovpf,
                                        alpha=0.05,
                                        Letters=letters,
                                        adjust="sidak")
setDT(CLD_steep_cpue_rimovpf)
setnames(x = CLD_steep_cpue_rimovpf, old = c('.group'),
         new = c('locality_g'))
CLD_steep_cpue_rimovpf$sp_scientificname <- "Perca fluviatilis"

pair_steep_cpue_rimovsl <- emmeans(ols_glm_bpuesp$`Sander lucioperca`,pairwise ~ locality, adjust="sidak", weights = "cells")
CLD_steep_cpue_rimovsl <- multcomp::cld(pair_steep_cpue_rimovsl,
                                        alpha=0.05,
                                        Letters=letters,
                                        adjust="sidak")
setDT(CLD_steep_cpue_rimovsl)
setnames(x = CLD_steep_cpue_rimovsl, old = c('.group'),
         new = c('locality_g'))
CLD_steep_cpue_rimovsl$sp_scientificname <- "Sander lucioperca"

test <- rbind(CLD_steep_cpue_rimovpf, CLD_steep_cpue_rimovcc)
test <- rbind(test, CLD_steep_cpue_rimovsl)
olsinka_vpuesp2 <- olsinka_vpuem2[sp_scientificname %in% c("Cyprinus carpio", "Perca fluviatilis", "Sander lucioperca")]
olsinka_vpuesp2 <- merge(olsinka_vpuesp2, test[, .(sp_scientificname, locality, locality_g)], by = c("locality", "sp_scientificname"))

ggplot(olsinka_vpuesp2, #bpue 
       aes(x = locality, y = bpue_mean, fill = locality_g)) +
  geom_boxplot()+
  scale_fill_viridis_d(option = 'C')+
  facet_grid(~sp_scientificname) +
  coord_cartesian(ylim = c(0, 25)) +
  theme(strip.text = element_text(face = "italic")) +
  labs(x = NULL, y = 'BPUE in g per 1000m² net night')+
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



