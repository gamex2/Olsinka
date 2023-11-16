
set.seed(3333)
ols_lm_bpue <- lme4::glmer.nb(data = olsinka_vpuem, formula = cpue_mean ~ locality + (1 |year))

par(mfrow = c(2,2))
plot(ols_lm_bpue)
dev.off()
summary(ols_lm_bpue)
pair_steep_cpue_rimov <- lsmeans(ols_lm_bpue, ~ locality)
CLD_steep_cpue_rimov <- multcomp::cld(pair_steep_cpue_rimov,
                               alpha=0.05,
                               Letters=letters,
                               adjust="sidak")

ggplot(olsinka_vpuem[sp_scientificname == "Cyprinus carpio"], #bpue 
       aes(x = locality, y = bpue_mean)) +
  geom_bar(stat="identity")+
  scale_fill_viridis_d(option = 'C')+
  facet_wrap(~year)+
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
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill = F)

ggplot(olsinka_vpuem, #bpue 
       aes(x = as.factor(year), y = bpue_mean)) +
  geom_bar(stat="identity")+
  scale_fill_viridis_d(option = 'C')+
  facet_wrap(~locality)+
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
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill = F)

#sp####
olsinka_vpuesp <- olsinka_vpuem[sp_scientificname %in% c("Cyprinus carpio", "Perca fluviatilis", "Sander lucioperca")]
#Dcasting cpue####
olsinka_vpuesp <- setDT(olsinka_vpuesp)
dcast_olsinka_bpue <- dcast(data = olsinka_vpuesp, formula = locality + year + sa_samplingid ~ sp_scientificname,
                            value.var = "bpue_mean")
dcast_olsinka_bpue <- setDT(dcast_olsinka_bpue)
dcast_olsinka_bpue[is.na(dcast_olsinka_bpue)] <- 0
lapply(dcast_olsinka_bpue[,c(4:6)], function(x) shapiro.test(x))

set.seed(3333)
ols_lm_bpuesp <- lapply(dcast_olsinka_bpue[,c(4:6)], function(x) glm.nb(data = dcast_olsinka_bpue, formula = x ~ locality + year))

par(mfrow = c(2,2))
plot(ols_lm_bpuesp$`Cyprinus carpio`)
plot(ols_lm_bpuesp$`Perca fluviatilis`)
plot(ols_lm_bpuesp$`Sander lucioperca`)
dev.off()
summary(ols_lm_bpuesp$`Cyprinus carpio`)
summary(ols_lm_bpuesp$`Perca fluviatilis`)
summary(ols_lm_bpuesp$`Sander lucioperca`)
pair_steep_cpue_rimovcc <- lsmeans(ols_lm_bpuesp$`Cyprinus carpio`, ~ locality)
CLD_steep_cpue_rimovcc <- multcomp::cld(pair_steep_cpue_rimovcc,
                                      alpha=0.05,
                                      Letters=letters,
                                      adjust="sidak")

pair_steep_cpue_rimovpf <- lsmeans(ols_lm_bpuesp$`Perca fluviatilis`, ~ locality)
CLD_steep_cpue_rimovpf <- multcomp::cld(pair_steep_cpue_rimovpf,
                                        alpha=0.05,
                                        Letters=letters,
                                        adjust="sidak")

pair_steep_cpue_rimovsl <- lsmeans(ols_lm_bpuesp$`Sander lucioperca`, ~ locality)
CLD_steep_cpue_rimovsl <- multcomp::cld(pair_steep_cpue_rimovsl,
                                        alpha=0.05,
                                        Letters=letters,
                                        adjust="sidak")
