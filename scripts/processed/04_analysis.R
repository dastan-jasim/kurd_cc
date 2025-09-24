load(".data/processed/dat_merge_num_k.Rdata")
load(".data/processed/dat_merge_num.Rdata")
load(".data/processed/dat_merge_imp_.Rdata")

# ------ MODEL --------

#index for dependent variable
# all start with 0

#ARMY RULE NOT ASKED IN TR WVS

dat_merge_num_k$dem_index <- dat_merge_num_k$sys_democ-
  dat_merge_num_k$sys_expert-
  dat_merge_num_k$sys_leader

dat_merge_num_k$dem_index <- dat_merge_num_k$dem_index+6

# index for imputed

dat_merge_imp_$dem_index <- dat_merge_imp_$sys_democ-
  dat_merge_imp_$sys_expert-
  dat_merge_imp_$sys_leader

dat_merge_imp_$dem_index <- dat_merge_imp_$dem_index+7


# CORRELATION TEST

sub_corr <- dat_merge_num_k[,-c(1:6,20,22:50)]
sub_corr_imp <- dat_merge_imp_[,-c(1:6,20,22:50)]


corr <- cor(na.omit(sub_corr))
corr_imp <- cor(na.omit(sub_corr_imp))
par(mfrow=c(1,1))

dev.off()

corrplot(corr,method='number',is.corr = F)
dev.off()

corrplot(corr_imp,method='number',is.corr = F)


# MODEL CREATION
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# WITH ONLY EXISTING DEP VAR SUPPORT DEMOC

kmod <- lm(dem_index~pol_interest+conf_gov,data=dat_merge_num_k)

kmod2 <- lm(dem_index~pol_interest+conf_gov+
              country,data=dat_merge_num_k)

kmod3 <- lm(dem_index~pol_interest+conf_gov+
              country+conf_armed+
              conf_part+conf_court+conf_parl+conf_police,data=dat_merge_num_k)

kmod4 <- lm(dem_index~pol_interest+conf_gov+country+conf_armed+
              conf_part+conf_court+conf_parl+conf_police+relig_pers+gender+age+edu,data=dat_merge_num_k)


# WITH IMPUTED DATASET

ikmod <- lm(dem_index~pol_interest+conf_gov,data=dat_merge_imp_)

ikmod2 <- lm(dem_index~pol_interest+conf_gov+
              country,data=dat_merge_imp_)

ikmod3 <- lm(dem_index~pol_interest+conf_gov+
              country+conf_armed+
              conf_part+conf_court+conf_parl+conf_police,data=dat_merge_imp_)

ikmod4 <- lm(dem_index~pol_interest+conf_gov+country+conf_armed+
              conf_part+conf_court+conf_parl+conf_police+relig_pers+gender+age+edu,data=dat_merge_imp_)



# Diagnostic

par(mfrow = c(2, 2))
plot(ikmod)
plot(ikmod2)
plot(ikmod3)
plot(ikmod4)


lmtest::bptest(kmod)
lmtest::bptest(kmod2)
lmtest::bptest(kmod3)
lmtest::bptest(kmod4)

par(mfrow = c(2, 2))
plot(kmod)
plot(kmod2)
plot(kmod3)
plot(kmod4)

library(lmtest)
lmtest::bptest(ikmod)
lmtest::bptest(ikmod2)
lmtest::bptest(ikmod3)
lmtest::bptest(ikmod4)





# Vergleich Imputed and not Imputed

texreg(list(kmod,kmod2,kmod3,kmod4,ikmod,ikmod2,ikmod3,ikmod4))

# Model Imputed allein

texreg(list(ikmod,ikmod2,ikmod3,ikmod4))

# MODEL NO KURD INSTITUTION TURKEY IRAN

irn<-dat_merge_num_k%>%                     
  filter(country=="Iran")

trc<-dat_merge_num_k%>%                     
  filter(country=="Turkey")
# imputed

iirn<-dat_merge_imp_%>%                     
  filter(country=="Iran")

itrc<-dat_merge_imp_%>%                     
  filter(country=="Turkey")

trc_mod <- lm(dem_index~pol_interest+
             conf_gov+
             conf_armed+
             conf_part+
             +conf_parl+
             +conf_police+
               conf_court+
             +gender+age+edu+relig_pers,data=trc)

irn_mod <- lm(dem_index~pol_interest+
             conf_gov+
             conf_armed+
             conf_part+
             +conf_parl+
             +conf_police+
               conf_court+
             +gender+age+edu+relig_pers,data=irn)

trc_mod_i <- lm(dem_index~pol_interest+
                conf_gov+
                conf_armed+
                conf_part+
                +conf_parl+
                +conf_police+
                  conf_court+
                +gender+age+edu+relig_pers,data=itrc)

irn_mod_i <- lm(dem_index~pol_interest+
                conf_gov+
                conf_armed+
                conf_part+
                +conf_parl+
                +conf_police+
                  conf_court+
                +gender+age+edu+relig_pers,data=iirn)


# Diagnostic IRAN TURKEY MODEL

par(mfrow = c(2, 2))
plot(trc_mod)
plot(irn_mod)
plot(trc_mod_i)
plot(irn_mod_i)

# model plot IRAN TURKEY I

plot_model(trc_mod_i,show.p = T,show.values = T,se=T, 
           value.offset = 0.5, title = "Turkey Subset Model of Imputed Data")

plot_model(irn_mod_i,show.p = T,show.values = T,se=T, 
           value.offset = 0.5, title = "Iran Subset Model of Imputed Data")

# all comparison

texreg(list(trc_mod,irn_mod,trc_mod_i, irn_mod_i))

# only imputed

texreg(list(trc_mod_i, irn_mod_i))

# MODEL KURD INSTITUTION 

iq<-dat_merge_num_k%>%                     
  filter(country=="Iraq")

sy<-dat_merge_num_k%>%                     
  filter(country=="Syria")
# imputed

iiq<-dat_merge_imp_%>%                     
  filter(country=="Iraq")

isy<-dat_merge_imp_%>%                     
  filter(country=="Syria")

kri_mod <- lm(dem_index~pol_interest+
                conf_gov+
                conf_armed+
                conf_part+
                +conf_parl+
                +conf_police+
                conf_court+
                conf_gov+conf_krg+
                conf_armed+conf_armed_kri+
                conf_part+conf_part_krd+
                +conf_parl+conf_parl_kri+
                +conf_police+conf_police_kri+
                +gender+age+edu+relig_pers,data=iq)

nes_mod <- lm(dem_index~pol_interest+
                conf_gov+
                conf_armed+
                conf_part+
                +conf_parl+
                +conf_police+
                conf_court+
                conf_gov+conf_nes+
                conf_armed+conf_armed_nes+
                conf_part+conf_part_nes+
                +conf_parl+conf_parl_nes+
                +conf_police+conf_police_nes+
                +gender+age+edu+relig_pers,data=sy)

kri_mod_i <- lm(dem_index~pol_interest+
                  conf_gov+
                  conf_armed+
                  conf_part+
                  +conf_parl+
                  +conf_police+
                  conf_court+
                  conf_gov+conf_krg+
                  conf_armed+conf_armed_kri+
                  conf_part+conf_part_krd+
                  +conf_parl+conf_parl_kri+
                  +conf_police+conf_police_kri+
                  +gender+age+edu+relig_pers,data=iiq)

kri_nurgov <- lm(dem_index~pol_interest+
             conf_gov+
             conf_armed+
             conf_part+
             +conf_parl+
             +conf_police+
             conf_court, data=iiq)

nes_mod_i <- lm(dem_index~pol_interest+
                  conf_gov+
                  conf_armed+
                  conf_part+
                  +conf_parl+
                  +conf_police+
                  conf_court+
                  conf_gov+conf_nes+
                  conf_armed+conf_armed_nes+
                  conf_part+conf_part_nes+
                  +conf_parl+conf_parl_nes+
                  +conf_police+conf_police_nes+
                  +gender+age+edu+relig_pers,data=isy)

nes_nurgov <- lm(dem_index~pol_interest+
             conf_gov+
             conf_armed+
             conf_part+
             +conf_parl+
             +conf_police+
             conf_court, data=isy)
# MODEL PLOT SYRIA IRAQ

plot_model(kri_mod_i,show.p = T,show.values = T,se=T, 
           value.offset = 0.45, title = "KRI Subset Model of Imputed Data")

plot_model(nes_mod_i,show.p = T,show.values = T,se=T, 
           value.offset = 0.45, title = "NES Subset Model of Imputed Data")

# Correlation plot

krg$rate_system <- as.numeric(krg$rate_system)
krg$rate_system_kri <- as.numeric(krg$rate_system_kri)
krg_imp$rate_system <- as.numeric(krg_imp$rate_system)
krg_imp$rate_system_kri <- as.numeric(krg_imp$rate_system_kri)

roj$rate_system <- as.numeric(roj$rate_system)
roj$rate_system_nes <- as.numeric(roj$rate_system_nes)
nes_imp$rate_system <- as.numeric(nes_imp$rate_system)
nes_imp$rate_system_nes <- as.numeric(nes_imp$rate_system_nes)

krg_corr <- krg[,-c(1:6,20,22:25,27:29,41:50)]
krg_corr_imp <- krg_imp[,-c(1:6,20,22:25,27:29,41:50)]

nes_corr <- roj[,-c(1:6,20,22:25,27:29,34:40,48:50)]
nes_corr_imp <- nes_imp[,-c(1:6,20,22:25,27:29,34:40,48:50)]


corr <- cor(na.omit(krg_corr))
corr_imp <- cor(na.omit(krg_corr_imp))

dev.off()

grantel1 <- corrplot(corr,method='number',is.corr = F)
dev.off()

grantel2 <- corrplot(corr_imp,method='number',is.corr = F)



corr <- cor(na.omit(nes_corr))
corr_imp <- cor(na.omit(nes_corr_imp))
dev.off()

grantel3 <- corrplot(corr,method='number',is.corr = F)
dev.off()

grantel4 <- corrplot(corr_imp,method='number',is.corr = F)



# Diagnostic SYRIA IRAQ MODEL

par(mfrow = c(2, 2))
plot(kri_mod)
plot(nes_mod)
plot(kri_mod_i)
plot(nes_mod_i)

# all comparison

texreg(list(kri_mod,nes_mod,kri_mod_i, nes_mod_i))

# only imputed

texreg(list(kri_nurgov,kri_mod_i, nes_nurgov, nes_mod_i))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# for whole model

plot_model(kmod4,show.p = T,show.values = T,se=T, title = "Fixed Effects Model of Original Data")
plot_model(ikmod4,show.p = T,show.values = T,se=T, 
           value.offset = 0.45, title = "Fixed Effects Model of Imputed Data")


#for kurd model

texreg(list(kri_mod,nes_mod,kri_mod_i, nes_mod_i))



#------- DESCRIPTIVE  ----------

# gender dem index by country

plot_data <- dat_merge_imp_ %>% select(dem_index, gender, country, sys_democ,
                                       sys_expert, sys_leader)
plot_data$gender <- as.factor(plot_data$gender)

# Create the grouped bar plot

#DEMOCRACY INDEX

ggplot(plot_data, aes(x = dem_index, y = country, color = gender, point_color = gender, fill = gender)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Democracy Index by Gender and Countries") +
  theme_ridges(center = TRUE)

# sys_democ

ggplot(plot_data, aes(x = sys_democ, y = country, color = gender, point_color = gender, fill = gender)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Support for Democratic System by Gender and Countries") +
  theme_ridges(center = TRUE)

# sys_leader

ggplot(plot_data, aes(x = sys_leader, y = country, color = gender, point_color = gender, fill = gender)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Support for System with Strong Leader by Gender and Countries") +
  theme_ridges(center = TRUE)

# sys_expert

ggplot(plot_data, aes(x = sys_expert, y = country, color = gender, point_color = gender, fill = gender)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Support for Expert Rule by Gender and Countries") +
  theme_ridges(center = TRUE)

##########################################################################
        
# dependent variable distribution

depimp<-sjPlot::plot_grpfrq(dat_merge_imp_$dem_index, dat_merge_imp_$country, 
                         axis.title = "Democracy Index Imputed Data", type = "boxplot")
depunimp<-sjPlot::plot_grpfrq(dat_merge_num_k$dem_index, dat_merge_num_k$country, 
                           axis.title = "Democracy Index Original Data", type = "boxplot")


depimp<-sjPlot::plot_grpfrq(dat_merge_imp_$dem_index, dat_merge_imp_$country, 
                            axis.title = "Democracy Index Imputed Data", type = "boxplot")

cowplot::plot_grid(plotlist = list(depimp, depunimp),ncol=1)

depunimp<-ggplot(dat_merge_num_k, aes(x = dem_index, y = country, color = country, point_color = country, fill = country)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250", "#32CD3250", "#CD262650"), labels = c("Iran", "Iraq", "Syria", "Turkey")) +
  scale_color_manual(values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0","#32CD3250", "#CD262650"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Dependent Variable Unimputed") +
  theme_ridges(center = TRUE)

depimp <- ggplot(dat_merge_imp_, aes(x = dem_index, y = country, color = country, point_color = country, fill = country)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250", "#32CD3250", "#CD262650"), labels = c("Iran", "Iraq", "Syria", "Turkey")) +
  scale_color_manual(values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0","#32CD3250", "#CD262650"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Dependent Variable Imputed") +
  theme_ridges(center = TRUE)

cowplot::plot_grid(plotlist = list(depimp, depunimp),ncol=1)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# COMPARE IMPUTATION MISSINGS IN SUBSETS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TRC_vismiss<-vis_miss(trc)+ggtitle("Missings Turkey")
IRN_vismiss<-vis_miss(irn)+ggtitle("Missings Iran")
IQ_vismiss<-vis_miss(iq)+ggtitle("Missings Iraq")
SY_vismiss<-vis_miss(sy)+ggtitle("Missings Syria")


cowplot::plot_grid(plotlist = list(TRC_vismiss, IRN_vismiss,
                                   IQ_vismiss,SY_vismiss),ncol=2)


dem_miss<-ggplot(dat_merge_num_k, 
       aes(x = sys_democ, 
           y = sys_expert)) + 
  geom_miss_point()+facet_wrap(~country)+ggtitle("Missings in Support Democracy & Expert Rule")
dem_miss2<-ggplot(dat_merge_num_k, 
       aes(x = sys_democ, 
           y = sys_leader)) + 
  geom_miss_point()+facet_wrap(~country)+ggtitle("Missings in Support Democracy & Strong Leader Rule")

cowplot::plot_grid(plotlist = list(dem_miss, dem_miss2),ncol=2)

# POLITICAL INTEREST
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# SYRIA DEMOCRACY INDEX COMPARISON
plot_grpfrq(dat_merge_num_k$dem_index, dat_merge_num_k$country, type = "box")

plot_grpfrq(dat_merge_imp_$dem_index, dat_merge_imp_$country, type = "box")

sjPlot::plot_frq(isy$sys_democ)
sjPlot::plot_frq(isy$sys_expert)
sjPlot::plot_frq(isy$sys_leader)

# VERGLEICHSPLOTS DEMOCRACY INDEX

tr<-dat_merge_num_k%>%
  filter(country=="Turkey")

trp<-ggplot(data=tr,aes(x=sys_democ,y=sys_expert))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Turkey: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

ir<-dat_merge_num_k%>%
  filter(country=="Iran")

irp<-ggplot(data=ir,aes(x=sys_democ,y=sys_expert))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iran: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

iq<-dat_merge_num_k%>%                     
  filter(country=="Iraq")

iqp<-ggplot(data=iq,aes(x=sys_democ,y=sys_expert))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iraq: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

sy<-dat_merge_num_k%>%                     
  filter(country=="Syria")

syp<-ggplot(data=sy,aes(x=sys_democ,y=sys_expert))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Syria: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

cowplot::plot_grid(plotlist = list(irp,trp,iqp,syp),ncol=1)

# _______________________________________________________________

tr<-dat_merge_num_k%>%
  filter(country=="Turkey")

trp<-ggplot(data=tr,aes(x=sys_democ,y=sys_leader))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Turkey: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

ir<-dat_merge_num_k%>%
  filter(country=="Iran")

irp<-ggplot(data=ir,aes(x=sys_democ,y=sys_leader))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iran: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

iq<-dat_merge_num_k%>%                     
  filter(country=="Iraq")

iqp<-ggplot(data=iq,aes(x=sys_democ,y=sys_leader))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iraq: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

sy<-dat_merge_num_k%>%                     
  filter(country=="Syria")

syp<-ggplot(data=sy,aes(x=sys_democ,y=sys_leader))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Syria: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

cowplot::plot_grid(plotlist = list(irp,trp,iqp,syp),ncol=1)

# DEMOCRATIC SYSTEM
tr<-dat_merge_num_k%>%
  filter(country=="Turkey")

trp<-ggplot(data=tr,aes(x=sys_democ,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Turkey: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

ir<-dat_merge_num_k%>%
  filter(country=="Iran")

irp<-ggplot(data=ir,aes(x=sys_democ,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iran: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

iq<-dat_merge_num_k%>%                     
  filter(country=="Iraq")

iqp<-ggplot(data=iq,aes(x=sys_democ,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iraq: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

sy<-dat_merge_num_k%>%                     
  filter(country=="Syria")

syp<-ggplot(data=sy,aes(x=sys_democ,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Syria: Support for Democratic System")+ ggpubr::stat_cor( label.x = 0)

cowplot::plot_grid(plotlist = list(irp,trp,iqp,syp),ncol=1)

# EXPERT SYSTEM
tr<-dat_merge_num_k%>%
  filter(country=="Turkey")

trp<-ggplot(data=tr,aes(x=sys_expert,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Turkey: Support Expert Rule")+ ggpubr::stat_cor( label.x = -3)

ir<-dat_merge_num_k%>%
  filter(country=="Iran")

irp<-ggplot(data=ir,aes(x=sys_expert,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iran: Support Expert Rule")+ ggpubr::stat_cor( label.x = -3)

iq<-dat_merge_num_k%>%                     
  filter(country=="Iraq")

iqp<-ggplot(data=iq,aes(x=sys_expert,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iraq: Support Expert Rule")+ ggpubr::stat_cor( label.x = -3)

sy<-dat_merge_num_k%>%                     #NUR EIGENE SURVEY
  filter(country=="Syria")

syp<-ggplot(data=sy,aes(x=sys_expert,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Syria: Support Expert Rule")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist=list(irp,trp,iqp,syp),ncol=1)

# LEADER SYSTEM
tr<-dat_merge_num_k%>%
  filter(country=="Turkey")

trp<-ggplot(data=tr,aes(x=sys_leader,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Turkey: Support Strong Leader")+ ggpubr::stat_cor( label.x = -3)

ir<-dat_merge_num_k%>%
  filter(country=="Iran")

irp<-ggplot(data=ir,aes(x=sys_leader,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iran: Support Strong Leader")+ ggpubr::stat_cor( label.x = -3)

iq<-dat_merge_num_k%>%                     
  filter(country=="Iraq")

iqp<-ggplot(data=iq,aes(x=sys_leader,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Iraq: Support Strong Leader")+ ggpubr::stat_cor( label.x = -3)

sy<-dat_merge_num_k%>%                   
  filter(country=="Syria")

syp<-ggplot(data=iq,aes(x=sys_leader,y=pol_interest))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Syria: Support Strong Leader")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(irp,trp,iqp,syp),ncol=1)


# RELATIONSHIP DEMOGRAPHY AUTONOMY SUPPORT
library(fmsb)

# %%%%%%%%%%%%%%%%%%%%%%%       IRAQ COMPARISON

iq_sp <- iiq[,c(8:13)]
miq_sp <- lapply(iq_sp, mean)

kiq_sp <- cbind(iiq$conf_armed_kri, iiq$conf_police_kri, iiq$conf_parl_kri,
               iiq$conf_krg, iiq$conf_part_krd, iiq$conf_court_kri)
kiq_sp <- as.data.frame(kiq_sp)
mkiq_sp <- lapply(kiq_sp, mean)

sp_plot1 <- rbind(rep(4,6) , rep(1,6) , as.numeric(miq_sp), as.numeric(mkiq_sp))
sp_plot1 <- as.data.frame(sp_plot1)
rownames(sp_plot1) <- NULL
colnames(sp_plot1) <- colnames(iq_sp)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.1,0.1,0.1,0.3) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.1,0.1,0.1,0.1) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( sp_plot1  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


# Add a legend
legend(x=0.7, y=1, legend = legendname, 
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1.5)


# %%%%%%%%%%%%%%%%%%%%%%%       SYRIA COMPARISON

sy_sp <- isy[,c(8:13)]
msy_sp <- lapply(sy_sp, mean)

ksy_sp <- cbind(isy$conf_armed_nes, isy$conf_police_nes, isy$conf_parl_nes,
                isy$conf_nes, isy$conf_part_nes, isy$conf_court_nes)
ksy_sp <- as.data.frame(ksy_sp)
mksy_sp <- lapply(ksy_sp, mean)

sp_plot2 <- rbind(rep(4,6) , rep(1,6) , as.numeric(msy_sp), as.numeric(mksy_sp))
sp_plot2 <- as.data.frame(sp_plot2)
rownames(sp_plot2) <- NULL
colnames(sp_plot2) <- colnames(sy_sp)


# plot with default options:
radarchart( sp_plot2  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


# Add a legend
legend(x=0.7, y=1, legend = legendname, 
       bty = "n", pch=20 , col=colors_in , text.col = "black",  cex=0.7, pt.cex=1.5)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%       IRAQ COMPARISON

iq_sp <- iiq[,c(8:13)]
miq_sp <- lapply(iq_sp, mean)

kiq_sp <- cbind(iiq$conf_armed_kri, iiq$conf_police_kri, iiq$conf_parl_kri,
                iiq$conf_krg, iiq$conf_part_krd, iiq$conf_court_kri)
kiq_sp <- as.data.frame(kiq_sp)
mkiq_sp <- lapply(kiq_sp, mean)

sp_plot1 <- rbind(rep(4,6) , rep(1,6) , as.numeric(miq_sp), as.numeric(mkiq_sp))
sp_plot1 <- as.data.frame(sp_plot1)
rownames(sp_plot1) <- NULL
colnames(sp_plot1) <- colnames(iq_sp)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.1,0.1,0.1,0.3) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.1,0.1,0.1,0.1) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( sp_plot1  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


# Add a legend
legend(x=0.7, y=1, legend = legendname, 
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1.5)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# BY EDUCATION GENDER 
collight <- c("#2C0DA660", "#217D9460", "#A461AD60", "#82267060", "#A19F9F60", "#66666660", "#34612E60", "#51BD1760", "#65780460", "#37542460")

collong <- c("#2C0DA690", "#217D9490", "#A461AD90", "#82267090", "#A19F9F90", "#66666690", "#34612E90", "#51BD1790", "#65780490", "#37542490")
collong_darf <- c("#2C0DA6", "#217D94", "#A461AD", "#822670", "#A19F9F", "#666666", "#34612E", "#51BD17", "#657804", "#375424")

collight_c <- c("#2C0DA660", "#217D9460", "#A461AD60", "#82267060")

plot_grpfrq(iiq$pol_interest, iiq$gender, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = T, legend.labels = c("male","female"))

plot_grpfrq(iiq$pol_interest, iiq$gender, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = T, legend.labels = c("male","female"))


IW_sub <- cbind(wvs7$Q259, wvs7$country, wvs7$kurd_dummy)
IW_sub <- as.data.frame(IW_sub)
colnames(IW_sub) <- c("closeness_IW", "Country", "Kurd")
IW_sub$Country <- as.character(IW_sub$Country)

IW_sub_i <- filter(IW_sub,IW_sub$Country=="1")
IW_sub_t <- filter(IW_sub,IW_sub$Country=="2")

plot_grpfrq(IW_sub_i$closeness_IW, IW_sub_i$Kurd, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, ylim = 120)

plot_grpfrq(IW_sub_t$closeness_IW, IW_sub_t$Kurd, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, ylim = 120)

# PERCEPTIONS DEMOCRACY
plot_grpfrq(dat_merge_num_k$democ_indec, dat_merge_imp_$country, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, show.prc = F)

plot_grpfrq(dat_merge_num_k$democ_econ, dat_merge_imp_$country, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, show.prc = F)

plot_grpfrq(dat_merge_num_k$democ_order, dat_merge_imp_$country, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, show.prc = F)

plot_grpfrq(dat_merge_num_k$democ_best, dat_merge_imp_$country, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, show.prc = F)

plot_grpfrq(dat_merge_num_k$democ_own, dat_merge_imp_$country, geom.colors = collong, 
            type = "bar",bar.pos = "stack", coord.flip = F, show.prc = F)




plot_grpfrq(iiq$rate_system_kri, iiq$edu, geom.colors = collight_c, 
            type = "box", coord.flip = T)

plot_grpfrq(dat_merge_imp_$sys_army, dat_merge_imp_$country, geom.colors = collight, 
            type = "violin", coord.flip = F)

# ARMY AND DEMOC

ggplot(data=dat_merge_imp_,aes(x=sys_army,y=sys_democ, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  xlab("Kurds: Support for Democracy by Army Rule")+ ggpubr::stat_cor( label.x = -0.5)
 


# AUF EINEN BLICK ################

# POL INTEREST KRAM SYSTEM TYPES
group.colors <- c(Iran="#2C0DA660", Iraq="#217D9460", Syria="#A461AD60", Turkey="#82267060")
 
 
ksup1<-ggplot(data=dat_merge_imp_,aes(x=sys_army,y=pol_interest, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Interest in Politics")+
  xlab("Support for Army Rule")+ ggpubr::stat_cor( label.x = -3)

ksup2<-ggplot(data=dat_merge_imp_,aes(x=sys_expert,y=pol_interest, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Interest in Politics")+
  xlab("Support for Expert Rule")+ ggpubr::stat_cor( label.x = -3)

ksup3<-ggplot(data=dat_merge_imp_,aes(x=sys_leader,y=pol_interest, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm",aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Interest in Politics")+
  xlab("Support for Strong Leader")+ ggpubr::stat_cor( label.x = -3)

ksup4<-ggplot(data=dat_merge_imp_,aes(x=sys_democ,y=pol_interest, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm",aes(col=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Interest in Politics")+
  xlab("Support for Democracy")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(ksup1,ksup2,ksup3,ksup4),ncol=2)

# POL INTEREST RELIGIOSITY 1 Religious 2 Not religious 3 Atheist

ggplot(data=dat_merge_imp_,aes(x=relig_pers,y=pol_interest, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  xlab("Kurds: Political Interest by Religiosity")+ ggpubr::stat_cor( label.x = -0.5)

# POL INTEREST EDU RELIGIOSITY 1 Religious 2 Not religious 3 Atheist

ggplot(data=dat_merge_imp_,aes(x=relig_pers,y=edu, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  xlab("Kurds: Education Level by Religiosity")+ ggpubr::stat_cor( label.x = -0.5)

# AUF EINEN BLICK EDU SYSTEM TYPE 

ksup1<-ggplot(data=dat_merge_imp_,aes(x=sys_army,y=edu, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Education level")+
  xlab("Support for Army Rule")+ ggpubr::stat_cor( label.x = -3)

ksup2<-ggplot(data=dat_merge_imp_,aes(x=sys_expert,y=edu, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Education level")+
  xlab("Support for Expert Rule")+ ggpubr::stat_cor( label.x = -3)

ksup3<-ggplot(data=dat_merge_imp_,aes(x=sys_leader,y=edu, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm",aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Education level")+
  xlab("Support for Strong Leader")+ ggpubr::stat_cor( label.x = -3)

ksup4<-ggplot(data=dat_merge_imp_,aes(x=sys_democ,y=edu, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm",aes(col=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Education level")+
  xlab("Support for Democracy")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(ksup1,ksup2,ksup3,ksup4),ncol=2)

# AUF EINEN BLICK Relig SYSTEM TYPE 

ksup1<-ggplot(data=dat_merge_imp_,aes(x=sys_army,y=relig_pers, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Religiosity")+
  xlab("Support for Army Rule")+ ggpubr::stat_cor( label.x = -3)

ksup2<-ggplot(data=dat_merge_imp_,aes(x=sys_expert,y=relig_pers, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm", aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Religiosity")+
  xlab("Support for Expert Rule")+ ggpubr::stat_cor( label.x = -3)

ksup3<-ggplot(data=dat_merge_imp_,aes(x=sys_leader,y=relig_pers, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm",aes(color=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Religiosity")+
  xlab("Support for Strong Leader")+ ggpubr::stat_cor( label.x = -3)

ksup4<-ggplot(data=dat_merge_imp_,aes(x=sys_democ,y=relig_pers, fill=country))+
  geom_jitter(alpha = 0.5,size=0.2,aes(col=country))+
  geom_smooth(method = "lm",aes(col=country))+
  scale_fill_manual(values=group.colors)+
  ylab("Religiosity")+
  xlab("Support for Democracy")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(ksup1,ksup2,ksup3,ksup4),ncol=2)



# TRUST INSTITUTIONS IQ SUPPORT DEMOC
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conftr1<-ggplot(data=iq,aes(x=sys_democ,y=conf_armed_kri))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Armed Forces")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr2<-ggplot(data=iq,aes(x=sys_democ,y=conf_police_kri))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Police")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr3<-ggplot(data=iq,aes(x=sys_democ,y=conf_parl_kri))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Parliament")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr4<-ggplot(data=iq,aes(x=sys_democ,y=conf_krg))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Government")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr5<-ggplot(data=iq,aes(x=sys_democ,y=conf_part_krd))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Parties")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr6<-ggplot(data=iq,aes(x=sys_democ,y=conf_court_kri))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Courts")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(conftr1,conftr2,conftr3,
                                   conftr4,conftr5,conftr6),ncol=2)

# TRUST INSTITUTIONS SY SUPPORT DEMOC
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conftr1<-ggplot(data=sy,aes(x=sys_democ,y=conf_armed_nes))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Armed Forces")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr2<-ggplot(data=sy,aes(x=sys_democ,y=conf_police_nes))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Police")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr3<-ggplot(data=sy,aes(x=sys_democ,y=conf_parl_nes))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Parliament")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr4<-ggplot(data=sy,aes(x=sys_democ,y=conf_nes))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Government")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr5<-ggplot(data=sy,aes(x=sys_democ,y=conf_part_nes))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Parties")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)
conftr6<-ggplot(data=sy,aes(x=sys_democ,y=conf_court_nes))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Kurdish Courts")+
  xlab("Kurds: Support for Democracy")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(conftr1,conftr2,conftr3,
                                   conftr4,conftr5,conftr6),ncol=2)


# TRUST INSTITUTIONS KRG COMPARISON
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



confir1<-ggplot(data=iq,aes(x=conf_armed_kri,y=conf_armed,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Iraqi Armed Forces")+
  xlab("Confidence in Kurdish Armed Forces")+ ggpubr::stat_cor( label.x = -3)
confir2<-ggplot(data=iq,aes(x=conf_police_kri,y=conf_police,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Iraqi Police")+
  xlab("Confidence in Kurdish Police")+ ggpubr::stat_cor( label.x = -3)
confir3<-ggplot(data=iq,aes(x=conf_parl_kri,y=conf_parl,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Parliament")+
  xlab("Confidence in Kurdish Parliament")+ ggpubr::stat_cor( label.x = -3)
confir4<-ggplot(data=iq,aes(x=conf_krg,y=conf_gov,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Government")+
  xlab("Confidence in Kurdish Government")+ ggpubr::stat_cor( label.x = -3)
confir5<-ggplot(data=iq,aes(x=conf_part_krd,y=conf_part,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Parties")+
  xlab("Confidence in Kurdish Parties")+ ggpubr::stat_cor( label.x = -3)
confir6<-ggplot(data=iq,aes(x=conf_court_kri,y=conf_court,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Courts")+
  xlab("Confidence in Kurdish Courts")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(confir1,confir2,confir3,
                          confir4,confir5,confir6),ncol=2)


# TRUST INSTITUTIONS NES COMPARISON
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


confsy1<-ggplot(data=sy,aes(x=conf_armed_nes,y=conf_armed,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Syrian Armed Forces")+
  xlab("Confidence in Syrian Democratic Forces")+ ggpubr::stat_cor( label.x = -3)
confsy2<-ggplot(data=sy,aes(x=conf_police_nes,y=conf_police,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Syrian Police")+
  xlab("Confidence in NES Police")+ ggpubr::stat_cor( label.x = -3)
confsy3<-ggplot(data=sy,aes(x=conf_parl_nes,y=conf_parl,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Parliament")+
  xlab("Confidence in Syrian Democratic Council")+ ggpubr::stat_cor( label.x = -3)
confsy4<-ggplot(data=sy,aes(x=conf_nes,y=conf_gov,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Government")+
  xlab("Confidence in Autonomous Administration of North and East Syria")+ ggpubr::stat_cor( label.x = -3)
confsy5<-ggplot(data=sy,aes(x=conf_part_nes,y=conf_part,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Parties")+
  xlab("Confidence in Kurdish Parties")+ ggpubr::stat_cor( label.x = -3)
confsy6<-ggplot(data=sy,aes(x=conf_court_nes,y=conf_court,col=dem_index))+
  geom_jitter(alpha = 0.5,size=0.02)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Confidence in Courts")+
  xlab("Confidence in NES Courts")+ ggpubr::stat_cor( label.x = -3)

cowplot::plot_grid(plotlist = list(confsy1,confsy2,confsy3,
                                   confsy4,confsy5,confsy6),ncol=2)



dat_merge_k$edu_level <- factor(dat_merge_k$edu, levels=c("Illiterate or no formal education",
                                                    "Elementary attended; completed or incompleted",
                                                    "Secondary school attended; completed or incompleted",
                                                    "Postsecondary school attended; completed or incompleted",
                                                    "Lower tertiary education",
                                                    "Upper tertiary education"))


ggplot(data=dat_merge_k,aes(x=edu_level,y=relig_pers,col=country))+
  geom_jitter(alpha = 0.5,size=1)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Religiosity")+
  xlab("Education")+ ggpubr::stat_cor( label.x = -3)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  coord_flip()


# plot dependent variable index

#TODO

# CONFIDENCE GOV

#TODO X AXIS LABEL

ggplot(dat_merge_num_k, aes(x = conf_gov, y = country, color = country, point_color = country, fill = country)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "height [cm]") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250", "#32CD3250", "#CD262650"), labels = c("Iran", "Iraq", "Syria", "Turkey")) +
  scale_color_manual(values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0","#32CD3250", "#CD262650"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Confidence in Government") +
  theme_ridges(center = TRUE)

# CONF ARMED FORCES

ggplot(dat_merge_num_k, aes(x = conf_armed, y = country, color = country, point_color = country, fill = country)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "height [cm]") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250", "#32CD3250", "#CD262650"), labels = c("Iran", "Iraq", "Syria", "Turkey")) +
  scale_color_manual(values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0","#32CD3250", "#CD262650"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Confidence in Armed Forces") +
  theme_ridges(center = TRUE)

# CONF POLICE

ggplot(dat_merge_num_k, aes(x = conf_police, y = country, color = country, point_color = country, fill = country)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "height [cm]") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250", "#32CD3250", "#CD262650"), labels = c("Iran", "Iraq", "Syria", "Turkey")) +
  scale_color_manual(values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0","#32CD3250", "#CD262650"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Confidence in Police") +
  theme_ridges(center = TRUE)


# CONF COURTS

ggplot(dat_merge_num_k, aes(x = conf_court, y = country, color = country, point_color = country, fill = country)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Confidence in Courts") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250", "#32CD3250", "#CD262650"), labels = c("Iran", "Iraq", "Syria", "Turkey")) +
  scale_color_manual(values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2","#32CD32", "#CD2626"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0","#32CD3250", "#CD262650"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Confidence in Courts") +
  theme_ridges(center = TRUE)


# ---- NANIAR plot ----



vis_miss(wvs7_sub)
vis_miss(krg_chr_NA)
vis_miss(roj_chr_NA)


ggplot(wvs7_sub,
       aes(x = country,
           y = region)) +
  geom_miss_point()

gg_miss_fct(wvs7_sub, country)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ---- Regional map ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(sf)
library(ggplot2)

# Load your shapefile or geojson file containing region boundaries
iran_1 <- st_read("./shapefile/IRN_adm1.shp")
iraq_1 <- st_read("./shapefile/IRQ_adm1.shp")
syria_1 <- st_read("./shapefile/SYR_adm1.shp")
turk_1 <- st_read("./shapefile/TUR_adm1.shp")

kurdistan <- rbind(iran_1, iraq_1, syria_1, turk_1)


test <- st_read("./shapefile/SYR_adm2.shp")

kurdistan <- kurdistan %>%
  mutate(kurdregion = case_when(conf_gov >0  ~ 1))

ggplot() +
  geom_sf(data = kurdistan, aes(fill=kurdregion), color = "black")+
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ANDERE
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF PART




kurdistan <- kurdistan %>%
  mutate(pol_interest = case_when(
    NAME_1 == "Aleppo" ~ 3.119048,
    NAME_1 == "Dihok" ~ 2.576271,
    NAME_1 == "Arbil" ~ 2.807018,
    NAME_1 == "As-Sulaymaniyah" ~ 2.19493,
    NAME_1 == "Alborz" ~ 2.333333,
    NAME_1 == "Bushehr" ~ 2.5,
    NAME_1 == "Fars" ~ 1.75,
    NAME_1 == "Ilam" ~ 2.315789,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.222222,
    NAME_1 == "North Khorasan" ~ 3,
    NAME_1 == "Khuzestan" ~ 2.666667,
    NAME_1 == "Kordestan" ~ 2.033333,
    NAME_1 == "Lorestan" ~ 2.357143,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 2.294118,
    NAME_1 == "West Azarbaijan" ~ 2.000000,
    NAME_1 == "Al Ḥasakah" ~ 3.444444,
    NAME_1 == "Ar Raqqah" ~ 3.5,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 2.062500,
    NAME_1 == "Elazığ" ~ 2.062500,
    NAME_1 == "Bingöl" ~ 2.062500,
    NAME_1 == "Tunceli" ~ 2.062500,
    NAME_1 == "Van" ~ 2.7,
    NAME_1 == "Mus" ~ 2.7,
    NAME_1 == "Bitlis" ~ 2.7,
    NAME_1 == "Hakkari" ~ 2.7,
    NAME_1 == "Sanliurfa" ~ 2.543478,
    NAME_1 == "Diyarbakir" ~ 2.543478,
    NAME_1 == "Mardin" ~ 2.622222,
    NAME_1 == "Batman" ~ 2.622222,
    NAME_1 == "Sirnak" ~ 2.622222,
    NAME_1 == "Siirt" ~ 2.622222
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = pol_interest), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Political Interest by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = pol_interest), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Political Interest by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# DEMOC OWN

aggregate(dat_merge_num_k$democ_own, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 

grad_lims <- c(1,10)


kurdistan <- kurdistan %>%
  mutate(democ_own = case_when(
    NAME_1 == "Aleppo" ~ 1.896825,
    NAME_1 == "Dihok" ~ 3.770492,
    NAME_1 == "Arbil" ~ 3.3,
    NAME_1 == "As-Sulaymaniyah" ~ 3.299825,
    NAME_1 == "Alborz" ~ 5,
    NAME_1 == "Bushehr" ~ 6.75,
    NAME_1 == "Fars" ~ 5.666667,
    NAME_1 == "Ilam" ~ 5.894737,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 5.840000,
    NAME_1 == "North Khorasan" ~ 7,
    NAME_1 == "Khuzestan" ~ 6.035714,
    NAME_1 == "Kordestan" ~ 7.259259,
    NAME_1 == "Lorestan" ~ 6.692308,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 6.058824,
    NAME_1 == "West Azarbaijan" ~ 8.333333,
    NAME_1 == "Al Ḥasakah" ~ 2.750000,
    NAME_1 == "Ar Raqqah" ~ 2.25,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 7.031250,
    NAME_1 == "Elazığ" ~ 7.031250,
    NAME_1 == "Bingöl" ~ 7.031250,
    NAME_1 == "Tunceli" ~ 7.031250,
    NAME_1 == "Van" ~ 6.098592,
    NAME_1 == "Mus" ~ 6.098592,
    NAME_1 == "Bitlis" ~ 6.098592,
    NAME_1 == "Hakkari" ~ 6.098592,
    NAME_1 == "Sanliurfa" ~ 6.068182,
    NAME_1 == "Diyarbakir" ~ 6.068182,
    NAME_1 == "Mardin" ~ 6.684211,
    NAME_1 == "Batman" ~ 6.684211,
    NAME_1 == "Sirnak" ~ 6.684211,
    NAME_1 == "Siirt" ~ 6.684211
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = democ_own), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Perception of How Democratic Own Country Is") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = democ_own), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Perception of How Democratic Own Country Is") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DEM INDEX

aggregate(dat_merge_imp_$dem_index, list(dat_merge_imp_$region), FUN=mean) 

grad_lims <- c(1,4)

kurdistan$dem_index <- 0

kurdistan <- kurdistan %>%
  mutate(dem_index = case_when(
    NAME_1 == "Aleppo" ~ 5.952381,
    NAME_1 == "Dihok" ~ 4.213115,
    NAME_1 == "Arbil" ~ 4.833333,
    NAME_1 == "As-Sulaymaniyah" ~ 4.462704,
    NAME_1 == "Alborz" ~ 5,
    NAME_1 == "Bushehr" ~ 4.75,
    NAME_1 == "Fars" ~ 4.75,
    NAME_1 == "Ilam" ~ 5.526316,
   # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 5.629630,
    NAME_1 == "North Khorasan" ~ 5,
    NAME_1 == "Khuzestan" ~ 5.233333,
    NAME_1 == "Kordestan" ~ 4.833333,
    NAME_1 == "Lorestan" ~ 5.071429,
 #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 5.941176,
    NAME_1 == "West Azarbaijan" ~ 5.3,
    NAME_1 == "Al Ḥasakah" ~ 5.964286,
 NAME_1 == "Ar Raqqah" ~ 4.583333,
    #NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 4.611111,
    NAME_1 == "Elazığ" ~ 4.611111,
    NAME_1 == "Bingöl" ~ 4.611111,
    NAME_1 == "Tunceli" ~ 4.611111,
    NAME_1 == "Van" ~ 4.569444,
    NAME_1 == "Mus" ~ 4.569444,
    NAME_1 == "Bitlis" ~ 4.569444,
    NAME_1 == "Hakkari" ~ 4.569444,
    NAME_1 == "Sanliurfa" ~ 4.726316,
    NAME_1 == "Diyarbakir" ~ 4.726316,
    NAME_1 == "Mardin" ~ 4.673469,
    NAME_1 == "Batman" ~ 4.673469,
    NAME_1 == "Sirnak" ~ 4.673469,
    NAME_1 == "Siirt" ~ 4.673469
  ))

# Ensure your "region" variable is a factor
kurdistan$NAME_1 <- as.factor(kurdistan$NAME_1)

  
ggplot() +
  geom_sf(data = kurdistan)+
  xlim(35.000, 50.000)+ ylim(32.000, 40.000)

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = dem_index), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6")+
  labs(title = "Democracy Index by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SYS DEMOC

aggregate(dat_merge_imp_$sys_democ, list(dat_merge_imp_$region), FUN=mean) 



kurdistan <- kurdistan %>%
  mutate(sys_democ = case_when(
    NAME_1 == "Aleppo" ~ 3.65873,
    NAME_1 == "Dihok" ~ 3.377049,
    NAME_1 == "Arbil" ~ 3.450000,
    NAME_1 == "As-Sulaymaniyah" ~ 3.254662,
    NAME_1 == "Alborz" ~ 4.000000,
    NAME_1 == "Bushehr" ~ 3.750000,
    NAME_1 == "Fars" ~ 3.250000,
    NAME_1 == "Ilam" ~ 3.473684,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 3.592593,
    NAME_1 == "North Khorasan" ~ 3.5,
    NAME_1 == "Khuzestan" ~ 3.700000,
    NAME_1 == "Kordestan" ~ 3.533333,
    NAME_1 == "Lorestan" ~ 3.642857,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 3.882353,
    NAME_1 == "West Azarbaijan" ~ 3.6,
    NAME_1 == "Al Ḥasakah" ~ 3.571429,
    NAME_1 == "Ar Raqqah" ~ 3.7,
#    NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 3.361111,
    NAME_1 == "Elazığ" ~ 3.361111,
    NAME_1 == "Bingöl" ~ 3.361111,
    NAME_1 == "Tunceli" ~ 3.361111,
    NAME_1 == "Van" ~ 2.986111,
    NAME_1 == "Mus" ~ 2.986111,
    NAME_1 == "Bitlis" ~ 2.986111,
    NAME_1 == "Hakkari" ~ 2.986111,
    NAME_1 == "Sanliurfa" ~ 3.010526,
    NAME_1 == "Diyarbakir" ~ 3.010526,
    NAME_1 == "Mardin" ~ 2.836735,
    NAME_1 == "Batman" ~ 2.836735,
    NAME_1 == "Sirnak" ~ 2.836735,
    NAME_1 == "Siirt" ~ 2.836735
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_democ), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6",limits = grad_lims)+
  labs(title = "Support for Democracy by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)


# Create the map compare intern scale gradient
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_democ), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6")+
  labs(title = "Support for Democracy by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SYS LEADER

aggregate(dat_merge_imp_$sys_leader, list(dat_merge_imp_$region), FUN=mean) 



kurdistan <- kurdistan %>%
  mutate(sys_leader = case_when(
    NAME_1 == "Aleppo" ~ 1.780423,
    NAME_1 == "Dihok" ~ 2.901639,
    NAME_1 == "Arbil" ~ 2.233333,
    NAME_1 == "As-Sulaymaniyah" ~ 2.533216,
    NAME_1 == "Alborz" ~ 2,
    NAME_1 == "Bushehr" ~ 3.750000,
    NAME_1 == "Fars" ~ 2.75,
    NAME_1 == "Ilam" ~ 2.157895,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.296296,
    NAME_1 == "North Khorasan" ~ 1.500000,
    NAME_1 == "Khuzestan" ~ 2.566667,
    NAME_1 == "Kordestan" ~ 2.6,
    NAME_1 == "Lorestan" ~ 2.714286,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 1.941176,
    NAME_1 == "West Azarbaijan" ~ 2.7,
    NAME_1 == "Al Ḥasakah" ~ 2,
    NAME_1 == "Ar Raqqah" ~ 2.708334,
    #    NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 2.972222,
    NAME_1 == "Elazığ" ~ 2.972222,
    NAME_1 == "Bingöl" ~ 2.972222,
    NAME_1 == "Tunceli" ~ 2.972222,
    NAME_1 == "Van" ~ 2.583333,
    NAME_1 == "Mus" ~ 2.583333,
    NAME_1 == "Bitlis" ~ 2.583333,
    NAME_1 == "Hakkari" ~ 2.583333,
    NAME_1 == "Sanliurfa" ~ 2.631579,
    NAME_1 == "Diyarbakir" ~ 2.631579,
    NAME_1 == "Mardin" ~ 2.673469,
    NAME_1 == "Batman" ~ 2.673469,
    NAME_1 == "Sirnak" ~ 2.673469,
    NAME_1 == "Siirt" ~ 2.673469
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_leader), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6",limits = grad_lims)+
  labs(title = "Support for Strong Leader by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_leader), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6")+
  labs(title = "Support for Strong Leader by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SYS EXPERT

aggregate(dat_merge_imp_$sys_expert, list(dat_merge_imp_$region), FUN=mean) 



kurdistan <- kurdistan %>%
  mutate(sys_expert = case_when(
    NAME_1 == "Aleppo" ~ 2.933333,
    NAME_1 == "Dihok" ~ 3.262295,
    NAME_1 == "Arbil" ~ 3.383333,
    NAME_1 == "As-Sulaymaniyah" ~ 3.258741,
    NAME_1 == "Alborz" ~ 4,
    NAME_1 == "Bushehr" ~ 2.250000,
    NAME_1 == "Fars" ~ 2.75,
    NAME_1 == "Ilam" ~ 2.789474,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.666667,
    NAME_1 == "North Khorasan" ~ 4,
    NAME_1 == "Khuzestan" ~ 2.9,
    NAME_1 == "Kordestan" ~ 3.1,
    NAME_1 == "Lorestan" ~ 2.857143,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 3,
    NAME_1 == "West Azarbaijan" ~ 2.6,
    NAME_1 == "Al Ḥasakah" ~ 3.2,
    NAME_1 == "Ar Raqqah" ~ 3.75,
    #    NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 2.777778,
    NAME_1 == "Elazığ" ~ 2.777778,
    NAME_1 == "Bingöl" ~ 2.777778,
    NAME_1 == "Tunceli" ~ 2.777778,
    NAME_1 == "Van" ~ 2.833333,
    NAME_1 == "Mus" ~ 2.833333,
    NAME_1 == "Bitlis" ~ 2.833333,
    NAME_1 == "Hakkari" ~ 2.833333,
    NAME_1 == "Sanliurfa" ~ 2.652632,
    NAME_1 == "Diyarbakir" ~ 2.652632,
    NAME_1 == "Mardin" ~ 2.489796,
    NAME_1 == "Batman" ~ 2.489796,
    NAME_1 == "Sirnak" ~ 2.489796,
    NAME_1 == "Siirt" ~ 2.489796
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_expert), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6",limits = grad_lims)+
  labs(title = "Support for Expert Rule by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_expert), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6")+
  labs(title = "Support for Expert Rule by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SYSTEM STATE TRUST VARIABLES

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SYS ARMY

aggregate(dat_merge_num_k$sys_army, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(sys_army = case_when(
    NAME_1 == "Aleppo" ~ 1.681818,
    NAME_1 == "Dihok" ~ 2.403509,
    NAME_1 == "Arbil" ~ 2.120690,
    NAME_1 == "As-Sulaymaniyah" ~ 2.157142,
    NAME_1 == "Alborz" ~ 1.4,
    NAME_1 == "Bushehr" ~ 3.250000,
    NAME_1 == "Fars" ~ 2.5,
    NAME_1 == "Ilam" ~ 2.473684,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.153846,
    NAME_1 == "North Khorasan" ~ 2,
    NAME_1 == "Khuzestan" ~ 2.035714,
    NAME_1 == "Kordestan" ~ 2.3,
    NAME_1 == "Lorestan" ~ 2.214286,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 1.705882,
    NAME_1 == "West Azarbaijan" ~ 2.300000,
    NAME_1 == "Al Ḥasakah" ~ 2.038462,
    NAME_1 == "Ar Raqqah" ~ 2.6,
    #    NAME_1 == "Dayr Az Zawr" ~ 4.291666,
#    NAME_1 == "Malatya" ~ 2.777778,
#    NAME_1 == "Elazığ" ~ 2.777778,
#    NAME_1 == "Bingöl" ~ 2.777778,
#    NAME_1 == "Tunceli" ~ 2.777778,
#    NAME_1 == "Van" ~ 2.833333,
#    NAME_1 == "Mus" ~ 2.833333,
#    NAME_1 == "Bitlis" ~ 2.833333,
#    NAME_1 == "Hakkari" ~ 2.833333,
#    NAME_1 == "Sanliurfa" ~ 2.652632,
#    NAME_1 == "Diyarbakir" ~ 2.652632,
#    NAME_1 == "Mardin" ~ 2.489796,
#    NAME_1 == "Batman" ~ 2.489796,
#    NAME_1 == "Sirnak" ~ 2.489796,
#    NAME_1 == "Siirt" ~ 2.489796
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_army), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6",limits = grad_lims)+
  labs(title = "Support for Army Rule by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = sys_army), color = "black") +
  scale_fill_gradient(low = "#A461AD", high = "#2C0DA6")+
  labs(title = "Support for Army Rule by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF ARMED

aggregate(dat_merge_num_k$conf_armed, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(conf_armed = case_when(
    NAME_1 == "Aleppo" ~ 1.526455,
    NAME_1 == "Dihok" ~ 2.020833,
    NAME_1 == "Arbil" ~ 1.5,
    NAME_1 == "As-Sulaymaniyah" ~ 1.970588,
    NAME_1 == "Alborz" ~ 3.666667,
    NAME_1 == "Bushehr" ~ 3.750000,
    NAME_1 == "Fars" ~ 3,
    NAME_1 == "Ilam" ~ 3.631579,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 3.629630,
    NAME_1 == "North Khorasan" ~ 4,
    NAME_1 == "Khuzestan" ~ 3.466667,
    NAME_1 == "Kordestan" ~ 3.466667,
    NAME_1 == "Lorestan" ~ 3.785714,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 3.812500,
    NAME_1 == "West Azarbaijan" ~ 3.600000,
    NAME_1 == "Al Ḥasakah" ~ 1.2,
    NAME_1 == "Ar Raqqah" ~ 1.3,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 3.617647,
    NAME_1 == "Elazığ" ~ 3.617647,
    NAME_1 == "Bingöl" ~ 3.617647,
    NAME_1 == "Tunceli" ~ 3.617647,
    NAME_1 == "Van" ~ 3.014085,
    NAME_1 == "Mus" ~ 3.014085,
    NAME_1 == "Bitlis" ~ 3.014085,
    NAME_1 == "Hakkari" ~ 3.014085,
    NAME_1 == "Sanliurfa" ~ 3.011236,
    NAME_1 == "Diyarbakir" ~ 3.011236,
    NAME_1 == "Mardin" ~ 3.191489,
    NAME_1 == "Batman" ~ 3.191489,
    NAME_1 == "Sirnak" ~ 3.191489,
    NAME_1 == "Siirt" ~ 3.191489
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_armed), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Confidence in State Army") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_armed), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Confidence in State Army") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF POLICE

aggregate(dat_merge_num_k$conf_police, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(conf_police = case_when(
    NAME_1 == "Aleppo" ~ 1.566137,
    NAME_1 == "Dihok" ~ 2.060000,
    NAME_1 == "Arbil" ~ 1.608696,
    NAME_1 == "As-Sulaymaniyah" ~ 2.095833,
    NAME_1 == "Alborz" ~ 3,
    NAME_1 == "Bushehr" ~ 3.5,
    NAME_1 == "Fars" ~ 3.421053,
    NAME_1 == "Ilam" ~ 3.421053,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 3.259259,
    NAME_1 == "North Khorasan" ~ 3.5,
    NAME_1 == "Khuzestan" ~ 3.166667,
    NAME_1 == "Kordestan" ~ 3.266667,
    NAME_1 == "Lorestan" ~ 3.571429,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 3.294118,
    NAME_1 == "West Azarbaijan" ~ 3.600000,
    NAME_1 == "Al Ḥasakah" ~ 1.320000,
    NAME_1 == "Ar Raqqah" ~ 1.3,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 3.638889,
    NAME_1 == "Elazığ" ~ 3.638889,
    NAME_1 == "Bingöl" ~ 3.638889,
    NAME_1 == "Tunceli" ~ 3.638889,
    NAME_1 == "Van" ~ 3.142857,
    NAME_1 == "Mus" ~ 3.142857,
    NAME_1 == "Bitlis" ~ 3.142857,
    NAME_1 == "Hakkari" ~ 3.142857,
    NAME_1 == "Sanliurfa" ~ 3.010870,
    NAME_1 == "Diyarbakir" ~ 3.010870,
    NAME_1 == "Mardin" ~ 3.166667,
    NAME_1 == "Batman" ~ 3.166667,
    NAME_1 == "Sirnak" ~ 3.166667,
    NAME_1 == "Siirt" ~ 3.166667
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_police), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Confidence State Police") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_police), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Confidence State Police") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF PARLIAMENT

aggregate(dat_merge_num_k$conf_parl, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(conf_parl = case_when(
    NAME_1 == "Aleppo" ~ 1.49359,
    NAME_1 == "Dihok" ~ 1.763636,
    NAME_1 == "Arbil" ~ 1.711538,
    NAME_1 == "As-Sulaymaniyah" ~ 2.069659,
    NAME_1 == "Alborz" ~ 3.666667,
    NAME_1 == "Bushehr" ~ 2.5,
    NAME_1 == "Fars" ~ 2.333333,
    NAME_1 == "Ilam" ~ 2.421053,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.961538,
    NAME_1 == "North Khorasan" ~ 3,
    NAME_1 == "Khuzestan" ~ 2.586207,
    NAME_1 == "Kordestan" ~ 2.793103,
    NAME_1 == "Lorestan" ~ 3.142857,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 2.411765,
    NAME_1 == "West Azarbaijan" ~ 2.400000,
    NAME_1 == "Al Ḥasakah" ~ 1.454545,
    NAME_1 == "Ar Raqqah" ~ 1.4,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 3.187500,
    NAME_1 == "Elazığ" ~ 3.187500,
    NAME_1 == "Bingöl" ~ 3.187500,
    NAME_1 == "Tunceli" ~ 3.187500,
    NAME_1 == "Van" ~ 2.5,
    NAME_1 == "Mus" ~ 2.5,
    NAME_1 == "Bitlis" ~ 2.5,
    NAME_1 == "Hakkari" ~ 2.5,
    NAME_1 == "Sanliurfa" ~ 2.704545,
    NAME_1 == "Diyarbakir" ~ 2.704545,
    NAME_1 == "Mardin" ~ 2.681818,
    NAME_1 == "Batman" ~ 2.681818,
    NAME_1 == "Sirnak" ~ 2.681818,
    NAME_1 == "Siirt" ~ 2.681818
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_parl), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Confidence State Parliament") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_parl), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Confidence State Parliament") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF GOV

aggregate(dat_merge_num_k$conf_gov, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(conf_gov = case_when(
    NAME_1 == "Aleppo" ~ 1.472934,
    NAME_1 == "Dihok" ~ 1.981481,
    NAME_1 == "Arbil" ~ 1.703704,
    NAME_1 == "As-Sulaymaniyah" ~ 2.239706,
    NAME_1 == "Alborz" ~ 2,
    NAME_1 == "Bushehr" ~ 2,
    NAME_1 == "Fars" ~ 2.5,
    NAME_1 == "Ilam" ~ 2.526316,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.777778,
    NAME_1 == "North Khorasan" ~ 3,
    NAME_1 == "Khuzestan" ~ 2.566667,
    NAME_1 == "Kordestan" ~ 2.833333,
    NAME_1 == "Lorestan" ~ 2.714286,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 2.235294,
    NAME_1 == "West Azarbaijan" ~ 2.900000,
    NAME_1 == "Al Ḥasakah" ~ 1.259259,
    NAME_1 == "Ar Raqqah" ~ 1.3,
 #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 3.545455,
    NAME_1 == "Elazığ" ~ 3.545455,
    NAME_1 == "Bingöl" ~ 3.545455,
    NAME_1 == "Tunceli" ~ 3.545455,
    NAME_1 == "Van" ~ 2.9,
    NAME_1 == "Mus" ~ 2.9,
    NAME_1 == "Bitlis" ~ 2.9,
    NAME_1 == "Hakkari" ~ 2.9,
    NAME_1 == "Sanliurfa" ~ 2.817204,
    NAME_1 == "Diyarbakir" ~ 2.817204,
    NAME_1 == "Mardin" ~ 2.840909,
    NAME_1 == "Batman" ~ 2.840909,
    NAME_1 == "Sirnak" ~ 2.840909,
    NAME_1 == "Siirt" ~ 2.840909
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_gov), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Confidence State Government") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_gov), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Confidence Government by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF PART

aggregate(dat_merge_num_k$conf_part, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(conf_part = case_when(
    NAME_1 == "Aleppo" ~ 1.805556,
    NAME_1 == "Dihok" ~ 1.470588,
    NAME_1 == "Arbil" ~ 1.446429,
    NAME_1 == "As-Sulaymaniyah" ~ 1.383677,
    NAME_1 == "Alborz" ~ 2,
    NAME_1 == "Bushehr" ~ 2,
    NAME_1 == "Fars" ~ 2.333333,
    NAME_1 == "Ilam" ~ 2.333333,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.346154,
    NAME_1 == "North Khorasan" ~ 3,
    NAME_1 == "Khuzestan" ~ 2.413793,
    NAME_1 == "Kordestan" ~ 2.137931,
    NAME_1 == "Lorestan" ~ 2.461538,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 2.176471,
    NAME_1 == "West Azarbaijan" ~ 1.800000,
    NAME_1 == "Al Ḥasakah" ~ 1.640000,
    NAME_1 == "Ar Raqqah" ~ 1.5,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 2.678571,
    NAME_1 == "Elazığ" ~ 2.678571,
    NAME_1 == "Bingöl" ~ 2.678571,
    NAME_1 == "Tunceli" ~ 2.678571,
    NAME_1 == "Van" ~ 2.514706,
    NAME_1 == "Mus" ~ 2.514706,
    NAME_1 == "Bitlis" ~ 2.514706,
    NAME_1 == "Hakkari" ~ 2.514706,
    NAME_1 == "Sanliurfa" ~ 2.714286,
    NAME_1 == "Diyarbakir" ~ 2.714286,
    NAME_1 == "Mardin" ~ 2.372093,
    NAME_1 == "Batman" ~ 2.372093,
    NAME_1 == "Sirnak" ~ 2.372093,
    NAME_1 == "Siirt" ~ 2.372093
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_part), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Confidence State Parties") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_part), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Confidence Parties by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF PART

aggregate(dat_merge_num_k$conf_court, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(conf_court = case_when(
    NAME_1 == "Aleppo" ~ 1.576719,
    NAME_1 == "Dihok" ~ 1.765957,
    NAME_1 == "Arbil" ~ 1.687500,
    NAME_1 == "As-Sulaymaniyah" ~ 2.293773,
    NAME_1 == "Alborz" ~ 4,
    NAME_1 == "Bushehr" ~ 3,
    NAME_1 == "Fars" ~ 1.75,
    NAME_1 == "Ilam" ~ 3.052632,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 3.222222,
    NAME_1 == "North Khorasan" ~ 3.5,
    NAME_1 == "Khuzestan" ~ 2.9,
    NAME_1 == "Kordestan" ~ 2.965517,
    NAME_1 == "Lorestan" ~ 3.357143,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 2.875000,
    NAME_1 == "West Azarbaijan" ~ 3.400000,
    NAME_1 == "Al Ḥasakah" ~ 1.541667,
    NAME_1 == "Ar Raqqah" ~ 1.277778,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 3.500000,
    NAME_1 == "Elazığ" ~ 3.500000,
    NAME_1 == "Bingöl" ~ 3.500000,
    NAME_1 == "Tunceli" ~ 3.500000,
    NAME_1 == "Van" ~ 2.9,
    NAME_1 == "Mus" ~ 2.9,
    NAME_1 == "Bitlis" ~ 2.9,
    NAME_1 == "Hakkari" ~ 2.9,
    NAME_1 == "Sanliurfa" ~ 2.615385,
    NAME_1 == "Diyarbakir" ~ 2.615385,
    NAME_1 == "Mardin" ~ 2.711111,
    NAME_1 == "Batman" ~ 2.711111,
    NAME_1 == "Sirnak" ~ 2.711111,
    NAME_1 == "Siirt" ~ 2.711111
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_court), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94",limits = grad_lims)+
  labs(title = "Confidence State Courts") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_court), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#217D94")+
  labs(title = "Confidence Courts by Region") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF PART

aggregate(dat_merge_num_k$pol_interest, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 



kurdistan <- kurdistan %>%
  mutate(interest_pol = case_when(
    NAME_1 == "Aleppo" ~ 3.119048,
    NAME_1 == "Dihok" ~ 2.573770,
    NAME_1 == "Arbil" ~ 2.833333,
    NAME_1 == "As-Sulaymaniyah" ~ 2.19493,
    NAME_1 == "Alborz" ~ 2.33333,
    NAME_1 == "Bushehr" ~ 2.5,
    NAME_1 == "Fars" ~ 1.75,
    NAME_1 == "Ilam" ~ 2.315789,
    # NAME_1 == "Esfahan" ~ 1,
    NAME_1 == "Kermanshah" ~ 2.222222,
    NAME_1 == "North Khorasan" ~ 3,
    NAME_1 == "Khuzestan" ~ 2.666667,
    NAME_1 == "Kordestan" ~ 2.033333,
    NAME_1 == "Lorestan" ~ 2.357143,
    #   NAME_1 == "Markazi" ~ 3,
    NAME_1 == "Tehran" ~ 2.294118,
    NAME_1 == "West Azarbaijan" ~ 2,
    NAME_1 == "Al Ḥasakah" ~ 3.428571,
    NAME_1 == "Ar Raqqah" ~ 3,
    #   NAME_1 == "Dayr Az Zawr" ~ 4.291666,
    NAME_1 == "Malatya" ~ 2.027778,
    NAME_1 == "Elazığ" ~ 2.027778,
    NAME_1 == "Bingöl" ~ 2.027778,
    NAME_1 == "Tunceli" ~ 2.027778,
    NAME_1 == "Van" ~ 2.694444,
    NAME_1 == "Mus" ~ 2.694444,
    NAME_1 == "Bitlis" ~ 2.694444,
    NAME_1 == "Hakkari" ~ 2.694444,
    NAME_1 == "Sanliurfa" ~ 2.515789,
    NAME_1 == "Diyarbakir" ~ 2.515789,
    NAME_1 == "Mardin" ~ 2.551020,
    NAME_1 == "Batman" ~ 2.551020,
    NAME_1 == "Sirnak" ~ 2.551020,
    NAME_1 == "Siirt" ~ 2.551020
  ))

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = interest_pol), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#07D9D9",limits = grad_lims)+
  labs(title = "Interest in Politics (Generally by Scale)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)



# Create the map compare gradient intern
ggplot() +
  geom_sf(data = kurdistan, aes(fill = interest_pol), color = "black") +
  scale_fill_gradient(low = "#A19F9F", high = "#07D9D9")+
  labs(title = "Interest in Politics (Internal Comparison)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 60.000)+ ylim(32.000, 40.000)











# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# AUTONOMY VARIABLES KRG AANES

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF AUTONOMY

aggregate(dat_merge_imp_$conf_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_krg, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_auton = case_when(
    NAME_1 == "Aleppo" ~ 3.26455,
    NAME_1 == "Dihok" ~ 2.885246,
    NAME_1 == "Arbil" ~ 2.116667,
    NAME_1 == "As-Sulaymaniyah" ~ 1.358392,
    NAME_1 == "Al Ḥasakah" ~ 3.142857,
    NAME_1 == "Ar Raqqah" ~ 3.333333
  ))

grad_lims <- c(1,4)

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Own Autonomy (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RATE SYSTEM AUTONOMY

aggregate(dat_merge_imp_$rate_system_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$rate_system_kri, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 




kurdistan <- kurdistan %>%
  mutate(statesys = case_when(
    NAME_1 == "Aleppo" ~ 1.785714,
    NAME_1 == "Dihok" ~ 3.573770,
    NAME_1 == "Arbil" ~ 2.833333,
    NAME_1 == "As-Sulaymaniyah" ~ 3.704545,
    NAME_1 == "Al Ḥasakah" ~ 1.964286,
    NAME_1 == "Ar Raqqah" ~ 1.75
  ))

grad_lims <- c(0,10)

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = statesys), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Political Rating of State System") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

#


kurdistan <- kurdistan %>%
  mutate(rate_auton = case_when(
    NAME_1 == "Aleppo" ~ 7.547619,
    NAME_1 == "Dihok" ~ 6.262295,
    NAME_1 == "Arbil" ~ 4.350000,
    NAME_1 == "As-Sulaymaniyah" ~ 2.038461,
    NAME_1 == "Al Ḥasakah" ~ 5.750000,
    NAME_1 == "Ar Raqqah" ~ 5.458334
  ))

grad_lims <- c(0,10)

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = rate_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Political Rating of Own Autonomy (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# CONF KRG NES

aggregate(dat_merge_imp_$conf_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_krg, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_auton = case_when(
    NAME_1 == "Aleppo" ~ 3.26455,
    NAME_1 == "Dihok" ~ 2.885246,
    NAME_1 == "Arbil" ~ 2.116667,
    NAME_1 == "As-Sulaymaniyah" ~ 1.309091,
    NAME_1 == "Al Ḥasakah" ~ 3.142857,
    NAME_1 == "Ar Raqqah" ~ 2.666666
  ))

grad_lims <- c(1,4)

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Own Autonomous Administration (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RATE CONF PARL

aggregate(dat_merge_imp_$conf_parl_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_parl_kri, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_parl_auton = case_when(
    NAME_1 == "Aleppo" ~ 3.148148,
    NAME_1 == "Dihok" ~ 2.491803,
    NAME_1 == "Arbil" ~ 1.833333,
    NAME_1 == "As-Sulaymaniyah" ~ 1.2,
    NAME_1 == "Al Ḥasakah" ~ 3.142857,
    NAME_1 == "Ar Raqqah" ~ 2.5
  ))

grad_lims <- c(1,4)

# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_parl_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Own Parliament (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RATE CONF PARTIES

aggregate(dat_merge_imp_$conf_part_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_part_krd, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_part_auton = case_when(
    NAME_1 == "Aleppo" ~ 2.367725,
    NAME_1 == "Dihok" ~ 2.491803,
    NAME_1 == "Arbil" ~ 1.700000,
    NAME_1 == "As-Sulaymaniyah" ~ 1.2,
    NAME_1 == "Al Ḥasakah" ~ 1.964286,
    NAME_1 == "Ar Raqqah" ~ 2.083334
  ))


# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_part_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Own Political Parties (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RATE OWN COURTS

aggregate(dat_merge_imp_$conf_court_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_court_kri, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_court_auton = case_when(
    NAME_1 == "Aleppo" ~ 2.849206,
    NAME_1 == "Dihok" ~ 2.442623,
    NAME_1 == "Arbil" ~ 1.783333,
    NAME_1 == "As-Sulaymaniyah" ~ 1.4,
    NAME_1 == "Al Ḥasakah" ~ 2.571429,
    NAME_1 == "Ar Raqqah" ~ 2.625
  ))


# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_court_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Courts (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RATE OWN POLICE

aggregate(dat_merge_imp_$conf_police_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_police_kri, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_police_auton = case_when(
    NAME_1 == "Aleppo" ~ 3.314815,
    NAME_1 == "Dihok" ~ 3.327869,
    NAME_1 == "Arbil" ~ 2.450000,
    NAME_1 == "As-Sulaymaniyah" ~ 2.116842,
    NAME_1 == "Al Ḥasakah" ~ 3.142857,
    NAME_1 == "Ar Raqqah" ~ 2.75
  ))


# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_police_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Police of Autonomy (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# RATE OWN ARMY

aggregate(dat_merge_imp_$conf_armed_nes, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 
aggregate(dat_merge_imp_$conf_armed_kri, list(dat_merge_num_k$region), 
          FUN = function(x) mean(x, na.rm = TRUE)) 


kurdistan <- kurdistan %>%
  mutate(conf_armed_auton = case_when(
    NAME_1 == "Aleppo" ~ 3.460317,
    NAME_1 == "Dihok" ~ 3.295082,
    NAME_1 == "Arbil" ~ 2.416667,
    NAME_1 == "As-Sulaymaniyah" ~ 2.210664,
    NAME_1 == "Al Ḥasakah" ~ 3.250000,
    NAME_1 == "Ar Raqqah" ~ 2.708334
  ))


# Create the map
ggplot() +
  geom_sf(data = kurdistan, aes(fill = conf_armed_auton), color = "black") +
  scale_fill_gradient(low = "#375424", high = "#51BD17",limits = grad_lims)+
  labs(title = "Confidence in Armed Forces of Autonomy (NES/KRG)") +
  theme_minimal()+
  labs(fill = NULL, color = NULL)+
  xlim(35.000, 47.000)+ ylim(32.000, 38.000)

### ----------VDEM KRAM -------------

library(gt)

setwd("E:/Uni/Promotion/00_Literally meine Promotion/MACRO CASE STUDY")

vdem <- haven::read_dta("./V-Dem-CY-Core-v13.dta")

vdem <-vdem %>%
  dplyr::filter(country_name == "Iraq"|
           country_name == "Iran"|
           country_name == "Syria"|
           country_name == "Turkey")

vdem$year <- as.numeric(as.character(vdem$year))

#|-----Regime by Civil Society----

ggplot(vdem, aes(x = year)) +
  geom_line(aes(y = v2x_regime, color = "Regime")) +
  geom_line(aes(y = v2x_cspart, color = "Civil Society")) +
  geom_line(aes(y = v2x_regime, color = "Regime"), size = 1.5, alpha = 0.5) +
  geom_line(aes(y = v2x_cspart, color = "Civil Society"), size = 1.5, alpha = 0.5) +
  facet_grid(rows = vars(country_name)) +
  labs(x = "Year", y = "Values") +
  scale_color_manual(
    values = c("Civil Society" = "#1D7369", "Regime" = "#7D7B7B"),
    labels = c("Civil Society Participation", "Regimes of the World Measure")
  ) +
  ggtitle("RoW and Civil Society Participation Over Years by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text = element_blank()) +
  scale_x_continuous(breaks = seq(1900, 2022, 5), expand = c(0, 0)) +  # Set breaks every 5 years
  xlim(c(1990, 2022)) +
  ylim(c(0, 2))

ggplot(vdem, aes(x = year)) +
  geom_line(aes(y = v2x_regime, color = "Regime")) +
  geom_line(aes(y = v2x_cspart, color = "Civil Society")) +
  geom_line(aes(y = v2x_regime, color = "Regime"), size = 1.5, alpha = 0.5) +
  geom_line(aes(y = v2x_cspart, color = "Civil Society"), size = 1.5, alpha = 0.5) +
  facet_grid(rows = vars(country_name)) +
  labs(x = "Year", y = "Values") +
  scale_color_manual(
    values = c("Civil Society" = "#1D7369", "Regime" = "#7D7B7B"),
    labels = c("Civil Society Participation", "Regimes of the World Measure")
  ) +
  ggtitle("RoW and Civil Society Participation Over Years by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_continuous(breaks = seq(1900, 2022, 5), expand = c(0, 0)) +  # Set breaks every 5 years
  xlim(c(1900, 2022)) +
  ylim(c(0, 2))

# 0: Closed autocracy: No multiparty elections for the chief executive or the legislature.
# 1: Electoral autocracy: De-jure multiparty elections for the chief executive and the
# legislature, but failing to achieve that elections are free and fair, or de-facto multiparty, or a
# minimum level of Dahl’s institutional prerequisites of polyarchy as measured by V-Dem’s
# Electoral Democracy Index (v2x_polyarchy).
# 2: Electoral democracy: De-facto free and fair multiparty elections and a minimum level of
# Dahl’s institutional prerequisites for polyarchy as measured by V- Dem’s Electoral
# Democracy Index (v2x_polyarchy), but either access to justice, or transparent law
# enforcement, or liberal principles of respect for personal liberties, rule of law, and judicial as
# well as legislative constraints on the executive not satisfied as measured by V-Dem’s Liberal
# Component Index (v2x_liberal).
# 3: Liberal democracy: De-facto free and fair multiparty elections and a minimum level of
# Dahl’s institutional prerequisites for polyarchy as measured by V- Dem’s Electoral
# Democracy Index (v2x_polyarchy) are guaranteed as well as access to justice, transparent
# law enforcement and the liberal principles of respect for personal liberties, rule of law, and
# judicial as well as legislative constraints on the executive satisfied as measured by V-Dem’s
# Liberal Component Index (v2x_liberal).


#|-----Regime by Exclusion by Social Group index----

ggplot(vdem, aes(x = year)) +
  geom_line(aes(y = v2x_regime, color = "Regime")) +
  geom_line(aes(y = v2xpe_exlsocgr, color = "Exclusion by Social Group index")) +
  geom_line(aes(y = v2x_regime, color = "Regime"), size = 1.5, alpha = 0.5) +
  geom_line(aes(y = v2xpe_exlsocgr, color = "Exclusion by Social Group index"), size = 1.5, alpha = 0.5) +
  facet_grid(rows = vars(country_name)) +
  labs(x = "Year", y = "Values") +
  scale_color_manual(values = c("Exclusion by Social Group index" = "#1D7369", "Regime" = "#7D7B7B"),
                     labels = c("Exclusion by Social Group index",
                                "Regimes of the World Measure")) +
  ggtitle("RoW and Exclusion by Social Group index Over Years by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text = element_blank()) +
  xlim(c(1990, 2022))

ggplot(vdem, aes(x = year)) +
  geom_line(aes(y = v2x_regime, color = "Regime")) +
  geom_line(aes(y = v2xpe_exlsocgr, color = "Exclusion by Social Group index")) +
  geom_line(aes(y = v2x_regime, color = "Regime"), size = 1.5, alpha = 0.5) +
  geom_line(aes(y = v2xpe_exlsocgr, color = "Exclusion by Social Group index"), size = 1.5, alpha = 0.5) +
  facet_grid(rows = vars(country_name)) +
  labs(x = "Year", y = "Values") +
  scale_color_manual(values = c("Exclusion by Social Group index" = "#1D7369", "Regime" = "#7D7B7B"),
                     labels = c("Exclusion by Social Group index",
                                "Regimes of the World Measure")) +
  ggtitle("RoW and Exclusion by Social Group index Over Years by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlim(c(1900, 2022))

# Exclusion is when individuals are denied access to services or participation in
# governed spaces (spaces that are part of the public space and the government should regulate,
#                  while excluding private spaces and organizations except when exclusion in those private
#                  spheres is linked to exclusion in the public sphere) based on their identity or belonging to a
# particular group. The point estimates for this index have been reversed such that the
# directionality is opposite to the input variables. That is, lower scores indicate a normatively
# better situation (e.g. more democratic) and higher scores a normatively worse situation (e.g.
#                                                                                          less democratic). Note that this directionality is opposite of that of other V-Dem indices,
# which generally run from normatively worse to better.
# Scale: Interval, from low to high (0-1)

#|-----Regime by Exclusion by Social Group index----


# ggplot(vdem, aes(x = year)) +
#   geom_line(aes(y = v2x_regime, color = "Regime")) +
#   geom_line(aes(y = v2xpe_exlpol, color = "Exclusion by Political Group index")) +
#   geom_line(aes(y = v2x_regime, color = "Regime"), size = 1.5, alpha = 0.5) +
#   geom_line(aes(y = v2xpe_exlpol, color = "Exclusion by Political Group index"), size = 1.5, alpha = 0.5) +
#   facet_grid(rows = vars(country_name)) +
#   labs(x = "Year", y = "Values") +
#   scale_color_manual(values = c("Exclusion by Political Group index" = "#1D7369", "Regime" = "#7D7B7B"),
#                      labels = c("Exclusion by Political Group index",
#                                 "Regimes of the World Measure")) +
#   ggtitle("RoW and Exclusion by Political Group index Over Years by Country") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   xlim(c(1900, 2022))



#|---- World Bank Military Expenditure----

bygdp <- read.csv("./mil.xpnd.percgdp.csv", sep=",")
bygdp <- bygdp%>%
  filter(bygdp$Country.Name== "Turkiye"|
           bygdp$Country.Name== "Iraq"|
           bygdp$Country.Name== "Iran"|
           bygdp$Country.Name== "Syrian Arab Republic")
bygdp<-gather(bygdp, key = "year", value = "mil_gdp", X1960, X1961,
       X1962,          X1963,          X1964,          X1965,          X1966,          X1967,         
       X1968,          X1969,          X1970,          X1971,          X1972,          X1973,         
       X1974,          X1975,          X1976,          X1977,          X1978,          X1979,         
       X1980,          X1981,          X1982,          X1983,          X1984,          X1985,         
       X1986,          X1987,          X1988,          X1989,          X1990,          X1991,         
       X1992,          X1993,          X1994,          X1995,          X1996,          X1997,         
       X1998,          X1999,          X2000,          X2001,          X2002,          X2003,         
       X2004,          X2005,          X2006,          X2007,          X2008,          X2009,         
       X2010,          X2011,          X2012,          X2013,          X2014,          X2015,         
       X2016,          X2017,          X2018,          X2019,          X2020,          X2021,         
       X2022,          X )

bygdp$year<-sub(".", "", bygdp$year)
bygdp$mil_gdp <- as.numeric(bygdp$mil_gdp)




byusd <- read.csv("./mil.xpnd.usd.csv", sep=",")
byusd <- byusd%>%
  filter(byusd$Country.Name== "Turkiye"|
           byusd$Country.Name== "Iraq"|
           byusd$Country.Name== "Iran"|
           byusd$Country.Name== "Syrian Arab Republic")

byusd<-gather(byusd, key = "year", value = "mil_usd", X1960, X1961,
              X1962,          X1963,          X1964,          X1965,          X1966,          X1967,         
              X1968,          X1969,          X1970,          X1971,          X1972,          X1973,         
              X1974,          X1975,          X1976,          X1977,          X1978,          X1979,         
              X1980,          X1981,          X1982,          X1983,          X1984,          X1985,         
              X1986,          X1987,          X1988,          X1989,          X1990,          X1991,         
              X1992,          X1993,          X1994,          X1995,          X1996,          X1997,         
              X1998,          X1999,          X2000,          X2001,          X2002,          X2003,         
              X2004,          X2005,          X2006,          X2007,          X2008,          X2009,         
              X2010,          X2011,          X2012,          X2013,          X2014,          X2015,         
              X2016,          X2017,          X2018,          X2019,          X2020,          X2021,         
              X2022,          X )

byusd$year<-sub(".", "", byusd$year)
byusd$mil_usd <- as.numeric(byusd$mil_usd)

total <- merge(bygdp,byusd,by=c("Country.Name","year"))



total$year <- as.numeric(total$year)

ggplot(total, aes(x = year)) +
  geom_line(aes(y = mil_gdp, color = "Military Expenditure in % GDP")) +
  geom_line(aes(y = mil_gdp, color = "Military Expenditure in % GDP"), size = 1.5, alpha = 0.5) +
  facet_wrap(~factor(Country.Name), scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("#1D7369", "#7D7B7B"),  # Use hex values directly
    labels = c("Military Expenditure in % GDP")
  ) +
  labs(x = "Year", y = "Values") +
  ggtitle("Military Expenditure of Iran, Iraq, Syria, and Turkey in % GDP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none") +
  xlim(c(1960, 2022))


ggplot(total, aes(x = year)) +
  geom_line(aes(y = mil_usd, color = "Military Expenditure in Current $")) +
  geom_line(aes(y = mil_usd, color = "Military Expenditure in Current $"), size = 1.5, alpha = 0.5) +
  facet_wrap(~factor(Country.Name), scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("#1D7369", "#7D7B7B"),  # Use hex values directly
    labels = c("Military Expenditure in Current $")
  ) +
  labs(x = "Year", y = "Values") +
  ggtitle("Military Expenditure of Iran, Iraq, Syria, and Turkey in Current USD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none") +
  xlim(c(1960, 2022))

