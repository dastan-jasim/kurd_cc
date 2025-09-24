load(".data/processed/dat_merge_num_k.Rdata")
load(".data/processed/dat_merge_num.Rdata")

vis_miss(dat_merge_num_k)

impu_select <- dat_merge_num_k %>%
  select(pol_interest, conf_armed, conf_police, conf_parl, conf_gov,
         conf_part, conf_court, rate_system, sys_leader, sys_expert,
         sys_democ, democ_own, relig_pers)

md.pattern(impu_select, rotate.names = T)

# |----pol_interest ----

mice_imputed1 <- data.frame(
  original = dat_merge_num_k$pol_interest,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$pol_interest,
  imputed_cart = complete(mice(impu_select, method = "cart"))$pol_interest,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$pol_interest
)
mice_imputed1

h1 <- ggplot(mice_imputed1, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Interest in Politics)") +
  theme_classic()
h2 <- ggplot(mice_imputed1, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed1, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed1, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_armed ----

mice_imputed2 <- data.frame(
  original = dat_merge_num_k$conf_armed,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$conf_armed,
  imputed_cart = complete(mice(impu_select, method = "cart"))$conf_armed,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$conf_armed
)
mice_imputed2

h1 <- ggplot(mice_imputed2, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence State Armed Forces)") +
  theme_classic()
h2 <- ggplot(mice_imputed2, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed2, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed2, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_police ----

mice_imputed3 <- data.frame(
  original = dat_merge_num_k$conf_police,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$conf_police,
  imputed_cart = complete(mice(impu_select, method = "cart"))$conf_police,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$conf_police
)
mice_imputed3

h1 <- ggplot(mice_imputed3, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence State Police)") +
  theme_classic()
h2 <- ggplot(mice_imputed3, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed3, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed3, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_parl ----

mice_imputed4 <- data.frame(
  original = dat_merge_num_k$conf_parl,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$conf_parl,
  imputed_cart = complete(mice(impu_select, method = "cart"))$conf_parl,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$conf_parl
)
mice_imputed4

h1 <- ggplot(mice_imputed4, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence State Parliament)") +
  theme_classic()
h2 <- ggplot(mice_imputed4, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed4, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed4, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_gov ----

mice_imputed5 <- data.frame(
  original = dat_merge_num_k$conf_gov,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$conf_gov,
  imputed_cart = complete(mice(impu_select, method = "cart"))$conf_gov,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$conf_gov
)
mice_imputed5

h1 <- ggplot(mice_imputed5, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence State Government)") +
  theme_classic()
h2 <- ggplot(mice_imputed5, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed5, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed5, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_part ----

mice_imputed6 <- data.frame(
  original = dat_merge_num_k$conf_part,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$conf_part,
  imputed_cart = complete(mice(impu_select, method = "cart"))$conf_part,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$conf_part
)
mice_imputed6

h1 <- ggplot(mice_imputed6, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence State Parties)") +
  theme_classic()
h2 <- ggplot(mice_imputed6, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed6, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed6, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_court ----

mice_imputed7 <- data.frame(
  original = dat_merge_num_k$conf_court,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$conf_court,
  imputed_cart = complete(mice(impu_select, method = "cart"))$conf_court,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$conf_court
)
mice_imputed7

h1 <- ggplot(mice_imputed7, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence State Courts)") +
  theme_classic()
h2 <- ggplot(mice_imputed7, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed7, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed7, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----rate_system ----

mice_imputed8 <- data.frame(
  original = dat_merge_num_k$rate_system,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$rate_system,
  imputed_cart = complete(mice(impu_select, method = "cart"))$rate_system,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$rate_system
)
mice_imputed8

h1 <- ggplot(mice_imputed8, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Rate State Political System)") +
  theme_classic()
h2 <- ggplot(mice_imputed8, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed8, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed8, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----sys_leader ----

mice_imputed9 <- data.frame(
  original = dat_merge_num_k$sys_leader,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$sys_leader,
  imputed_cart = complete(mice(impu_select, method = "cart"))$sys_leader,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$sys_leader
)
mice_imputed9

h1 <- ggplot(mice_imputed9, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Support Strong Leader System)") +
  theme_classic()
h2 <- ggplot(mice_imputed9, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed9, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed9, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----sys_expert ----

mice_imputed10 <- data.frame(
  original = dat_merge_num_k$sys_expert,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$sys_expert,
  imputed_cart = complete(mice(impu_select, method = "cart"))$sys_expert,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$sys_expert
)
mice_imputed10

h1 <- ggplot(mice_imputed10, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Support Expert Rule)") +
  theme_classic()
h2 <- ggplot(mice_imputed10, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed10, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed10, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----sys_democ ----

mice_imputed11 <- data.frame(
  original = dat_merge_num_k$sys_democ,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$sys_democ,
  imputed_cart = complete(mice(impu_select, method = "cart"))$sys_democ,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$sys_democ
)
mice_imputed11

h1 <- ggplot(mice_imputed11, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Support Democracy)") +
  theme_classic()
h2 <- ggplot(mice_imputed11, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed11, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed11, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----democ_own ----

mice_imputed12 <- data.frame(
  original = dat_merge_num_k$democ_own,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$democ_own,
  imputed_cart = complete(mice(impu_select, method = "cart"))$democ_own,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$democ_own
)
mice_imputed12

h1 <- ggplot(mice_imputed12, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Democracy in own country)") +
  theme_classic()
h2 <- ggplot(mice_imputed12, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed12, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed12, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


# |----relig_pers ----

mice_imputed13 <- data.frame(
  original = dat_merge_num_k$relig_pers,
  imputed_pmm = complete(mice(impu_select, method = "pmm"))$relig_pers,
  imputed_cart = complete(mice(impu_select, method = "cart"))$relig_pers,
  imputed_lasso = complete(mice(impu_select, method = "lasso.norm"))$relig_pers
)
mice_imputed13

h1 <- ggplot(mice_imputed13, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Religiosity)") +
  theme_classic()
h2 <- ggplot(mice_imputed13, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed13, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed13, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# ||+----autonomous var impu ----

# ONLY USE AUTONOMY DATASETS FROM DAT MERGE NUM AND NOT OTHERS

roj <- dat_merge_num_k %>%
  dplyr::filter(dat_merge_num_k$country == "Syria")

krg <- dat_merge_num_k %>%
  dplyr::filter(dat_merge_num_k$country == "Iraq")

# |----rate_system_kri ----

mice_imputed14 <- data.frame(
  original = krg$rate_system_kri,
  imputed_pmm = complete(mice(krg, method = "pmm"))$rate_system_kri,
  imputed_cart = complete(mice(krg, method = "cart"))$rate_system_kri,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$rate_system_kri
)
mice_imputed14

h1 <- ggplot(mice_imputed14, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence KRI System)") +
  theme_classic()
h2 <- ggplot(mice_imputed14, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed14, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed14, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_parl_kri ----

mice_imputed15 <- data.frame(
  original = krg$conf_parl_kri,
  imputed_pmm = complete(mice(krg, method = "pmm"))$conf_parl_kri,
  imputed_cart = complete(mice(krg, method = "cart"))$conf_parl_kri,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$conf_parl_kri
)
mice_imputed15

h1 <- ggplot(mice_imputed15, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence KRI Parliament)") +
  theme_classic()
h2 <- ggplot(mice_imputed15, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed15, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed15, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_krg ----

mice_imputed16 <- data.frame(
  original = krg$conf_krg,
  imputed_pmm = complete(mice(krg, method = "pmm"))$conf_krg,
  imputed_cart = complete(mice(krg, method = "cart"))$conf_krg,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$conf_krg
)
mice_imputed16

h1 <- ggplot(mice_imputed16, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence KRG)") +
  theme_classic()
h2 <- ggplot(mice_imputed16, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed16, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed16, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_part_krd ----

mice_imputed17 <- data.frame(
  original = krg$conf_part_krd,
  imputed_pmm = complete(mice(krg, method = "pmm"))$conf_part_krd,
  imputed_cart = complete(mice(krg, method = "cart"))$conf_part_krd,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$conf_part_krd
)
mice_imputed17

h1 <- ggplot(mice_imputed17, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence Parties KRI)") +
  theme_classic()
h2 <- ggplot(mice_imputed17, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed17, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed17, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_court_kri ----

mice_imputed18 <- data.frame(
  original = krg$conf_court_kri,
  imputed_pmm = complete(mice(krg, method = "pmm"))$conf_court_kri,
  imputed_cart = complete(mice(krg, method = "cart"))$conf_court_kri,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$conf_court_kri
)
mice_imputed18

h1 <- ggplot(mice_imputed18, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence Courts KRI)") +
  theme_classic()
h2 <- ggplot(mice_imputed18, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed18, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed18, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_police_kri ----

mice_imputed19 <- data.frame(
  original = krg$conf_police_kri,
  imputed_pmm = complete(mice(krg, method = "pmm"))$conf_police_kri,
  imputed_cart = complete(mice(krg, method = "cart"))$conf_police_kri,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$conf_police_kri
)
mice_imputed19

h1 <- ggplot(mice_imputed19, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence Police KRI)") +
  theme_classic()
h2 <- ggplot(mice_imputed19, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed19, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed19, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_armed_kri ----

mice_imputed20 <- data.frame(
  original = krg$conf_armed_kri,
  imputed_pmm = complete(mice(krg, method = "pmm"))$conf_armed_kri,
  imputed_cart = complete(mice(krg, method = "cart"))$conf_armed_kri,
  imputed_lasso = complete(mice(krg, method = "lasso.norm"))$conf_armed_kri
)
mice_imputed20

h1 <- ggplot(mice_imputed20, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence Armed Forces KRI)") +
  theme_classic()
h2 <- ggplot(mice_imputed20, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed20, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed20, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# NES KRAM 

# |----rate_system_nes ----

mice_imputed21 <- data.frame(
  original = roj$rate_system_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$rate_system_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$rate_system_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$rate_system_nes
)
mice_imputed21

h1 <- ggplot(mice_imputed21, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence AANES System)") +
  theme_classic()
h2 <- ggplot(mice_imputed21, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed21, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed21, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_parl_nes ----

mice_imputed22 <- data.frame(
  original = roj$conf_parl_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$conf_parl_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$conf_parl_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$conf_parl_nes
)
mice_imputed22

h1 <- ggplot(mice_imputed22, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence in Syrian Democratic Council)") +
  theme_classic()
h2 <- ggplot(mice_imputed22, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed22, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed22, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_nes ----

mice_imputed23 <- data.frame(
  original = roj$conf_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$conf_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$conf_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$conf_nes
)
mice_imputed23

h1 <- ggplot(mice_imputed23, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence in AANES)") +
  theme_classic()
h2 <- ggplot(mice_imputed23, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed23, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed23, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_part_nes ----

mice_imputed24 <- data.frame(
  original = roj$conf_part_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$conf_part_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$conf_part_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$conf_part_nes
)
mice_imputed24

h1 <- ggplot(mice_imputed24, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence in Parties AANES)") +
  theme_classic()
h2 <- ggplot(mice_imputed24, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed24, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed24, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_court_nes ----

mice_imputed25 <- data.frame(
  original = roj$conf_court_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$conf_court_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$conf_court_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$conf_court_nes
)
mice_imputed25

h1 <- ggplot(mice_imputed25, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence in Courts AANES)") +
  theme_classic()
h2 <- ggplot(mice_imputed25, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed25, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed25, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# |----conf_police_nes ----

mice_imputed26 <- data.frame(
  original = roj$conf_police_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$conf_police_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$conf_police_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$conf_police_nes
)
mice_imputed26

h1 <- ggplot(mice_imputed26, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence in Police AANES)") +
  theme_classic()
h2 <- ggplot(mice_imputed26, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed26, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed26, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


# |----conf_armed_nes ----

mice_imputed27 <- data.frame(
  original = roj$conf_armed_nes,
  imputed_pmm = complete(mice(roj, method = "pmm"))$conf_armed_nes,
  imputed_cart = complete(mice(roj, method = "cart"))$conf_armed_nes,
  imputed_lasso = complete(mice(roj, method = "lasso.norm"))$conf_armed_nes
)
mice_imputed27

h1 <- ggplot(mice_imputed27, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution (Confidence in Armed Forces AANES)") +
  theme_classic()
h2 <- ggplot(mice_imputed27, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed27, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed27, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso imputed distribution") +
  theme_classic()

cowplot::plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


# ----- CART IMP DATASET ----

dat_merge_imp <- dat_merge_num_k%>%
  filter(study=="WVS")

dat_merge_imp$pol_interest <- complete(mice(dat_merge_imp, method = "cart"))$pol_interest
dat_merge_imp$conf_armed <- complete(mice(dat_merge_imp, method = "cart"))$conf_armed
dat_merge_imp$conf_police <- complete(mice(dat_merge_imp, method = "cart"))$conf_police
dat_merge_imp$conf_parl <- complete(mice(dat_merge_imp, method = "cart"))$conf_parl
dat_merge_imp$conf_gov <- complete(mice(dat_merge_imp, method = "cart"))$conf_gov
dat_merge_imp$conf_part <- complete(mice(dat_merge_imp, method = "cart"))$conf_part
dat_merge_imp$conf_court <- complete(mice(dat_merge_imp, method = "cart"))$conf_court
dat_merge_imp$rate_system <- complete(mice(dat_merge_imp, method = "cart"))$rate_system
dat_merge_imp$sys_leader <- complete(mice(dat_merge_imp, method = "cart"))$sys_leader
dat_merge_imp$sys_expert <- complete(mice(dat_merge_imp, method = "cart"))$sys_expert
dat_merge_imp$sys_democ <- complete(mice(dat_merge_imp, method = "cart"))$sys_democ
# kein sys army
dat_merge_imp$democ_own <- complete(mice(dat_merge_imp, method = "cart"))$democ_own
dat_merge_imp$relig_pers <- complete(mice(dat_merge_imp, method = "cart"))$relig_pers

# KRG subset imp

krg_imp <- krg

krg_imp$rate_system <- as.numeric(krg_imp$rate_system)
krg_imp$rate_system_kri <- as.numeric(krg_imp$rate_system_kri)

krg_imp$pol_interest <- complete(mice(krg_imp, method = "cart"))$pol_interest
krg_imp$conf_armed <- complete(mice(krg_imp, method = "cart"))$conf_armed
krg_imp$conf_police <- complete(mice(krg_imp, method = "cart"))$conf_police
krg_imp$conf_parl <- complete(mice(krg_imp, method = "cart"))$conf_parl
krg_imp$conf_gov <- complete(mice(krg_imp, method = "cart"))$conf_gov
krg_imp$conf_part <- complete(mice(krg_imp, method = "cart"))$conf_part
krg_imp$conf_court <- complete(mice(krg_imp, method = "cart"))$conf_court
krg_imp$rate_system <- complete(mice(krg_imp, method = "cart"))$rate_system
krg_imp$sys_leader <- complete(mice(krg_imp, method = "cart"))$sys_leader
krg_imp$sys_expert <- complete(mice(krg_imp, method = "cart"))$sys_expert
krg_imp$sys_democ <- complete(mice(krg_imp, method = "cart"))$sys_democ
# kein sys army
krg_imp$democ_own <- complete(mice(krg_imp, method = "cart"))$democ_own
krg_imp$relig_pers <- complete(mice(krg_imp, method = "cart"))$relig_pers

krg_imp$rate_system_kri <- complete(mice(krg_imp, method = "cart"))$rate_system_kri
krg_imp$conf_parl_kri <- complete(mice(krg_imp, method = "cart"))$conf_parl_kri
krg_imp$conf_krg <- complete(mice(krg_imp, method = "cart"))$conf_krg
krg_imp$conf_part_krd <- complete(mice(krg_imp, method = "cart"))$conf_part_krd
krg_imp$conf_court_kri <- complete(mice(krg_imp, method = "cart"))$conf_court_kri
krg_imp$conf_police_kri <- complete(mice(krg_imp, method = "cart"))$conf_police_kri
krg_imp$conf_armed_kri <- complete(mice(krg_imp, method = "cart"))$conf_armed_kri


# ROJ subset imp


nes_imp <- roj

nes_imp$rate_system <- as.numeric(nes_imp$rate_system)
nes_imp$rate_system_nes <- as.numeric(nes_imp$rate_system_nes)

nes_imp$pol_interest <- complete(mice(nes_imp, method = "cart"))$pol_interest
nes_imp$conf_armed <- complete(mice(nes_imp, method = "cart"))$conf_armed
nes_imp$conf_police <- complete(mice(nes_imp, method = "cart"))$conf_police
nes_imp$conf_parl <- complete(mice(nes_imp, method = "cart"))$conf_parl
nes_imp$conf_gov <- complete(mice(nes_imp, method = "cart"))$conf_gov
nes_imp$conf_part <- complete(mice(nes_imp, method = "cart"))$conf_part
nes_imp$conf_court <- complete(mice(nes_imp, method = "cart"))$conf_court
nes_imp$rate_system <- complete(mice(nes_imp, method = "cart"))$rate_system
nes_imp$sys_leader <- complete(mice(nes_imp, method = "cart"))$sys_leader
nes_imp$sys_expert <- complete(mice(nes_imp, method = "cart"))$sys_expert
nes_imp$sys_democ <- complete(mice(nes_imp, method = "cart"))$sys_democ
# kein sys army
nes_imp$democ_own <- complete(mice(nes_imp, method = "cart"))$democ_own
nes_imp$relig_pers <- complete(mice(nes_imp, method = "cart"))$relig_pers

nes_imp$rate_system_nes <- complete(mice(nes_imp, method = "cart"))$rate_system_nes
nes_imp$conf_parl_nes <- complete(mice(nes_imp, method = "cart"))$conf_parl_nes
nes_imp$conf_nes <- complete(mice(nes_imp, method = "cart"))$conf_nes
nes_imp$conf_part_nes <- complete(mice(nes_imp, method = "cart"))$conf_part_nes
nes_imp$conf_court_nes <- complete(mice(nes_imp, method = "cart"))$conf_court_nes
nes_imp$conf_police_nes <- complete(mice(nes_imp, method = "cart"))$conf_police_nes
nes_imp$conf_armed_nes <- complete(mice(nes_imp, method = "cart"))$conf_armed_nes

# bring KRG & ROJ into imp dataset

dat_merge_imp_ <- plyr::rbind.fill(dat_merge_imp, krg_imp, nes_imp) #num merged set

save(dat_merge_imp_, file = ".data/processed/dat_merge_imp_.Rdata")
