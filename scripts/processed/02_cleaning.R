wvs7$country <- as.character(wvs7$B_COUNTRY)

krg <- as.data.frame(krg)
krg[5:47] <- lapply(krg[5:47], as.numeric)

roj <- roj[-1]
roj <- as.data.frame(roj)
roj[5:47] <- lapply(roj[5:47], as.numeric)
# Filtering Dataset by country

wvs7 <- wvs7%>%
  filter(B_COUNTRY=="Iran"|
           B_COUNTRY=="Turkey")

wvs7$country <- wvs7$B_COUNTRY

krg$country <- "Iraq"
roj$country <- "Syria"

# Standardizing regional variable
# MUST BE RECODED
wvs7$region <- as.character(wvs7$N_REGION_WVS)
wvs7$region <- as.factor(wvs7$region) # remove unused values
wvs7$region <- droplevels(wvs7$region)
wvs7$region <- as.character(wvs7$region)

krg$region <- recode(krg$X10,
                    "8"="Duhok", "9"="Erbil", "10"="Sulaimaniya", "11"="Halabja", .default = "Other")

roj$region <- recode(roj$X1.1,
                     "1"="Jazira", "2"="Euphrates", "3"= "Afrin", "4"= "Manbij", "5"="Raqqa", "6"="Tabqa")
# Standardizing ethnic variable for Kurds Iran Iraq
# no ethnic and no language variable
# language home not asked

wvs7$Q290 <- as.factor(wvs7$Q290)
wvs7$Q290 <- droplevels(wvs7$Q290)
wvs7$Q290 <- as.character(wvs7$Q290)

# language at home var
wvs7$Q272 <- as.factor(wvs7$Q272)
wvs7$Q272 <- droplevels(wvs7$Q272)
wvs7$Q272 <- as.character(wvs7$Q272)

#wvs7$X051 <- as.character(wvs7$X051)

wvs7$ethnicity <- recode(wvs7$Q290, "IQ: Kurdish"="Kurd",
                        "IR: Kurd"="Kurd",
                        "IQ: Arab"="Arab",
                        "IR: Arab"="Arab",
                        "Not asked"="Not asked",
                        "IR: Persian"="Persian",
                        .default="Other")

krg$ethnicity <- recode(krg$X4, "4"="Kurd", .default = "Other")
roj$ethnicity <- recode(roj$X4, "4"="Kurd", .default = "Other")

# Standardizing year variable

wvs7$year <- as.character(wvs7$A_YEAR)
krg$year <- "2022"
roj$year <- "2023"

# Standardizing study id
wvs7$study <- "WVS"
krg$study <- "OS"
roj$study <- "OS"


wvs7$study_id <- str_c(wvs7$study,"_",wvs7$country,"_",wvs7$year)
krg$study_id <- "OS_Iraq_2022"
roj$study_id <- "OS_Syria_2023"

#------- create subset -----------
wvs7_sub <- select(wvs7,study,study_id,year,country,region,ethnicity,
                   Q199,# interest in politics
                   Q65,# trust armed forces
                   Q69,# trust police
                    Q73,# trust parliament
                   Q71,# trust government
                   Q72,# trust political parties
                   Q70,# trust courts
                   Q252,# rate political system
                   Q235,# strong leader
                   Q236,# experts
                   Q237,# army
                   Q238,# democracy
                   #E120,# economy badly 
                   #E121,# indecisive
                   #E122,# bad maintaining order
                   #E123,# may have problems but best
                   #E150, # follow politics in news              
                   Q251,# how democ own
                   Q289,# rel denom
                   Q173,# relig pers
                   Q272,#language home
                   Q260,#gender
                   Q262,#age
                   Q273,#marital status
                   Q275,#education
                   Q279)#employment

# TODO Freischalten alle Variablen

# set all NA's

wvs7_sub[wvs7_sub == "Not asked in survey"] <- NA
wvs7_sub[wvs7_sub == "Not asked"] <- NA
wvs7_sub[wvs7_sub == "Don´t know"] <- "Don't know"

krg_chr_NA <- krg_chr
roj_chr_NA <- roj_chr

krg_chr_NA[krg_chr_NA == "No answer"] <- NA
roj_chr_NA[roj_chr_NA == "No answer"] <- NA

krg[krg == "98"] <- NA
krg[krg == "97"] <- NA

roj[roj == "98"] <- NA
roj[roj == "97"] <- NA

# standardize variable names

names(wvs7_sub)<-c("study","study_id","year","country","region",
                         "ethnicity","pol_interest",
                  "conf_armed","conf_police",
                  "conf_parl","conf_gov","conf_part",
                  "conf_court","rate_system","sys_leader",
                    "sys_expert","sys_army", "sys_democ",
                  "democ_own","rel_denom",
                  "relig_pers","language_home","gender","age",
                   "marit","edu", "employment")

# delete unnecessary cols in OS

krg <- krg[, -c(1:6, 18, 22, 27,32,35,48:50)]
krg <- krg[, -c(37)]
krg <- krg[, -c(37)]

krg <- krg[, c(42,41,40,37,38,39,1:36)]
krg <- krg[, -c(7)]
krg <- krg[, -c(14)]

roj <- roj[, -c(1:6, 18, 22, 27,32,35,48:50)]
roj <- roj[, -c(37)]
roj <- roj[, -c(37)]

roj <- roj[, c(42,41,40,37,38,39,1:36)]
roj <- roj[, -c(7)]
roj <- roj[, -c(14)]

names(krg)<-c("study","study_id","year","country","region",
                  "ethnicity","language_home","gender","age",
              "marit","edu", "employment", "employ_sector",
              "rel_denom","relig_pers","pol_interest",
              "pol_news","democ_own","sys_leader",
              "sys_army","sys_expert", "sys_democ", "democ_econ", "democ_indec",
              "democ_order","democ_best","rate_system","rate_system_kri",
              "conf_parl", "conf_parl_kri","conf_gov","conf_krg",
              "conf_part","conf_part_krd","conf_court","conf_court_kri",
              "conf_police","conf_police_kri","conf_armed","conf_armed_kri"
                  )


names(roj)<-c("study","study_id","year","country","region",
              "ethnicity","language_home","gender","age",
              "marit","edu", "employment", "employ_sector",
              "rel_denom","relig_pers","pol_interest",
              "pol_news","democ_own","sys_leader",
              "sys_army","sys_expert", "sys_democ", "democ_econ", "democ_indec",
              "democ_order","democ_best","rate_system","rate_system_nes",
              "conf_parl", "conf_parl_nes","conf_gov","conf_nes",
              "conf_part","conf_part_nes","conf_court","conf_court_nes",
              "conf_police","conf_police_nes","conf_armed","conf_armed_nes"
)

# create subset in which character variables are recoded

krg_chr <- krg
roj_chr <- roj

#------- create subset -----------
#

#----- HARMONIZATION -----
# Since WVS has more cases the variable levels are adapted to WVS levels


# --- //  ---- pol_interest----
#
# recode political interest
#



krg_chr$pol_interest <- recode(krg_chr$pol_interest,
                  "1"=	"Very interested",
                  "2"=	"Somewhat interested",
                  "3"=	"Not very interested",
                  "4"=	"Not at all interested",
                  "98"=	"No Answer"
)


roj_chr$pol_interest <- recode(roj_chr$pol_interest,
                               "1"=	"Very interested",
                               "2"=	"Somewhat interested",
                               "3"=	"Not very interested",
                               "4"=	"Not at all interested",
                               "98"=	"No Answer"
)

# --- //  ---- conf_armed----
#
# recode confidence/trust armed forces
#

wvs7_sub$conf_armed <- recode(wvs7_sub$conf_armed,
                             "A great deal"="A great deal",
                             "Quite a lot"="Quite a lot",
                             "Not very much"="Not very much",
                             "None at all"="None at all",
                             "Don´t know"="Don't know",
                             "No answer"="No answer",
                             .default = "Missing")

krg_chr$conf_armed <- recode(krg_chr$conf_armed,
                             "1"=	"None at all",
                             "2"=	"Not very much",
                             "3"=	"Quite a lot",
                             "4"=	"A great deal",
                             "97"=	"Don't know",
                             "98"=	"No answer")
krg_chr$conf_armed_kri <- recode(krg_chr$conf_armed_kri,
                             "1"=	"None at all",
                             "2"=	"Not very much",
                             "3"=	"Quite a lot",
                             "4"=	"A great deal",
                             "97"=	"Don't know",
                             "98"=	"No answer")
                               
roj_chr$conf_armed <- recode(roj_chr$conf_armed,
                             "1"=	"None at all",
                             "2"=	"Not very much",
                             "3"=	"Quite a lot",
                             "4"=	"A great deal",
                             "97"=	"Don't know",
                             "98"=	"No answer")
roj_chr$conf_armed_nes <- recode(roj_chr$conf_armed_nes,
                                 "1"=	"None at all",
                                 "2"=	"Not very much",
                                 "3"=	"Quite a lot",
                                 "4"=	"A great deal",
                                 "97"=	"Don't know",
                                 "98"=	"No answer")
                               
# --- //  ---- conf_police----
#
# recode confidence/trust police
#

wvs7_sub$conf_police <- recode(wvs7_sub$conf_police,
                             "A great deal"="A great deal",
                             "Quite a lot"="Quite a lot",
                             "Not very much"="Not very much",
                             "None at all"="None at all",
                             "Don´t know"="Don't know",
                             "No answer"="No answer",
                             .default = "Missing")


krg_chr$conf_police <- recode(krg_chr$conf_police,
                             "1"=	"None at all",
                             "2"=	"Not very much",
                             "3"=	"Quite a lot",
                             "4"=	"A great deal",
                             "97"=	"Don't know",
                             "98"=	"No answer")
krg_chr$conf_police_kri <- recode(krg_chr$conf_police_kri,
                                 "1"=	"None at all",
                                 "2"=	"Not very much",
                                 "3"=	"Quite a lot",
                                 "4"=	"A great deal",
                                 "97"=	"Don't know",
                                 "98"=	"No answer")

roj_chr$conf_police <- recode(roj_chr$conf_police,
                              "1"=	"None at all",
                              "2"=	"Not very much",
                              "3"=	"Quite a lot",
                              "4"=	"A great deal",
                              "97"=	"Don't know",
                              "98"=	"No answer")
roj_chr$conf_police_nes <- recode(roj_chr$conf_police_nes,
                                  "1"=	"None at all",
                                  "2"=	"Not very much",
                                  "3"=	"Quite a lot",
                                  "4"=	"A great deal",
                                  "97"=	"Don't know",
                                  "98"=	"No answer")
# --- //  ---- conf_parl----
#
# recode confidence/trust parliament
#

wvs7_sub$conf_parl <- recode(wvs7_sub$conf_parl,
                              "A great deal"="A great deal",
                              "Quite a lot"="Quite a lot",
                              "Not very much"="Not very much",
                              "None at all"="None at all",
                              "Don´t know"="Don't know",
                              "No answer"="No answer",
                              .default = "Missing")

krg_chr$conf_parl <- recode(krg_chr$conf_parl,
                              "1"=	"None at all",
                              "2"=	"Not very much",
                              "3"=	"Quite a lot",
                              "4"=	"A great deal",
                              "97"=	"Don't know",
                              "98"=	"No answer")
krg_chr$conf_parl_kri <- recode(krg_chr$conf_parl_kri,
                                  "1"=	"None at all",
                                  "2"=	"Not very much",
                                  "3"=	"Quite a lot",
                                  "4"=	"A great deal",
                                  "97"=	"Don't know",
                                  "98"=	"No answer")


roj_chr$conf_parl <- recode(roj_chr$conf_parl,
                            "1"=	"None at all",
                            "2"=	"Not very much",
                            "3"=	"Quite a lot",
                            "4"=	"A great deal",
                            "97"=	"Don't know",
                            "98"=	"No answer")
roj_chr$conf_parl_nes <- recode(roj_chr$conf_parl_nes,
                                "1"=	"None at all",
                                "2"=	"Not very much",
                                "3"=	"Quite a lot",
                                "4"=	"A great deal",
                                "97"=	"Don't know",
                                "98"=	"No answer")

# --- //  ---- conf_gov----
#
# recode confidence/trust government
#

wvs7_sub$conf_gov <- recode(wvs7_sub$conf_gov,
                            "A great deal"="A great deal",
                            "Quite a lot"="Quite a lot",
                            "Not very much"="Not very much",
                            "None at all"="None at all",
                            "Don´t know"="Don't know",
                            "No answer"="No answer",
                            .default = "Missing")


krg_chr$conf_gov <- recode(krg_chr$conf_gov,
                            "1"=	"None at all",
                            "2"=	"Not very much",
                            "3"=	"Quite a lot",
                            "4"=	"A great deal",
                            "97"=	"Don't know",
                            "98"=	"No answer")
krg_chr$conf_krg <- recode(krg_chr$conf_krg,
                                "1"=	"None at all",
                                "2"=	"Not very much",
                                "3"=	"Quite a lot",
                                "4"=	"A great deal",
                                "97"=	"Don't know",
                                "98"=	"No answer")

roj_chr$conf_gov <- recode(roj_chr$conf_gov,
                           "1"=	"None at all",
                           "2"=	"Not very much",
                           "3"=	"Quite a lot",
                           "4"=	"A great deal",
                           "97"=	"Don't know",
                           "98"=	"No answer")
roj_chr$conf_nes <- recode(roj_chr$conf_nes,
                           "1"=	"None at all",
                           "2"=	"Not very much",
                           "3"=	"Quite a lot",
                           "4"=	"A great deal",
                           "97"=	"Don't know",
                           "98"=	"No answer")
# --- //  ---- conf_part----
#
# recode confidence/trust parties
#

wvs7_sub$conf_part <- recode(wvs7_sub$conf_part,
                           "A great deal"="A great deal",
                           "Quite a lot"="Quite a lot",
                           "Not very much"="Not very much",
                           "None at all"="None at all",
                           "Don´t know"="Don't know",
                           "No answer"="No answer",
                           .default = "Missing")

krg_chr$conf_part <- recode(krg_chr$conf_part,
                           "1"=	"None at all",
                           "2"=	"Not very much",
                           "3"=	"Quite a lot",
                           "4"=	"A great deal",
                           "97"=	"Don't know",
                           "98"=	"No answer")
krg_chr$conf_part_krd <- recode(krg_chr$conf_part_krd,
                           "1"=	"None at all",
                           "2"=	"Not very much",
                           "3"=	"Quite a lot",
                           "4"=	"A great deal",
                           "97"=	"Don't know",
                           "98"=	"No answer")

roj_chr$conf_part <- recode(roj_chr$conf_part,
                            "1"=	"None at all",
                            "2"=	"Not very much",
                            "3"=	"Quite a lot",
                            "4"=	"A great deal",
                            "97"=	"Don't know",
                            "98"=	"No answer")
roj_chr$conf_part_nes <- recode(roj_chr$conf_part_nes,
                                "1"=	"None at all",
                                "2"=	"Not very much",
                                "3"=	"Quite a lot",
                                "4"=	"A great deal",
                                "97"=	"Don't know",
                                "98"=	"No answer")
# --- //  ---- conf_court----
#
# recode confidence/trust courts/judicial system
#

wvs7_sub$conf_court <- recode(wvs7_sub$conf_court,
                            "A great deal"="A great deal",
                            "Quite a lot"="Quite a lot",
                            "Not very much"="Not very much",
                            "None at all"="None at all",
                            "Don´t know"="Don't know",
                            "No answer"="No answer",
                            .default = "Missing")

krg_chr$conf_court <- recode(krg_chr$conf_court,
                            "1"=	"None at all",
                            "2"=	"Not very much",
                            "3"=	"Quite a lot",
                            "4"=	"A great deal",
                            "97"=	"Don't know",
                            "98"=	"No answer")
krg_chr$conf_court_kri <- recode(krg_chr$conf_court_kri,
                                "1"=	"None at all",
                                "2"=	"Not very much",
                                "3"=	"Quite a lot",
                                "4"=	"A great deal",
                                "97"=	"Don't know",
                                "98"=	"No answer")

roj_chr$conf_court <- recode(roj_chr$conf_court,
                             "1"=	"None at all",
                             "2"=	"Not very much",
                             "3"=	"Quite a lot",
                             "4"=	"A great deal",
                             "97"=	"Don't know",
                             "98"=	"No answer")
roj_chr$conf_court_nes <- recode(roj_chr$conf_court_nes,
                                 "1"=	"None at all",
                                 "2"=	"Not very much",
                                 "3"=	"Quite a lot",
                                 "4"=	"A great deal",
                                 "97"=	"Don't know",
                                 "98"=	"No answer")
# --- //  ---- rate_system----
#
# recode rating of system
#

# Here WVS is recoded to AB 5-level
# Only AB 5 has 0-10 scale; WVS has 


wvs7_sub$rate_system <- recode(wvs7_sub$rate_system,
                              "Very good"="10",
                              "9"="9",
                              "8"="8",
                              "7"="7",
                              "6"="6",
                              "5"="5",
                              "4"="4",
                              "3"="3",
                              "2"="2",
                              "Bad"="1",
                              "No answer"="No answer",
                              "Don´t know"="Don't know",
                              .default = "Missing")

krg$rate_system <- recode(krg$rate_system,
                              "0"="1", "1"="1","2"="2","3"="3",
                          "4"="4","5"="5","6"="6","7"="7","8"="8",
                          "9"="9","10"="10")
krg_chr$rate_system <- recode(krg_chr$rate_system,
                          "0"="1", "1"="1","2"="2","3"="3",
                          "4"="4","5"="5","6"="6","7"="7","8"="8",
                          "9"="9","10"="10")


krg$rate_system_kri <- recode(krg$rate_system_kri,
                          "0"="1", "1"="1","2"="2","3"="3",
                          "4"="4","5"="5","6"="6","7"="7","8"="8",
                          "9"="9","10"="10")
krg_chr$rate_system_kri <- recode(krg_chr$rate_system_kri,
                              "0"="1", "1"="1","2"="2","3"="3",
                              "4"="4","5"="5","6"="6","7"="7","8"="8",
                              "9"="9","10"="10")
                                 

roj$rate_system <- recode(roj$rate_system,
                          "0"="1", "1"="1","2"="2","3"="3",
                          "4"="4","5"="5","6"="6","7"="7","8"="8",
                          "9"="9","10"="10")
roj_chr$rate_system <- recode(roj_chr$rate_system,
                              "0"="1", "1"="1","2"="2","3"="3",
                              "4"="4","5"="5","6"="6","7"="7","8"="8",
                              "9"="9","10"="10")


roj$rate_system_nes <- recode(roj$rate_system_nes,
                              "0"="1", "1"="1","2"="2","3"="3",
                              "4"="4","5"="5","6"="6","7"="7","8"="8",
                              "9"="9","10"="10")
roj_chr$rate_system_nes <- recode(roj_chr$rate_system_nes,
                                  "0"="1", "1"="1","2"="2","3"="3",
                                  "4"="4","5"="5","6"="6","7"="7","8"="8",
                                  "9"="9","10"="10")
# --- //  ---- sys_leader----
#
# recode support having a strong leader
#

wvs7_sub$sys_leader <- recode(wvs7_sub$sys_leader,
                             "Very good"="Very good",
                             "Fairly good"="Fairly good",
                             "Fairly Bad"="Fairly bad",
                             "Very bad"="Very bad",
                             "Don´t know"="Don't know",
                             "No answer"="No answer",
                             .default="Missing")

krg_chr$sys_leader <- recode(krg_chr$sys_leader,
                             "1"=	"Very good",
                             "2"=	"Fairly good",
                             "3"=	"Fairly bad",
                             "4"=	"Very bad",
                             "98"=	"No answer")

roj_chr$sys_leader <- recode(roj_chr$sys_leader,
                             "1"=	"Very good",
                             "2"=	"Fairly good",
                             "3"=	"Fairly bad",
                             "4"=	"Very bad",
                             "98"=	"No answer")

# --- //  ---- sys_expert ----
#
# recode support having a experts rule
#
wvs7_sub$sys_expert <- recode(wvs7_sub$sys_expert,
                              "Very good"="Very good",
                              "Fairly good"="Fairly good",
                              "Fairly bad"="Fairly bad",
                              "Very bad"="Very bad",
                              "Don´t know"="Don't know",
                              "No answer"="No answer",
                              .default="Missing")

krg_chr$sys_expert <- recode(krg_chr$sys_expert,
                             "1"=	"Very good",
                             "2"=	"Fairly good",
                             "3"=	"Fairly bad",
                             "4"=	"Very bad",
                             "98"=	"No answer")

roj_chr$sys_expert <- recode(roj_chr$sys_expert,
                             "1"=	"Very good",
                             "2"=	"Fairly good",
                             "3"=	"Fairly bad",
                             "4"=	"Very bad",
                             "98"=	"No answer")
# --- //  ---- sys_army ----
#
# recode support having a army rule (only in WVS)
#
wvs7_sub$sys_army <- recode(wvs7_sub$sys_army,
                            "Very good"="Very good",
                            "Fairly good"="Fairly good",
                            "Fairly bad"="Fairly bad",
                            "Very bad"="Very bad",
                            "Don´t know"="Don't know",
                            "No answer"="No answer",
                            .default="Missing")

krg_chr$sys_army <- recode(krg_chr$sys_army,
                             "1"=	"Very good",
                             "2"=	"Fairly good",
                             "3"=	"Fairly bad",
                             "4"=	"Very bad",
                             "98"=	"No answer")


roj_chr$sys_army <- recode(roj_chr$sys_army,
                           "1"=	"Very good",
                           "2"=	"Fairly good",
                           "3"=	"Fairly bad",
                           "4"=	"Very bad",
                           "98"=	"No answer")

# --- //  ---- sys_democ ----
#
# recode support having a democratic system
#
wvs7_sub$sys_democ <- recode(wvs7_sub$sys_democ,
                             "Very good"="Very good",
                             "Fairly good"="Fairly good",
                             "Fairly bad"="Fairly bad",
                             "Very bad"="Very bad",
                             "Don´t know"="Don't know",
                             "No answer"="No answer",
                             .default="Missing")

krg_chr$sys_democ <- recode(krg_chr$sys_democ,
                           "1"=	"Very good",
                           "2"=	"Fairly good",
                           "3"=	"Fairly bad",
                           "4"=	"Very bad",
                           "98"=	"No answer")

roj_chr$sys_democ <- recode(roj_chr$sys_democ,
                            "1"=	"Very good",
                            "2"=	"Fairly good",
                            "3"=	"Fairly bad",
                            "4"=	"Very bad",
                            "98"=	"No answer")
# --- //  ---- democ_econ ----
#
# in democracy economy runs badly
#


krg_chr$democ_econ <- recode(krg_chr$democ_econ,
                             "1"=	"Agree strongly",
                             "2"=	"Agree",
                             "3"=	"Disagree",
                             "4"=	"Strongly disagree",
                             "97"=	"Don't know",
                             "98"=	"No answer")

roj_chr$democ_econ <- recode(roj_chr$democ_econ,
                             "1"=	"Agree strongly",
                             "2"=	"Agree",
                             "3"=	"Disagree",
                             "4"=	"Strongly disagree",
                             "97"=	"Don't know",
                             "98"=	"No answer")

# --- //  ---- democ_indec ----
#
# democracies are indecisive
#


krg_chr$democ_indec <- recode(krg_chr$democ_indec,
                             "1"=	"Agree strongly",
                             "2"=	"Agree",
                             "3"=	"Disagree",
                             "4"=	"Strongly disagree",
                             "97"=	"Don't know",
                             "98"=	"No answer")

roj_chr$democ_indec <- recode(roj_chr$democ_indec,
                              "1"=	"Agree strongly",
                              "2"=	"Agree",
                              "3"=	"Disagree",
                              "4"=	"Strongly disagree",
                              "97"=	"Don't know",
                              "98"=	"No answer")

# --- //  ---- democ_order ----
#
# democracies aren't good at maintaining order
#
krg_chr$democ_order <- recode(krg_chr$democ_order,
                              "1"=	"Agree strongly",
                              "2"=	"Agree",
                              "3"=	"Disagree",
                              "4"=	"Strongly disagree",
                              "97"=	"Don't know",
                              "98"=	"No answer")

roj_chr$democ_order <- recode(roj_chr$democ_order,
                              "1"=	"Agree strongly",
                              "2"=	"Agree",
                              "3"=	"Disagree",
                              "4"=	"Strongly disagree",
                              "97"=	"Don't know",
                              "98"=	"No answer")

# --- //  ---- democ_best ----
#
# democracies have problems but are best system
#

krg_chr$democ_best <- recode(krg_chr$democ_best,
                              "1"=	"Agree strongly",
                              "2"=	"Agree",
                              "3"=	"Disagree",
                              "4"=	"Strongly disagree",
                              "97"=	"Don't know",
                              "98"=	"No answer")

roj_chr$democ_best <- recode(roj_chr$democ_best,
                             "1"=	"Agree strongly",
                             "2"=	"Agree",
                             "3"=	"Disagree",
                             "4"=	"Strongly disagree",
                             "97"=	"Don't know",
                             "98"=	"No answer")


# --- //  ---- pol_news ----
#
# to what extent/ how often follow politics in news 
#

#Warning message:
#Unreplaced values treated as NA as `.x` is not
#compatible.
#Please specify replacements exhaustively or supply
#`.default`. 

krg_chr$pol_news <- recode(krg_chr$pol_news,
                           "1"=	"Every day",
                           "2"=	"Several times a week",
                           "3"=	"Once or twice a week",
                           "4"=	"Less often",
                           "5"=	"Never","7"=	"Don't know","98"=	"No answer")

roj_chr$pol_news <- recode(roj_chr$pol_news,
                           "1"=	"Every day",
                           "2"=	"Several times a week",
                           "3"=	"Once or twice a week",
                           "4"=	"Less often",
                           "5"=	"Never","7"=	"Don't know","98"=	"No answer")

# --- //  ---- democ_own ----
#
# Democraticness in own country
# 

# in abIII there is coding mistake, in different regions numbers 11-15 were coded, those set NA
# 9 and 10 together since wvs scale only goes from 1-10 and AB goes from 0-10


wvs7_sub$democ_own <- recode(wvs7_sub$democ_own,
                                 "Not at all democratic"="1","2"="2","3"="3","4"="4",
                                 "5"="5","6"="6","7"="7","8"="8",
                                 "9"="9",
                                 "No answer"="No answer",
                                 "Don´t know"="Don't know",
                                 "Completely democratic"="10",
                                 .default="Missing")

krg_chr$democ_own <- as.character(krg_chr$democ_own)
krg_chr$democ_own <- recode(krg_chr$democ_own,
       "0"="1","1"="1", "2"="2","3"="3","4"="4",
       "5"="5","6"="6","7"="7","8"="8",
       "9"="9",
       "10"="10",
       .default="Missing")

roj_chr$democ_own <- as.character(roj_chr$democ_own)
roj_chr$democ_own <- recode(roj_chr$democ_own,
                            "0"="1","1"="1", "2"="2","3"="3","4"="4",
                            "5"="5","6"="6","7"="7","8"="8",
                            "9"="9",
                            "10"="10",
                            .default="Missing")

#
# --- //  ---- rel_denom ----
#

wvs7_sub$rel_denom <- recode(wvs7_sub$rel_denom,
                            "Muslim"="Muslim",
                             "Orthodox (Russian/Greek/etc.)"="Christian",
                            "Other Christian (Pentecostal/Free church/Jehova...)"="Christian",
                            "Other"="Other",
                             "No answer"="No answer",
                             "Don't know"="Don't know",
                            "Do not belong to a denomination"="No religious denomination",
                            .default="Missing")

krg_chr$rel_denom <- recode(krg_chr$rel_denom,
                            "1"=	"No religious denomination",
                            "2"=	"Christian",
                            "3"=	"Muslim",
                            "4"=	"Yezidi",
                            "5"=	"Other",
                            "97"=	"Don't know",
                            "98"=	"No answer"
)


roj_chr$rel_denom <- recode(roj_chr$rel_denom,
                            "1"=	"No religious denomination",
                            "2"=	"Christian",
                            "3"=	"Muslim",
                            "4"=	"Yezidi",
                            "5"=	"Other",
                            "97"=	"Don't know",
                            "98"=	"No answer"
)

#
# --- //  ---- relig_pers ----
# Describe oneself as religous person
# athesist category of WVS is left as it is

wvs7_sub$relig_pers <- recode(wvs7_sub$relig_pers,
                             "No answer"="No answer", 
                             "Don´t know"="Don't know",
                             "A religious person"="A religious person",
                             "Not a religious person"="No religious person",
                             "A convinced atheist"="An atheist",
                             .default="Missing")


krg_chr$relig_pers <- recode(krg_chr$relig_pers,
                             "1"=	"A religious person",
                             "2"=	"No religious person",
                             "3"=	"An atheist",
                             "97"=	"Don't know",
                             "98"=	"No answer"
)

roj_chr$relig_pers <- recode(roj_chr$relig_pers,
                             "1"=	"A religious person",
                             "2"=	"No religious person",
                             "3"=	"An atheist",
                             "97"=	"Don't know",
                             "98"=	"No answer"
)

#
# --- //  ---- language_home ----
#

wvs7_sub$language_home <- as.character(wvs7_sub$language_home)

wvs7_sub$language_home <- recode(wvs7_sub$language_home,
                                "Arabic"="Arabic",
                                "Azerbaijani;  Azeri" ="Azeri",
                                "Assyrian Neo-Aramaic"="Assyrian Neo-Aramaic",
                                "Persian; Farsi; Dari"="Persian",
                                "Gilaki"="Gilaki",
                                "Kurdish; Yezidi"="Kurdish",
                                "Lurish; Luri; Bakhtiari"="Kurdish",
                                "Turkmen"="Turkmen",
                                "Other"="Other")


krg_chr$language_home <- recode(krg_chr$language_home,
                                "1"=	"Arabic",
                                "2"=	"Aramean/Syriac",
                                "3"=	"Armenian",  
                                "4"=	"Kurdish",
                                "5"=	"Turkish",
                                "6"=	"Turkmen",
                                "7"=	"Other",
                                "97"=	"Don't know",
                                "98"=	"No answer"
 )


roj_chr$language_home <- recode(roj_chr$language_home,
                                "1"=	"Arabic",
                                "2"=	"Aramean/Syriac",
                                "3"=	"Armenian",  
                                "4"=	"Kurdish",
                                "5"=	"Turkish",
                                "6"=	"Turkmen",
                                "7"=	"Other",
                                "97"=	"Don't know",
                                "98"=	"No answer"
)



#
# --- //  ---- gender ----
#


wvs7_sub$gender <- recode(wvs7_sub$gender,
                         "Missing; Unknown"="Missing",
                         "Not asked in survey"="Missing",
                         "Not applicable"="Missing",
                        "No answer"="No answer",
                         "Don´t know"="Don't know",
                        "Male"="Male","Female"="Female")

krg_chr$gender <- recode(krg_chr$gender,
                         "1"=	"Male",
                         "2"=	"Female",
                         "97"=	"Don't know",
                         "98"=	"No answer")

roj_chr$gender <- recode(roj_chr$gender,
                         "1"=	"Male",
                         "2"=	"Female",
                         "97"=	"Don't know",
                         "98"=	"No answer")
#
# --- //  ---- age ----
#
# WVS:
wvs7_sub$age <- as.character(wvs7_sub$age)

krg$age[krg$age == "1983"] <- 39
krg_chr$age[krg_chr$age == "1983"] <- "39"

krg$age[krg$age == "1985"] <- 37
krg_chr$age[krg_chr$age == "1985"] <- "37"

roj$age[roj$age == "1989"] <- 34
roj_chr$age[roj_chr$age == "1989"] <- "34"
#
# --- //  ---- marit ----
#

wvs7_sub$marit <- recode(wvs7_sub$marit,
                        "No answer"="No answer", 
                        "Don´t know"="Don't know", 
                        "Married"="Married", 
                        "Living together as married"="Other", 
                        "Divorced"="Divorced", 
                        "Separated"="Separated", 
                        "Widowed"="Widowed", 
                        "Single"="Bachelor",
                        .default = "Missing")

krg_chr$marit <- recode(krg_chr$marit,
                        "1"=	"Bachelor",
                        "2"=	"Divorced", 
                        "3"=	"Don't know",
                        "4"=	"Married",
                        "5"=	"Other",
                        "6"=	"Separated",
                        "7"=	"Widowed",
                        "97"=	"Don't know",
                        "98"=	"No answer")
roj_chr$marit <- recode(roj_chr$marit,
                        "1"=	"Bachelor",
                        "2"=	"Divorced", 
                        "3"=	"Don't know",
                        "4"=	"Married",
                        "5"=	"Other",
                        "6"=	"Separated",
                        "7"=	"Widowed",
                        "97"=	"Don't know",
                        "98"=	"No answer")
#
# --- //  ---- edu ----

wvs7_sub$edu <- recode(wvs7_sub$edu,
                        "Missing; Unknown"="Missing", 
                        "Not asked in survey"="Missing",
                        "Early childhood education (ISCED 0) / no education"="Illiterate or no formal education",

                        "Primary education (ISCED 1)"="Elementary attended; completed or incompleted", 
                        
                        "Lower secondary education (ISCED 2)"="Secondary school attended; completed or incompleted",

                        "Upper secondary education (ISCED 3)"="Postsecondary school attended; completed or incompleted", 
                        "Post-secondary non-tertiary education (ISCED 4)"="Postsecondary school attended; completed or incompleted",

                        "Short-cycle tertiary education (ISCED 5)"="Lower tertiary education", 
                        "Bachelor or equivalent (ISCED 6)"="Lower tertiary education", 
                        
                        "University with degree/Higher education - upper-level tertia"="Upper tertiary education",
                        "Master or equivalent (ISCED 7)"="Upper tertiary education",
                       "Doctoral or equivalent (ISCED 8)"="Upper tertiary education",
                       "No answer"="No answer", 
                        "Don´t know"="Don't know")

krg_chr$edu <- recode(krg_chr$edu,
                      "1"=	"Illiterate or no formal education",
                      "2"=	"Elementary attended; completed or incompleted",
                      "3"=	"Secondary school attended; completed or incompleted",
                      "4"=	"Postsecondary school attended; completed or incompleted",
                      "5"=	"Lower tertiary education",
                      "6"=	"Upper tertiary education",
                      "97"=	"Don't know",
                      "98"=	"No answer")

roj_chr$edu <- recode(roj_chr$edu,
                      "1"=	"Illiterate or no formal education",
                      "2"=	"Elementary attended; completed or incompleted",
                      "3"=	"Secondary school attended; completed or incompleted",
                      "4"=	"Postsecondary school attended; completed or incompleted",
                      "5"=	"Lower tertiary education",
                      "6"=	"Upper tertiary education",
                      "97"=	"Don't know",
                      "98"=	"No answer")
#
# --- //  ---- employment ----
# Variable q1005 in AB gives info on employment but not fulltime/parttime info
# Therefore employment variable consists of q1005 and q1006


# MUST BE DONE 




krg_chr$employment <- recode(krg_chr$employment,
                             "1"=	"Employed",
                             "2"=	"Employed Full time",
                             "3"=	"Housewife", 
                             "4"=	"Part time",
                             "5"=	"Self employed",
                             "6"=	"Student",
                             "7"=	"Unemployed",
                             "8"=	"Retired",
                             "9"=	"Other",
                             "97"=	"Don't know",
                             "98"=	"No answer"
)


roj_chr$employment <- recode(roj_chr$employment,
                             "1"=	"Employed",
                             "2"=	"Employed Full time",
                             "3"=	"Housewife", 
                             "4"=	"Part time",
                             "5"=	"Self employed",
                             "6"=	"Student",
                             "7"=	"Unemployed",
                             "8"=	"Retired",
                             "9"=	"Other",
                             "97"=	"Don't know",
                             "98"=	"No answer"
)
#
# --- //  ---- employ_sector ---

# TODO employ sector erst mal nicht included

#wvs7_sub$employ_sector  <- recode(wvs7_sub$employ_sector,
#                                 "Missing; Unkown"=  "Missing", 
#                                   
#                                  "Not asked in survey"="Missing", 
#                                  
#                                  "Not applicable"="Missing", 
#                                   
#                                  "No answer"="No answer", 
#                                   
#                                  "Don't know"="Don't know", 
#                                                           
#  "Employer/manager of establishment with 500 or more employed"=
#    "Employer/manager of establishment with 10 or more employed", 
#                                                             
#   "Employer/manager of establishment with 100 or more employed"=
#    "Employer/manager of establishment with 10 or more employed", 
#                                                            
#   "Employer/manager of establishment with 10 or more employed"=
#    "Employer/manager of establishment with 10 or more employed", 
#                                                           
#  "Employer/manager of establishment w. less than 500 employed"=
#    "Employer/manager of establishment with 10 or more employed", 
#                                                             
#  "Employer/manager of establishment w. less than 100 employed"=
#    "Employer/manager of establishment with 10 or more employed", 
#                                                             
# "Employer/manager of establishment with less than 10 employed"=
#   "Employer/manager of establishment with less than 10 employed", 
#                                                          
#                   "Professional worker"="Professional worker", 
#                                                       
#   "Middle level non-manual office worker"="Tertiary Sector worker", 
#   "Supervisory Non manual -office worker"="Tertiary Sector worker",
#   "Junior level non manual"="Tertiary Sector worker", 
#      "Non manual -office worker"="Tertiary Sector worker", 
#        "Foreman and supervisor"="Tertiary Sector worker", 
#                                                           
#               "Skilled manual"="Secondary Sector (Manual Laborer)", 
#               "Semi-skilled manual worker"="Secondary Sector (Manual Laborer)", 
#               "Unskilled manual"="Secondary Sector (Manual Laborer)", 
#                                                           
#               "Farmer: has own farm"="Primary Sector (Worker or Owner)", 
#               "Agricultural worker"="Primary Sector (Worker or Owner)", 
#                                                           
#             "Member of armed forces"="Security Sector Employee", 
#                                     "Never had a job"="Other", 
#                                         "Other"="Other")
# 
# 
# krg_chr$employ_sector <- recode(krg_chr$employ_sector,
#                                 "1" =	"Employer/director of an institution with 10 employees or more",
#                                 "2" =	"Employer/director of an institution with less than 10 employees",
#                                 "3"	="Professional such as lawyer, accountant, teacher, doctor, etc",
#                                 "4"	="Manual laborer",
#                                 "5"	="Agricultural worker/owner of a farm",
#                                 "6"	="Member of the armed forces/public security",
#                                 "7"	="Owner of a shop/grocery store",
#                                 "8"	="Government employee",
#                                 "9"	="Private sector employee",
#                                 "10"	="Craftsperson",
#                                 "11"	="Director of an institution or a high ranking governmental employee",
#                                 "12"	="Working at the armed forces or the police",
#                                 "13"	="A governmental employee",
#                                 "97"	="Don't know",
# "98"	="No answer"
# )
# 
# roj_chr$employ_sector <- recode(roj_chr$employ_sector,
#                                 "1" =	"Employer/director of an institution with 10 employees or more",
#                                 "2" =	"Employer/director of an institution with less than 10 employees",
#                                 "3"	="Professional such as lawyer, accountant, teacher, doctor, etc",
#                                 "4"	="Manual laborer",
#                                 "5"	="Agricultural worker/owner of a farm",
#                                 "6"	="Member of the armed forces/public security",
#                                 "7"	="Owner of a shop/grocery store",
#                                 "8"	="Government employee",
#                                 "9"	="Private sector employee",
#                                 "10"	="Craftsperson",
#                                 "11"	="Director of an institution or a high ranking governmental employee",
#                                 "12"	="Working at the armed forces or the police",
#                                 "13"	="A governmental employee",
#                                 "97"	="Don't know",
#                                 "98"	="No answer"
# )


#-----MERGE WVS&OS -----



dat_merge_k <- plyr::rbind.fill(wvs7_sub, krg_chr, roj_chr) #chr merged set
dat_merge_num_k <- plyr::rbind.fill(wvs7_sub, krg, roj) #num merged set



#----- CREATION NUMERIC DATASET -----
# for the numerical dataset all Missing cases and cases with no
# answer will be set NA so an analysis is possible,
# all numeric scales will be adapted to start with 1
# and will be checked for correct numerical value by comparison with
# dat_merge


dat_merge_k[dat_merge_k == "98"] <- NA
dat_merge_k[dat_merge_k == "97"] <- NA

dat_merge_k[dat_merge_k == "Missing"] <- NA
dat_merge_k[dat_merge_k == "No answer"] <- NA
dat_merge_k[dat_merge_k == "Don't know"] <- NA

dat_merge_num_k[dat_merge_num_k == "98"] <- NA
dat_merge_num_k[dat_merge_num_k == "97"] <- NA

dat_merge_num_k[dat_merge_num_k == "Missing"] <- NA
dat_merge_num_k[dat_merge_num_k == "No answer"] <- NA
dat_merge_num_k[dat_merge_num_k == "Don't know"] <- NA

# TODO PROVINZENFRAGE
#-------KURDDUMMY----

# MIT DERSIM OHNE ANTEP OHNE ERZURUM
# Dersim ist mit TR: TRB1 Malatya,Elazig,Bingol,Tunceli
# Antep ist mit  TR: TRC1 Gaziantep,Adiyaman,Kilis 
# Erzurum ist mit TR: TRA1 Erzurum,Erzincan,Bayburt


dat_merge_k$kurd_dummy <- case_when(dat_merge_k$ethnicity=="Kurd"~1,
                                    dat_merge_k$language_home=="Kurdish"~1,
                                    dat_merge_k$region=="TR: TRC3 Mardin,Batman,Sirnak,Siirt"|
                                      dat_merge_k$region=="TR: TRC2 Sanliurfa,Diyarbakir"|
                                      dat_merge_k$region=="TR: TRB1 Malatya,Elazig,Bingol,Tunceli"|  
                                      dat_merge_k$region=="TR: TRB2 Van,Mus,Bitlis,Hakkari"~1, 
                                      TRUE~0)
dat_merge_num_k$kurd_dummy <- case_when(dat_merge_num_k$ethnicity=="Kurd"~1,
                                        dat_merge_num_k$language_home=="Kurdish"~1,
                                        dat_merge_num_k$region=="TR: TRC3 Mardin,Batman,Sirnak,Siirt"|
                                          dat_merge_num_k$region=="TR: TRC2 Sanliurfa,Diyarbakir"|
                                          dat_merge_k$region=="TR: TRB1 Malatya,Elazig,Bingol,Tunceli"|  
                                          dat_merge_num_k$region=="TR: TRB2 Van,Mus,Bitlis,Hakkari"~1, 
                                    TRUE~0)

# für wvs auch wegen plot

wvs7$kurd_dummy <- case_when(wvs7$ethnicity=="Kurd"~1,
                                        wvs7$Q272=="Kurdish"~1,
                                        wvs7$region=="TR: TRC3 Mardin,Batman,Sirnak,Siirt"|
                                          wvs7$region=="TR: TRC2 Sanliurfa,Diyarbakir"|
                                          dat_merge_k$region=="TR: TRB1 Malatya,Elazig,Bingol,Tunceli"|  
                                          wvs7$region=="TR: TRB2 Van,Mus,Bitlis,Hakkari"~1, 
                                        TRUE~0)
#//-- pol_interest -----
# pol_interest

dat_merge_num_k$pol_interest <- recode(dat_merge_num_k$pol_interest,
                                       "1"=4,
                                       "2"=3,
                                       "3"=2,
                                       "4"=1,
                                       "Very interested"=4,
                                       "Somewhat interested"=3,
                                       "Not very interested"=2,
                                       "Not at all interested"=1)
#//-- rate_system -----

dat_merge_num_k$rate_system <- as.numeric(dat_merge_num_k$rate_system)

#//-- rate_system_kri -----

dat_merge_num_k$rate_system_kri <- as.numeric(dat_merge_num_k$rate_system_kri)

#//-- rate_system_nes -----

dat_merge_num_k$rate_system_nes <- as.numeric(dat_merge_num_k$rate_system_nes)

#//-- sys_leader -----

# 
dat_merge_num_k$sys_leader <- recode(dat_merge_num_k$sys_leader,"1"=4,
                                   "2"=3,
                                   "3"=2,
                                   "4"=1,
                                   "Very good"=4,
                                   "Fairly good"=3,
                                   "Fairly bad"=2,
                                   "Very bad"=1)
#//-- sys_expert -----

# 
dat_merge_num_k$sys_expert <- recode(dat_merge_num_k$sys_expert,"1"=4,
                                     "2"=3,
                                     "3"=2,
                                     "4"=1,
                                     "Very good"=4,
                                     "Fairly good"=3,
                                     "Fairly bad"=2,
                                     "Very bad"=1)
#//-- sys_army -----

# 
dat_merge_num_k$sys_army <- recode(dat_merge_num_k$sys_army,"1"=4,
                                     "2"=3,
                                     "3"=2,
                                     "4"=1,
                                     "Very good"=4,
                                     "Fairly good"=3,
                                     "Fairly bad"=2,
                                     "Very bad"=1)
#//-- sys_democ -----

# 
dat_merge_num_k$sys_democ <- recode(dat_merge_num_k$sys_democ,"1"=4,
                                   "2"=3,
                                   "3"=2,
                                   "4"=1,
                                   "Very good"=4,
                                   "Fairly good"=3,
                                   "Fairly bad"=2,
                                   "Very bad"=1)

#//-- conf_armed -----
dat_merge_num_k$conf_armed <- recode(dat_merge_num_k$conf_armed,"1"=1,
                                    "2"=2,
                                    "3"=3,
                                    "4"=4,
                                    "A great deal"=4,
                                    "Quite a lot"=3,
                                    "Not very much"=2,
                                    "None at all"=1)

#//-- conf_police -----
# 
dat_merge_num_k$conf_police <- dplyr::recode(dat_merge_num_k$conf_police,"1"=1,
                                     "2"=2,
                                     "3"=3,
                                     "4"=4,
                                     "A great deal"=4,
                                     "Quite a lot"=3,
                                     "Not very much"=2,
                                     "None at all"=1)
#//-- conf_parl -----
dat_merge_num_k$conf_parl <- dplyr::recode(dat_merge_num_k$conf_parl,"1"=1,
                                             "2"=2,
                                             "3"=3,
                                             "4"=4,
                                             "A great deal"=4,
                                             "Quite a lot"=3,
                                             "Not very much"=2,
                                             "None at all"=1)

#//-- conf_gov -----
dat_merge_num_k$conf_gov <- dplyr::recode(dat_merge_num_k$conf_gov,"1"=1,
                                           "2"=2,
                                           "3"=3,
                                           "4"=4,
                                           "A great deal"=4,
                                           "Quite a lot"=3,
                                           "Not very much"=2,
                                           "None at all"=1)
#//-- conf_part -----
dat_merge_num_k$conf_part <- dplyr::recode(dat_merge_num_k$conf_part,"1"=1,
                                          "2"=2,
                                          "3"=3,
                                          "4"=4,
                                          "A great deal"=4,
                                          "Quite a lot"=3,
                                          "Not very much"=2,
                                          "None at all"=1)
#//-- conf_court -----

# 
dat_merge_num_k$conf_court <- dplyr::recode(dat_merge_num_k$conf_court,"1"=1,
                                           "2"=2,
                                           "3"=3,
                                           "4"=4,
                                           "A great deal"=4,
                                           "Quite a lot"=3,
                                           "Not very much"=2,
                                           "None at all"=1)

#//-- conf_court -----
dat_merge_num_k$rate_system <- as.numeric(dat_merge_num_k$rate_system)

# democ_econ
dat_merge_num_k$democ_econ <- recode(dat_merge_num_k$democ_econ,
                                    "Don't know"=0,
                                    "Strongly disagree"=1,
                                    "Disagree"=2,
                                    "Agree"=3,
                                    "Agree strongly"=4)
# democ_indec
dat_merge_num_k$democ_indec <- recode(dat_merge_num_k$democ_indec,
                                   "Don't know"=0,
                                   "Strongly disagree"=1,
                                   "Disagree"=2,
                                   "Agree"=3,
                                   "Agree strongly"=4)
# democ_order
dat_merge_num_k$democ_order <- recode(dat_merge_num_k$democ_order,
                                    "Don't know"=0,
                                    "Strongly disagree"=1,
                                    "Disagree"=2,
                                    "Agree"=3,
                                    "Agree strongly"=4)
# democ_best
dat_merge_num_k$democ_best <- recode(dat_merge_num_k$democ_best,"3"=0,
                                   "4"=4,
                                   "5"=3,
                                   "6"=2,
                                   "7"=1)
# pol_news
dat_merge_num_k$pol_news <- recode(dat_merge_num_k$pol_news,"3"=0,
                                   "4"=4,
                                   "5"=3,
                                   "6"=2,
                                   "7"=1)

#//-- democ_own -----

# 
# not at all democratic 1 to completely democratic 10
dat_merge_num_k$democ_own <- recode(dat_merge_num_k$democ_own,
              "0"=1,
              "1"=1,"2"=2,"3"=3,
              "4"=4,"5"=5,"6"=6,"7"=7,
              "8"=8,"9"=9,"10"=10, .default = 0)
#//-- gender -----

# 
dat_merge_num_k$gender <- recode(dat_merge_num_k$gender,
                                 "1"=1,
                                 "2"=2,
                                 "Male"=1,
                                 "Female"=2)
#//-- age -----

# 

dat_merge_num_k$age <- as.numeric(dat_merge_num_k$age)
#//-- marit -----

#  // einfach bei character bleiben

dat_merge_num_k$marit <- dat_merge_k$marit

#//-- edu -----

dat_merge_num_k$edu <- recode(dat_merge_num_k$edu,
               "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
               "Illiterate or no formal education"=1,
               "Elementary attended; completed or incompleted"=2,
               "Secondary school attended; completed or incompleted"=3,
               "Postsecondary school attended; completed or incompleted"=4,
               "Lower tertiary education"=5,
               "Upper tertiary education"=6)

#//-- relig_pers -----
# TODO Bei der nächsten Ausführung numerische Lassen

# dummy for religiousness

dat_merge_num_k$relig_pers <- recode(dat_merge_num_k$relig_pers,
                                     "1"=	"1",
                                     "2"=	"2",
                                     "3"=	"3",
                                     "A religious person"="1",
                                     "No religious person"="2",
                                     "97"=	"Don't know",
                                     "98"=	"No answer"
)
dat_merge_num_k$relig_pers <- as.numeric(dat_merge_num_k$relig_pers)

dat_merge_k$relig_pers <- recode(dat_merge_k$relig_pers,
                                 "1"=	"A religious person",
                                 "2"=	"No religious person",
                                 "3"=	"An atheist",
                                 "A religious person"="A religious person",
                                 "No religious person"="No religious person",
                                 "97"=	"Don't know",
                                 "98"=	"No answer"
)


# FILTER NON KURDS OUT

dat_merge_num_k<-dat_merge_num_k%>%                     
  filter(kurd_dummy==1)


dat_merge_k<-dat_merge_num_k%>%                     
  filter(kurd_dummy==1)
# ------ SAVE DATASETS --------

save(dat_merge_k, file = ".data/processed/dat_merge_k.Rdata")
save(dat_merge_num_k, file = ".data/processed/dat_merge_num_k.Rdata")