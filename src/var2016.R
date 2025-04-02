### Clean the datasets of 2016

# Install and load packages
# install.packages("")
library("easypackages")
libraries("haven", "tidyverse", "labelled")
getwd()

## Membership in organization
secd3 <- read_dta("../data/cleaned2016/section_d8_clean.dta")
secd3 <- secd3 %>%
  filter(d8 != 0) %>% # drop if no HH member in an organization
  mutate(hhid = pa * 100 + hh_no) %>%
  select(hhid, qmid_1, qmid_2, qmid_3, d8_groups)
secd3 <- pivot_longer(data = secd3,
                      cols = !c(hhid, d8_groups), 
                      names_to = "memnum", 
                      values_to = "hhmid")
secd3 <- secd3 %>%
  filter(!is.na(hhmid)) %>% # drop organizations with no member
  distinct(d8_groups, hhmid, .keep_all = TRUE) %>% # drop duplicates
  group_by(hhmid) %>% # group by then collapse with count
  summarise(org_ind = n()) %>%
  ungroup()

# Health
secp1 <- read_dta("../data/cleaned2016/section_p1.dta")
secp1 <- secp1 %>%
  filter(!is.na(p3)) %>% # keep only members with days unable to work
  mutate(hhmid = pa * 10000 + hh_no * 100 + p1_id) %>%
  select(hhmid, p3)

# Climatic shocks 
seco2 <- read_dta("../data/cleaned2016/section_o2.dta")
seco2 <- seco2 %>%
  mutate(hhid = pa * 100 + hh_no) %>%
  mutate(shockclim = ifelse(o2 %in% c(1, 2, 3, 5), 1, 0))
seco2 <- seco2 %>%
  group_by(hhid) %>% 
  summarize(shockclim5nr = sum(shockclim)) %>%
  ungroup()

## Household head and household characteristics
secb <- read_dta("../data/cleaned2016/section_b_clean.dta")
secb <- secb %>% 
  mutate(hhid = pa * 100 + hh_no) %>%
  mutate(hhmid = pa * 10000 + hh_no * 100 + b0)
secb <- secb %>% 
  group_by(hhid) %>% 
  mutate(hhsize = n()) %>%
  mutate(texp_pp_clothing = sum(b11) / hhsize) %>%
  ungroup()
secb <- secb %>%
  filter(b5 == 1) %>% # select only household heads
  mutate(gender = ifelse(b2 == 1, 1, 0)) %>%
  mutate(age = b3) %>%
  mutate(married = ifelse(b4 == 1 | b4 == 2, 1, 0)) %>%
  mutate(if_edu_informal = ifelse(b9 == 2, 1, 0)) %>%
  mutate(edu = case_match(
    b9,
    1 ~ 0,
    2 ~ 0,
    11 ~ 1,
    12 ~ 2,
    13 ~ 3,
    14 ~ 4,
    15 ~ 5,
    16 ~ 6,
    17 ~ 7,
    18 ~ 8,
    21 ~ 9,
    22 ~ 10,
    23 ~ 11,
    24 ~ 12,
    31 ~ 9,
    32 ~ 10,
    33 ~ 11,
    34 ~ 12,
    35 ~ 11,
    36 ~ 12,
    25 ~ 13,
    27 ~ 13,
    26 ~ 15,
    28 ~ 15)) %>%
  select(hhid, hhmid, hhsize, texp_pp_clothing, gender, age, married, edu, if_edu_informal, woreda)
secb <- left_join(secb, secd3, by = "hhmid") %>% 
  mutate(org_ind = ifelse(is.na(org_ind), 0, org_ind))
secb <- left_join(secb, secp1, by = "hhmid") %>% 
  mutate(p3 = ifelse(is.na(p3), 0, p3))
secb <- left_join(secb, seco2, by = "hhid") %>% 
  mutate(shockclim5nr = ifelse(is.na(shockclim5nr), 0, shockclim5nr))
rm(secd3, secp1, seco2)

# Assets
secg <- read_dta("../data/cleaned2016/section_g_clean.dta")
secg <- secg %>% 
  mutate(hhid = pa * 100 + hh_no) %>%
  mutate(assetva = ifelse(is.na(g3), 0, g1 * g3))
secg <- secg %>%
  group_by(hhid) %>% 
  summarize(assettot = sum(assetva)) %>%
  ungroup()

# Farm size
sech2 <- read_dta("../data/cleaned2016/section_h2_clean.dta")
sech2 <- sech2 %>%
  mutate(hhid = pa * 100 + hh_no)
sech2 <- sech2 %>% # data entry error
  mutate(landconv1 = case_when(
  hhid==4140300214 ~ 6,
  hhid==7050600117 ~ 6,
  h5u==2 ~ 1,
  .default = landconv1))
sech2 <- sech2 %>%
  group_by(hhid) %>% 
  summarize(farmsize = sum(parcelsize)) %>%
  ungroup()

## Technology adoption
secn <- read_dta("../data/cleaned2016/section_n1_clean.dta")
secn <-  secn %>% mutate(hhid = pa * 100 + hh_no)
secn <- secn %>% 
  rename(techtype = n1_pratice) %>%
  mutate(dtech = ifelse(n2==2 | is.na(n2), 0, n2)) %>%
  mutate(techpct = ifelse(is.na(n4), 0, n4)) %>%
  mutate(techyr = ifelse(is.na(n5), 0, 2016 - n5)) %>%
  filter(!techtype %in% c(13, 14, 18, 19, 20, 21, 23)) %>% # no or less than 1 percent adopters
  select(hhid, techtype, dtech, techpct, techyr)
var_label(secn$hhid) = "Household ID"
var_label(secn$techtype) = "Type of technology or practice"
var_label(secn$dtech) = "Adopter of this technology (Yes=1, No=0)"
var_label(secn$techpct) = "Percentage of farm land with this technology"
var_label(secn$techyr) = "Number of years since adopting this technology"
table(secn$techtype, secn$dtech)
secn <- pivot_wider(data = secn, 
                    id_cols = hhid, 
                    names_from = techtype, 
                    values_from = c(dtech, techpct, techyr),
                    names_sep = "")

# Merge all datasets into one
rt2016 <- list(secb, secg, sech2, secn) %>% 
  reduce(inner_join, by = "hhid") %>%
  mutate(year = 2016)
rm(secb, secn, secg, sech2)
var_label(rt2016$hhid) = "Household ID"
var_label(rt2016$hhsize) = "Household size"
var_label(rt2016$texp_pp_clothing) = "Total per person expenditures on clothing and footware in the past 12 months"
var_label(rt2016$hhmid) = "Household member ID"
var_label(rt2016$gender) = "Gender of HH head"
var_label(rt2016$age) = "Age of HH head"
var_label(rt2016$married) = "HH head married (Yes=1, No=0)"
var_label(rt2016$edu) = "Years of formal education for HH head"
var_label(rt2016$if_edu_informal) = "Whether HH head received informal education (Yes=1, No=0)"
var_label(rt2016$org_ind) = "Number of organizations HH head is in"
var_label(rt2016$p3) = "Number of days in past 12 months HH head unable to work"
var_label(rt2016$shockclim5nr) = "Number of climatic shocks in the past 5 years"
var_label(rt2016$assettot) = "Total value of assets owned"
var_label(rt2016$farmsize) = "Total farm size (ha)"
var_label(rt2016$year) = "Year"

na_count <-sapply(rt2016, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
