### Clean the datasets of 2014

# Install and load packages
#install.packages("")
library("easypackages")
libraries("haven", "tidyverse", "labelled")
getwd()

## Membership in organization
secd3 <- read_dta("../data/cleaned2014/SEC_D3_clean.dta")
secd3 <- secd3 %>%
  filter(d8 != 0) %>% # drop if no HH member in an organization
  mutate(hhid = pa * 100 + hh_no) %>%
  select(hhid, hhmid_1, hhmid_2, hhmid_3, d8_type)
secd3 <- pivot_longer(data = secd3,
                      cols = !c(hhid, d8_type), 
                      names_to = "memnum", 
                      values_to = "hhmid")
secd3 <- secd3 %>%
  filter(!is.na(hhmid)) %>% # drop organizations with no member
  distinct(d8_type, hhmid, .keep_all = TRUE) %>% # drop duplicates
  group_by(hhmid) %>% # group by then collapse with count
  summarise(org_ind = n()) %>%
  ungroup()

## Health
secp1 <- read_dta("../data/cleaned2014/SEC_P1.dta")
secp1 <- secp1 %>%
  filter(!is.na(p3)) %>% # keep only members with days unable to work
  mutate(hhmid = pa * 10000 + hh_no * 100 + p1_id) %>%
  select(hhmid, p3)

# Climatic shocks 
seco2 <- read_dta("../data/cleaned2014/SEC_O2.dta")
seco2 <- seco2 %>%
  mutate(hhid = pa * 100 + hh_no) %>%
  mutate(shockclim = ifelse(o2 %in% c(1, 2, 3, 5), 1, 0))
seco2 <- seco2 %>%
  group_by(hhid) %>% 
  summarize(shockclim5nr = sum(shockclim)) %>%
  ungroup()

## Household head and household characteristics
secb <- read_dta("../data/cleaned2014/SEC_B_clean.dta")
secb <- secb %>% 
  mutate(hhid = pa * 100 + hh_no)
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
secg <- read_dta("../data/cleaned2014/SEC_G_clean.dta")
secg <- secg %>% 
  mutate(hhid = pa * 100 + hh_no) %>%
  mutate(assetva = ifelse(is.na(g3), 0, g1 * g3))
secg <- secg %>%
  group_by(hhid) %>% 
  summarize(assettot = sum(assetva)) %>%
  ungroup()

# Farm size
sech2 <- read_dta("../data/cleaned2014/SEC_H2_clean.dta")
sech2 <- sech2 %>%
  mutate(hhid = pa * 100 + hh_no)
sech2 <- sech2 %>% # data entry error
  mutate(h5u = case_when( 
  hhid==7050600914  & h5a==520 ~ 17,
  .default = h5u))
sech2 <- sech2 %>%
  group_by(hhid) %>% 
  summarize(farmsize = sum(parcelsize)) %>%
  ungroup()

# Risk preference
secrisk <- read_dta("../data/cleaned2014/risk_final.dta")
secrisk <- secrisk %>%
  filter(respondent == 1) %>% # select only household heads
  select(hhid, e1, e2, e4, e5, risk0to9, ambi0to9)
  
## Technology adoption
secn <- read_dta("../data/cleaned2014/SEC_N.dta")
secn <-  secn %>% mutate(hhid = pa * 100 + hh_no)
secn <- secn %>% 
  rename(techtype = n1_type) %>%
  mutate(dtech = ifelse(n2==2 | is.na(n2), 0, n2)) %>%
  mutate(techpct = ifelse(is.na(n4), 0, n4)) %>%
  mutate(techyr = ifelse(is.na(n5), 0, 2014 - n5)) %>%
  filter(!techtype %in% c(11, 13, 14, 19, 20, 21)) %>% # no or less than 1 percent adopters
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
rt2014 <- list(secb, secg, sech2, secrisk, secn) %>% 
  reduce(inner_join, by = "hhid") %>%
  mutate(year = 2014)
rm(secb, secg, sech2, secrisk, secn)
var_label(rt2014$hhid) = "Household ID"
var_label(rt2014$hhsize) = "Household size"
var_label(rt2014$texp_pp_clothing) = "Total per person expenditures on clothing and footware in the past 12 months"
var_label(rt2014$hhmid) = "Household member ID"
var_label(rt2014$gender) = "Gender of HH head"
var_label(rt2014$age) = "Age of HH head"
var_label(rt2014$married) = "HH head married (Yes=1, No=0)"
var_label(rt2014$edu) = "Years of formal education for HH head"
var_label(rt2014$if_edu_informal) = "Whether HH head received informal education (Yes=1, No=0)"
var_label(rt2014$org_ind) = "Number of organizations HH head is in"
var_label(rt2014$p3) = "Number of days in past 12 months HH head unable to work"
var_label(rt2014$shockclim5nr) = "Number of climatic shocks in the past 5 years"
var_label(rt2014$assettot) = "Total value of assets owned"
var_label(rt2014$farmsize) = "Total farm size (ha)"
var_label(rt2014$year) = "Year"

#na_count <- sapply(rt2014, function(y) sum(length(which(is.na(y)))))
#na_count <- data.frame(na_count)