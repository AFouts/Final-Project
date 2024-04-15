# Imports data
library(readstata13)
data <- read.dta13("1_data/CRD_Baseline_Endline_Tmt_clean.dta")

# Identify the treatment arm of each village
village_treatment <- unique(data[.(respondent_BL=="RESPONDENT - LONG"),c("vil_code", "treatment")])
village_treatment <- data_village_treatment[order(data_village_treatment$vil_code),]
village_treatment$treated = ifelse(village_treatment$treatment == 1 | village_treatment$treatment == 2, 1, 0) 
village_treatment$treated_market = ifelse(village_treatment$treatment == 2, 1, 0) 
village_treatment$treated_church = ifelse(village_treatment$treatment == 1, 1, 0) 

# Treatment arm dummies for respondents
data <- merge(data, village_treatment,by="vil_code")

# Treatment effect
library(lessR)
data_long_all <- data[.(respondent_BL=="RESPONDENT - LONG"),]
data_long_all$market_sale_total_EL[is.na(data_long_all$market_sale_total_EL)] <- 0
data_long_all$market_sale_total_BL[is.na(data_long_all$market_sale_total_BL)] <- 0
data_long_all$rosca_amt_receives_BL[is.na(data_long_all$rosca_amt_receives_BL)] <- 0
data_long_all$rosca_amt_receives_EL[is.na(data_long_all$rosca_amt_receives_EL)] <- 0
data_long_all$discord_village_num_BL[is.na(data_long_all$discord_village_num_BL)] <- 0
data_long_all$discord_village_num_EL[is.na(data_long_all$discord_village_num_EL)] <- 0

library(fixest)

## All
income_all <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
market_sale_all <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
rosca_all <- feols(rosca_amt_receives_EL ~ treated + rosca_amt_receives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
discord_all <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)

## Market
data_long_market <- data_long_all[.(treated_market==1 | treated ==0),]
income_market <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
market_sale_market <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
rosca_market <- feols(rosca_amt_receives_EL ~ treated + rosca_amt_receives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
discord_market <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)

## Churches
data_long_church <- data_long_all[.(treated_church==1 | treated ==0),]
income_church <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
market_sale_church <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
rosca_church <- feols(rosca_amt_receives_EL ~ treated + rosca_amt_receives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
discord_church <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)

