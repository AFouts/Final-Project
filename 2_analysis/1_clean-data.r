# Source data

## Imports data
library(readstata13)
data = read.dta13("1_data/CRD_Baseline_Endline_Tmt_clean.dta")

## Identify the treatment arm of each village
library(lessR)
village_treatment = unique(data[.(respondent_BL=="RESPONDENT - LONG"),c("vil_code", "treatment")])
village_treatment <- village_treatment[order(village_treatment$vil_code),]
village_treatment$treated = ifelse(village_treatment$treatment == 1 | village_treatment$treatment == 2, 1, 0) 
village_treatment$treated_market = ifelse(village_treatment$treatment == 2, 1, 0) 
village_treatment$treated_church = ifelse(village_treatment$treatment == 1, 1, 0) 

## Treatment arm dummies for respondents
data <- merge(data, village_treatment,by="vil_code")

## Dealing with the missing zeros
data$market_sale_total_EL[is.na(data$market_sale_total_EL) & !is.na(data$today_EL)] <- 0
data$market_sale_total_BL[is.na(data$market_sale_total_BL)] <- 0
data$rosca_amt_gives_EL[is.na(data$rosca_amt_gives_EL) & !is.na(data$today_EL)] <- 0
data$rosca_amt_gives_BL[is.na(data$rosca_amt_gives_BL)] <- 0
data$discord_spouse_num_EL[is.na(data$discord_spouse_num_EL) & !is.na(data$today_EL)] <- 0
data$discord_spouse_num_BL[is.na(data$discord_spouse_num_BL)] <- 0
data$discord_family_num_EL[is.na(data$discord_family_num_EL) & !is.na(data$today_EL)] <- 0
data$discord_family_num_BL[is.na(data$discord_family_num_BL)] <- 0
data$discord_village_num_EL[is.na(data$discord_village_num_EL) & !is.na(data$today_EL)] <- 0
data$discord_village_num_BL[is.na(data$discord_village_num_BL)] <- 0

## Creates relevant new variables
data$discord_num_EL = data$discord_spouse_num_EL + data$discord_family_num_EL + data$discord_village_num_EL
data$discord_num_BL = data$discord_spouse_num_BL + data$discord_family_num_BL + data$discord_village_num_BL

# Treatment effect (participants)
data_long_all = data[.(respondent_BL=="RESPONDENT - LONG"),]

## Market arm
data_long_market = data_long_all[.(treated_market==1 | treated ==0),]

## Church arm
data_long_church = data_long_all[.(treated_church==1 | treated ==0),]

# Treatment effect (friends)
data_spillover_all = data[.(respondent_BL=="RESPONDENT - FRIEND"),]

## Market arm
data_spillover_market = data_spillover_all[.(treated_market==1 | treated ==0),]

## Church arm
data_spillover_church = data_spillover_all[.(treated_church==1 | treated ==0),]