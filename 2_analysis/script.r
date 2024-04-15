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

# Dealing with the missing zeros
data$market_sale_total_EL[is.na(data$market_sale_total_EL)] <- 0
data$market_sale_total_BL[is.na(data$market_sale_total_BL)] <- 0
data$rosca_amt_gives_BL[is.na(data$rosca_amt_gives_BL)] <- 0
data$rosca_amt_gives_EL[is.na(data$rosca_amt_gives_EL)] <- 0
data$discord_village_num_BL[is.na(data$discord_village_num_BL)] <- 0
data$discord_village_num_EL[is.na(data$discord_village_num_EL)] <- 0

# Treatment effect (participants)
library(lessR)
data_long_all <- data[.(respondent_BL=="RESPONDENT - LONG"),]

## All arms
library(fixest)
income_all <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
market_sale_all <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
rosca_all <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
discord_all <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)

## Market
data_long_market <- data_long_all[.(treated_market==1 | treated ==0),]
income_market <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
market_sale_market <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
rosca_market <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
discord_market <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)

## Churches
data_long_church <- data_long_all[.(treated_church==1 | treated ==0),]
income_church <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
market_sale_church <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
rosca_church <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
discord_church <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)

## Tables
library(modelsummary)
participants_all <- modelsummary(list("Monetary income" = income_all, "Market sales" = market_sale_all, "ROSCA contributions" = rosca_all, "Village discord" = discord_all), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_all.tex")
participants_market <- modelsummary(list("Monetary income" = income_market, "Market sales" = market_sale_market, "ROSCA contributions" = rosca_market, "Village discord" = discord_market), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_market.tex")
participants_church <- modelsummary(list("Monetary income" = income_church, "Market sales" = market_sale_church, "ROSCA contributions" = rosca_church, "Village discord" = discord_church), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_church.tex")

# Treatment effect (friends)
data_spillover_all <- data[.(respondent_BL=="RESPONDENT - FRIEND"),]

## All arms
income_all_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
market_sale_all_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
rosca_all_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
discord_all_spillover <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)

## Market
data_spillover_market <- data_spillover_all[.(treated_market==1 | treated ==0),]
income_market_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
market_sale_market_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
rosca_market_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
discord_market_spillover <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)

## Churches
data_spillover_church <- data_spillover_all[.(treated_church==1 | treated ==0),]
income_church_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
market_sale_church_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
rosca_church_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
discord_church_spillover <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)

## Tables
library(modelsummary)
spillover_all <- modelsummary(list("Monetary income" = income_all_spillover, "Market sales" = market_sale_all_spillover, "ROSCA contributions" = rosca_all_spillover, "Village discord" = discord_all_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_all.tex")
spillover_market <- modelsummary(list("Monetary income" = income_market_spillover, "Market sales" = market_sale_market_spillover, "ROSCA contributions" = rosca_market_spillover, "Village discord" = discord_market_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_market.tex")
spillover_church <- modelsummary(list("Monetary income" = income_church_spillover, "Market sales" = market_sale_church_spillover, "ROSCA contributions" = rosca_church_spillover, "Village discord" = discord_church_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_church.tex")
