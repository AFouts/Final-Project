# Treatment effect (participants)

## All arms
library(fixest)
income_all <- fixest::feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
market_sale_all <- fixest::feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
rosca_all <- fixest::feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
discord_all <- fixest::feols(discord_num_EL ~ treated + discord_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)

## Market arm
income_market <- fixest::feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
market_sale_market <- fixest::feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
rosca_market <- fixest::feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
discord_market <- fixest::feols(discord_num_EL ~ treated + discord_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)

## Church arm
income_church <- fixest::feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
market_sale_church <- fixest::feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
rosca_church <- fixest::feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
discord_church <- fixest::feols(discord_num_EL ~ treated + discord_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)

# Treatment effect (friends)

## All arms
income_all_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
market_sale_all_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
rosca_all_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
discord_all_spillover <- feols(discord_num_EL ~ treated + discord_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)

## Market arm
income_market_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
market_sale_market_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
rosca_market_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
discord_market_spillover <- feols(discord_num_EL ~ treated + discord_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)

## Church arm
income_church_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
market_sale_church_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
rosca_church_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
discord_church_spillover <- feols(discord_num_EL ~ treated + discord_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)