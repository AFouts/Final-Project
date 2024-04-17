renv::restore()

# Imports data
library(readstata13)
data = read.dta13("1_data/CRD_Baseline_Endline_Tmt_clean.dta")

# Identify the treatment arm of each village
library(lessR)
village_treatment = unique(data[.(respondent_BL=="RESPONDENT - LONG"),c("vil_code", "treatment")])
village_treatment <- village_treatment[order(village_treatment$vil_code),]
village_treatment$treated = ifelse(village_treatment$treatment == 1 | village_treatment$treatment == 2, 1, 0) 
village_treatment$treated_market = ifelse(village_treatment$treatment == 2, 1, 0) 
village_treatment$treated_church = ifelse(village_treatment$treatment == 1, 1, 0) 

# Treatment arm dummies for respondents
data <- merge(data, village_treatment,by="vil_code")

# Dealing with the missing zeros
data$market_sale_total_EL[is.na(data$market_sale_total_EL) & !is.na(data$today_EL)] <- 0
data$market_sale_total_BL[is.na(data$market_sale_total_BL)] <- 0
data$rosca_amt_gives_EL[is.na(data$rosca_amt_gives_EL) & !is.na(data$today_EL)] <- 0
data$rosca_amt_gives_BL[is.na(data$rosca_amt_gives_BL)] <- 0
data$discord_village_num_EL[is.na(data$discord_village_num_EL) & !is.na(data$today_EL)] <- 0
data$discord_village_num_BL[is.na(data$discord_village_num_BL)] <- 0

# Treatment effect (participants)
data_long_all = data[.(respondent_BL=="RESPONDENT - LONG"),]

## All arms
library(fixest)
income_all <- fixest::feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
market_sale_all <- fixest::feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
rosca_all <- fixest::feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)
discord_all <- fixest::feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_all)

## Market arm
data_long_market = data_long_all[.(treated_market==1 | treated ==0),]
income_market <- fixest::feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
market_sale_market <- fixest::feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
rosca_market <- fixest::feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)
discord_market <- fixest::feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_market)

## Churche arm
data_long_church = data_long_all[.(treated_church==1 | treated ==0),]
income_church <- fixest::feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
market_sale_church <- fixest::feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
rosca_church <- fixest::feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)
discord_church <- fixest::feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_long_church)

## Tables
library(modelsummary)
modelsummary(list('Monetary income' = income_all, 'Market sales' = market_sale_all, 'ROSCA contributions' = rosca_all, 'Village discord' = discord_all), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_all.tex")
modelsummary(list('Monetary income' = income_market, 'Market sales' = market_sale_market, 'ROSCA contributions' = rosca_market, 'Village discord' = discord_market), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_market.tex")
modelsummary(list('Monetary income' = income_church, 'Market sales' = market_sale_church, 'ROSCA contributions' = rosca_church, 'Village discord' = discord_church), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_church.tex")

## Figures
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
plot_models(income_all, income_market, income_church,
            market_sale_all, market_sale_market, market_sale_church,
            rosca_all, rosca_market, rosca_church,
            discord_all, discord_market, discord_church,
            rm.terms = c("income_BL", "market_sale_total_BL", "rosca_amt_gives_BL", "discord_village_num_BL","age_BL","sex_BL [FEMALE, MALE]","dist_kga"),
            vline.color = "red",
            spacing = 0.9,
            show.values = TRUE,
            axis.labels = c("Treatment Effect"),
            m.labels = c("Income\nAll arms",
                         "Village discord\nAll arms",
                         "Village discord\nMarket arm",
                         "Village discord\nChurch arm",
                         "Income\nMarket arm",
                         "Income\nChurch arm",
                         "Market sales\nAll arms",
                         "Market sales\nMarket arm",
                         "Market sales\nChurch arm",
                         "ROSCA giving\nAll arms",
                         "ROSCA giving\nMarket arm",
                         "ROSCA giving\nChurch arm"),
            colors = c("brown", "blue", "gold",
                       "brown", "blue", "gold",
                       "brown", "blue", "gold",
                       "brown", "blue", "gold")) + theme(legend.title=element_blank(),
                                                               legend.key.spacing.y = unit(0.88, units="cm"),
                                                               legend.text=element_text(size=7),
                                                               legend.position = c(0.85, 0.5),
                                                               axis.text.y = element_blank(),
                                                               axis.title.y = element_blank(),
                                                               axis.ticks.y = element_blank(),
                                                               panel.grid.major.y = element_blank(),
                                                               panel.grid.minor.y = element_blank(),
                                                               panel.grid.major.x = element_line(size=.1, color="black"),
                                                               panel.background = element_blank())
ggsave("4_figures/treatment_effect.pdf")

# Treatment effect (friends)
data_spillover_all = data[.(respondent_BL=="RESPONDENT - FRIEND"),]

## All arms
income_all_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
market_sale_all_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
rosca_all_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)
discord_all_spillover <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_all)

## Market arm
data_spillover_market = data_spillover_all[.(treated_market==1 | treated ==0),]
income_market_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
market_sale_market_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
rosca_market_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)
discord_market_spillover <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_market)

## Churche arm
data_spillover_church = data_spillover_all[.(treated_church==1 | treated ==0),]
income_church_spillover <- feols(income_EL ~ treated + income_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
market_sale_church_spillover <- feols(market_sale_total_EL ~ treated + market_sale_total_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
rosca_church_spillover <- feols(rosca_amt_gives_EL ~ treated + rosca_amt_gives_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)
discord_church_spillover <- feols(discord_village_num_EL ~ treated + discord_village_num_BL + age_BL + sex_BL + dist_kga | wave, cluster = ~ vil_code, data = data_spillover_church)

## Tables
library(modelsummary)
modelsummary(list("Monetary income" = income_all_spillover, "Market sales" = market_sale_all_spillover, "ROSCA contributions" = rosca_all_spillover, "Village discord" = discord_all_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_all.tex")
modelsummary(list("Monetary income" = income_market_spillover, "Market sales" = market_sale_market_spillover, "ROSCA contributions" = rosca_market_spillover, "Village discord" = discord_market_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_market.tex")
modelsummary(list("Monetary income" = income_church_spillover, "Market sales" = market_sale_church_spillover, "ROSCA contributions" = rosca_church_spillover, "Village discord" = discord_church_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_church.tex")

## Figures
plot_models(income_all_spillover, income_market_spillover, income_church_spillover,
            market_sale_all_spillover, market_sale_market_spillover, market_sale_church_spillover,
            rosca_all_spillover, rosca_market_spillover, rosca_church_spillover,
            discord_all_spillover, discord_market_spillover, discord_church_spillover,
            rm.terms = c("income_BL", "market_sale_total_BL", "rosca_amt_gives_BL", "discord_village_num_BL","age_BL","sex_BL [FEMALE, MALE]","dist_kga"),
            vline.color = "red",
            spacing = 0.9,
            show.values = TRUE,
            axis.labels = c("Treatment Effect"),
            m.labels = c("Income\nAll arms",
                         "Village discord\nAll arms",
                         "Village discord\nMarket arm",
                         "Village discord\nChurch arm",
                         "Income\nMarket arm",
                         "Income\nChurch arm",
                         "Market sales\nAll arms",
                         "Market sales\nMarket arm",
                         "Market sales\nChurch arm",
                         "ROSCA giving\nAll arms",
                         "ROSCA giving\nMarket arm",
                         "ROSCA giving\nChurch arm"),
            colors = c("brown", "blue", "gold",
                       "brown", "blue", "gold",
                       "brown", "blue", "gold",
                       "brown", "blue", "gold")) + theme(legend.title=element_blank(),
                                                               legend.key.spacing.y = unit(0.88, units="cm"),
                                                               legend.text=element_text(size=7),
                                                               legend.position = c(0.85, 0.5),
                                                               axis.text.y = element_blank(),
                                                               axis.title.y = element_blank(),
                                                               axis.ticks.y = element_blank(),
                                                               panel.grid.major.y = element_blank(),
                                                               panel.grid.minor.y = element_blank(),
                                                               panel.grid.major.x = element_line(size=.1, color="black"),
                                                               panel.background = element_blank())
ggsave("4_figures/treatment_effect_spillover.pdf")