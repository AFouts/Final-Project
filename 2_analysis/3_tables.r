# Treatment effect (participants)

library(modelsummary)
modelsummary(list('Monetary income' = income_all, 'Market sales' = market_sale_all, 'ROSCA contributions' = rosca_all, 'Village discord' = discord_all), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_all.tex")
modelsummary(list('Monetary income' = income_market, 'Market sales' = market_sale_market, 'ROSCA contributions' = rosca_market, 'Village discord' = discord_market), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_market.tex")
modelsummary(list('Monetary income' = income_church, 'Market sales' = market_sale_church, 'ROSCA contributions' = rosca_church, 'Village discord' = discord_church), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/participants_church.tex")

# Treatment effect (friends)

library(modelsummary)
modelsummary(list("Monetary income" = income_all_spillover, "Market sales" = market_sale_all_spillover, "ROSCA contributions" = rosca_all_spillover, "Village discord" = discord_all_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_all.tex")
modelsummary(list("Monetary income" = income_market_spillover, "Market sales" = market_sale_market_spillover, "ROSCA contributions" = rosca_market_spillover, "Village discord" = discord_market_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_market.tex")
modelsummary(list("Monetary income" = income_church_spillover, "Market sales" = market_sale_church_spillover, "ROSCA contributions" = rosca_church_spillover, "Village discord" = discord_church_spillover), gof_map = c("nobs", "r.squared"), stars = TRUE, output = "3_tables/spillover_church.tex")