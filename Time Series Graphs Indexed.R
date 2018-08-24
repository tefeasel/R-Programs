library(haven)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(dygraphs)

setwd("\\Elasticity Study V2016")

residential <- read_sas("res_model_data.sas7bdat")

names(residential) <- tolower(names(residential))

residential_graph <- residential %>%
                group_by(year) %>%
                summarize(sales = sum(mwh),
                          use = mean(mwh/cust),
                          elec_price = mean(elec_price),
                          gas_price = mean(gas_price),
                          income_pc = mean((fypdpia/gdp_deflator/100)/(fpopa/1000)),
                          pop = sum(fpopa),
                          eeindex = mean(eeindex)) %>%
                mutate(sales_pct = (sales/head(sales, n = 1)), 
                       eprice_pct = (elec_price/head(elec_price, n = 1)),
                       gprice_pct = (gas_price/head(gas_price, n = 1)), 
                       inc_pc_pct = (income_pc/head(income_pc, n = 1)), 
                       pop_pct = (pop/head(pop, n = 1)),
                       ee_pct = (eeindex/head(eeindex, n=1)),
                       year = as.Date(paste(year,1,1, sep = "-"), format = "%Y-%m-%d"),
                       base = 1,
                       use_pct = use/head(use, n=1))

resplot <- ggplot(residential_graph, aes(year)) +
                geom_line(aes(y = sales_pct, col = "Sales"), size = 1.8) +
                geom_line(aes(y = gprice_pct, col = "Real Natural Gas Price"), size = 1.1) +
                geom_line(aes(y = inc_pc_pct, col = "Real Income Per Capita"), size = 1.1) +
                geom_line(aes(y = pop_pct, col = "Population"), size = 1.1) +
                geom_line(aes(y = eprice_pct, col = "Real Electricity Price"), size = 1.8) +
                geom_line(aes(y = base)) +
                geom_rect(aes(xmin = as.Date("2008-01-01", format = "%Y-%m-%d"), xmax = as.Date("2009-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf), 
                        alpha = .005, col = "grey92") +
                geom_rect(aes(xmin = as.Date("2001-01-01", format = "%Y-%m-%d"), xmax = as.Date("2001-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf), 
                        alpha = .005, col = "grey92") +
                geom_rect(aes(xmin = as.Date("1992-01-01", format = "%Y-%m-%d"), xmax = as.Date("1992-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf), 
                        alpha = .005, col = "grey92") +
                scale_x_date(date_labels = ("%Y"), date_breaks = ("3 years")) +
                scale_y_continuous(labels = scales::percent) +
                labs(title = "AEP Residential Sales, Prices, Income and Population Since 1992 (Index Year = 1992)", 
                        x = "", y = " ", fill = "") +
                scale_colour_brewer(name = "", palette = "Spectral") +
                theme_hc() +
                theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
resplot
#ggsave(resplot, file = "residential.png", scale = 1, width = 11, height = 8)

commercial <- read_sas("com_model_data.sas7bdat")

names(commercial) <- tolower(names(commercial))

commercial_graph <- commercial %>%
        group_by(year) %>%
        summarize(sales = sum(mwh),
                  elec_price = mean(elec_price),
                  gas_price = mean(gas_price),
                  comgdp = sum(comgdp),
                  use = mean(mwh/cust)) %>%
        mutate(sales_pct = (sales/head(sales, n = 1)), 
               eprice_pct = (elec_price/head(elec_price, n = 1)),
               gprice_pct = (gas_price/head(gas_price, n = 1)), 
               comgdp_pct = (comgdp/head(comgdp, n = 1)),
               year = as.Date(paste(year,1,1, sep = "-"), format = "%Y-%m-%d"),
               base = 1)

complot <- ggplot(commercial_graph, aes(year)) +
        geom_line(aes(y = sales_pct, col = "Sales"), size = 1.8) +
        geom_line(aes(y = gprice_pct, col = "Real Natural Gas Price"), size = 1.1) +
        geom_line(aes(y = comgdp_pct, col = "Real Commercial GDP"), size = 1.1) +
        geom_line(aes(y = eprice_pct, col = "Real Electricity Price"), size = 1.8) +
        geom_line(aes(y = base)) +
        geom_rect(aes(xmin = as.Date("2008-01-01", format = "%Y-%m-%d"), xmax = as.Date("2009-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf), 
                  alpha = .005, col = "grey92") +
        geom_rect(aes(xmin = as.Date("2001-01-01", format = "%Y-%m-%d"), xmax = as.Date("2001-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf), 
                  alpha = .005, col = "grey92") +
        geom_rect(aes(xmin = as.Date("1992-01-01", format = "%Y-%m-%d"), xmax = as.Date("1992-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf), 
                  alpha = .005, col = "grey92") +
        scale_x_date(date_labels = ("%Y"), date_breaks = ("3 years")) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "AEP Commercial Sales, Prices and GDP Since 1992 (Index Year = 1992)", 
             x = "", y = " ", fill = "") +
        scale_colour_brewer(name = "", palette = "Spectral") +
        theme_hc() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
complot

#ggsave(complot, file = "commercial.png", scale = 1, width = 11, height = 8)

eeplot <- ggplot(residential_graph, aes(year)) +
        geom_line(aes(y = 1/use_pct, col = "Use"), size = 1.8) +
        geom_line(aes(y = 1)) +
        geom_rect(aes(xmin = as.Date("2008-01-01", format = "%Y-%m-%d"), xmax = as.Date("2009-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf),
                  alpha = .005, col = "grey92") +
        geom_rect(aes(xmin = as.Date("2001-01-01", format = "%Y-%m-%d"), xmax = as.Date("2001-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf),
                  alpha = .005, col = "grey92") +
        geom_rect(aes(xmin = as.Date("1992-01-01", format = "%Y-%m-%d"), xmax = as.Date("1992-01-01", format = "%Y-%m-%d"), ymin = -Inf, ymax = Inf),
                  alpha = .005, col = "grey92") +
        scale_x_date(date_labels = ("%Y"), date_breaks = ("3 years")) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "AEP (Inverted) Residential Usage Index (Index Year = 1992)",
             x = "", y = " ", fill = "") +
        scale_colour_brewer(name = "", palette = "Spectral") +
        theme_hc() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
eeplot
#ggsave(eeplot, file = "res invert use.png", scale = 1, width = 11, height = 8)
