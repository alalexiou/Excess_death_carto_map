# LA Excess deaths per 100,000
# in April copared to the linear trend
# Ben Barr and Alexandros Alexiou @ UoL

library(data.table)
library(ggplot2)
library(scales)
library(Cairo)
CairoWin() # for antializing

### Deaths dataset

exc_deaths <- fread((normalizePath("./raw_data/excess_april_may.csv")))
names(exc_deaths)[5] <- "acm_rate"

# no crisis deaths
exc_deaths$acm_deaths_proj <- round((exc_deaths$acm_deaths - exc_deaths$ex_deaths), 1)
exc_deaths$acm_deaths_proj_rate <- round((exc_deaths$acm_deaths_proj / exc_deaths$an_pop)*100000, 1)

# round
exc_deaths$ex_deaths <- round(exc_deaths$ex_deaths, 1)
exc_deaths$acm_rate <- round(exc_deaths$acm_rate, 1)
exc_deaths$excess_rate <- round(exc_deaths$excess_rate, 1)

# all April - May
dth_both_las <- exc_deaths
dth_both_las$month <- month.abb[dth_both_las$month]
dth_both_las <- dth_both_las[dth_both_las$month == "Apr" | dth_both_las$month == "May", ]
dth_both_las <- dth_both_las[, c(1,2, 7,8,9,  3,4,5,6, 10,11)]
dth_both_las$area_code <- factor(dth_both_las$area_code)
write.csv(dth_both_las, "./outputs/all_april_may_deaths_only.csv", row.names = F)

### Death dataset April + May

exc_deaths <- fread((normalizePath("./raw_data/excess_april_may.csv")))

# note missing May data
# Aylesbury Vale E07000004
# Chiltern E07000005
# South Bucks E07000006
# Wycombe E07000007

# remove May for previous years as well, 40 rows total
temp <- exc_deaths[exc_deaths$area_code %in% c("E07000004", "E07000005", "E07000006","E07000007"),]
temp <- temp[temp$month != 5,]

exc_deaths <- exc_deaths[!exc_deaths$area_code %in% c("E07000004", "E07000005", "E07000006","E07000007"),]
exc_deaths <- rbind(exc_deaths, temp)

# all April + May
dth_all_las <- exc_deaths
dth_all_las$month <- month.abb[dth_all_las$month]
dth_all_las <- dth_all_las[dth_all_las$month == "Apr" | dth_all_las$month == "May", ]
dth_all_las <- as.data.table(aggregate(cbind(acm_deaths, ex_deaths) ~ area_code+area_name+an_pop+year, data = dth_all_las, "sum"))

dth_all_las$acm_rate <- (dth_all_las$acm_deaths / dth_all_las$an_pop)*100000
dth_all_las$excess_rate <- (dth_all_las$ex_deaths / dth_all_las$an_pop)*100000

# no crisis deaths
dth_all_las$acm_deaths_proj <- round((dth_all_las$acm_deaths - dth_all_las$ex_deaths), 1)
dth_all_las$acm_deaths_proj_rate <- round((dth_all_las$acm_deaths_proj / dth_all_las$an_pop)*100000, 1)

# round
dth_all_las$ex_deaths <- round(dth_all_las$ex_deaths, 1)
dth_all_las$acm_rate <- round(dth_all_las$acm_rate, 1)
dth_all_las$excess_rate <- round(dth_all_las$excess_rate, 1)

dth_all_las$area_code <- factor(dth_all_las$area_code)

write.csv(dth_all_las, "./outputs/all_sum_april_may_deaths.csv", row.names = F)


### Plots ###

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

mcolours <- gg_color_hue(3)
mcolours <- mcolours[c(3,1,2)]


for(i in 1:316) {
  dth_all <- dth_all_las[dth_all_las$area_code == levels(dth_all_las$area_code)[i],]  
  
  dth_all$`All-cause Mortality, April and May ` <- "Actual 2010-2019"
  
  # I need two points to make the line in the plot
  temp1 <- dth_all[dth_all$year>=2019, ]
  temp1$`All-cause Mortality, April and May ` <- "Actual 2020 (provisional)"
  
  temp2 <- dth_all[dth_all$year>=2019, ]
  temp2$`All-cause Mortality, April and May ` <- "Projected 2020 w/o COVID19 crisis"
  temp2$acm_rate[temp2$year == 2020] <- temp2$acm_deaths_proj_rate[temp2$year == 2020]
  
  dth_all <- dth_all[dth_all$year != 2020, ]
  dth_all <- rbind(dth_all, temp1, temp2)
  
  dth_all$`All-cause Mortality, April and May ` <- factor(dth_all$`All-cause Mortality, April and May `, levels = c("Actual 2010-2019", "Actual 2020 (provisional)", "Projected 2020 w/o COVID19 crisis"))

  
  
  file_plot <- ggplot(dth_all, aes(year, acm_rate, colour=`All-cause Mortality, April and May `, linetype = `All-cause Mortality, April and May `)) + #
    geom_line() + geom_point() +
    theme_light() +
    scale_x_continuous(breaks = (2010:2020), labels = 2010:2020, minor_breaks = seq(2010:2020)) + #labels = paste0("Apr-May ", 2010:2020), minor_breaks = seq(2010:2020))
    scale_y_continuous(minor_breaks = NULL) +
    scale_color_manual(values = mcolours) +
    # ylim(c(0,400)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
          legend.position="top",  legend.margin=margin(), legend.direction = "vertical",
          legend.key.size = unit(0.3, "cm"),
          # legend.spacing.x = unit(0.3, 'cm'),
          text = element_text(size=9)) +  
    labs(x = "", y = "Mortality rate per 100,000")

  ggsave(plot = file_plot, paste0("./excess_deaths_plots/", levels(dth_all_las$area_code)[i], ".png"), 
         width = 3, height = 4,
         scale = 0.8, 
         dpi = 100, 
         type = "cairo-png")
  
  print(i)
}


# Fix missing data for May plots
# Aylesbury Vale E07000004
# Chiltern E07000005
# South Bucks E07000006
# Wycombe E07000007

for(i in 56:59) {
  dth_all <- dth_all_las[dth_all_las$area_code == levels(dth_all_las$area_code)[i],]  
  
  dth_all$`All-cause Mortality, April only` <- "Actual 2010-2019"
  
  # I need two points to make the line in the plot
  temp1 <- dth_all[dth_all$year>=2019, ]
  temp1$`All-cause Mortality, April only` <- "Actual 2020 (provisional)"
  
  temp2 <- dth_all[dth_all$year>=2019, ]
  temp2$`All-cause Mortality, April only` <- "Projected 2020 w/o COVID19 crisis"
  temp2$acm_rate[temp2$year == 2020] <- temp2$acm_deaths_proj_rate[temp2$year == 2020]
  
  dth_all <- dth_all[dth_all$year != 2020, ]
  dth_all <- rbind(dth_all, temp1, temp2)
  
  dth_all$`All-cause Mortality, April only` <- factor(dth_all$`All-cause Mortality, April only`, levels = c("Actual 2010-2019", "Actual 2020 (provisional)", "Projected 2020 w/o COVID19 crisis"))
  
  
  file_plot <- ggplot(dth_all, aes(year, acm_rate, colour=`All-cause Mortality, April only`, linetype = `All-cause Mortality, April only`)) + #
    geom_line() + geom_point() +
    theme_light() +
    scale_x_continuous(breaks = (2010:2020), labels = 2010:2020, minor_breaks = seq(2010:2020)) + #labels = paste0("Apr-May ", 2010:2020), minor_breaks = seq(2010:2020))
    scale_y_continuous(minor_breaks = NULL) +
    scale_color_manual(values = mcolours) +
    # ylim(c(0,400)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
          legend.position="top",  legend.margin=margin(), legend.direction = "vertical",
          legend.key.size = unit(0.3, "cm"),
          # legend.spacing.x = unit(0.3, 'cm'),
          text = element_text(size=9)) +  
    labs(x = "", y = "Mortality rate per 100,000")
  
  ggsave(plot = file_plot, paste0("./excess_deaths_plots/missing_May/", levels(dth_all_las$area_code)[i], ".png"), 
         width = 3, height = 4,
         scale = 0.8, 
         dpi = 100, 
         type = "cairo-png")
  
  print(i)
}





### Prepare data for CARTO ###

temp1 <- dth_all_las
names(temp1)[5:10] <- c("tot_acm_deaths","tot_exc_deaths","tot_acm_rate","tot_exc_rate","tot_proj_deaths","tot_proj_rate")

temp2 <- dth_both_las[dth_both_las$month == "Apr", ]
temp2$month <- NULL
names(temp2)[5:10] <- c("apr_acm_deaths","apr_exc_deaths","apr_acm_rate","apr_exc_rate","apr_proj_deaths","apr_proj_rate")

temp3 <- dth_both_las[dth_both_las$month == "May", ]
temp3$month <- NULL
names(temp3)[5:10] <- c("may_acm_deaths","may_exc_deaths","may_acm_rate","may_exc_rate","may_proj_deaths","may_proj_rate")

dth_carto <- merge(temp1, temp2, by = c("area_code","area_name","an_pop","year"), all = T)
table(is.na(dth_carto))
dth_carto <- merge(dth_carto, temp3, by = c("area_code","area_name","an_pop","year"), all = T)
table(is.na(dth_carto))

# subset
dth_carto <- dth_carto[dth_carto$year == 2020,]
# Link to plot
dth_carto$Link <- paste0("https://raw.githubusercontent.com/alalexiou/Excess_death_carto_map/master/excess_deaths_plots/", dth_carto$area_code, ".png")

# types of variables for qgis
# write.csv(sapply(dth_carto, class), "./outputs/all_april_and_may_deaths_plot_links.csvt", row.names = F)

# NA should be "" for shapefile
dth_carto[is.na(dth_carto), ] <- ""

# Final
write.csv(dth_carto, "./outputs/all_april_and_may_deaths_plot_links.csv", row.names = F)


###
