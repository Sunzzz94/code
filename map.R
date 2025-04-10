rm(list = ls()) 
setwd('D:/GBD/数据/map/') 
install.packages('ggmap')
install.packages('rgdal')
install.packages('maps')
install.packages('dplyr')
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
install.packages("rgdal", dependencies=TRUE, type="source")

#######ASIR SCZ##########
EC <- read.csv('D:/GBD/数据/map/2011 各国(1).csv', header = TRUE, sep =",")
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence'&
                     EC$cause=='Schizophrenia') 
ASR_2021 <- ASR_2021[,c(2,8,9,10)]
ASR_2021$val <- round(ASR_2021$val,1)
ASR_2021$lower <- round(ASR_2021$lower,1) 
ASR_2021$upper <- round(ASR_2021$upper,1) 

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))
library(ggplot2)
library(dplyr)
library(scales)
range(ASR_2021$val)
p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(10,12,14,16,18,20,100),
                                     labels = c("10~12","12~14","14~16","16~18",
                                                "18~20", "20+"),  
                                     ude.lowest = T,right = T))
total$val2 <- as.character(total$val2)
total$val2[is.na(total$val2)] <- "NA"
colors <- c("10~12" = "#F2F1E6", "12~14" = "#F3D78A", "14~16" = "#F8984F", "16~18" = "#E66101", 
            "18~20" = "#C74647", "20+" = "#982C2C","NA" = "white")
p <- ggplot() +
  geom_polygon(data = total, 
               aes(x = long, y = lat, group = group, fill = val2),
               colour = "black", size = 0.2) + 
  scale_fill_manual(values = colors, 
                    na.value = "white", 
                    breaks = c("10~12", "12~14", "14~16", "16~18", "18~20", "20+", "NA")) +
  theme_void() +
  labs(x = "", y = "") +
  guides(fill = guide_legend(title = 'AISR(/10^5)', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小

# 显示图形
p
###ASIR BD###
EC <- read.csv('D:/GBD/数据/map/2011 各国(1).csv', header = TRUE, sep =",")
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence'&
                     EC$cause=='Bipolar disorder') 
ASR_2021 <- ASR_2021[,c(2,8,9,10)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1) 
ASR_2021$upper <- round(ASR_2021$upper,1) 

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))
library(ggplot2)
library(dplyr)
library(scales)
range(ASR_2021$val)
p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(10,25,35,40,50,70,100),
                                     labels = c("10~25","25~35","35~40","40~50",
                                                "50~70", "70+"),  
                                     include.lowest = T,right = T))

total$val2 <- as.character(total$val2)
total$val2[is.na(total$val2)] <- "NA"
colors <- c("10~25" = "#F2F1E6", "25~35" = "#F3D78A", "35~40" = "#F8984F", "40~50" = "#E66101", 
            "50~70" = "#C74647", "70+" = "#982C2C","NA" = "white")

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=0.2) + 
  scale_fill_manual(values = colors, 
                    na.value = "white", 
                    breaks = c("10~25", "25~35", "35~40", "40~50", "50~70", "70+", "NA")) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='ASIR(/10^5)', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p2 <- p2 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
### 
p2


###ASIR MDD###

EC <- read.csv('D:/GBD/数据/map/重度抑郁各国.csv', header = TRUE, sep =",")
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence'&
                     EC$cause=='DeprMajor depressive disorder')
ASR_2021 <- ASR_2021[,c(2,8,9,10)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1) 
ASR_2021$upper <- round(ASR_2021$upper,1) 

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))
library(ggplot2)
library(dplyr)
library(scales)
range(ASR_2021$val)
p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(1500,3500,5500,6500,7500,8500,10000),
                                     labels = c("1500~3500","3500~5500","5500~6500","6500~7500",
                                                "7500~8500", "8500+"),  
                                     include.lowest = T,right = T))

total$val2 <- as.character(total$val2)
total$val2[is.na(total$val2)] <- "NA"
colors <- c("1500~3500" = "#F2F1E6", "3500~5500" = "#F3D78A", "5500~6500" = "#F8984F", "6500~7500" = "#E66101", 
            "7500~8500" = "#C74647", "8500+" = "#982C2C","NA" = "white")

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=0.2) + 
  scale_fill_manual(values = colors, 
                    na.value = "white", 
                    breaks = c("1500~3500", "3500~5500", "5500~6500", "6500~7500", "7500~8500", "8500+", "NA")) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='ASIR(/10^5)', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p2 <- p2 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
### 
p2

####ASDR SCZ######
EC <- read.csv('D:/GBD/数据/map/2011 各国(1).csv', header = TRUE, sep =",")
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='DALYs (Disability-Adjusted Life Years)'&
                     EC$cause=='Schizophrenia') 
ASR_2021 <- ASR_2021[,c(2,8,9,10)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1) 
ASR_2021$upper <- round(ASR_2021$upper,1) 

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))
library(ggplot2)
library(dplyr)
library(scales)
range(ASR_2021$val)
p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(120,135,150,180,210,230,300),
                                     labels = c("120~135","135~150","150~180","180~210",
                                                "210~230", "230+"),  
                                     include.lowest = T,right = T))

total$val2 <- as.character(total$val2)
total$val2[is.na(total$val2)] <- "NA"
colors <- c("120~135" = "#F9F8CA", "135~150" = "#96D2B0", "150~180" = "#35B9C5", "180~210" = "#2E9DBE", 
            "210~230" = "#2681B6", "230+" = "#1E469B","NA" = "white")

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=0.2) + 
  scale_fill_manual(values = colors, 
                    na.value = "white", 
                    breaks = c("120~135", "135~150", "150~180", "180~210", "210~230", "230+", "NA")) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='ASDR(/10^5)', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p2 <- p2 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
### 
p2

###ASDR Bd####
EC <- read.csv('D:/GBD/数据/map/2011 各国(1).csv', header = TRUE, sep =",")
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='DALYs (Disability-Adjusted Life Years)'&
                     EC$cause=='Bipolar disorder') 
ASR_2021 <- ASR_2021[,c(2,8,9,10)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1) 
ASR_2021$upper <- round(ASR_2021$upper,1) 

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))
library(ggplot2)
library(dplyr)
library(scales)
range(ASR_2021$val)
p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(30,90,120,160,200,240,300),
                                     labels = c("30~90","90~120","120~160","160~200",
                                                "200~240", "240+"),  
                                     include.lowest = T,right = T))

total$val2 <- as.character(total$val2)
total$val2[is.na(total$val2)] <- "NA"
colors <- c("30~90" = "#F9F8CA", "90~120" = "#96D2B0", "120~160" = "#35B9C5", "160~200" = "#2E9DBE", 
            "200~240" = "#2681B6", "240+" = "#1E469B","NA" = "white")

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=0.2) + 
  scale_fill_manual(values = colors, 
                    na.value = "white", 
                    breaks = c("30~90", "90~120", "120~160", "160~200", "200~240", "240+", "NA")) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='ASDR(/10^5)', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p2 <- p2 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
### 
p2


#######ASDR MDD
EC <- read.csv('D:/GBD/数据/map/重度抑郁各国.csv', header = TRUE, sep =",")
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='DALYs (Disability-Adjusted Life Years)'&
                     EC$cause=='Major depressive disorder') 
ASR_2021 <- ASR_2021[,c(2,8,9,10)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1)
ASR_2021$upper <- round(ASR_2021$upper,1) 

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))
library(ggplot2)
library(dplyr)
library(scales)
range(ASR_2021$val)
p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(200,500,700,900,1100,1300,3000),
                                     labels = c("200~500","500~700","700~900","900~1100",
                                                "1100~1300", "1300+"),  
                                     include.lowest = T,right = T))

total$val2 <- as.character(total$val2)
total$val2[is.na(total$val2)] <- "NA"
colors <- c("200~500" = "#F9F8CA", "500~700" = "#96D2B0", "700~900" = "#35B9C5", "900~1100" = "#2E9DBE", 
            "1100~1300" = "#2681B6", "1300+" = "#1E469B","NA" = "white")

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=0.2) + 
  scale_fill_manual(values = colors, 
                    na.value = "white", 
                    breaks = c("200~500", "500~700", "700~900", "900~1100", "1100~1300", "1300+", "NA")) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='ASDR(/10^5)', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p2 <- p2 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
### 
p2

