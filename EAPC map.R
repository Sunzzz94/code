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
install.packages("rgdal", repos="http://R-Forge.R-project.org")

#BD ASIR

#读入xlsx文件
install.packages("openxlsx")
library(openxlsx)
EC <- read.xlsx('D:/GBD/数据/map/allEAPC.xlsx',sheet = 1)  
EAPC_2021 <- subset(EC,EC$meansure=='incidence'&EC$cause=='Bipolar disorder') 
EAPC_2021 <- EAPC_2021[,c(1,4,5,6)]
EAPC_2021$EAPC <- round(EAPC_2021$EAPC,2) 
EAPC_2021$LCI <- round(EAPC_2021$LCI,2) 
EAPC_2021$UCI <- round(EAPC_2021$UCI,2) 
#EAPC_2021 <- EAPC_2021[-1, ]
####  map for ASR
library(ggplot2)
worldData <- map_data('world')
country_asr <- EAPC_2021
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

p <- ggplot()

range(EAPC_2021$EAPC)

total <- total %>%
  mutate(val2 = cut(EAPC, 
                    breaks = c(-0.2, -0.1, -0.05, -0.01, 0.01, 0.02, 0.05, 0.1, 0.12, 2.0),
                    labels = c("-0.2~-0.1", "-0.1~-0.05", "-0.05~-0.01", "-0.01~0.01", 
                               "0.01~0.02", "0.02~0.05", "0.05~0.1", "0.1~0.12", "0.12+"),
                    include.lowest = TRUE, 
                    right = TRUE))

# 自定义颜色向量
colors <- c("-0.2~-0.1" = "#1E469B", "-0.1~-0.05" = "#2464ab", "-0.05~-0.01" = "#4792c4", 
            "-0.01~0.01" = "#ADD8E6", "0.01~0.02" = "#fddbc4", "0.02~0.05" = "#fcae91", 
            "0.05~0.1" = "#d46153", "0.1~0.12" = "#b1182d", "0.12+" = "#660020","NA" = "white")

# 绘制地图
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=.2) + 
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='EAPC', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p1 <- p1 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
# 显示图形
p1

#Depressive disorders ASIR


EC <- read.xlsx('D:/GBD/数据/map/allEAPC.xlsx',sheet = 1)  
EAPC_2021 <- subset(EC,EC$meansure=='incidence'&EC$cause=='Depressive disorders') 
EAPC_2021 <- EAPC_2021[,c(1,4,5,6)]
EAPC_2021$EAPC <- round(EAPC_2021$EAPC,2) 
EAPC_2021$LCI <- round(EAPC_2021$LCI,2) 
EAPC_2021$UCI <- round(EAPC_2021$UCI,2) 
#EAPC_2021 <- EAPC_2021[-1, ]
####  map for ASR
worldData <- map_data('world')
country_asr <- EAPC_2021
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

p <- ggplot()

range(EAPC_2021$EAPC)

total <- total %>%
  mutate(val2 = cut(EAPC, 
                    breaks = c(-2, -1, -0.5, -0.01, 0.01, 0.1, 0.5, 0.8, 1.0, 2.0),
                    labels = c("-2~-1", "-1~-0.5", "-0.5~-0.01", "-0.01~0.01", 
                               "0.01~0.1", "0.1~0.5", "0.5~0.8", "0.8~1.0", "1.0+"),
                    include.lowest = TRUE, 
                    right = TRUE))

# 自定义颜色向量
colors <- c("-2~-1" = "#1E469B", "-1~-0.5" = "#2464ab", "-0.5~-0.01" = "#4792c4", 
            "-0.01~0.01" = "#ADD8E6", "0.01~0.1" = "#fddbc4", "0.1~0.5" = "#fcae91", 
            "0.5~0.8" = "#d46153", "0.8~1.0" = "#b1182d", "1.0+" = "#660020","NA" = "white")

# 绘制地图
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=.2) + 
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='EAPC', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p1 <- p1 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
# 显示图形
p1


#SCZ ASIR
EC <- read.xlsx('D:/GBD/数据/map/allEAPC.xlsx',sheet = 1)  
EAPC_2021 <- subset(EC,EC$meansure=='incidence'&EC$cause=='Schizophrenia') 
EAPC_2021 <- EAPC_2021[,c(1,4,5,6)]
EAPC_2021$EAPC <- round(EAPC_2021$EAPC,2) 
EAPC_2021$LCI <- round(EAPC_2021$LCI,2)
EAPC_2021$UCI <- round(EAPC_2021$UCI,2)
#EAPC_2021 <- EAPC_2021[-1, ]
####  map for ASR
worldData <- map_data('world')
country_asr <- EAPC_2021
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

p <- ggplot()

range(EAPC_2021$EAPC)

total <- total %>%
  mutate(val2 = cut(EAPC, 
                    breaks = c(-0.25, -0.2, -0.1, -0.01, 0.01, 0.2, 0.5, 1.0, 1.5, 2.0),
                    labels = c("-0.25~-0.2","-0.2~-0.1", "-0.1~-0.01",  "-0.01~0.01", 
                               "0.01~0.2", "0.2~0.5", "0.5~1.0", "1.0~1.5",  "1.5+"),
                    include.lowest = TRUE, 
                    right = TRUE))

# 自定义颜色向量
colors <- c("-0.25~-0.2" = "#1E469B", "-0.2~-0.1" = "#2464ab", "-0.1~-0.01" = "#4792c4", 
            "-0.01~0.01" = "#ADD8E6", "0.01~0.2" = "#fddbc4", "0.2~0.5" = "#fcae91", 
            "0.5~1.0" = "#d46153", "1.0~1.5" = "#b1182d", "1.5+" = "#660020","NA" = "white")

# 绘制地图
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=.2) + 
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='EAPC', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p1 <- p1 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
# 显示图形
p1


#SCZ ASDR
EC <- read.xlsx('D:/GBD/数据/map/allEAPC.xlsx',sheet = 1)  
EAPC_2021 <- subset(EC,EC$meansure=='DALYs (Disability-Adjusted Life Years)'&EC$cause=='Schizophrenia') 
EAPC_2021 <- EAPC_2021[,c(1,4,5,6)]
EAPC_2021$EAPC <- round(EAPC_2021$EAPC,2) 
EAPC_2021$LCI <- round(EAPC_2021$LCI,2) 
EAPC_2021$UCI <- round(EAPC_2021$UCI,2)
#EAPC_2021 <- EAPC_2021[-1, ]
####  map for ASR
worldData <- map_data('world')
country_asr <- EAPC_2021
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

p <- ggplot()

range(EAPC_2021$EAPC)

total <- total %>%
  mutate(val2 = cut(EAPC, 
                    breaks = c(-0.4, -0.2, -0.1, -0.01, 0.01, 0.2, 0.5, 1.0, 1.5, 2.0),
                    labels = c("-0.4~-0.2","-0.2~-0.1", "-0.1~-0.01",  "-0.01~0.01", 
                               "0.01~0.2", "0.2~0.5", "0.5~1.0", "1.0~1.5",  "1.5+"),
                    include.lowest = TRUE, 
                    right = TRUE))

# 自定义颜色向量
colors <- c("-0.4~-0.2" = "#1E469B", "-0.2~-0.1" = "#2464ab", "-0.1~-0.01" = "#4792c4", 
            "-0.01~0.01" = "#ADD8E6", "0.01~0.2" = "#fddbc4", "0.2~0.5" = "#fcae91", 
            "0.5~1.0" = "#d46153", "1.0~1.5" = "#b1182d", "1.5+" = "#660020","NA" = "white")

# 绘制地图
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=.2) + 
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='EAPC', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p1 <- p1 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
# 显示图形
p1

#ASDR Depressive
EC <- read.xlsx('D:/GBD/数据/map/allEAPC.xlsx',sheet = 1)  
EAPC_2021 <- subset(EC,EC$meansure=='DALYs (Disability-Adjusted Life Years)'&EC$cause=='Depressive disorders') 
EAPC_2021 <- EAPC_2021[,c(1,4,5,6)]
EAPC_2021$EAPC <- round(EAPC_2021$EAPC,2) 
EAPC_2021$LCI <- round(EAPC_2021$LCI,2) 
EAPC_2021$UCI <- round(EAPC_2021$UCI,2) 
#EAPC_2021 <- EAPC_2021[-1, ]
####  map for ASR
worldData <- map_data('world')
country_asr <- EAPC_2021
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

p <- ggplot()

range(EAPC_2021$EAPC)

total <- total %>%
  mutate(val2 = cut(EAPC, 
                    breaks = c(-2, -1, -0.5, -0.01, 0.01, 0.1, 0.5, 0.8, 1.0, 2.0),
                    labels = c("-2~-1", "-1~-0.5", "-0.5~-0.01", "-0.01~0.01", 
                               "0.01~0.1", "0.1~0.5", "0.5~0.8", "0.8~1.0", "1.0+"),
                    include.lowest = TRUE, 
                    right = TRUE))

# 自定义颜色向量
colors <- c("-2~-1" = "#1E469B", "-1~-0.5" = "#2464ab", "-0.5~-0.01" = "#4792c4", 
            "-0.01~0.01" = "#ADD8E6", "0.01~0.1" = "#fddbc4", "0.1~0.5" = "#fcae91", 
            "0.5~0.8" = "#d46153", "0.8~1.0" = "#b1182d", "1.0+" = "#660020","NA" = "white")

# 绘制地图
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=.2) + 
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='EAPC', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p1 <- p1 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
# 显示图形
p1
#Bipolar disorder ASDR
EC <- read.xlsx('D:/GBD/数据/map/allEAPC.xlsx',sheet = 1)  
EAPC_2021 <- subset(EC,EC$meansure=='DALYs (Disability-Adjusted Life Years)'&EC$cause=='Bipolar disorder') 
EAPC_2021 <- EAPC_2021[,c(1,4,5,6)]
EAPC_2021$EAPC <- round(EAPC_2021$EAPC,2) 
EAPC_2021$LCI <- round(EAPC_2021$LCI,2) 
EAPC_2021$UCI <- round(EAPC_2021$UCI,2) 
#EAPC_2021 <- EAPC_2021[-1, ]
####  map for ASR
worldData <- map_data('world')
country_asr <- EAPC_2021
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

p <- ggplot()

range(EAPC_2021$EAPC)

total <- total %>%
  mutate(val2 = cut(EAPC, 
                    breaks = c(-0.2, -0.1, -0.05, -0.01, 0.01, 0.02, 0.05, 0.1, 0.12, 2.0),
                    labels = c("-0.2~-0.1", "-0.1~-0.05", "-0.05~-0.01", "-0.01~0.01", 
                               "0.01~0.02", "0.02~0.05", "0.05~0.1", "0.1~0.12", "0.12+"),
                    include.lowest = TRUE, 
                    right = TRUE))

# 自定义颜色向量
colors <- c("-0.2~-0.1" = "#1E469B", "-0.1~-0.05" = "#2464ab", "-0.05~-0.01" = "#4792c4", 
            "-0.01~0.01" = "#ADD8E6", "0.01~0.02" = "#fddbc4", "0.02~0.05" = "#fcae91", 
            "0.05~0.1" = "#d46153", "0.1~0.12" = "#b1182d", "0.12+" = "#660020","NA" = "white")

# 绘制地图
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=val2),
                       colour="black", size=.2) + 
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(x="", y="") +
  guides(fill = guide_legend(title='EAPC', 
                             keyheight = unit(0.5, "cm"), 
                             keywidth = unit(0.5, "cm"))) +
  theme(legend.position = 'right',
        legend.text = element_text(size = 8),  # 调整图例文本大小
        legend.title = element_text(size = 10, face = "bold")) # 调整图例标题大小
p1 <- p1 + theme(legend.margin = margin(r = 0.5, unit = "cm"))
# 显示图形
p1
