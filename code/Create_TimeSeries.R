##################################
###### Time Series / Facets #######
##################################


years <- c(2010:2019,2021) # 1-yr data not available for 2020
years2 <- c(2010,2015,2020)

names(years) <- years
names(years2) <- years2
geo1 <- "state"
geo1 <- "cbsa"

survey1 <- "acs1"
survey1 <- "acs5"



## create time series dataframe 
df1_ts <- map_dfr(years, ~{
  get_acs(
    geography = geo1,
    variables = var1,
    year = .x,
    survey = survey1,
    output = "wide"
  )
}, .id = "year")

df1_ts <- df1_ts  %>%
  mutate(NET_MV75E = MVIN75E - MVOUT75E) %>%
  mutate(NET_MV75M = (MVIN75M^2+MVOUT75M^2)^(1/2)) %>%
  mutate(NET_MVE = MVINE - MVOUTE) %>%
  mutate (POP75PLUS = (POPM75E+POPM80E+POPM85E+POPF75E+POPF80E+POPF85E)) %>%
  mutate (MVNET75E = MVIN75E- MVOUT75E) %>%
  mutate (MVNET75M = (MVIN75M^2+MVOUT75M^2)^(1/2)) %>%
  mutate(NET_75MV_RATE =NET_MV75E/(POP75PLUS)) %>%
  mutate(NET_MV_RATE =NET_MVE /TOTPOPE) 


df1_ts <- df1_ts %>% 
  filter (NAME != "Puerto Rico") %>%
  filter(!str_detect(NAME, "AK")) %>% 
  filter(!str_detect(NAME, "PR")) %>%   
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  mutate (MVNETE = MVINE- MVOUTE) %>%
  mutate(MVNETM = (MVINM^2+MVOUTM^2)^(1/2)) %>%
  mutate (MVIN60_70E = MVIN60E + MVIN65E) %>%
  mutate (MVIN60_70M = (MVIN60M^2+MVIN65M^2)^(1/2)) %>%
  mutate (MVOUT60_70E = MVOUT60E + MVOUT65E) %>%
  mutate (MVOUT60_70M = (MVOUT60M^2+MVOUT65M^2)^(1/2)) %>%
  mutate (MVNET60_70E = MVIN60_70E - MVOUT60_70E) %>%
  mutate (MVNET60_70M = (MVIN60_70M^2+MVOUT60_70M^2)^(1/2)) %>%
  mutate (MVNET75E = MVIN75E- MVOUT75E) %>%
  mutate (MVNET75M = (MVIN75M^2+MVOUT75M^2)^(1/2)) %>%
  mutate (MVNETBELOW75 = MVNETE - MVNET75E) %>%
  mutate (MVINABROAD60_70E = MVINABROAD60E +MVINABROAD65E) %>%
  mutate (MVINABROAD60_70M = (MVINABROAD60M^2 +MVINABROAD65M^2)^(1/2)) %>%
  mutate (POP75PLUS = (POPM75E+POPM80E+POPM85E+POPF75E+POPF80E+POPF85E)) %>%
  mutate (MVNET75_RATE =MVNET75E/POP75PLUS) %>%
  mutate (MVNET_RATE = MVNETE /TOTPOPE) %>%
  mutate (MVNET75_RATEM = MVNET75M/POP75PLUS) %>%
  mutate (MNNET_RATEM   = MVNETM / TOTPOPE)



geo2 <- "Florida"
geo2 <- "California"
geo2 <- "New York"
geo2 <- "Arizona"
geo2 <- "Ohio"
geo2 <- "Oregon"
geo2 <- "Idaho"

geo2 <- "Phoenix-Mesa-Glendale"

## Timeseries01 - Annual Moves 
## with Error Ribbon

age2 <- "75+"
age2 <- "All Ages"
title1 <- paste ("Annual Net Domestic Migration ",age2, " | ", geo2, sep="")
title1 <- paste ("Annual Domestic Move Ins ",age2, " | ", geo2, sep="")
title1 <- paste ("Annual Domestic Move Outs ",age2, " | ", geo2, sep="")
caption1 <- "Shaded area represent margin of error around ACS estimate"


df1_ts <- df1_ts %>%
  mutate (estimate = NET_MV75E) %>%
  mutate (moe = NET_MV75M)

df1_ts <- df1_ts %>%
  mutate (estimate = MVIN75E) %>%
  mutate (moe = MVIN75M)

df1_ts <- df1_ts %>%
  mutate (estimate = MVOUT75E) %>%
  mutate (moe = MVOUT75M)

df1_ts <- df1_ts %>%
  mutate (estimate = MVNETE) %>%
  mutate (moe = MVNETM)

limit1 <- max(abs(df1_ts$estimate)+max(df1_ts$moe)) * c(-1, 1)

df2_ts <- df1_ts  %>%
  filter (NAME== geo2)

ggplot(df2_ts, aes(x = year, y = estimate, group = 1)) + 
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe), 
              fill = "navy",
              alpha = 0.4) + 
  geom_line(color = "navy") + 
  geom_point(color = "navy", size = 2) + 
  theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = comma, limits = limit1) + 
  geom_hline(yintercept = 0, color = "grey", size = 1) +
  labs(title = title1,
       x = "Year",
       y = "ACS estimate",
       caption = caption1)




## Timeseries02 - Ratio of Net Moves 
## with Error Ribbon

title1 <- paste ("Annual Net Domestic Migration Rate | ", geo2, sep="")
caption1 <- "Shaded area represent 90% margin of error around ACS estimate \n 2020 values not available."

df1_ts <- df1_ts %>%
  mutate (Age_75_Only = MVNET75_RATE) %>%
  mutate (Total_Population = MVNET_RATE) 

df2_ts <- df1_ts  %>%
  filter (NAME== geo2)

colors <- c("Age_75_Only" = "purple", "Total_Population" = "black")

ggplot(df2_ts, aes(x=year, group=1)) +                    
  geom_line(aes(y=Age_75_Only, color="Age_75_Only"), size=1) +
  geom_ribbon(aes(ymax = Age_75_Only + MVNET75_RATEM, ymin = Age_75_Only - MVNET75_RATEM), 
              fill = "purple",
              alpha = 0.3) + 
  geom_line(aes(y=Total_Population, color="Total_Population"), size=1) +
  geom_ribbon(aes(ymax = Total_Population + MNNET_RATEM, ymin = Total_Population - MNNET_RATEM), 
              fill = "grey",
              alpha = 0.6) +  
  geom_point(aes(y=Age_75_Only, color = "Age_75_Only"), size = 2) +
  geom_point(aes(y=Total_Population, color = "Total_Population"), size = 2) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) + 
  geom_hline(yintercept = 0, color = "grey", size = 1) +
  labs(title = title1,
       x = "Year",
       y = "ACS estimate",
       caption = caption1,
       color ="Legend") +
  scale_color_manual(values = colors)






write_xlsx((df1_ts %>% select(
  "NAME",
  "year",
  "MVINE",
  "MVOUTE",
  "MVIN60_70E",
  "MVOUT60_70E",
  "MVIN75E",
  "MVOUT75E",
  "MVINABROADE",
  "MVINABROAD60E",
  "MVINABROAD65E",
  "MVINABROAD75E",  
)),
"processed_data/df1_ts__.xlsx")

