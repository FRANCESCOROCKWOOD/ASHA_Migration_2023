############################################
###### Maps ###############################
###########################################

subtitle1 <- "2021 1-Year ACS Estimates"  ## for states
subtitle1 <- "2015-2019 5-Year ACS Estimates"  ## for states pre-pandemic
subtitle1 <- "2016-2020 5-Year ACS Estimates" ## for CBSA


fill1 <- "ACS Estimate  "

## Maps - Total Population  
## Map_POP_[geography]_[age]_[survey]
## Example: "Map_POP_State_75_1Yr"

title1 <- "Population by State | All Ages"
title1 <- "Population by CBSA | All Ages"



## Maps: Total Population 

df1$estimate <- df1$TOTPOPE
df1$moe <- df1$TOTPOPM

limit1 <- max(abs(df1$estimate)) * c(0, 1)

ggplot() +
  geom_sf(data=state_outline, fill=NA, color="darkgreen") +
  geom_sf(data=df1, aes(fill=estimate)) +
  scale_fill_distiller(palette = "Greys", 
                       type="div",
                       limit = limit1,
                       direction = 1,
                       guide = "colourbar",
                       label = comma) + 
  labs(title = title1,
       subtitle = subtitle1,
       caption = caption1,
       fill = fill1) + 
  theme_void()


## Maps - Move In / Move Out / Net Moves
## Map_[direction]_[geography]_[age]_[survey]
##    [direction] | MoveIn, MoveOut, NetMoves
## Example: "Map_MoveIn_State_75_1Yr"

palette1 <- "Greens"
title1 <- "75+ Persons Moving In From a Different State"
title1 <- "Persons Aged 60-70 Moving In From a Different State"
title1 <- "75+ Persons Moving In From a Different CBSA"
df1$estimate <- df1$MVIN60_70E
df1$estimate <- df1$MVIN75E
df1$moe <- df1$MVIN60_70M
df1$moe <- df1$MVIN75M

palette1 <- "Reds"
title1 <- "75+ Persons Moving to a Different State"
title1 <- "Persons Aged 60-70 Moving to a Different State"
title1 <- "75+ Persons Moving Out to a Different CBSA"
df1$estimate <- df1$MVOUT60_70E
df1$estimate <- df1$MVOUT75E
df1$moe <- df1$MVOUT60_70M
df1$moe <- df1$MVOUT75M

palette1 <- "RdYlGn"
title1 <- "Net Migration of Persons 75+ by State"
title1 <- "Net Mitration of Persons Aged 60-70 by State"
title1 <- "Net Migration of Persons 75+ by CBSA"
title1 <- "Net Migration of Persons 75+ by CBSA - Excluding NYC Metro"


df1$estimate <- df1$MVNET60_70E
df1$estimate <- df1$MVNET75E
df1$moe <- df1$MVNET60_70M
df1$moe <- df1$MVNET75M

limit1 <- max(abs(df1$estimate)) * c(0, 1)  ## for in and out migration figures
limit1 <- max(abs(df1$estimate)) * c(-1, 1) ## for net figures

## df1 <- df1 %>%
## filter(estimate >= -4000)

map1 <- ggplot() +
  geom_sf(data=df1, aes(fill= estimate)) +
  geom_sf(data=state_outline, fill=NA, color="sienna4") +
  scale_fill_distiller(palette = palette1, 
                       type="div",
                       limit = limit1,
                       direction = 1,
                       guide = "colourbar",
                       label = comma) + 
  labs(title = title1,
       subtitle = subtitle1,
       caption = caption1,
       fill = fill1) + 
  theme_void()

map1
figure09<-map1
figure09






## Maps - International Move Ins 
## Map_iMoveIne_[geography]_[age]_[survey]
## Example: "Map_iMoveIn_State_75_1Yr"

title1 <- "75+ Persons Moving In From Abroad"
title1 <- "Persons Aged 60-70 Moving In From Abroad"

df1$estimate <- df1$MVINABROAD60_70E
df1$estimate <- df1$MVINABROAD75E

df1$moe <- df1$MVINABROAD60_70M
df1$moe <- df1$MVINABROAD75M

limit1 <- max(abs(df1$estimate)) * c(-1, 1)
ggplot(data=df1, aes(fill= estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "RdYlGn", 
                       type="div",
                       limit = limit1,
                       direction = 1,
                       guide = "colourbar",
                       label = comma) + 
  labs(title = title1,
       subtitle = subtitle1,
       caption = caption1,
       fill = fill1) + 
  theme_void()


