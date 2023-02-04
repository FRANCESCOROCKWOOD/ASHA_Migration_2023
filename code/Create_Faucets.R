## Create Faucets ##
## This section need work ##



## create dataframe/sf with annual 1-year ACS values
joined_states <- df1 %>%
  select(NAME) %>%
  left_join(df1_ts, by = c("NAME"="NAME")) %>%
  mutate(percent = MVNETE) %>%
  select (NAME, year, GEOID, MVNETE, MVNET75E, MVNET_RATE) %>%
  mutate (RATIO = MVNET_RATE) 

joined_states <- within(joined_states, RATIO <- replace(RATIO, (RATIO < -1.0), -1))
joined_states <- within(joined_states, RATIO <- replace(RATIO, (RATIO > 1.0), 1))

## create dataframe/sf with five-year ACS values 
joined_states2 <- df1 %>%
  select(NAME) %>%
  left_join(NET_MV_S5, by = c("NAME"="NAME")) %>%
  mutate(percent = NET_MVE) %>%
  select (NAME, year, GEOID, NET_MVE, NET_MV75E, NET_MV_RATIO) %>%
  mutate (RATIO = NET_MV_RATIO) 


joined_states2 <- within(joined_states2, RATIO <- replace(RATIO, (RATIO < -1.0), -1))
joined_states2 <- within(joined_states2, RATIO <- replace(RATIO, (RATIO > 1.0), 1))


## USFacet01 | Maps of Net Moves by State - All Ages | 2010-2021 ##
limit <- max(abs(joined_states$MVNETE)) * c(-1, 1)
tm_shape(joined_states) + 
  tm_facets(by = "year", scale.factor = 4) + 
  tm_fill(col = "MVNETE",
          style = "quantile",
          n = 6,
          palette = "RdYlGn",
          limit = limit,
          title = "Net Moves - All Ages ",) + 
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.6, 0.01),
            panel.label.bg.color = "white")


## USFacet02 |Map of Net Moves by State - 75+ | 2010-2021 ##
limit <- max(abs(joined_states$NET_MV75E)) * c(-1, 1)
tm_shape(joined_states) + 
  tm_facets(by = "year", scale.factor = 4) + 
  tm_fill(col = "NET_MV75E",
          style = "quantile",
          n = 6,
          palette = "RdYlGn",
          limit = limit,
          title = "Net Moves - 75+ ",) + 
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.6, 0.01),
            panel.label.bg.color = "white")



## USFacet03 | Map of Net Moves by State - Ratio of 75+ to Total POP | 5-year intervals ##


tm_shape(joined_states2) + 
  tm_facets(by = "year", scale.factor = 4) + 
  tm_fill(col = "RATIO",
          style = "quantile",
          n = 6,
          palette = "RdYlGn",
          title = "5-year Intervals \nNet Moves \nRatio of 75+ to Total Popultion ",) + 
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.9, 0.15
            ),
            panel.label.bg.color = "white")


