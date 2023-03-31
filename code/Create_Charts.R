############################################
###### CHARTS ##############################
############################################

subtitle1 <- "2021 1-Year ACS Estimates with 90% Margin of Error"
subtitle1 <- "2015-2019 5-Year ACS Estimates with 90% Margin of Error"
subtitle1 <- "2016-2020 5-Year ACS Estimates with 90% Margin of Error"

x_label1 <- "Number of Persons Moving in Past Year "


## Move Ins Chart
## Chart_MoveIn_[geography]_[age]_[survey]
## Example: "Chart_MoveIn_State_75_1Yr

title1 <- "Persons Aged 60-70 Moving In From a Different State"
title1 <- "75+ Persons Moving In From a Different State"
title1 <- "75+ Persons Moving In From a Different CBSA"

df1$estimate <- df1$MVIN60_70E
df1$estimate <- df1$MVIN75E
df1$moe <- df1$MVIN60_70M
df1$moe <- df1$MVIN75M

df1chart <- df1 %>%
  slice_max(estimate, n = 15) %>%
  select(NAME, estimate, moe) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe))+
  geom_point(size=3, color = "darkgreen") +  
  theme_minimal(base_size = 12) + 
  scale_x_continuous(labels=comma)+
  labs(title = title1, 
       subtitle = subtitle1, 
       y = "", 
       x = x_label1, 
       caption = caption1) 
plot(df1chart) 



## Move Outs Chart 
## Chart_MoveOut_[geography]_[age]_[survey]
## Example: "Chart_MoveOut_State_75_1Yr"

title1 <- "Persons Aged 60-70 Moving Out to a Different State"
title1 <- "75+ Persons Moving Out to a Different State"
title1 <- "75+ Persons Moving Out From a Different CBSA"
df1$estimate <- df1$MVOUT60_70E
df1$estimate <- df1$MVOUT75E
df1$moe <- df1$MVOUT60_70M
df1$moe <- df1$MVOUT75M

df1chart <- df1 %>%
  slice_max(estimate, n = 15) %>%
  select(NAME, estimate, moe) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe))+
  geom_point(size=3, color = "darkred") +  
  theme_minimal(base_size = 12) + 
  scale_x_continuous(labels=comma)+
  labs(title = title1, 
       subtitle = subtitle1, 
       y = "", 
       x = x_label1, 
       caption = caption1) 
plot(df1chart) 


## Net Moves Chart 
## Chart_NetMoves_[slice]_[geography]_[age]_[survey]
## Example: "Chart_NetMoves_MAX_State_75_1Yr"

title1 <- "Net Mitration of Persons Aged 60-70 by State"
title1 <- "Net Migration of Persons 75+ by State"
title1 <- "Net Migration of Persons 75+ by CBSA"

df1$estimate <- df1$MVNET60_70E
df1$estimate <- df1$MVNET75E
df1$moe <- df1$MVNET60_70M
df1$moe <- df1$MVNET75M

df1chart <- df1 %>%
  slice_min(estimate, n = 10) %>%  ## slice_max or slice_min
  select(NAME, estimate, moe) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe))+
  geom_point(size=3, color = "purple") +  
  theme_minimal(base_size = 12) + 
  scale_x_continuous(labels=comma)+
  labs(title = title1, 
       subtitle = subtitle1, 
       y = "", 
       x = x_label1, 
       caption = caption1) 
plot(df1chart)

figure05<-df1chart
figure06<-df1chart


## International Move In Chart 
## Chart_iMoveIn_[geography]_[age]_[survey]
## Example: "Chart_iMoveIn_State_75_1Yr"


title1 <- "Persons Aged 60-70 Moving In From Abroad"
title1 <- "75+ Persons Moving In From Abroad"
df1$estimate <- df1$MVINABROAD60_70E
df1$estimate <- df1$MVINABROAD75E
df1$moe <- df1$MVINABROAD60_70M
df1$moe <- df1$MVINABROAD75M

df1chart <- df1 %>%
  slice_max(estimate, n = 15) %>%
  select(NAME, estimate, moe) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe))+
  geom_point(size=3, color = "darkgreen") +  
  theme_minimal(base_size = 12) + 
  scale_x_continuous(labels=comma)+
  labs(title = title1, 
       subtitle = subtitle1, 
       y = "", 
       x = x_label1, 
       caption = caption1) 
plot(df1chart)


## Flow Chart
## Flow_MovedIn_[geography]_[age]_[survey]
## Example: "Flow_MovedIn_CBSA_75_2015


title1 <- "75+ Persons Moving From NYC Metro"
subtitle1 <- "2011-2015 5-Year ACS Estimate"

Flow$estimate <- Flow$MOVEDIN
Flow$moe <- Flow$MOVEDIN_M

df1chart <- Flow %>%
  slice_max(estimate, n = 20) %>%
  select(FULL2_NAME, estimate, moe) %>%
  ggplot(aes(y = reorder(FULL2_NAME, estimate), x = estimate)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe))+
  geom_point(size=3, color = "darkgreen") +  
  theme_minimal(base_size = 12) + 
  scale_x_continuous(labels=comma)+
  labs(title = title1, 
       subtitle = subtitle1, 
       y = "", 
       x = x_label1, 
       caption = caption1) 
plot(df1chart) 


