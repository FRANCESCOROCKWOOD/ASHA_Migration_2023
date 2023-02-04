
#################################################
############# Plots #############################
##############################################


## Plot_AgeCohort_[geography]_[age1]_[age2]_[survey]
## Example: "Plot_CBSA_[In]_75_ALL_5Yr"

df1 <- df1 %>% 
  mutate(NAME = str_remove(NAME, "-.*$"))

title1 <- "Net Migration Coorelation - CBSA"
title1 <- "Net Migration Coorelation - States"
title1 <- "Moving In from Different State"
title1 <- "Moving Out to Different State"

title1 <- "Moving In from Different CBSA"
title1 <- "Moving Out to Different CBSA"


subtitle2 <- "75+ v. All Ages | 2016-2020"
subtitle2 <- "60-70 v. All Ages | 2016-2020"
subtitle2 <- "75+ v. All Ages | 2015-2019"
subtitle2 <- "60-70 v. All Ages | 2015-2019"
subtitle2 <- "60-70 v. All Ages | 2021"
subtitle2 <- "75+ v. All Ages | 2021"

x_label2 <- "All Ages"
y_label2 <- "Ages 60-70"
y_label2 <- "Ages 75+"

x1 = df1$MVNETE
x1 = df1$MVINE
x1 = df1$MVOUTE

y1 = df1$MVNET60_70E
y1 = df1$MVNET75E

y1 = df1$MVIN75E
y1 = df1$MVOUT75E
y1 = df1$MVIN60E
y1 = df1$MVOUT60E 


# Predicts the future values
model <- lm(y1~x1, data=df1)
df1$predict1 <- predict(model, df1)
df1 <- df1 %>%
  mutate(var1 = y1- predict1)
summary(model)

threshold1 <- 2500  ## applied to variance
## 1500 for States/75/1YR
## 1250 for States/75/5Yr
## 3500 for States/60/1Yr
## 5000 for States/60/5Yr
## 9999 for CBSA/75
## 3000 for CBSA/60

threshold2 <- 20000  ## applied to net migration 
## 3000  for States/75/1Yr
## 2500  for States/75/5Yr
## 15000 for States/60/1Yr
##  8000 for States/60/5Yr
##  1800 for CBSA/75
##  5000 for CBSA/65

ggplot(df1, aes(x = x1, y = y1)) +
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "seagreen4", alpha=0.2)+ 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "firebrick1", alpha=0.1) + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "tomato", alpha =0.1) + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "darkolivegreen1", alpha=0.2) +
  geom_text_repel(aes(label=case_when(abs(y1) > threshold2 ~ NAME,
                                      abs(var1) > threshold1 ~ NAME,
                                      TRUE ~ "")),
                  min.segment.length = 0, seed= 22, box.padding =0.6, 
                  max.overlaps = 1000,
                  point.padding = 0.4,
  ) +
  geom_point(size=2.0, color = case_when(abs(y1) > threshold2 & df1$var >= 0 ~ "darkgreen",
                                         abs(y1) > threshold2 & df1$var < 0 ~ "red",
                                         df1$var1 > threshold1 ~ "darkgreen",
                                         df1$var1 < -threshold1 ~ "red",
                                         TRUE ~ "grey")) + 
  geom_smooth(method = "lm", color="grey", size=0.5, se=FALSE) + 
  
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) + 
  labs(title=title1,subtitle=subtitle2,x=x_label2,  
       y=y_label2, caption=caption1)
