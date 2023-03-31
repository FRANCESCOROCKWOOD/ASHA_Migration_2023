#################################################
############# Plots #############################
##############################################


## Plot_AgeCohort_[geography]_[age1]_[age2]_[survey]
## Example: "Plot_CBSA_[In]_75_ALL_5Yr"

df1 <- df1 %>% 
  mutate(NAME = str_remove(NAME, "-.*$"))  ## shorten name of metros 


## Assign values to variable below based on current run ##

title1 <- "Net Migration Coorelation - CBSA"
title1 <- "Net Migration Coorelation - States"
title1 <- "Moving In from Different State"
title1 <- "Moving Out to Different State"

title1 <- "Moving In from Different CBSA"
title1 <- "Moving Out to Different CBSA"


subtitle2 <- "75+ v. All Ages | 2016-2020" ## most current CBSA
subtitle2 <- "60-70 v. All Ages | 2016-2020"
subtitle2 <- "75+ v. All Ages | 2015-2019"
subtitle2 <- "60-70 v. All Ages | 2015-2019"
subtitle2 <- "60-70 v. All Ages | 2021"
subtitle2 <- "75+ v. All Ages | 2021"
subtitle2 <- "45-60 v. All Ages | 2021"

x_label2 <- "All Ages"
y_label2 <- "Ages 45-60"
y_label2 <- "Ages 60-70"
y_label2 <- "Ages 75+"


x1 = df1$MVNETE  ## net migration for all ages
x1 = df1$MVINE   ## in-migration for all ages
x1 = df1$MVOUTE  ## out-migration for all ages

y1 = df1$MVNET60_70E
y1 = df1$MVNET45_60E
y1 = df1$MVNET75E      ## net migration for 75+
y1 = df1$MVIN75E
y1 = df1$MVOUT75E
y1 = df1$MVIN60E
y1 = df1$MVOUT60E 


df1 <- df1 %>%
  mutate(x1 = x1) %>%
  mutate(y1 = y1)


# Predicts the future values
model <- lm(y1~x1, data=df1)
df1$predict1 <- predict(model, df1)
df1 <- df1 %>%
  mutate(var1 = y1- predict1) 
summary(model)

model$coef

df1 <- df1 %>%
  mutate(color1 = case_when(df1$var1 >= 0 ~ "olivedrab4",
                              df1$var1 < 0 ~ "orange2"))

threshold1 <- 1500  ## applied to variance
## 1500 for States/75/1YR
## 1250 for States/75/5Yr
## 3500 for States/60/1Yr
## 5000 for States/60/5Yr
##  800 for CBSA/75
## 3000 for CBSA/60

threshold2 <- 3000  ## applied to net migration value 
##  3000  for States/75/1Yr
## 2500  for States/75/5Yr
## 15000 for States/60/1Yr
##  8000 for States/60/5Yr
##  1800 for CBSA/75
##  5000 for CBSA/65

plot1 <- ggplot(df1, aes(x = x1, y = y1)) +
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "seagreen4", alpha=0.2)+ 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "firebrick1", alpha=0.1) + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "grey", alpha =0.1) + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "grey", alpha=0.2) +
  geom_smooth(method = "lm", color="grey", size=0.5, se=FALSE) + 
  geom_vline(xintercept = 3, linetype="dotted", color = "darkgreen", size=0.75) +
  geom_hline(yintercept = 3, linetype="dotted", color = "darkgreen", size=0.75) +
  geom_text_repel(aes(label=case_when(abs(y1) > threshold2 ~ NAME,
                                      abs(var1) > threshold1 ~ NAME,
                                      TRUE ~ "")),
                  min.segment.length = 0, seed= 22, box.padding =0.6, 
                  max.overlaps = 1000,
                  point.padding = 0.4,
  ) +
  geom_point(size=2.0,
             color = case_when(abs(y1) > threshold2 & df1$var >= 0 ~ df1$color1,
                                         abs(y1) > threshold2 & df1$var < 0 ~ df1$color1,
                                         df1$var1 > threshold1 ~ df1$color1,
                                         df1$var1 < -threshold1 ~ df1$color1,
                                         TRUE ~ "grey")) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) + 
  labs(title=title1,subtitle=subtitle2,x=x_label2,  
       y=y_label2, caption=caption1)

plot1


### Q1 - Upper right quadrant ##
## Plot_AgeCohort_[geography]_[age1]_[age2]_[survey]_[Quardrant]
## Example: "Plot_CBSA_[In]_75_ALL_5Yr_Q1"


threshold1 <- 250
threshold2 <- 0


## Filters for Upper Right Quadrant (Figure 11)
df2 <- df1
df2 <- filter(df1, df1$x1>0)
df2 <- filter(df2, df2$y1>0)
df2 <- filter(df2, df2$x1<100000)
fitline_limit1 <- 100000


## Filters for Lower Left Quadrant (Figure 12)
df2 <- df1
df2 <- filter(df1, df1$x1<0)
df2 <- filter(df2, df2$y1<0)
df2 <- filter(df2, df2$x1>-100000)

## Filters for Upper Left Quadrant (Figure 13)
df2 <- df1
df2 <- filter(df1, df1$x1<0)
df2 <- filter(df2, df2$y1>0)

## Filters for Lower Right Quadrant (Figure 14)
df2 <- df1
df2 <- filter(df1, df1$x1>0)
df2 <- filter(df2, df2$y1<0)

plot1 <- ggplot(df2, aes(x = x1, y = y1)) +
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "seagreen4", alpha=0.2)+ 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "firebrick1", alpha=0.1) + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "grey", alpha =0.1) + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "grey", alpha=0.2) +
  geom_vline(xintercept = 3, linetype="dotted", color = "darkgreen", size=0.75) +
  geom_hline(yintercept = 3, linetype="dotted", color = "darkgreen", size=0.75) +
  geom_text_repel(aes(label=NAME),
                  min.segment.length = 0, seed= 22, box.padding =0.6, 
                  max.overlaps = 1000,
                  point.padding = 0.4,
  ) +
  geom_point(size=2.0,
             color = case_when(abs(df2$var1) > threshold1 ~ df2$color1,
                               TRUE ~ "grey")) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) + 
  labs(title=title1,subtitle=subtitle2,x=x_label2,  
       y=y_label2, caption=caption1) +
  geom_abline(intercept = model$coef[1], slope= model$coef[2], color="grey", 
                   linetype="solid", size=0.5)

plot1
figure14 <-plot1




