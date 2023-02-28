
################################################
#######  SCRATCH / TEMP ##########################
#################################################





###

## geom_smooth(data=subset(df1, df1$x1<100000 & df1$x1 >0), method = "lm", color="grey", size=0.5, se=FALSE) +


###


ggplot(df1, aes(x = x1, y = y1)) +
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
  geom_point(shape=case_when(df1$var1 > 0 ~ 24,
                             df1$var1 < 0 ~ 25),
             size=2.0,
             fill = case_when(abs(y1) > threshold2 & df1$var >= 0 ~ "darkgreen",
                              abs(y1) > threshold2 & df1$var < 0 ~ "red",
                              df1$var1 > threshold1 ~ "darkgreen",
                              df1$var1 < -threshold1 ~ "red",
                              TRUE ~ "grey")) + 
  
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) + 
  labs(title=title1,subtitle=subtitle2,x=x_label2,  
       y=y_label2, caption=caption1)



###
 
geo2 <- 35620  ## "New York-Newark-Jersey City, NY-NJ-PA Metro Area"

NYC_Flow <-get_flows(
  geography = "metropolitan statistical area",
  breakdown = "AGE",
  msa = geo2,
  breakdown_labels = TRUE,
  output = "wide",
  year = 2015
) %>%
  filter (AGE_label== "75 year and over") 





write_xlsx(NYC_Flow2, "processed_data/NYC_Flow.xlsx")





str(x)
summary(x$AGE_label)
x$FULL2_NAME


##################







install.packages("mapdeck")
library(mapdeck)

token1 <- "pk.eyJ1IjoiZnJvY2t3b29kIiwiYSI6ImNsZHJtMzRoZjFvMDEzb21ka3E4YjNkODAifQ.pg_9fOSp2O3khnqH_Pwysw"


token <- token1


mapdeck(token = token1) 

## us_components <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", 
##                               product = "components")
## write_xlsx(us_components,"processed_data/us_Components.xlsx")                              


msa1 <- "New York-Newark-Jersey City, NY-NJ-PA Metro Area"
msa1 <- 35620  ## GEOID for NYC


nyc_outflow <- get_flows(
  geography = "metropolitan statistical area")



nyc_outflow <- get_flows(
  geography = "metropolitan statistical area",
  msa = msa1,
  year = 2020,
  geometry = TRUE
) %>%
  filter(variable == "MOVEDOUT") %>%
  na.omit() %>%
  arrange(desc(estimate))



nyc_outflow %>%
  slice_max(estimate, n = 5) %>%
  mutate(weight = estimate / 2500) %>%
  mapdeck(token = token1) %>%
  add_arc(origin = "centroid2",
          destination = "centroid1",
          stroke_width = "weight",
          palette = "viridis",
          update_view = FALSE) 


write_xlsx(nyc_outflow,"processed_data/nyc_outflow.xlsx") 


################


travis_inflow <- get_flows(
  geography = "cbsa",
  msa = 35620,
  year = 2019,
  geometry = "TRUE"
) %>% 
  filter(variable == "MOVEDOUT") %>%
  na.omit() %>%
  mutate(GEOID = GEOID2) %>%
  mutate(NAME = FULL2_NAME) %>%
  select (-c("GEOID1","FULL1_NAME",
             "GEOID2","FULL2_NAME",
             "centroid1")) %>%
  arrange(desc(estimate))

travis_inflow$FULL1_NAME
write_xlsx(travis_inflow, "travis_inflow.xlsx")

limit1 <- max(abs(travis_inflow$estimate)) * c(0, 1)

subtitle1 <- "temp"
ggplot() +
  geom_sf(data=travis_inflow, aes(fill= estimate)) +
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


ggplot(data=travis_inflow, aes(color=estimate))



