
################################################
#######  SCRATCH / TEMP ##########################
#################################################


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



