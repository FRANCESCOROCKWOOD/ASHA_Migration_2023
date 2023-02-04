######################################
#### Set Master Variables ############
######################################

rm(list=ls())

geo1 <- "state"  ## "state" or "cbsa"
survey1 <- "acs1"  ## "acs1" or "acs5"
year1 <- 2021  ## 2021 for 1-yr, 2019 for state 5-yr (2015-2019), 2020 for 5-yr CBSA data
caption1 <- "Source: ACS Data Tables via the tidycensus R package" 

popvar <-  c(TOTPOP        = "B01001_001",
             POPM40        = "B01001_014",
             POPM45        = "B01001_015",          
             POPM50        = "B01001_016",
             POPM55        = "B01001_017",          
             POPM60        = "B01001_018",   
             POPM62        = "B01001_019",      
             POPM65        = "B01001_020",  
             POPM67        = "B01001_021",          
             POPM70        = "B01001_022",             
             POPM75        = "B01001_023",
             POPM80        = "B01001_024",          
             POPM85        = "B01001_025",
             POPF40        = "B01001_038",
             POPF45        = "B01001_039",          
             POPF50        = "B01001_040",
             POPF55        = "B01001_041",          
             POPF60        = "B01001_042",   
             POPF62        = "B01001_043",      
             POPF65        = "B01001_044",  
             POPF67        = "B01001_045",          
             POPF70        = "B01001_046",             
             POPF75        = "B01001_047",
             POPF80        = "B01001_048",          
             POPF85        = "B01001_049"         
)

mvvar <-  c(MVIN      = "B07001_065",
            MVIN40    = "B07001_073",
            MVIN45    = "B07001_074",
            MVIN50    = "B07001_075",
            MVIN55    = "B07001_076",
            MVIN60    = "B07001_077",
            MVIN65    = "B07001_078",
            MVIN70    = "B07001_079",
            MVIN75    = "B07001_080",    
            MVOUT     = "B07401_065",
            MVOUT40   = "B07401_073",
            MVOUT45   = "B07401_074",
            MVOUT50   = "B07401_075",
            MVOUT55   = "B07401_076",
            MVOUT60   = "B07401_077",
            MVOUT65   = "B07401_078",
            MVOUT70   = "B07401_079",
            MVOUT75   = "B07401_080")


mvvari <- c(MVINABROAD40    = "B07001_089",
            MVINABROAD45    = "B07001_090",
            MVINABROAD50    = "B07001_091",
            MVINABROAD55    = "B07001_092",
            MVINABROAD60    = "B07001_093",
            MVINABROAD65    = "B07001_094",
            MVINABROAD70    = "B07001_095",
            MVINABROAD75    = "B07001_096")

var1 <- c(popvar, mvvar, mvvari)


df1 <- get_acs(
  geography = geo1, 
  variables = var1,
  survey = survey1,
  year = year1,
  output= "wide",
  geometry = TRUE,
  resolution = "20m") %>%
  shift_geometry()

df1 <- df1 %>% 
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
  mutate (POP75PLUS = (POPM75E+POPM80E+POPM85E+POPF75E+POPF80E+POPF85E))


state_outline <- get_acs(
  geography = "state", 
  variables = c(TOTPOP ="B01001_001"),
  survey = survey1,
  year = year1,
  output= "wide",
  geometry = TRUE,
  resolution = "20m") %>%
  shift_geometry()

state_outline <- state_outline %>% 
  filter(!str_detect(NAME, "Alaska")) %>% 
  filter(!str_detect(NAME, "Puerto Rico")) 
