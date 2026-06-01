#water quality from wetland sites

library(tidyverse)
library(cder)
library(dataRetrieval)

Meins = cdec_query("MEI", sensors = c(266, #FDOM
                                      25, #water temperature in F
                                      62, #pH
                                      221, #turbidity FNU
                                      28, #Chlorophyll
                                      61, #dissolved oxygen in mg/l
                                      275, #phycocyanin flouresecence
                                      100), #EC in uS/cm
                   start.date = ymd("2026-04-01"), end.date = today()) %>%
  mutate(Value = case_when(SensorType == "TEMP W" ~ (Value-32)*5/9,
                           TRUE ~ Value))

ggplot(Meins, aes(x = ObsDate, y = Value)) + geom_line()+
  facet_wrap(~SensorType, scales = "free_y")

#query data from USGS
USGSstations = read_waterdata_continuous(monitoring_location_id = c("USGS-11455730", "USGS-381126121554801", "USGS-381154121545401"),
                          
                          time = c("2026-01-01", "2026-05-21"))

#reformat USGS data so it works with CDEC data
USGS = USGSstations %>%
  rename(StationID = monitoring_location_id, SensorNumber = parameter_code, DateTime = time, Value = value,
         SensorUnits = unit_of_measure) %>%
  mutate(SensorType = case_when(SensorUnits == "%" ~ "DO Percent",
                                SensorUnits== "pH Units" ~ "PH VAL",
                                SensorUnits== "mg/l" ~ "DIS OXY",
                                SensorUnits== "uS/cm" ~ "EL COND",
                                SensorUnits== "ug/l" ~ "CHLORPH",
                                SensorUnits== "degC" ~ "TEMP W",
                                SensorUnits== "QSE" ~ "FDOM",
                                SensorUnits== "RFU" ~ "FLURFUB",
                                SensorUnits== "_FNU" ~ "TURB WF"),
         Station = case_match(StationID, "USGS-11455730" ~ "Lower Joice Island",
                              "USGS-381126121554801" ~ "Nurse Slough",
                              "USGS-381154121545401" ~ "Denverton"))

#combine data
allWQ = bind_rows(USGS, mutate(Meins, Station = "Meins Interior", SensorNumber = as.character(SensorNumber)))

# Plot it
ggplot(allWQ, aes(x = DateTime, y = Value, color = Station)) + geom_line()+
  facet_wrap(~SensorType, scales = "free_y")
