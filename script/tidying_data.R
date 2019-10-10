library(tidyverse)

S_Cobar<-read_csv("data/NSWSouthCobar_Entity-WaterSample.csv")
Geochem1<- read_csv("data/NSWGeochemistry1_Entity-WaterSample.csv")
Sample_repAug2019<- read_csv("data/CSIRO sample report Aug 2019cp.csv")
Alks_Aug2019<-read_csv("data/GS New South Wales and Mt. Isa ALKS 05082019cp.csv")
Asu<-read_csv("data/asu19100cp.csv")


#work on datasheet Entity-watersample


Geochem1_tidy<-Geochem1 %>%
  select(7:9,12,14,17,20:27,42:45) %>%                               # xtract columns of interest
  unite(1,2,3,4, col= "SampleID", sep="-")%>%                  # combine column to get sampleID
  mutate("SampleName"=toupper(SiteName))%>%                    # homogenise samplename to capital
  separate_rows(SampleName, sep=" ")  %>%                      # ????? need to split row into 2 when duplicate sampling
  filter(str_detect(SampleName,"MX")|is.na(SampleName)) %>%    # need to delete row that do not correspond to samples
  rename(m_asl=ElevationMetresAboveSeaLevel)%>%                 #rename column for clarity
  mutate("StationDeposit"=toupper(StationDeposit)) %>% 
  #mutate(WaterTable_depth_m = sub("[^0-9]*([0-9\\.]*).*","\\1",Minex_S_Cobar$WaterTable))
  select(16,1,2,5:10,12:15)
  
  
view(S_Cobar)

S_Cobar_tidy<-S_Cobar %>% 
  select(7:9,12,14,17,20:27,42:45) %>%                               # xtract columns of interest
  unite(1,2,3,4, col= "SampleID", sep="-")%>%                  # combine column to get sampleID
  mutate("SampleName"=toupper(SiteName))%>%                    # homogenise samplename to capital
  separate_rows(SampleName, sep=" ")  %>%                      # ????? need to split row into 2 when duplicate sampling
  filter(str_detect(SampleName,"MX")|is.na(SampleName)) %>%    # need to delete row that do not correspond to samples
  rename(m_asl=ElevationMetresAboveSeaLevel)%>%                 #rename column for clarity
  mutate("StationDeposit"=toupper(StationDeposit)) %>% 
  #mutate(WaterTable_depth_m = sub("[^0-9]*([0-9\\.]*).*","\\1",Minex_S_Cobar$WaterTable))
  select(16,1,2,5:10,12:15)


# separate(WaterTable, c("depth_water_table","Obs_water_table"), sep= " ")                                                             # change fro scientific writing to regular numeric 
  
  #Watertable homogenise to number and split into other column for accuracy that is defined by either a space or not separated only followed by ?
  
  
  
  
                                                

