library(tidyverse)

S_Cobar<-read_csv("data/NSWSouthCobar_Entity-WaterSample.csv")
Geochem1<- read_csv("data/NSWGeochemistry1_Entity-WaterSample.csv")

Sample_repAug2019<- read_csv("data/CSIRO sample report Aug 2019cp.csv", skip=11)
  
Alks_Aug2019<-read_csv("data/GS New South Wales and Mt. Isa ALKS 05082019cp.csv")

Asu<-read_csv("data/asu19100cp.csv", skip = 11)
fixnamesAsu<-Asu 
  fixnamesAsu<- paste(colnames(fixnamesAsu),fixnamesAsu[1,])
Asu<- read_csv("data/asu19100cp.csv", skip = 14,col_names = fixnamesAsu)

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
  mutate(Conductivity= as.numeric(Conductivity))%>%
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
  
  Obs_Field_NSW<-full_join(Geochem1_tidy, S_Cobar_tidy)  
    # put in long format and replace NA by unknown
   Obs_Field_NSW_long<- gather(Obs_Field_NSW, 'Latitude', 'Longitude', 'Northing', 'Easting', 'Temperature', 'Conductivity', 'PreferredPH', 'PreferredEh', key ="Measurement", value="Value") %>% 
     select(SampleName, StationDeposit, Measurement, Value) %>% 
     mutate(SampleName = replace_na(SampleName,"unknown")) %>%
     mutate(StationDeposit = replace_na(StationDeposit,"unknown")) 
  
 Alks_tidy<-Alks_Aug2019 %>%
   rename(SampleName = 'SAMPLE ID.') %>% 
   mutate("SampleName"=toupper(SampleName))
   

 Alks_std<- Alks_tidy %>%                                         
filter( str_starts(SampleName, pattern = "STD"))

 Alks_tidy<-Alks_tidy %>% 
   select(SampleName, Alkalinity, `volume (mL)`, pH ) %>% 
   filter(str_starts (SampleName, "STD", negate = TRUE)) %>% 
   mutate(Alkalinity = as.numeric(Alkalinity)) 
 
 #put in long format
   Alks_tidy_long<-gather(Alks_tidy,'Alkalinity','pH', key = "Measurement", value= "Value") %>% 
     select(1,3,4)
 
 
# work on asu 
 asu_tidy<-Asu %>% 
   select(2,3,5:12,14:18,20:37,39:50) %>% 
   rename( SampleID_AN = 'X2 Sample ID', SampleID_CAT = 'X3 NA') 
   
#tidying asu
 asu_tidy_zero <- asu_tidy %>%
   # Make long format (columns: SampleID_AN, SampleID_CAT, component, reading)
   gather(Measurement, Value, -SampleID_AN, -SampleID_CAT) %>% 
   separate(Measurement, into = c("Measurement", "Units"), sep = " ") %>% 
   #If below detection limit ("<0.X"), replace with 0, otherwise convert character value to numeric
   mutate(reading = ifelse(str_detect(Value, "<0"), 0, as.numeric(Value))) %>% 
   filter( !is.na(Value) ) %>%  # discard missing readings 
   mutate(SampleID_AN = replace_na(SampleID_AN,"unknown")) %>% 
   mutate(SampleID_CAT = replace_na(SampleID_CAT, "unknown")) %>% 
   unite(SampleID_AN, SampleID_CAT, col = "SampleName1") %>% 
   mutate(AN = stringr::str_detect(SampleName1, "AN")) %>%
   mutate(CAT=stringr::str_detect(SampleName1,"CAT")) %>% 
   mutate(AN = ifelse(AN == TRUE, "Y", "N")) %>% 
   mutate(CAT = ifelse(CAT == TRUE, "Y", "N")) %>% 
   separate(SampleName1, into = c("SampleName1", "SampleName2"), sep = "_") %>% 
   separate(SampleName1, into = c("SampleName1", "extra1"), sep = " ") %>% 
   separate(SampleName2, into = c("SampleName2", "extra2"), sep = " ") %>% 
   select(-extra1, -extra2) %>% 
   mutate(SampleName1 = na_if(SampleName1, "unknown")) %>% 
   mutate(SampleName2 = na_if(SampleName2, "unknown")) %>% 
   mutate(SampleName = ifelse(is.na(SampleName1), SampleName2, SampleName1)) %>% 
   select(SampleName, AN, CAT, Measurement, Units, Value )
  
 
  #mutate(SampleName3 = replace_na(SampleName1, 'SampleName2')) 
   #I am trying to create a column "SampleName3" that would replace NAs in SAmpleName1 by the values in SampleName2, and replace NAs in SampleName2 by values in SampleName 1. 
   # by doin this I could then have a clean dataset that can be merged with others.
   
   
   #mutate(SampleName1, replace(SampleName1, str_detect(SampleName1, "unknown"), 'SampleName2')) %>% 
   #mutate(SampleName2, replace(SampleName2, str_detect(SampleName2, "unknown"), 'SampleName1'))
 
   #I need to create column SampleName that only contains the "MX" type sample get rid of Unknown
   #then hopefully merge will work 
   
   
   #Suggestion from Peter, but it does not work when uniting again see what trick I can do?
   #mutate(SampleName1 = na_if(SampleName1, "unknown")) %>% 
   #mutate(SampleName2 = na_if(SampleName2, "unknown")) %>% 
   
   
  
 #create asu dataframe with standard values
 asu_std<- asu_tidy_zero %>% 
   filter(str_detect(SampleName, "MX", negate = TRUE))  
  
 
 #tidying asu for later merging of dataframe
   asu_for_merging<- asu_tidy_zero %>% 
     filter(str_detect(SampleName, "MX", negate = FALSE)) %>% 
     mutate(Value = as.numeric(Value))
     
     #mutate(SampleName = )
   
 # filter(stringr::str_detect())
  #ifelse(substr(SampleID_AN, 1,2) == "MX")
   
   #separate(SampleID_AN, into = c("SampleID", "SampleType"), sep = " ")


 #work on the samples from the August 2019 report
 RepAug_lab<-Sample_repAug2019 %>% 
   select(1,2,6:9)
  RepAug_tidy<-Sample_repAug2019 %>% 
   select(2:4) %>% 
      rename(SampleName= 'Client name', d18O_smow='?18O VSMOW',d2H_smow= '?D VSMOW')
  #make long format 
  RepAug_tidy_long<-RepAug_tidy %>% 
   gather('d18O_smow', 'd2H_smow', key= "Measurement", value= "Value") %>% 
   filter(!is.na(SampleName)) %>% 
    separate(Measurement, into =c("Measurement","Units"), sep = "_") %>% 
    separate(SampleName, into = c("SampleName", "SampleType"), sep = " ") %>% 
    mutate(AN = stringr::str_detect(SampleType, "AN")) %>%
    mutate(CAT=stringr::str_detect(SampleType,"CAT")) %>% 
    mutate(AN = ifelse(AN == TRUE, "Y", "N")) %>% 
    mutate(CAT = ifelse(CAT == TRUE, "Y", "N")) %>% 
    select(SampleName, AN, CAT, Measurement, Units, Value )


  
  # combine all tidied dataframes into one
  
  NSW_Hydro<- full_join(RepAug_tidy_long, asu_for_merging  , by = NULL) %>% 
    separate(SampleName, into = c("extra", "SampleName"), sep = "_") %>% 
    
  