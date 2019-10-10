library(tidyverse)

S_Cobar<-read_csv("data/NSWSouthCobar_Entity-WaterSample.csv")
Geochem1<- read_csv("data/NSWGeochemistry1_Entity-WaterSample.csv")
Sample_repAug2019<- read_csv("data/CSIRO sample report Aug 2019cp.csv")
Alks_Aug2019<-read_csv("data/GS New South Wales and Mt. Isa ALKS 05082019cp.csv")
Asu<-read_csv("data/asu19100cp.csv")


#work on datasheet Entity-watersample


Minex_S_Cobar<-Geochem1 %>%
  select(7:9,12,14,17,20:26) %>%                          #extract columns of interest
  unite(1,2,3,4, col= "SampleID", sep="-")%>%             #combine column to get sampleID
  mutate("SampleName"=toupper(SiteName))%>%               # homogenise samplename to capital
                                                          # ????? need to split row into 2 when duplicate sampling
  rename(m_asl=ElevationMetresAboveSeaLevel) %>%          #rename column for clarity
                                                          # change fro scientific writing to regular numeric 
  
  #Watertable homogenise to number and split into other column for accuracy that is defined by either a space or not separated only followed by ?
  
  