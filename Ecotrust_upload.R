############################DOWNLOAD CENSUS DATA###########################################################
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidycensus,sf,tidyverse,psych,rgdal,tigris,sqldf,devtools,tmap,tmaptools,corrplot)

options(tigris_class = "sf",tigris_use_cache = T)
options(stringsAsFactors = F)
options(row.names=F)
options(dplyr.width = Inf)
census_api_key("e4d87ec7a5aa0680df4d1e7e6805cf4220357c06", install = T)

setwd("C:\\Users\\Nick Chun\\Desktop\\EcoTrust")
acs5<-load_variables(2015,"acs5",cache= T)
#acs1<-load_variables(2015,"acs1",cache= T)
#sf1<-load_variables(2010,"sf1",cache= F)
acs5_profile<-load_variables(2015,"acs5/profile",cache=T)
#####View variable list for 5yr & 1yr ACS, use search and filter functions
#View(acs5)
#View(acs1)
#View(sf1)
#View(acs5_profile)

##################################### SET YOUR PARAMETERS ################################################

                              
                              table<- c("DP05_0001E",
                                        "DP03_0062E",
                                        "DP05_0072PE",
                                        "DP05_0004E",
                                        "DP05_0021E",
                                        "DP03_0009PE",
                                        "DP04_0058PE",
                                        "DP04_0047PE",
                                        "DP02_0066PE",
                                        "DP02_0067PE",
                                        "DP02_0071PE",
                                        "DP03_0070PE",
                                        "DP04_0071PE",
                                        "DP04_0110E",
                                        "DP04_0114E",
                                        "DP04_0115E",
                                        "DP04_0117E",
                                        "DP04_0123E",
                                        "DP04_0124E",
                                        "DP04_0141PE",
                                        "DP04_0142PE")
                              
                              year<- 2016
                              span<-5
                              # acs5, acs1, sf1, sf3,acs5/profile
                              dataset<- "acs5"
                              
                              ###########View fips codes if you want to filter on your geography (geo) by a specific state or county (NULL is default)                                  
                              #View(fips_codes)
                              
                              geo<- "tract"
                              state<- c("OR","WA")
                              county<- NULL
                              filter<- c("41005","41009","41051","41067","41071","53011","53059")
                              
###########View fips codes if you want to filter on your geography (geo) by a specific state or county (NULL is default)                                  

# Creating Sensible Column Names
#Create Fieldnames
vars <- acs5_profile %>% filter(grepl(paste(table, collapse = "|"), name)) 
vars1 <- vars$name

vars_name<-as.data.frame(vars1) %>% filter(!grepl("M",vars1))
vars_name$vars1<- as.character(vars_name$vars1)
colnames(vars_name)<- "Table_Number"
vars_name$Table_Number<- as.character(vars_name$Table_Number)
vars_name$Table_Number<-substr(vars_name[,'Table_Number'],1,nchar(vars_name[,'Table_Number'])-1)

label_name<-vars$label
label_name<-unique(label_name)
label_name<-as.data.frame(label_name)
colnames(label_name)[1]<-"Variable"
name<-cbind(vars_name,label_name)

name_long<- name
name_long$Variable <-c("P_HS","P_BA","P_dis","unemp","MHI","P_SSI","rent" ,"nocar",
                       "P_nfuel","mort","mort30","mort35","nomort","nomort30",
                       "nomort35","P_rent30","P_rent35","Total_pop","P_5","P_65","P_WNH")

name$Variable<-c("P_HS","P_BA","P_dis","unemp","MHI","P_SSI","rent" ,"nocar",
                 "P_nfuel","mort","mort30","mort35","nomort","nomort30",
                 "nomort35","P_rent30","P_rent35","Total_pop","P_5","P_65","P_WNH")

###Data Fetch WIDE -> For GIS-----
                                data1<-get_acs(geography = geo,variables = vars_name$Table_Number,year= year, output= "tidy" ,
                                               state=state,county=county, survey = dataset, geometry = F )    
                                data1<-data1%>% filter(grepl(paste(filter,collapse = "|"),GEOID))


#Update fieldnames for long form
data1<-left_join(data1,name,by=c("variable"="Table_Number"))
data1<-data1%>% dplyr::select(GEOID,NAME,Variable,estimate,moe)

###transform into long form----
datawide_est1<-data1[-ncol(data1)] %>% spread(Variable, estimate)
datawide_moe1<-data1[-ncol(data1)+1] %>% spread(Variable, moe)
data1<-left_join(datawide_est1,datawide_moe1[-1],suffix=c("_est","_moe"), by= c("NAME","NAME"))


                                tract<- get_acs(geography=geo, variables="B01003_001", year=year,state= state,county = county, geometry=T)
                                tract<-tract %>% filter(grepl(paste(filter,collapse = "|"),GEOID)) %>% left_join(data1,by="GEOID")
                                
                                tract_temp<-tract %>% mutate(county=substr(GEOID,1,5))%>% rename(MHI ="MHI_est") %>%
                                  filter(grepl("41051|41067|41005",county))
                                
                                
                                
                                  ##### Demo Map----
                                  x<-tm_shape(tract_temp) +
                                  tm_polygons("P_WNH_est",breaks=c(0,50,75,90,100),palette="-Oranges", border.col = "gray40",
                                  title="Share of the White;\nNot Hispanic Population")+ 
                                  
                                  tm_shape(tract_temp) +
                                  tm_dots("MHI",palette="-Purples", breaks=c(0,35000,50000,100000,200000),size=.1,border.col="gray40")+
                                  tm_legend(title.size=1,legend.text.size=.7,scale=1)+
                                    tm_scale_bar(position=c(.2,.01),size=.6)+
                                    tm_credits("Data from the 2016 5yr ACS dataset\n@ United States Census Bureau",size=.8,
                                               position=c(.7,.01))+
                                    tm_compass(position=c(.95,.9),size=1,fontsize=1)+
                                   tm_layout("Share of the White; Not Hispanic & MHI:\nTri-County Area",
                                             title.position = c(.45,.9),title.size =1.3,
                                             legend.position = c("LEFT","BOTTOM"))
  
                                  x
                                  
                                  save_tmap(x,"WNH_MHI.png",scale=1,height=6)
                                      
                                              

#Export layers----

writeOGR(as(tract,"Spatial"),dsn=".","tracts_msa",driver="ESRI Shapefile", overwrite_layer = T)
#Export to Node
tract_csv<- as.data.frame(tract)%>% select(-geometry)
write.csv(tract_csv,"tracts_msa.csv",row.names = F)
