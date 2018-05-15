############################DOWNLOAD CENSUS DATA###########################################################
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidycensus,sf,tidyverse,psych,rgdal,tigris,sqldf,devtools,tmap,tmaptools,corrplot)

#install_github("jamgreen/lehdr")
#library(lehdr)
options(tigris_class = "sf",tigris_use_cache = T)
options(stringsAsFactors = F)
options(row.names=F)
options(dplyr.width = Inf)
census_api_key("e4d87ec7a5aa0680df4d1e7e6805cf4220357c06", install = T)

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
                                
                                tract_temp<-tract %>% mutate(county=substr(GEOID,1,5))%>% rename(MHI ="MHI_est") %>% filter(county=='41051')
                                
                                
                                
                                  
                                  tm_shape(tract_temp) +
                                  tm_polygons("P_WNH_est",breaks=c(0,50,75,90,100),palette="-Oranges",
                                  title="Share of the White;\nNot Hispanic Population")+ 
                                  
                                  tm_shape(tract_temp) +
                                  tm_dots("MHI",palette="-Purples", breaks=c(0,35000,50000,100000,200000),size=8)+
                                  tm_legend(position=c("right","top"),title.size=10,legend.text.size=8,scale=.1,frame=T)
                                  
                                              

                                


#########################################################################################################
######################################### TRANSFORM TO Z SCORES #########################################
#########################################################################################################

var2<-data1[c(-2)]
var2<-var2[c(1,2,23,3,24,4,25,5,26,6,27,7,28,8,29,9,30,10,31,11,32,12,33,13,34,14,35,15,36,16,37,17,38,18,39,19,40,20,41,21,42,22,43)]
var_set<- var2

#Create indicator fields and new MOEs
var_set<-var_set%>% mutate(mort=mort_est,
                           mort30=mort30_est,
                           mort35=mort35_est,
                           obdn=rowSums(.[c(6,8,14,16)]),
                           obdn_moe=sqrt(mort30_moe^2 + mort35_moe^2 + nomort30_moe^2 + nomort35_moe^2),
                           ownest=rowSums(.[c(4,12)]),
                           ownest_moe=sqrt(mort_moe^2 + nomort_moe^2),
                           P_obdn=if_else(ownest==0,0,round(100*(obdn/ownest),1)),
                           P_obdn_moe=if_else(ownest==0,0,round(100*moe_prop(obdn,ownest,obdn_moe,ownest_moe),1)),
                           MHI=MHI_est,
                           nocar=nocar_est,
                           P_5 =P_5_est, 
                           P_65=P_65_est,
                           Pop_dep=rowSums(.[c(18,20)]),
                           P_dep=round((rowSums(.[c(18,20)])/Total_pop_est)*100,1),
                           P_BA=P_BA_est,
                           P_dis=P_dis_est,
                           P_HS=P_HS_est,
                           P_nfuel=P_nfuel_est,
                           P_rent30=P_rent30_est,
                           P_rent35=P_rent35_est,
                           P_rbdn=rowSums(.[c(30,32)]),
                           P_rbdn_moe=round(sqrt(P_rent30_moe^2 + P_rent35_moe^2),1),
                           P_SSI=P_SSI_est,
                           P_WNH=P_WNH_est,
                           rent=rent_est,
                           unemp=unemp_est,
                           Pop_dep_moe=sqrt(P_5_moe^2 + P_65_moe^2),
                           Total_pop=Total_pop_est,
                           P_dep_moe=round(100*moe_prop(Pop_dep,Total_pop,Pop_dep_moe,Total_pop_moe),1))%>% 
  
  dplyr::select(GEOID,Total_pop,P_dep,P_WNH,P_HS,P_BA,P_obdn,P_rbdn,P_dis,P_nfuel,P_SSI,MHI,unemp,rent,nocar, P_dep_moe,P_WNH_moe, 
                P_HS_moe,P_BA_moe,P_obdn_moe,P_rbdn_moe,P_dis_moe,P_nfuel_moe,P_SSI_moe,MHI_moe,unemp_moe,rent_moe,nocar_moe)


var_set<-var_set %>% mutate(
  P_dep_cv= round(100*(P_dep_moe/1.645)/P_dep,1),
  P_WNH_cv= round(100*(P_WNH_moe/1.645)/P_WNH,1),
  P_HS_cv= round(100*(P_HS_moe/1.645)/P_HS,1),
  P_BA_cv= round(100*(P_BA_moe/1.645)/P_BA,1),
  P_obdn_cv= round(100*(P_obdn_moe/1.645)/P_obdn,1),
  P_rbdn_cv= round(100*(P_rbdn_moe/1.645)/P_rbdn,1),
  P_dis_cv= round(100*(P_dis_moe/1.645)/P_dis,1),
  P_nfuel_cv= round(100*(P_nfuel_moe/1.645)/P_nfuel,1),
  P_SSI_cv= round(100*(P_SSI_moe/1.645)/P_SSI,1),
  MHI_cv= round(100*(MHI_moe/1.645)/MHI,1),
  unemp_cv= round(100*(unemp_moe/1.645)/unemp,1),
  rent_cv= round(100*(rent_moe/1.645)/rent,1),
  nocar_cv= round(100*(nocar_moe/1.645)/nocar),1)

data<- var_set[c(1:15)]

#Correlation test for raw estimates to measure collinearity 
cor(data[-1:-2],use = "complete.obs")

##################Check distribution and apply appropriate normalization method #############################
data %>% ggplot(aes(P_WNH^3)) + geom_density()
data %>% ggplot(aes(nocar^(1/4))) + geom_density()
data %>% ggplot(aes(P_HS^6)) + geom_density()
data %>% ggplot(aes((sqrt(P_dep)))) + geom_density()
data %>% ggplot(aes(P_nfuel^(1/6))) + geom_density()
data %>% ggplot(aes(P_obdn^(1/2))) + geom_density()
data %>% ggplot(aes(P_rbdn)) + geom_density()
data %>% ggplot(aes(P_SSI^(1/2))) + geom_density()
data %>% ggplot(aes(P_dis^(1/4))) + geom_density()

#Square root & Cube values to normalize their distribution (bell curve) for the z-score conversion
data2<- data%>% group_by(GEOID) %>% dplyr::select(P_dep,MHI,P_BA,unemp,rent,P_obdn,P_SSI)%>% summarise_all(sqrt)
data2<-data2 %>% setNames(c(names(.)[1],paste0(names(.)[-1],"_rt")))
data3<-data%>% group_by(GEOID)%>% summarise(P_WNH= P_WNH^3,nocar=nocar^(1/4),P_HS= P_HS^6,P_nfuel=P_nfuel^(1/6),P_dis=P_dis^(1/4),P_rbdn=P_rbdn) %>% 
  dplyr::rename(P_WNH_ex= P_WNH,
                nocar_rt=nocar,
                P_HS_ex=P_HS,
                P_nfuel_rt=P_nfuel,
                P_dis_rt=P_dis,
                P_rbdn_norm=P_rbdn)

data<-left_join(data,data2, by= "GEOID")
data<-left_join(data,data3, by= "GEOID")
rm(data2,data3)
names(data)

# Create fields that transform the squared values into z-scores which represents 
#the standardized deviation from the mean
data_output<- data %>% mutate(
  P_dep_z = (P_dep_rt - mean(P_dep_rt, na.rm=T))/sd(P_dep_rt,na.rm=T),
  P_HS_z = -(P_HS_ex - mean(P_HS_ex, na.rm=T))/sd(P_HS_ex,na.rm=T),
  P_BA_z= -(P_BA_rt - mean(P_BA_rt, na.rm=T))/sd(P_BA_rt,na.rm=T),
  MHI_z = -(MHI_rt - mean(MHI_rt, na.rm=T))/sd(MHI_rt,na.rm=T),
  P_WNH_z= -(P_WNH_ex - mean(P_WNH_ex, na.rm=T))/sd(P_WNH_ex,na.rm=T),
  unemp_z= (unemp_rt - mean(unemp_rt, na.rm=T))/sd(unemp_rt,na.rm=T),
  rent_z=(rent_rt - mean(rent_rt, na.rm=T))/sd(rent_rt,na.rm=T),
  nocar_z= (nocar_rt - mean(nocar_rt, na.rm=T))/sd(nocar_rt,na.rm=T),
  P_nfuel_z= (P_nfuel_rt-mean(P_nfuel_rt, na.rm=T))/sd(P_nfuel_rt,na.rm=T),
  P_dis_z= (P_dis_rt-mean(P_dis_rt, na.rm=T))/sd(P_dis_rt,na.rm=T),
  P_SSI_z= (P_SSI_rt-mean(P_SSI_rt, na.rm=T))/sd(P_SSI_rt,na.rm=T),
  P_rbdn_z= (P_rbdn_norm-mean(P_rbdn_norm, na.rm=T))/sd(P_rbdn_norm,na.rm=T),
  P_obdn_z= (P_obdn_rt-mean(P_obdn_rt, na.rm=T))/sd(P_obdn_rt,na.rm=T))

########################################################################################################
########################################## COMPOSITE VARIABLE ##########################################
########################################################################################################

#Create composite variable which averages the z-sscores. Represents a standardized measurement of vulnerability...
#in relation to the tracts as a whole: P_dep_z,P_HS_z,P_BA_z,MHI_z,P_WNH_z,unemp_z,rent_z,nocar_z

# I'd recommend using P_dep_z, P_BA_z, P_WNH_z, unemp_z, nocar_z, P_dis_z, P_rbdn_z and P_obdn_z for the composite. 
#P_HS is non-normally distributed and heavily correlates with P_BA
#MHI_z is strongly correlated with everything so just remove that
#rent_z is strongly correlated with no_car_z, I think the latter is a more valid for vulnerability.
#P_nfuel_z is non-normally distributed.
#P_SSI and no car are too unreliable (cv >40).


                                                              cor(data_output[c(29:41)],use = "complete.obs")
                                                              
                                                              var_set%>%select(ends_with("cv"))%>%filter(!is.infinite(nocar_cv),!is.infinite(P_rbdn_cv),
                                                                                                         !is.infinite(P_SSI_cv))%>% summarise_all(mean, na.rm=T)
                                                              
                                                              composite_list<- c("P_dep_z", "P_BA_z","P_WNH_z","unemp_z",
                                                                                 "P_dis_z","P_obdn_z","P_rbdn_z")
# Check collinearity
corrplot(data_output %>% select(composite_list) %>% cor(.,use="complete.obs"),
         type="upper",order="hclust",tl.col="black")

composite<-data_output%>% dplyr::select(composite_list)%>% 
  mutate(Vul= rowMeans(.,na.rm=T))%>% mutate(Vul_P= pnorm(Vul)) %>% dplyr::select(Vul, Vul_P)

##########################################################################################################################33

data_output<-left_join(data_output,var_set[c(1,16:42)],by= "GEOID")
data_output<-cbind(data_output,composite)
data_output$GEOID<- as.character(data_output$GEOID)
data_output<-data_output %>% select(-68)

#write.csv(data_output, "ouput.csv")



                                                              tract<- tract%>% left_join(.,data_output, by = c("GEOID")) %>% filter(estimate>0)
                                                              tract<-tract%>% select(-ends_with("_ex"),-ends_with("rt"),-ends_with("norm"),-ends_with("moe"))
                                                              
                                                              
                                                              tract %>% ggplot(aes(Vul)) + geom_density()
                                                              tm_shape(tract)+ tm_polygons("Vul_P", title="Social Vulnerability Index")




tract<-tract%>% select(1,2,4,5,6,32,19,7,33,23,8,34,20,9,35,21,10,36,31,
                       11,37,30,12,38,28,13,39,27,14,40,29,15,41,22,16,42,
                       24,17,43,25,18,44,26,45,46)


writeOGR(as(tract,"Spatial"),dsn=".","tracts_msa",driver="ESRI Shapefile", overwrite_layer = T)
#Export to Node
tract_node<- as.data.frame(tract)%>% select(1,2,4,5:10,14:25,35:37,44,45)
write.csv(tract_node,"socvul_msa.csv",row.names = F)
