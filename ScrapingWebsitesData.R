#Cleans everything (any previously datasets, functions created,..)
rm(list = ls())
pkgs <- c('lubridate', "rvest", "readr","sjmisc","dplyr","stringr","qdapRegex","gsubfn","tidyverse", "httr", "jsonlite","readxl","yesno")


for (i in pkgs) {
  if (!i %in% installed.packages()) {
    install.packages(i)
  }
  library(i, character.only = TRUE)
}


folder<-dirname(rstudioapi::getSourceEditorContext()$path)
if (str_contains(folder,'/WebScraping')){
  folder<-dirname(folder)
} else {
folder<-dirname(dirname(folder)) # move two dirs above HIM
#condition if executed from CntrImport.R, move it one more time up
if(str_contains(folder,"/HIM")){
  folder<-dirname(folder) 
}
}

folder<-paste0(folder,'/OutWebsitesData')
if(!dir.exists(paste0(folder))){
  dir.create(paste0(folder)) #create folder for raw files
}

functions_folder<-paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/DataCountries')
#source other functions
functions <- list.files(functions_folder, pattern = ".R$", recursive = TRUE)
for (i in 1:length(functions)) {
  source(paste0(functions_folder,'/',functions[i]))
}


#Temporary on hold
data_yesterday<-read.csv(paste0(dirname(folder),'/',format(Sys.Date()-1,'%y%m%d'),'_qry_COVID_running_cases_country_date.CSV')) %>%
  mutate(DateReport1=as.Date(parse_date_time(DateReport1, c("dmy","ymd","mdy")))) %>% 
  filter(DateReport1==Sys.Date()-1) %>% 
  mutate(ADM0NAME=str_to_title(ADM0NAME))

#Switch true= verified countries, false= all countries
exp_ready = TRUE

if(exp_ready == TRUE){
  
  #Important to keep same spelling as in function name (one country = one word)
  listcountries<-c('Georgia',
                   'Russia',
                   'Serbia',
                   'Slovakia',
                   'Kyrgyzstan',
                   'Ukraine',
                   'France',
                   'Denmark',
                   'Moldova',
                   'Romania',
                   'Portugal',
                   'North Macedonia',
                   'Croatia',
                   'Faroe',
                   'Gibraltar'
                   
                   )
}else{
  #Important to keep same spelling as in function name (one country = one word)
  listcountries<-gsub('\\.R','',functions)
}


#General function that gets data but handles errors if needed
getdata <- function(ctr) {
  out <- tryCatch(
    { print(paste('Getting data for ',ctr))
      suppressWarnings(get(paste0('Data_',ctr))())
    },
    error=function(cond) {
      message(paste("Could not read data for ",ctr,'. Please contact Celine or Roman and take data manually'))
      return(data.frame(ADM0NAME=ctr,
                        TotalCases=NA,
                        NewCases=NA,
                        TotalDeaths=NA,
                        NewDeaths=NA,
                        NewTests=NA,
                        TotalTests=NA,
                        Hosp_occ=NA,
                        ICU_occ=NA))
    },
    finally={
      print('Over')
    }
  )    
  return(out)
}

web_scrap<-data.frame()
for (ctry in listcountries){
  web_scrap_<-getdata(ctry)
  web_scrap<-bind_rows(web_scrap,web_scrap_)
}

#read reference Cntry_web_ref.csv
cntry_ref <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Cntry_web_ref.csv'))
cntry_ref <- cntry_ref %>%
  mutate(country=str_to_title(ADM0NAME))
#%>% mutate(country=if_else(country=="Czech Republic","CzechRepublic",country)) 

web_scrap_epi<- cntry_ref %>% 
  left_join(web_scrap,by=c('country'='ADM0NAME')) %>% 
  mutate(DateReport1=Sys.Date()) %>% 
  mutate(ImportFlag=if_else(!is.na(TotalCases) | !is.na(TotalDeaths) ,3,1))%>%
  mutate(CasesTotal=if_else(!is.na(TotalCases),as.character(TotalCases),"")) %>%
  mutate(DeathsTotal=if_else(!is.na(TotalDeaths),as.character(TotalDeaths),""))%>%
  select(DateReport1,country,CasesTotal,DeathsTotal,ImportFlag,NewCases,NewDeaths)

cntry_ref_severity <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Cntry_web_ref_severity.csv'),fileEncoding="UTF-8-BOM")
cntry_ref_severity <- cntry_ref_severity %>%
  mutate(country=str_to_title(ADM0NAME))

web_scrap_severity<- cntry_ref_severity %>% 
  left_join(web_scrap,by=c('country'='ADM0NAME')) %>% 
  mutate(DateReport1=Sys.Date()) %>% 
  select(country,DateReport1,NewTests,Hosp_occ,ICU_occ) %>% 
  replace(is.na(.), '')

ComparingData<-function(ctr,Variable){
  data_yesterday_ctr<-data_yesterday %>% 
    filter(ADM0NAME==ctr)
  data_today_ctr<-web_scrap %>% 
    filter(ADM0NAME==ctr)
  Variable<-all_of(Variable)
  Variable_Today<-(data_today_ctr %>%  select(Variable))[1,1]
  Variable_Yesterday<-(data_yesterday_ctr %>%  select(Variable))[1,1]
  if(!is.na(Variable_Today)){
    if(Variable_Today==Variable_Yesterday)
    {message(paste0(ctr,': ',Variable,' is equal to yesterday. Website probably not yet updated. Please update manually later'))}
    if(Variable=='TotalDeaths' | Variable=='TotalCases'){
      if(Variable_Today<Variable_Yesterday)
      {message(paste0(ctr,': ',Variable,' is lower than yesterday. Not normal. Please investigate'))}
    }
    }
}

QuickValidation<-function(ctr){
  TotalCasesToday<-(web_scrap %>% filter(ADM0NAME==ctr) %>% select(TotalCases))[1,1]
  NewCasesToday<-(web_scrap %>% 
    filter(ADM0NAME==ctr) %>% select(NewCases))[1,1]
  TotalCasesYesterday<-(data_yesterday %>% filter(ADM0NAME==ctr) %>% select(TotalCases))[1,1]
  if(!is.na(NewCasesToday) & !is.na(TotalCasesToday) & (TotalCasesToday!=TotalCasesYesterday)){
    if(TotalCasesToday!=TotalCasesYesterday+NewCasesToday){
      message(paste0(ctr,': Total Cases (',TotalCasesToday,') for today is not equal to Total Cases Yesterday (',
                     TotalCasesYesterday,') + New Cases Today (',NewCasesToday,'). Please investigate'))    }
  }
  
  TotalDeathsToday<-(web_scrap %>% filter(ADM0NAME==ctr) %>% select(TotalDeaths))[1,1]
  NewDeathsToday<-(web_scrap %>% 
                    filter(ADM0NAME==ctr) %>% select(NewDeaths))[1,1]
  TotalDeathsYesterday<-(data_yesterday %>% filter(ADM0NAME==ctr) %>% select(TotalDeaths))[1,1]
  if(!is.na(NewDeathsToday)&!is.na(TotalDeathsToday) & (TotalDeathsToday!=TotalDeathsYesterday)){
    if(TotalDeathsToday!=TotalDeathsYesterday+NewDeathsToday){
      message(paste0(ctr,': Total Deaths (',TotalDeathsToday,') for today is not equal to Total Deaths Yesterday (',
                     TotalDeathsYesterday,') + New Deaths Today (',NewDeathsToday,'). Please investigate'))
    }
  }
}

#Uncomment to try this functionnality
listcountries_<-str_replace_all(
  listcountries,
  c("Russia" = "Russian Federation",
    "CzechRepublic" = 'Czech Republic',
    "Faroe" = 'Faroe Islands'))

for (ctr in listcountries_){
  ComparingData(ctr,'NewCases')
  ComparingData(ctr,'TotalCases')
  ComparingData(ctr,'NewDeaths')
  ComparingData(ctr,'TotalDeaths')
  QuickValidation(ctr)
}


write.csv(web_scrap_epi,paste0(folder,'/WebScraping_Epi_',format(Sys.time(), "%Y-%m-%d"),'.csv'),row.names=FALSE)
write.csv(web_scrap_severity,paste0(folder,'/WebScraping_Severity_',format(Sys.time(), "%Y-%m-%d"),'.csv'),row.names=FALSE)






