library(readr)
library(seqinr)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(reshape2)


substrRight = function(x,n){
  substr(x,nchar(x)-n+1,nchar(x))
}

A = read_delim('https://hcup-us.ahrq.gov/toolssoftware/ccs/AppendixASingleDX.txt', skip = 2, skip_empty_rows = TRUE)

AppA = A %>% 
  # dplyr::select(2) %>% 
  # trimws()
  mutate(across(where(is.character),str_trim)) %>% 
  mutate(last = substrRight(.$`03/24/2016`,1)) 

nrow(AppA)

# Dis list
AppA_dis = AppA %>% 
  na.omit()


AppA$last = as.numeric(AppA$last)

AppA = AppA %>% 
  mutate(is.num = (nchar(AppA$last)>1))

AppA_data = AppA %>% 
  select(2,4) %>% 
  setNames(c('data','cond')) %>% 
  mutate(cond = as.character(.$cond))

data_save = data.frame(matrix(ncol = 2,nrow=0)) 

class(data_save$cond) = "character"

dis_index = 0

for(i in 1:nrow(AppA)){
  if (is.na(AppA_data[i,][[2]])){
    data_save = rbind(data_save,AppA_data[i,])
    dis_index = dis_index+1
    
  }else{
    data_save[dis_index,2]= paste(AppA_data[i,][[1]], data_save[dis_index,2])
  }
}

test = data.frame(str_split(data_save$cond, " ", simplify = TRUE))


final = cbind(data_save$data,test) %>% 
  reshape2::melt(id.var = "data_save$data") %>% 
  select(1,3) %>% 
  na.omit() %>% 
  filter(value!='NA') %>% 
  filter(value!='') %>% 
  setNames(c('diagnoses', 'code')) %>% 
  arrange(diagnoses)
  
write_csv(final,'C:/Users/fengb/OneDrive/Desktop/AppA.csv')
