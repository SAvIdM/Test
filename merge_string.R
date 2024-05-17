merge_string<-function(merged,firstperiod,secondperiod,stringcut){ 
  
  inh1 <- paste0("inh_",firstperiod)
  inh1 <- get(inh1)
  inh2 <- paste0("inh_",secondperiod)
  inh2 <- get(inh2)
  
  df <-merged %>%
    select(id_inh.x,id_inh.y,year.x,year.y)
  total<-merged
  
  
  #1919
  unmatched_1<-inh1%>%
    anti_join(df, join_by("id_inh" == "id_inh.x"))
  
  #1924 unmatched
  unmatched_2<-inh2%>%
    anti_join(df, join_by("id_inh" == "id_inh.y"))
  
  
  
  ### Gen cutoff:
  #1919:
  #Corrections: First name
  unmatched_1$first_name<-sub("F\\. X\\.","Franz Xaver",unmatched_1$first_name) #+33
  unmatched_1$first_name<-sub("Frz\\.","Franz",unmatched_1$first_name) #+33
  unmatched_1$first_name<-sub("Frdr\\.","Friedrich",unmatched_1$first_name)#+4
  unmatched_1$first_name<-sub("Xav\\.","Xaver",unmatched_1$first_name)
  unmatched_1$first_name<-sub("P\\.?","Peter",unmatched_1$first_name)#+12
  unmatched_1$first_name<-sub("Gg\\.?","Georg",unmatched_1$first_name)#+22
  unmatched_1$first_name<-sub("Ldw\\.?","Ludwig",unmatched_1$first_name)#+1 (1579)
  unmatched_1$first_name<-sub("Xav\\.","Xaver",unmatched_1$first_name)#+2
  unmatched_1$first_name<-sub("X\\.","Xaver",unmatched_1$first_name)#+2
  unmatched_1$first_name<-sub("Fr\\.","Franz",unmatched_1$first_name)#+6
  unmatched_1$first_name<-sub("Ad\\.","Adam",unmatched_1$first_name)
  unmatched_1$first_name<-sub("F\\.","Franz",unmatched_1$first_name)
  
  #Corrections: Last name
  unmatched_1$last_name<-sub("v\\.","von",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Mair","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Mayr","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Meyr","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Meir","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Meier","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Meyer","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Mayer","Maier",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Schmid","Schmidt",unmatched_1$last_name)
  unmatched_1$last_name<-sub("Schmitt","Schmidt",unmatched_1$last_name)
  
  
  #Cutoffs
  unmatched_1 <-unmatched_1 %>%
    mutate(FN_short = sub("^Dr\\.\\s*","",first_name)) %>%
    mutate(FN_short = trimws(FN_short,which=c("both"))) %>%
    mutate(FN_short = substr(FN_short,1,stringcut)) 
  
  #1924:
  #Corrections: First name
  unmatched_2$first_name<-sub("F\\. X\\.","Franz Xaver",unmatched_2$first_name) 
  unmatched_2$first_name<-sub("Frz\\.","Franz",unmatched_2$first_name) 
  unmatched_2$first_name<-sub("Frdr\\.","Friedrich",unmatched_2$first_name)
  unmatched_2$first_name<-sub("Xav\\.","Xaver",unmatched_2$first_name)
  unmatched_2$first_name<-sub("P\\.?","Peter",unmatched_2$first_name)
  unmatched_2$first_name<-sub("Gg\\.?","Georg",unmatched_2$first_name)
  unmatched_2$first_name<-sub("Ldw\\.?","Ludwig",unmatched_2$first_name)
  unmatched_2$first_name<-sub("Xav\\.","Xaver",unmatched_2$first_name)#+2
  unmatched_2$first_name<-sub("X\\.","Xaver",unmatched_2$first_name)#+2
  unmatched_2$first_name<-sub("Fr\\.","Franz",unmatched_2$first_name)
  unmatched_2$first_name<-sub("Ad\\.","Adam",unmatched_2$first_name)
  unmatched_2$first_name<-sub("F\\.","Franz",unmatched_2$first_name) #--> look into
  
  
  #Corrections: Last name
  unmatched_2$last_name<-sub("v\\.","von",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Mair","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Mayr","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Meyr","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Meir","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Meier","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Meyer","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Mayer","Maier",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Schmid","Schmidt",unmatched_2$last_name)
  unmatched_2$last_name<-sub("Schmitt","Schmidt",unmatched_2$last_name)#+1
  
  
  #Cutoff
  unmatched_2 <-unmatched_2 %>%
    mutate(FN_short = sub("^Dr\\.\\s*","",first_name)) %>%
    mutate(FN_short = trimws(FN_short,which=c("both"))) %>%
    mutate(FN_short = substr(FN_short,1,stringcut))
  
  
  ##Removing dups:
  unique_unmatched_1 <- unmatched_1[!duplicated(unmatched_1[, c("last_name", "FN_short", "address")]), ]
  
  unique_unmatched_2 <- unmatched_2[!duplicated(unmatched_2[, c("last_name", "FN_short", "address")]), ]
  
  
  
  #Merge keep all 1924
  df <- right_join(x = unique_unmatched_1, y = unique_unmatched_2,
                   join_by("last_name" == "last_name", "address" == "address", "FN_short" == "FN_short"),
                   relationship = "one-to-one",keep=TRUE)
  df6<-df
  
  #Matched at this step:
  df1<-df%>%
    filter(!is.na(year.x))%>%
    relocate(full_name.y,.after = full_name.x)%>%
    relocate(address.y,.after = address.x)
  
  #same w/ thing less cols
  df2<-df1%>%
    select(address.x,address.y, year.x, id_inh.x, id_inh.y, full_name.x,full_name.y, year.y)
  
  
  #All Matched up to this step
  df3<-df1%>%
    select(-FN_short.x,-FN_short.y)
  merged<-rbind(total,df3)
  
  #unmatched first period
  df4<-inh1%>%
    anti_join(merged, by=c("id_inh"="id_inh.x"))
  #unmatched second period
  df5<-inh2%>%
    anti_join(merged, by=c("id_inh"="id_inh.y"))
  #Message
  print(paste(sprintf("%.1f%%",100*nrow(df1)/nrow(inh1))," of individuals are found"))
  
  return(list(df1,df2,merged,df4,df5))
}