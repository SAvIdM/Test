
#Fullname merge function###
merge_fullname <- function(merged,firstperiod,secondperiod,unmatched_1,unmatched_2) {
  
  inh1 <- paste0("inh_",firstperiod)
  inh1 <- get(inh1)
  inh2 <- paste0("inh_",secondperiod)
  inh2 <- get(inh2)
  
  
  df1 <- unmatched_1[!duplicated(unmatched_1[, c("full_name")]), ]
  df2 <- unmatched_2[!duplicated(unmatched_2[, c("full_name")]), ]
  
  
  
  # Merge by fullname (same person, different address)
  df <- right_join(x = df1, y = df2,
                   join_by("full_name" == "full_name"),
                   relationship = "one-to-one",
                   keep = TRUE)
  
  df$both <- ifelse(!is.na(df$year.x),1,0)
  print(paste(sprintf("%.1f%%",100*sum(df$both)/nrow(inh1))," of individuals are found"))
  df<-df%>%
    select(-both)
  #INHABITANTS FOUND IN THIS STEP
  df1<-df%>%
    filter(!is.na(year.x))
  
  #ALL FOUND UP TO THIS STEP
  df2<-rbind(merged,df1)
  
  #ALL LEFT - 1919:
  df3<-inh1%>%
    anti_join(df2, by = c("id_inh"="id_inh.x"))
  #ALL LEFT - 1924: 
  df4<-inh2%>%
    anti_join(df2, by = c("id_inh"="id_inh.y"))
  
  return(list(df,df1,df2,df3,df4))
}
