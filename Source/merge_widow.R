


merge_widow <- function(merged,inh_1,inh_2,widows,unmatched_1,unmatched_2) {
  
  inh1 <- inh_1
  inh2 <- inh_2
  
  #Match 1919 widows to 1924: this step should be a inner_join?
  widowmatch <- right_join(x=unmatched_2, y=widows,
                           join_by("id_inh" == "id_inh"),
                           relationship = "one-to-one"
  )   
  widowmatch <- widowmatch %>%
    filter(!is.na(address.x))
  widowmatch <- widowmatch %>%
    select(-full_name.y,-address.y)
  widowmatch <- widowmatch %>%
    rename_all(~gsub("\\.x", "", .))
  widowmatch <- widowmatch %>%
    rename_all(~gsub("\\.y", "", .))
  
  
  #Remove dups:
  # First period
  df1 <- unmatched_1[!duplicated(unmatched_1[, c("last_name", "address")]), ]
  #
  dfw <- widowmatch[!duplicated(widowmatch[, c("last_name", "address")]), ]
  
  
  # Second period
  df2 <- unmatched_2[!duplicated(unmatched_2[, c("last_name", "address")]), ]
  # Merge
  df <- inner_join(x = df1, y = dfw,
                   join_by("last_name" == "last_name", "address" == "address"),
                   relationship = "one-to-one",
                   keep =TRUE)
  
  df$both <- ifelse(!is.na(df$year.x),1,0)
  print(paste(sprintf("%.1f%%",100*sum(df$both)/nrow(inh1))," of individuals are found"))
  df<-df%>%
    select(-both)
  #MATCHED IN THIS STEP:
  df1 <- df %>%
    filter(!is.na(year.x))
  
  #All merged up to this step
  df2<-rbind(merged,df1)
  
  #ALL LEFT - 1919:
  df3<-inh1%>%
    anti_join(df2, by = c("id_inh"="id_inh.x"))
  #ALL LEFT - 1924: 
  df4<-inh2%>%
    anti_join(df2, by = c("id_inh"="id_inh.y"))
  
  
  
  return(list(df,df1,df2,df3,df4))
}

