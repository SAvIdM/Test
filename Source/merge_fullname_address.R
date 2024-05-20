# requires inh_
merge_fullname_address <- function(inh1,inh2,second) {
  
  #inh1 <- inh1
  
  #inh2 <- inh2
  
  # First period no dups
  df1 <- inh1[!duplicated(inh1[, c("full_name", "address")]), ]
  # Second period no dups
  df2 <- inh2[!duplicated(inh2[, c("full_name", "address")]), ]

  
  #All inhabitants in second period + matching ones from first period
  df <- right_join(x = df1, y = df2,
                   join_by("full_name" == "full_name", "address" == "address"),
                   relationship = "one-to-one", 
                   keep = TRUE)
  
  df$both <- ifelse(!is.na(df$year.x),1,0)
  print(paste(sprintf("%.1f%%",100*sum(df$both)/nrow(inh1))," of individuals live in the same address in period ", second))
  df<-df%>%
    select(-both)
  
  #Only matches
  df1<-df%>%
    filter(!is.na(year.x))
  
  
  #1919 not matched
  df4<-inh1%>%
    anti_join(df1, by = c("id_inh"="id_inh.x"))
  #1924 not matched
  df5<-inh2%>%
    anti_join(df1, by = c("id_inh"="id_inh.y"))
  
  
  
  return(list(df,df1,df4,df5))
}
