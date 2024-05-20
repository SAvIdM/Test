#Clear env.
rm(list=ls())

#Set Path:
#Stefano
setwd("/Users/stefanoandre/Dropbox")

#set periods:
first<-1919
second<-1924

#Load Data:
#First period
path <- paste0("Directories/output/derived/directories/munich_", first, "_inh_clean.RDS")
inhabitants1 <- readRDS(path)
# name <- paste0("inhabitants_", first)
# assign(name, data)

#Second period
path <- paste0("Directories/output/derived/directories/munich_", second, "_inh_clean.RDS")
inhabitants2 <- readRDS(path)


# Remove duplicates (TALK TO MATHIAS ABOUT THIS STEP: i dont know what to do with these dups)
inhabitants1 <- inhabitants1[!duplicated(inhabitants1[, c("full_name", "address")], fromLast = TRUE), ]
inhabitants2 <- inhabitants2[!duplicated(inhabitants2[, c("full_name", "address")], fromLast = TRUE), ]



match_inhabitants <- function(inhabitants1, inhabitants2) {
 
  
  # Load Packages
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(lmtest)
  library(data.table) 
  
  
  ###SOURCING STEPS 1-4
  source("/Users/stefanoandre/GitHub/Test/Source/merge_fullname_address_2.R") #merge_fullname_address (STEP 1)
  source("/Users/stefanoandre/GitHub/Test/Source/merge_fullname_2.R") #merge_fullname (STEP 2)
  source("/Users/stefanoandre/GitHub/Test/Source/merge_widow_2.R") #merge_widow (STEP 3)
  source("/Users/stefanoandre/GitHub/Test/Source/merge_string.R") #merge_string (STEP 4)

  
  
  ############################################
  #STEP 0 - Prepare data:
  address_merge <- function(df1, df2) {
    inhabitants_1 <- df1 %>% filter(!is.na(first_name) & owner == 0) 
    inhabitants_2 <- df2 %>% filter(!is.na(first_name) & owner == 0)
    
    # Find how many addresses are the same
    address_1 <- inhabitants_1 %>%
      group_by(address) %>%
      summarise(HH = n_distinct(id_inh))
    address_2 <- inhabitants_2 %>%
      group_by(address) %>%
      summarise(HH = n_distinct(id_inh))
    
    # Which in 2 exist in 1?
    merge_add_1_2 <- left_join(x = address_2, y = address_1,
                               by = "address",
                               relationship = "one-to-one")
    
    merge_add_1_2$both <- ifelse(!is.na(merge_add_1_2$HH.y), 1, 0)
    print(paste(sprintf("%.1f%%", 100 * sum(merge_add_1_2$both) / nrow(merge_add_1_2)), " of addresses exist in both"))
    
    # Give out addresses that exist in both
    add <- merge_add_1_2 %>% filter(both == 1)  
    add <- select(add, address, both)
    return(add)
  }
  
  # Apply to get addresses found in both years
  addresses_both <- address_merge(inhabitants1, inhabitants2)
  
  # Filter by address that exist in both
  prepare_inhabitants <- function(df, address) {
    inh <- df %>% filter(!is.na(first_name) & owner == 0)
    
    df <- right_join(x = inh, y = address,
                     by = "address",
                     relationship = "many-to-one")
    print(paste(sprintf("%.1f%%", 100 * nrow(df) / nrow(inh)), " of individuals live in addresses that exist in both periods"))
    
    df <- select(df, address, id_inh, street_orig, number_orig, year, floor, last_name_orig, first_name_orig, street, number, number_app, first_name, last_name, full_name, male_id, occupation_clean)
    
    return(df)
  }
  
  # Inhabitants data with addresses that exist in both periods
  inh_1 <- prepare_inhabitants(inhabitants1, addresses_both) 
  inh_2 <- prepare_inhabitants(inhabitants2, addresses_both)
  
  # DF with second period widows
  widows <- inhabitants2 %>%
    filter(widow == 1) %>%
    select(id_inh, full_name, address)
  
  ############################################
  ### STEP 1 - FULLNAME + ADDRESS 
  result1 <- merge_fullname_address(inh_1, inh_2,second)
  merge <- result1[[1]]  # All inhabitants in second period + matching ones from first period                
  matched_1 <- result1[[2]]   # Only full matches               
  merged <- result1[[2]]   # All merged so far
  unmatched_1 <- result1[[3]]       # 1919 not matched     
  unmatched_2 <- result1[[4]]       # 1924 not matched 
  
  # Should be zero
  check <- matched_1 %>%
    filter(address.x != address.y)
  rm(check)
  
  ############################################
  ### STEP 2 - FULLNAME (same person, different address)
  result2 <- merge_fullname(merged, inh_1, inh_2, unmatched_1, unmatched_2) 
  merge <- result2[[1]] # from what's left: all 1924 inhabitants + matching inhabitants from 1919
  matched_2 <- result2[[2]] # only matches from this step 
  merged <- result2[[3]]  # all matches so far
  unmatched_1 <- result2[[4]] # left to match first period
  unmatched_2 <- result2[[5]] # left to match second period
  
  # Should be zero 
  check <- matched_2 %>%
    filter(address.x == address.y)
  rm(check)
  
  ############################################
  # STEP 3 - Recheck the dups in the "widowmatch" df
  result3 <- merge_widow(merged, inh_1, inh_2, widows, unmatched_1, unmatched_2)
  merge3 <- result3[[1]] # all second period + matching from first period
  matched_3 <- result3[[2]] # full matches in this step
  merged <- result3[[3]] # all matches so far
  unmatched_1 <- result3[[4]]
  unmatched_2 <- result3[[5]]
  
  # Should be zero 
  check <- matched_3 %>%
    filter(address.x != address.y)
  rm(check)
  
  ############################################e
  # STEP 4 - String matching
  result4 <- merge_string(merged, inh_1, inh_2, unmatched_1, unmatched_2,3)
  merge4 <- result4[[1]] # merged in this step
  matched_4 <- result4[[2]] # same w/ thing less cols
  merged <- result4[[3]] # all merged up to this step
  unmatched_1 <- result4[[4]]
  unmatched_2 <- result4[[5]]
  
  ############################################
  # STEP 5 - Bind all matches 
  rate <- 100 * (nrow(merged) / nrow(inh_1))
  print(rate)
  
  # Rename cols
  merged <- merged %>%
    rename_all(~gsub("\\.y", "", .))
  
  # Bind with unmatched 
  all <- bind_rows(merged, unmatched_2)
  # Check
  check <- all %>%
    anti_join(inh_2, by = c("id_inh"))
  rm(check)
  
  # Adding columns for found and moved
  all <- all %>%
    mutate(found = ifelse(is.na(address.x), 0, 1))
  all <- all %>%
    mutate(moved = ifelse(all$address.x == all$address, 0, 1))
  
  # Changing positions
  all <- all %>%
    relocate(address, .after = address.x)
  
  return(list(all, unmatched_1, unmatched_2))
}

result <- match_inhabitants(inhabitants1, inhabitants2)
all <- result[[1]]
unmatched_1 <- result[[2]]
unmatched_2 <- result[[3]]
