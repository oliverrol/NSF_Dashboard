#use if you need to change your file location to make sure the files load correctly
setwd("C:/Users/olive/OneDrive - UW/NSF Civic 2/Dashboard/data/2025-09-30 Chris data/data") #Put in the file path to your needed folder

#install the required packages
# install.packages("olctools")
# install.packages("geosphere")

library(dplyr) 
library(olctools) #for converting Google OLC to lat lon
library(stringr) #for coalesce function
library(geosphere) #for lat lon distance calculations
library(stringr)

#import all of the data sets
df_all <- read.csv(
  "micropantries_all.csv", header = TRUE)

df_val <- read.csv(
  "survey_validate_mp.csv", header = TRUE)

df_add <- read.csv(
  "survey_add_mp.csv", header = TRUE)

#gets rid of the columns that we don't need
df_all <- df_all[, -which(names(df_all) == "PUGET_SOUND")]
df_all <- df_all[, -which(names(df_all) == "LOCATION")]
df_all <- df_all[, -which(names(df_all) == "POPULATION")]
df_all <- df_all[, -which(names(df_all) == "DETAIL")]


#renames the columns
colnames(df_val) <- c("validation_date", "ID", "exists", "ADDRESS", 
                      "lat_lon", "type_fridge", "location_type",
                      "overhead_protection", "outdoor_indoor", 
                      "size", "number_shelves", "tight_fitting_door", 
                      "contents", "photo", "additional_info" )
colnames(df_add) <- c("validation_date", "ADDRESS", "lat_lon", "type_fridge", "location_type",
                      "overhead_protection", "outdoor_indoor", 
                      "size", "number_shelves", "tight_fitting_door", 
                      "contents", "photo", "additional_info" )


df_add = df_add %>% mutate(
  ID = 9000 + row_number()
  )

#checking to see which columns are on the 
missing_cols <- setdiff(names(df_val), names(df_add))
print(missing_cols)

#Adding the exists column to df_add
df_add = df_add %>% mutate(
  exists = 'Yes, it still exists.'
)

#keeping the ID's of add because I need to add their lat lon in the main one
manual = df_add %>% select('ID')


#Stacking both of the validated dataframes
merge_data = rbind(df_val, df_add)

# #merge together the columns of the add and validate dataframes
# x_cols <- grep("\\.x$", names(merge_data), value = TRUE)  
# for (x_col in x_cols) {
#   base <- sub("\\.x$", "", x_col)
#   y_col <- paste0(base, ".y")
#   
#   if (y_col %in% names(merge_data)) {
#     merge_data[[base]] <- coalesce(merge_data[[x_col]], merge_data[[y_col]])
#     merge_data[[x_col]] <- NULL
#     merge_data[[y_col]] <- NULL
#   }
# }



#Checking that all IDs are Unique
df_all = df_all %>% distinct(ID, .keep_all = TRUE)
merge_data = merge_data %>% distinct(ID, .keep_all = TRUE)

merge_all = df_all %>% full_join(
  merge_data,
  by = 'ID',
  suffix = c("", ".y")
)


#See which ones are validated
merge_all = merge_all %>% mutate(
  exists_short = case_when(
    exists == 'Yes, it still exists.' ~ 'YES',
    exists == 'No, it does not exist anymore, and there is no other micropantry in the same block' ~ 'NO',
    .default = 'DONT KNOW'
  )
)

manually = merge_all %>% filter(ID %in% manual$ID)

# Clean and extract lat/lon
df_clean <- manually %>%
  mutate(
    # Remove parentheses and extra spaces
    lat_lon = str_replace_all(lat_lon, "[()]", ""),
    
    # Extract latitude and longitude using regex
    LAT = case_when(
      str_detect(lat_lon, "N") ~ as.numeric(str_extract(lat_lon, "\\d+\\.\\d+(?=N)")),
      TRUE ~ as.numeric(str_extract(lat_lon, "^-?\\d+\\.\\d+"))
    ),
    LON = case_when(
      str_detect(lat_lon, "W") ~ -as.numeric(str_extract(lat_lon, "\\d+\\.\\d+(?=W)")),
      TRUE ~ as.numeric(str_extract(lat_lon, "(?<=,\\s?)-?\\d+\\.\\d+$"))
    )
  )

#putting them on the merge_all
merge_all = merge_all %>% rows_update(df_clean, by= 'ID')
merge_all$ID <- seq_len(nrow(merge_all))


write.csv(merge_all, 'micropantries_oo.csv', row.names = FALSE)

