#use if you need to change your file location to make sure the files load correctly
getwd()
setwd("micropantries/data") #Put in the file path to your needed folder

#insall the required packages
install.packages("olctools")
install.packages("geosphere")

library(dplyr) 
library(olctools) #for converting Google OLC to lat lon
library(stringr) #for coalesce function
library(geosphere) #for lat lon distance calculations
 

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

#make the validated column have yes for all the ones that have been validated 
df_add$validated <- 'yes'
df_val$validated <- 'yes'

#merges together the files into one big dataframe
merge_data <- merge(df_all, df_add, by="ADDRESS", all = TRUE)
merge_data <- merge(merge_data, df_val, by="ID", all = TRUE)

#merge together the columns of the add and validate dataframes
x_cols <- grep("\\.x$", names(merge_data), value = TRUE)  
for (x_col in x_cols) {
  base <- sub("\\.x$", "", x_col)
  y_col <- paste0(base, ".y")
  
  if (y_col %in% names(merge_data)) {
    merge_data[[base]] <- coalesce(merge_data[[x_col]], merge_data[[y_col]])
    merge_data[[x_col]] <- NULL
    merge_data[[y_col]] <- NULL
  }
}

#new column for the two different lat lon with at least a 30 meter distance off 
merge_data$alt_lat = NA
merge_data$alt_lon = NA

#get the geolocation data from the add and validate files and makes sure they are the same as the micropantries_all.csv and other lat lon stuff
for(i in rownames(merge_data)){
  curr_form <- merge_data[i,"lat_lon"]
  if(!(is.na(curr_form) || curr_form == "")){ #if it is NA or empty
    if (grepl("\\d+\\.?\\d*.*\\s+.*-?\\d+\\.?\\d*", curr_form)){ #if already in some sort of lat lon form
      lat_lon <- str_extract_all(curr_form, "-?[0-9.]+")[[1]] #just to get the numbers out of the string
      lat <- as.numeric(lat_lon[1])
      lon <- as.numeric(lat_lon[2])
    } else if (grepl("Seattle, Washington", curr_form) || nchar(curr_form) == 8) { #shorter Google OLC code
      lat_lon <- recover_olc(sub("^\\s*(\\S+).*", "\\1", curr_form), 47.6, -122.3) #47.6, -122.3 is Seattle 
      lat_lon <- decode_olc(lat_lon)
      lat <- lat_lon[1,3]
      lon <- lat_lon[1,4]
    } else if(nchar(curr_form)==11) { #longer Google OLC code
      lat_lon <- decode_olc(curr_form)
      lat <- lat_lon[1,3]
      lon <- lat_lon[1,4]
    }
    if(lon > 0){ #it should be negative but some of the changes aren't negative for some reason (Seattle has only negative longitude)
      lon <- lon*-1
    }
    if(is.na(merge_data[i,"LAT"]) && is.na(merge_data[i,"LON"])){
      merge_data[i,"LAT"] <- lat 
      merge_data[i,"LON"] <- lon
    } else {
      survey_point <- c(lon, lat)
      original_point <- c(merge_data[i,"LON"], merge_data[i,"LAT"])
      
      distance <- distHaversine(survey_point, original_point)
      if(distance > 30){
        cat("from the survey: ", lat, lon, "   from the file:", merge_data[i,"LAT"], merge_data[i,"LON"], "\n" )
        cat(distance, " meters away \n")
        cat("Original lat lon before conversion: ", curr_form, "\n")
        cat("\n")
        merge_data[i,"alt_lat"] <- lat 
        merge_data[i,"alt_lon"] <- lon
      } else {
        merge_data[i,"LAT"] <- lat 
        merge_data[i,"LON"] <- lon
      }
      
    }
  
  }
}
merge_data <- merge_data[, -which(names(merge_data) == "lat_lon")] #removes the original lat lon column from the surveys 
  
#fills the validation_date and validated columns with no for all the ones that haven't been validated yet and shortens the exists column
for(i in rownames(merge_data)){  
  if(is.na(merge_data[i,"validation_date"])){
    merge_data[i,"validation_date"] <- "not yet validated"
  }
  if(is.na(merge_data[i,"validated"])){
    merge_data[i,"validated"] <- "no"
  }
  if(grepl("Yes", merge_data[i,"exists"])){
    merge_data[i,"exists"] <- "yes"
  } else if(grepl("No", merge_data[i,"exists"])){
    merge_data[i,"exists"] <- "no"
  }
}

#makes the column titles lowercase
names(merge_data) <- tolower(names(merge_data))

#moves the address column 
merge_data <- merge_data %>% relocate(address, .before = city)

#changes some names
colnames(merge_data)[names(merge_data) == "network"] <- "data_source"
colnames(merge_data)[names(merge_data) == "type_fridge"] <- "type_pantry"

#makes 3 new columns of width, length, and height 
merge_data$width = NA
merge_data$length = NA
merge_data$height = NA

#takes in the width, length, and height from size and seperates them to 3 different columns
for(i in rownames(merge_data)){
  curr_size <- merge_data[i,"size"]

  if(!(is.na(curr_size) || curr_size == "")){ #if it is NA or empty
    matches <- regmatches(curr_size, gregexpr("(\\d+(?:\\.\\d+)?)[[:space:]\"]*([HhTtWwLlDd])", curr_size))[[1]]
    
    if (length(matches) > 0) {
      # Parse labelled matches
      df <- do.call(rbind, lapply(matches, function(m) {
        parts <- regmatches(m, regexec("(\\d+(?:\\.\\d+)?)[[:space:]\"]*([HhTtWwLlDd])", m))[[1]]
        data.frame(value = as.numeric(parts[2]), label = toupper(parts[3]), stringsAsFactors = FALSE)
      }))
      
      # Initialize variables
      height <- width <- length <- NA
      
      # Assign based on label
      if (any(df$label %in% c("H", "T"))) height <- df$value[df$label %in% c("H", "T")][1]
      if (any(df$label == "W")) width <- df$value[df$label == "W"][1]
      if (any(df$label %in% c("L", "D"))) length <- df$value[df$label %in% c("L", "D")][1]
      
    } else {
      # If there is no labels, assume H, W, L order
      nums <- as.numeric(unlist(regmatches(curr_size, gregexpr("\\d+(?:\\.\\d+)?", curr_size))))
      height <- nums[1]
      width  <- nums[2]
      length <- nums[3]
    }
    if(curr_size == "82\"H X 30\" X 16\"D" ){ #it broke only at this one so I put it in manually for the width
      width <- 30
    }
    merge_data[i,"height"] <- height
    merge_data[i,"width"] <- width
    merge_data[i,"length"] <- length
    #cat(curr_size," ", height," ", width," ",length, "\n")
  }  
}
merge_data <- merge_data[, -which(names(merge_data) == "size")] #removes the original size column

merge_data$contact_phone = NA
merge_data$contact_email = NA
merge_data$contact_website = NA

#seperate the contact column to contact_phone, contact_email, contact_website
for (i in 1:nrow(merge_data)) {
  #print(paste("Row", i, ":"))
  
  # Split the contact column by spaces
  contacts <- strsplit(merge_data$contact[i], " ")[[1]]
  
  # Loop through each word
  for (j in 1:length(contacts)) {
    curr_contact <- gsub(";$", "", contacts[j]) #gets rid of semi colon at the end of the string
    if(!grepl("www.thelittlefreepantries.org/find-a-pantry", curr_contact) && !(is.na(curr_contact))){
      if(grepl("@", curr_contact)){
        merge_data[i,"contact_email"] <- curr_contact
      } else if(grepl("^\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?\\d{4}$", curr_contact)) { #regex expression to see if it's a phone number
        merge_data[i,"contact_number"] <- curr_contact
      } else {
        merge_data[i,"contact_website"] <- curr_contact
      }
    }
  }
}

#outdoor_indoor, overhead_protection, and tight_fitting_door manual entry cleaning
for(i in rownames(merge_data)){
  curr_outin_entry <- merge_data[i,"outdoor_indoor"]
  if(grepl("no second cover of the type many other outdoor LFPs", curr_outin_entry)){
    merge_data[i,"outdoor_indoor"] <- "Outdoor without a cover"
  } else if(grepl("One piece stainless steel roof but no other \"cover\"", curr_outin_entry)){
    merge_data[i,"outdoor_indoor"] <- "Outdoor with a cover"
  } else if(grepl("Outdoor with a cover;3 pantry boxes with roofs , all 3 are missing the doors.", curr_outin_entry)){
    merge_data[i,"outdoor_indoor"] <- "Outdoor with a cover"
  }
  
  curr_door_entry <- merge_data[i,"tight_fitting_door"]
  if(grepl("There are doors, not especially tight and the latch is a little shakey", curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  } else if(grepl("Has a bolt, doors are misaligned, making it hard to latch", curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  } else if(grepl("Door is not tight fitting. Has gap.", curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  } else if(grepl("Door is missing", curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  } else if(grepl("latch", curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  } else if(grepl("Loose",curr_door_entry) || grepl("loose",curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  } else if(grepl("Two doors", curr_door_entry)){
    merge_data[i,"tight_fitting_door"] <- "No"
  }
  
  curr_overhead_entry <- merge_data[i,"overhead_protection"]
  if(grepl("integrated", curr_overhead_entry) || grepl("Integrated", curr_overhead_entry)){
    merge_data[i,"overhead_protection"] <- "Yes"
  } else if(grepl("no for the unrefrigerated pantry", curr_overhead_entry)){
    merge_data[i,"overhead_protection"] <- "No"
  } else if(grepl("Under a tree", curr_overhead_entry)){
    merge_data[i,"overhead_protection"] <- "No"
  }
}

View(merge_data)

#If you want to write it to a csv, uncomment the code below
#write.csv(merge_data, "micropantries.csv") 
