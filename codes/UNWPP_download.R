library(readr)
library(dplyr)
library(data.table)

## Increase the time limit for downloads
options(timeout = 2000000)

## Function to download 2024 World Population Prospects data
download_wpp24 <-
  function(dir_download = "data", indicator = "both") {

    # Function to download and save a file with a warning if it already exists
    download_file <-
      function(url, destfile) {
        if (file.exists(destfile)) { # Check if the file already exists
          warning(paste("The file", destfile, "already exists. Skipping download."))
        } else {
          download.file(url, destfile = destfile, mode = "wb")
        }
      }

    # Create data directory if it does not exist
    if (!dir.exists(dir_download)) {
      dir.create(dir_download)
    }

    # URLs for the data files
    url_fertility <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz"
    url_female_lifetable <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz"
    url_male_lifetable <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz"
    url_pop <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz"


    if(indicator == "both"){
      # Read and save as CSV files if not already saved
      if(!file.exists(file.path(dir_download,"WPP2024_Fertility_by_Age1.csv"))) {
        print("Reading Fertility")
        fertility <- fread(url_fertility)
        print("Saving Fertility")
        fwrite(fertility, file.path(dir_download,"WPP2024_Fertility_by_Age1.csv"), row.names = FALSE)
      } else {
        warning("Fertility data already saved as CSV. Skipping.")
      }

      if(!file.exists(file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv"))) {
        print("Reading Female Life Table")
        mortality_female <- fread(url_female_lifetable)
        print("Saving Female Life Table")
        fwrite(
          mortality_female,
          file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv"), row.names = FALSE
        )
      } else {
        warning("Female mortality data already saved as CSV. Skipping.")
      }

      if (!file.exists(file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv"))){
        print("Reading Male Life Table")
        mortality_male <- fread(url_male_lifetable)
        print("Saving Male Life Table")
        fwrite(
          mortality_male,
          file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv"), row.names = FALSE
        )
      } else {
        warning("Male mortality data already saved as CSV. Skipping.")
      }
    }
    if(indicator == "fertility"){
      # Read and save as CSV files if not already saved
      if(!file.exists(file.path(dir_download,"WPP2024_Fertility_by_Age1.csv"))) {
        print("Reading Fertility")
        fertility <- fread(url_fertility)
        print("Saving Fertility")
        fwrite(fertility, file.path(dir_download,"WPP2024_Fertility_by_Age1.csv"), row.names = FALSE)
      } else {
        warning("Fertility data already saved as CSV. Skipping.")
      }
    }
    if(indicator == "mortality"){

      if(!file.exists(file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv"))) {
        print("Reading Female Life Table")
        mortality_female <- fread(url_female_lifetable)
        print("Saving Female Life Table")
        fwrite(
          mortality_female,
          file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv"), row.names = FALSE
        )
      } else {
        warning("Female mortality data already saved as CSV. Skipping.")
      }

      if (!file.exists(file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv"))){
        print("Reading Male Life Table")
        mortality_male <- fread(url_male_lifetable)
        print("Saving Male Life Table")
        fwrite(
          mortality_male,
          file.path(dir_download,"WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv"), row.names = FALSE
        )
      } else {
        warning("Male mortality data already saved as CSV. Skipping.")
      }
    }


    if (!file.exists(file.path(dir_download,"WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv"))){
      print("Reading Population")
      pop <- fread(url_pop)
      print("Saving Population")
      fwrite(
        pop,
        file.path(dir_download,"WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv"), row.names = FALSE
      )
    } else {
      warning("Population data already saved as CSV. Skipping.")
    }

    # Return a message indicating the process is complete
    return("2024 World Population Prospects data downloaded and saved successfully as CSV files.")
}

## Function to download previous World Population Prospects data versions
download_wpp_previous <-
  function(dir_download = "data", wpp_version_year, save_data = "both", indicator = NULL) {

    # Function to download and save a file with a warning if it already exists - made by Amanda Martins (MPIDR, 2024)
    download_file <-
      function(url, destfolder, destfile) {
        if (file.exists(file.path(destfolder,destfile))) { # Check if the file already exists
          warning(paste("The file", destfile, "already exists. Skipping download."))
        } else {
          download.file(url, destfile = file.path(destfolder,destfile), mode = "wb")
        }
      }

    # Create data directory if it does not exist
    if (!dir.exists(dir_download)) {
      dir.create(dir_download)
    }
    if(save_data %in% c("raw","both")){
      if(!dir.exists(file.path(dir_download,"raw"))){
        dir.create(file.path(dir_download,"raw"))
      }
    }

    # Some parameters ---

    year <- wpp_version_year # choosing a year
    url_versions <- paste0("https://population.un.org/wpp/assets/Excel%20Files/5_Archive/WPP",year,"-CSV-data.zip") # url for version
    wpp_downloaded_file <- paste0("WPP",year,"-CSV-data.zip") # downloaded file
    if(save_data %in% c("raw","both")){
      wpp_downloaded_folder <- file.path(dir_download,"raw") # downloaded folder - storaged in the computer
    } else{
      wpp_downloaded_folder <- tempdir() # downloaded folder - temporary folder
    }
    # indicator
    ## possible choices:
    #' fertility         - fertility by age and year
    #' mortality         - life tables by age and year
    #' indicators_mv     - general indicators, medium variant
    #' indicators_ov     - general indicators, other variants
    #' pop_agesex_5x5_mv - pop by age and sex 5 years age groups, medium variant
    #' pop_agesex_1x1_mv - pop by age and sex 1 year age groups, medium variant
    #' pop_agesex_1x1_ov - pop by age and sex 1 year age groups, other variant
    #' pop_sex_1x1_tv    - pop by sex for each projected year, all the variants
    if(save_data %in% c("both","unzipped")){
      if(is.null(indicator)){
        stop("You must to declare one of the indicators for downloading these data...")
      }
      if(length(indicator) > 1){
        stop("You must to declare just one indicator each time... try to import each of your interested indicators.")
      }
    }
    if(indicator == "fertility"){
      ind = paste0("WPP",year,"_Fertility_by_Age.csv")
    }
    if(indicator == "mortality"){
      ind = paste0("WPP",year,"_Life_Table.csv")
    }
    if(indicator == "indicators_mv"){
      ind = paste0("WPP",year,"_Period_Indicators_Medium.csv")
    }
    if(indicator == "indicators_ov"){
      ind = paste0("WPP",year,"_Period_Indicators_OtherVariants.csv")
    }
    if(indicator == "pop_agesex_5x5_mv"){
      ind = paste0("WPP",year,"_PopulationByAgeSex_5x5_Medium.csv")
    }
    if(indicator == "pop_agesex_1x1_mv"){
      ind = paste0("WPP",year,"_PopulationByAgeSex_Medium.csv")
    }
    if(indicator == "pop_agesex_1x1_ov"){
      ind = paste0("WPP",year,"_PopulationByAgeSex_OtherVariants.csv")
    }
    if(indicator == "pop_sex_1x1_tv"){
      ind = paste0("WPP",year,"_TotalPopulationBySex.csv")
    }

    # downloading raw data...
    download_file(url_versions, destfolder = wpp_downloaded_folder, destfile = wpp_downloaded_file)

    # unzip file with interested indicator
    if(file.exists(file.path(dir_download,ind))){
      warning(paste("The file", ind, "already exists. Skipping unzip process."))
    } else{
      unzip(zipfile = file.path(wpp_downloaded_folder,wpp_downloaded_file),files = ind, exdir = dir_download)
    }

    # Return a message indicating the process is complete
    invisible(gc())
    return(paste0(year,"'s World Population Prospects data review downloaded and saved successfully as CSV files."))
  }

# Example

# download_wpp_previous(wpp_version_year = 2019, indicator = "pop_agesex_1x1_mv")
