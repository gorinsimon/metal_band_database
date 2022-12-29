# ///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#
#                       CREATING A DATABSE OF METAL BANDS                      #
#                                                                              #
#          Collect information on metal bands with web pages scraping          #
#       The site www.spirit-of-metal.com was used to create the database       #
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////////#
#                      **** Code author: Simon Gorin ****                      #

#-------------------------------#
#          1. Packages       ####
#-------------------------------#

# Ensures that the project root that contains the R script indicated is set up
# correctly
here::i_am("data/create_band_database.R")

library(tidyverse) # collections of package for data science
library(httr)      # required to make GET request to website
library(rvest)     # to help scrape web pages
library(stringr)   # for common string operations
library(here)      # to work with relative path

# Load custom functions to scrap info on bands
source(here("functions", "collect_bands_info.R"))


#-------------------------------#
#       2. Web scrapping     ####
#-------------------------------#

# Retrieve on the encyclopedia section of the website the html chunk containing
# the total number of bands in their database.
number_of_bands_html <-
  GET("https://www.spirit-of-metal.com/en/encyclopedia") %>%
  content(as = 'text', encoding = 'UTF-8') %>%
  read_html() %>%
  html_elements("span.stats-figure")

# Computes the total number of bands in the website's database
number_of_bands <-
  as.numeric(str_extract(as.character(number_of_bands_html[1]), "(\\d+)"))

# Calculates the total number of pages to scrape to have the url of all the
# bands, considering that each page contains 100 band.
n_page_to_scrap <- ceiling(number_of_bands/100)

# Collect the url (without the base url) of all the bands
all_bands_info <- map_df(1:n_page_to_scrap,
                         get_band_urls)

#-------------------------------#
#          3. Cleaning       ####
#-------------------------------#

# Retrieve the ID of active band indicated as "Active" but with a date of
# separation. These bands are likely to have been mislabeled as "Active" instead
# of "Split-Up" and will be latter relabeled as "Split-Up"
dat_active_wrong <-
  all_bands_info %>%
  filter(status == "Active",
         formation != "Unknown",
         !is.na(separation)) %>%
  pull(id)

# Retrieve the ID of active band indicated as "Split-Up" but without a date of
# separation. These bands are likely to have been mislabeled as "Active" instead
# of "Split-Up". These bands will be removed as they cannot be used reliably for
# temporal analysis.
dat_split_wrong <-
  all_bands_info%>%
  filter(status == "Split-Up",
         formation != "Unknown",
         is.na(separation)) %>%
  pull(id)

# Clean the data
dat_clean <-
  all_bands_info %>%
  filter(!(id %in% dat_split_wrong)) %>%
  mutate(
    # Recode bands wrongly indicated as "Active" but that actually "Split-Up"
    status = if_else(id %in% dat_active_wrong, "Split-Up", status),
    # Recode all country names in a consistent manner ("countrycode" package is
    # required).
    country = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "country.name",
      # The list below is required because the package cannot ambiguously match
      # these country names. Note that in case of update of the database, you
      # need to check that there is no additional ambiguous country names in the
      # database.
      custom_match =
        c(
          "Panamá" = "Panama",
          "Viêt-Nam" = "Vietnam",
          "Tadjikistan" = "Tajikistan"
        ),
      warn = FALSE)
  )


#-------------------------------#
#           4. Saving        ####
#-------------------------------#

# Save the database as csv
write_csv2(
  all_bands_info,
  here("data", glue::glue("metal_band_database_{lubridate::today()}.csv"))
)