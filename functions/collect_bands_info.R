#------------------------------------------------------------------------------#
# Function retrieving info from a band based on a url
#------------------------------------------------------------------------------#

get_band_summary <- function(url) {
  
  # Get the html code for the band's page
  band_html <- GET(glue::glue("https://www.spirit-of-metal.com{url}")) %>%
    read_html()
  
  # Extract band's name from the html code
  band_name <- band_html %>%
    html_elements("h2") %>%
    as.character() %>%
    str_match("(<h2 class=\"ribbon\">)(.+)(</h2>)") %>%
    .[,3]
  
  # Extract from the html code the chunk with information about the band
  band_info <- band_html %>%
    html_elements("#profile") %>%
    html_elements("span") %>%
    as.character()
  
  # Extract band info using regex
  band_info_extract <-
    band_info %>%
    str_match(">([^>.]+)<") %>%
    .[,2] %>%
    .[!is.na(.)]
  
  # Retrieve the popularity of the band
  popularity <- sum(str_count(band_info, "fa-star"))
  
  # Name of the band extracted from
  name <- band_name
  
  # Alias of the band (if any)
  alias <-
    lead(band_info_extract)[str_detect(band_info_extract, "Also known as")]
  alias <- ifelse(length(alias) == 0, NA, alias)
  
  # Style(s) of the band
  style <- lead(band_info_extract)[str_detect(band_info_extract, "Style")]
  style <- ifelse(length(style) == 0, NA, style)
  
  # Status of the band (Active, Split-Up,...)
  status <- lead(band_info_extract)[str_detect(band_info_extract, "Status")]
  status <- ifelse(length(status) == 0, NA, status)
  
  # Date of creation
  formation <-
    lead(band_info_extract)[str_detect(band_info_extract, "Formed In")]
  formation <- ifelse(length(formation) == 0, NA, formation)
  
  # Date of separation
  separation <-
    lead(band_info_extract)[str_detect(band_info_extract, "Separated In")]
  separation <- ifelse(length(separation) == 0, NA, separation)
  
  # Country of origin
  country <- lead(band_info_extract)[str_detect(band_info_extract, "Country")]
  country <- ifelse(length(country) == 0, NA, country)
  
  # City of origin
  city <- lead(band_info_extract)[str_detect(band_info_extract, "City")]
  city <- ifelse(length(city) == 0, NA, city)
  
  # Popularity (on the website?) with a 5-star system
  popularity <- ifelse(length(popularity) == 0, NA, popularity)
  
  # The number of user who indicated they are fan of the band (on the website)
  fans <- lead(band_info_extract)[str_detect(band_info_extract, "Fans")]
  fans <- ifelse(length(fans) == 0, NA, fans)
  
  # Tibble gathering all the above variables
  band_summary <-
    tibble(name = name,
           alias = alias,
           style = style,
           status = status,
           formation = formation,
           separation = separation,
           country = country,
           city = city,
           popularity = popularity,
           fans = fans)
  
}

#------------------------------------------------------------------------------#
# Function collecting the url (without the website's base url) of all the bands
# on the page https://www.spirit-of-metal.com/en/bands/{n}, where {n} is a
# positive numeric value and each page contains up to 100 bands.
#------------------------------------------------------------------------------#

get_band_urls <- function(n) {
  
  urls <- GET(glue::glue("https://www.spirit-of-metal.com/en/bands/{n}")) %>%
    read_html() %>%
    html_elements("div.BandResult > a") %>%
    html_attr("href")
  
  band_summary_all_temp <- map_df(urls, get_band_summary)
}