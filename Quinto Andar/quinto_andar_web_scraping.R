library(dplyr)
library(tibble)
library(httr)
library(rvest)
library(readxl)
library(stringr)
library(stringi)
library(jsonlite)

setwd("C:/Users/Chacha/Desktop/FGV/Mestrado/Quinto Andar")

# initial dataset (if you have something scraped already)
#initial_properties <- read.csv("quinto_andar.csv")


# functions to extract id and latitude/longitude
extract_listing_id <- function(url) {
  id <- str_match(url, "/imovel/(\\d+)/")[,2]
  return(id)
}

get_latlon <- function(page, id){
  page_text <- as.character(page)
  lat <- str_match(page_text, '"latitude":\\s*(-?\\d+\\.\\d+)')[,2] %>% as.numeric()
  lon <- str_match(page_text, '"longitude":\\s*(-?\\d+\\.\\d+)')[,2] %>% as.numeric()
  lat_lon <- c(lat,lon)
  return(lat_lon)
}

# neighborhoods information
neighborhoods_df <- read_xlsx("C:/Users/gusta/Downloads/Lista de bairros de sao paulo.xlsx") %>%
  mutate(
    neighborhood = Neighborhood %>%
      tolower() %>%
      stri_trans_general("Latin-ASCII") %>%
      gsub("[^a-z0-9 ]", "", .) %>%
      gsub("\\s+", "-", .)
  )

neighborhood_links <- character(nrow(neighborhoods_df))
all_property_links <- c()

# building links for each neighborhood -- adjust if you need to save time
for (i in seq_len(nrow(neighborhoods_df))) {
  neighborhood_name <- tolower(neighborhoods_df$neighborhood[i])
  neighborhood_links[i] <- paste0("https://www.quintoandar.com.br/alugar/imovel/", neighborhood_name, 
                                  "-sao-paulo-sp-brasil")
}
neighborhoods_df$neighborhood_link <- neighborhood_links
rm(neighborhood_links, neighborhood_name)

# collecting property links (3 pages per neighborhood) -- adjust to save time
for(i in seq_len(nrow(neighborhoods_df))){
  url <- neighborhoods_df$neighborhood_link[i]
  for(x in 1:3){
    tryCatch({
      url_page <- paste0(url, '?pagina=', x)
      page <- read_html(url_page)
      links <- page %>%
        html_elements('[data-testid="house-card-container-rent"] a') %>%
        html_attr("href")  
      
      full_links <- paste0("https://www.quintoandar.com", links)
      all_property_links <- c(all_property_links, full_links)
    }, error = function(e) {
      message(sprintf("Error in iteration %d: %s", i, e$message))
    })
  } 
}

property_infos <- list()
# collecting property info
for(i in seq_len(length(all_property_links))){
  tryCatch({
    page <- read_html(all_property_links[i])
    
    general_info <- page %>% 
      html_element('.Highlighted_wrapper__0tnnb h1') %>% 
      html_text2()
    
    street <- page %>%
      html_element(".ContentView_smallMapWrapper__k_DHE h4") %>%
      html_text2()
    
    price <- page %>% 
      html_elements("._91tt-n p") %>% 
      html_text2()
    
    district <- page %>%
      html_element(".ContentView_smallMapWrapper__k_DHE small") %>%
      html_text2()
    
    house_info <- page %>% 
      html_elements("[data-testid = 'house-main-info'] p") %>%
      html_text2()
    
    suite_idx <- grep("suíte", house_info, ignore.case = TRUE)
    non_suite_idx <- setdiff(seq_along(house_info), suite_idx)
    house_info <- house_info[c(non_suite_idx, suite_idx)]
    
    amenities <- page %>% 
      html_elements(".AmenitiesList_itemsWrapper__PLY3c span") %>% 
      html_text2() %>% 
      str_c(collapse = ", ")
    
    listing_id <- extract_listing_id(all_property_links[i])
    latlon <- get_latlon(page, listing_id)
    
    property_infos[[i]] <- tibble(
      apt_or_house = general_info,
      street = street,
      neighborhood = district,
      area_m2 = house_info[1],
      total_price = price[1],
      rent_price = price[2],
      bedrooms = house_info[2],
      bathrooms = house_info[3],
      parking_spots = house_info[4],
      floor = house_info[5],
      pets_allowed = house_info[6],
      furnished = house_info[7],
      near_subway = house_info[8],
      suites = house_info[9],
      amenities = amenities,
      latitude = latlon[1],
      longitude = latlon[2],
      listing_id = listing_id,
      scraped_date = Sys.Date()
    )
  }, error = function(e) {
    message(sprintf("Error in iteration %d: %s", i, e$message))
  })
}

#if you have already scraped something, comment the next line and uncomment the two that follow 
final_properties <- bind_rows(property_infos)
#properties_new <- bind_rows(property_infos)
#final_properties <- rbind(initial_properties, properties_new)



# filtering function
filter_listings <- function(base, price_limit = NULL, min_bedrooms = NULL, min_bathrooms = NULL, 
                            near_subway = NULL, min_area_m2 = NULL, furnished = NULL, 
                            bedroom_closet = NULL, kitchen_cabinet = NULL) {
  
  if (!is.null(min_bedrooms)) {
    base <- base %>% filter(bedrooms >= min_bedrooms)
  }
  if (!is.null(price_limit)) {
    base <- base %>% filter(total_price <= price_limit)
  }
  if (!is.null(min_bathrooms)) {
    base <- base %>% filter(bathrooms >= min_bathrooms)
  }
  if (!is.null(near_subway)) {
    base <- base %>% filter(near_subway == near_subway)
  }
  if (!is.null(min_area_m2)) {
    base <- base %>% filter(area_m2 >= min_area_m2)
  }
  if (!is.null(furnished)) {
    base <- base %>% filter(furnished == furnished)
  }
  if (!is.null(bedroom_closet) && bedroom_closet == 1) {
    base <- base %>% filter(str_detect(amenities, "Armários no quarto"))
  }
  if (!is.null(kitchen_cabinet) && kitchen_cabinet == 1) {
    base <- base %>% filter(str_detect(amenities, "Armários na cozinha"))
  }
  
  return(base)
}

write.csv(final_properties, file = "quinto_andar.csv", row.names = FALSE)
