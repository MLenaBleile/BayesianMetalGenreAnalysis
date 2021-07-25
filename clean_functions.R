get_data = function(dataset='original'){
  if(dataset == 'original'){
    full.data.set <- readr::read_csv('data/metal_bands_2017.csv')
  }else if(dataset == 'metallum'){
    full.data.set <- readr::read_csv("data/metallum.csv")}else{
      full.data.set <- readr::read_csv("data/metalstorm_scrape.csv")}
  
  if(dataset != 'original'){
    full.data.set$origin= full.data.set$country
    full.data.set$band_name = full.data.set$name
  }
  
  full.data.set}
  #View(full.data.set)
  
  
clean_country = function(full.data.set){
  countries <- unique(full.data.set$origin)
  length(countries)
  #View(sort(countries))
  sort(countries)
  
  ##Problem 1:  Some bands are associated with more than one country.  
  ## For these bands, I will assign them to the first country.
  
  ##Example:
  #USA.Cyprus <- full.data.set %>% filter(origin=="USA, Cyprus")
  #View(USA.Cyprus)
  
  ##There are 26 of these.  I think this has to be done manually.  So here goes.
  
  countries <- recode(full.data.set$origin,  
                      "Australia, United Kingdom"= "Australia",
                      "United Kingdom, USA" = "United Kingdom",
                      "Israel, The Netherlands" = "Israel",
                      "Tunisia, France" = "Tunisia",
                      "Colombia, USA" = "Colombia",
                      "Hungary, United Kingdom"  = "Hungary",
                      "Greece, Sweden" = "Greece",
                      "Sweden, Finland"  = "Sweden",
                      "Norway, Finland" = "Norway",
                      "Belgium, United Kingdom" = "Belgium",
                      "Russia, Canada" = "Russia",
                      "Iraq, USA" = "Iraq",
                      "Kyrgyzstan, Germany" = "Kyrgyzstan",
                      "Greece, USA"   = "Greece",
                      "Israel, Germany" = "Israel",
                      "Portugal, United Kingdom" = "Portugal",
                      "Iran, Norway" = "Iran",
                      "Finland, Sweden"  = "Finland",
                      "Lithuania, United Kingdom" = "Lithuania",
                      "Ukraine, Canada" = "Ukraine",
                      "Mexico, USA" = "Mexico",
                      "Canada, France" = "Canada",
                      "Lithuania, Germany" = "Lithuania",
                      "United Kingdom, Greece" = "United Kingdom",
                      "Italy, Spain" = "Italy",
                      "Poland, Sweden"  = "Poland",
                      "Denmark, USA" = "Denmark",
                      "USA, Cyprus"  = "USA" ,
                      "USA, Italy" = "USA",
                      "USA, Finland" = "USA",
                      "Bosnia and Herzegovina, Croatia"= "Bosnia and Herzegovina")
  
  new_countries = countries
  #print(length(countries))
  for(country_idx in 1:length(countries)){
    country= countries[country_idx]
    new_countries[country_idx] = stringr::str_split(country, ",")[[1]]
    #print(stringr::str_split(country, ",")[[1]])
  }
  
  ## There is one band with the country listed as NA.  Investigate!
  
  missing.origin <- full.data.set %>% filter(is.na(origin))
  ## Enshine - Sweeden (461)
  ## Clouds - United Kingdom (990)
  ##  Subterranean Masquerade - Israel (1423)
  ##  Twilight Of The Gods - Sweden (2615)
  ## Archivist - Austria (2997)
  ## Metallic Taste Of Blood - United Kingdom (3445)
  ## Level 10 - Canada (3942)
  ## Death Penalty  - United Kingdom (4202)
  if(dataset == 'original'){
    countries[c(461, 990, 1423, 2615, 2997, 3445, 3942, 4202)] <- 
      c("Sweden", "United Kingdom", "Israel", "Sweden", "Austria", "United Kingdom", 
        "Canada", "United Kingdom")
    
    unique(sort(new_countries))}else{
      full.data.set = full.data.set[!is.na(full.data.set$origin),]
    }
  region <- countrycode(new_countries,  origin = "country.name",destination = "region")
  
  updated.countries <- cbind(cbind(full.data.set, data.frame(countries=new_countries)), data.frame(region=region))
 updated.countries 
}
  
  #updated.countries$band_name=updated.countries$name
  
  ##Problem 2:  Some bands are listed multiple times.
  ## Let's compare the repeats.
  
remove_repeats = function(updated.countries){ ##5000
  length(unique(updated.countries$band_name))  ## 4949.  Thus there are up to 51 repeated bands.
  
  ## The vector of 51 repeated bands.
  repeats <- updated.countries %>% group_by(band_name) %>% count() %>% filter(n>1) %>% pull(band_name)
  
  ## Compare entries for repeated bands.
  
  repeated.band.entries <- updated.countries %>% filter(band_name %in% repeats) %>% arrange(band_name)
  #View(repeated.band.entries)
  
  
  
  without.band.repeats <- updated.countries %>% filter(!(band_name %in% repeats ))
  #%>% slice(c(51:4734, 4736:5000)) ##4949 rows
  #length(unique(without.band.repeats$band_name)) ## 8203 rows
  
  without.band.repeats}



