`%notin%` = Negate(`%in%`)
tol <- as.data.frame(read.spss("data/tol.sav"))
tol_full <- tol
# tol_full$cntry[which(tol_full$cntry == "EI")] <- "IE"
# tol_full$cntry[which(tol_full$cntry == "ES")] <- "EA"
# tol_full$cntry[which(tol_full$cntry == "SL")] <- "SI"
# tol_full$cntry[which(tol_full$cntry == "UK")] <- "GB"
tol <- na.omit(tol)
agg_3 <- as.data.frame(read.spss("data/agg_3.sav"))
agg_9 <- as.data.frame(read.spss("data/agg_9.sav"))
theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- select(world, name, continent)
world <- subset(world, (continent %in% c("Europe","Asia","Africa")))
world1 <- world
world1$name <- recode(world$name,"Austria" ~ "AT", "Belgium" ~ "BE", "Bulgaria" ~ "BG", "Switzerland"~ "CH",
                      "Cyprus" ~ "CY",  "Germany" ~ "DE", "Denmark"  ~ "DK", "Estonia"    ~ "EE",
                      "Spain" ~ "ES",   "Finland" ~ "FI", "France" ~"FR",    "United Kingdom" ~"UK",
                      "Hungary"~"HU",   "Ireland"~"EI",   "Netherlands" ~"NL",
                      "Norway"~"NO",    "Poland"~"PL",    "Portugal"~"PT",   "Russia" ~ "RU",
                      "Sweden"~"SE",    "Slovenia"~"SL",  "Slovakia"~"SK",   "Ukraine"~"UA",
                      "Czech Rep."~"CZ",   "Italy"~"IT",     "Serbia"~"RS", "Belarus" ~ "BY",
                      "Lithuania" ~ "LT", "Latvia" ~ "LV", "Iceland" ~ "IS", "Morocco" ~ "MA",
                      "Liechtenstein" ~ "LI", "Luxembourg" ~ "LU", "Algeria" ~ "DZ",
                      "Tunisia" ~ "TN", "Turkey" ~ "TR", "Georgia" ~ "GE", "Azerbaijan" ~ "AZ")
world1 <- na.omit(world1)

world_2006 <- merge(world1, agg_3, by = "name")
world_2018 <- merge(world1, agg_9, by = "name")

world_2006$year <- 2006
world_2018$year <- 2018
world <- rbind(world_2006, world_2018)

world$year <- as.factor(world$year)

world$name[which(world$name == "EI")] <- "IE"
world$name[which(world$name == "ES")] <- "EA"
world$name[which(world$name == "SL")] <- "SI"
world$name[which(world$name == "UK")] <- "GB"



world$code <- tolower(world$name)

world_full <- ne_countries(scale = "medium", returnclass = "sf") %>% select(name, continent)

{code <- sort(c("AT","BE","BG","CH","CY","DE","DK","EA","EE","FI","FR","GB","HU",
                "IE","NL","NO","PL","PT","RU","SE","SI","SK","UA","CZ","IT","RS"))
  iso <- data.frame(code)
  iso$country <- recode(code, "AT"~"Austria","BE"~"Belgium","BG"~"Bulgaria","CH"~"Switzerland","CY"~"Cyprus",
                        "DE"~"Germany","DK"~"Denmark","EA"~"Spain","FI"~"Finland","FR"~"France",
                        "GB"~"United Kingdom","HU"~"Hungary","IE"~"Ireland","NL"~"Netherlands","NO"~"Norway",
                        "PL"~"Poland","PT"~"Portugal","RU"~"Russia","SE"~"Sweden","SI"~"Slovenia",
                        "SK"~"Slovakia","UA"~"Ukraine","CZ"~"Czechia","IT"~"Italy","RS"~"Serbia",
                        "EE"~"Estonia")
  iso1 <- iso[1:9,]
  iso2 <- iso[10:18,]
  iso3 <- iso[19:26,]} # ISO