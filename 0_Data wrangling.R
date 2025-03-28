
# Section 0: Data loading and wrangling ----

library(tidyverse)
library(readxl)
library(stringi)


## Section 0.1 data loading and joining ----

#Loading data from round 1 and round 2

data_DE <- read.csv("data/data_full_DE.csv")
data_FR <- read.csv("data/data_full_FR.csv")
data_DK <- read.csv("data/data_full_DK.csv")
data_LV <- read.csv("data/data_full_LV.csv")
data_IT <- read.csv("data/data_full_IT.csv")

data_r2_DE<-read.csv("data/data_full_r2_DE.csv")
data_r2_FR<-read.csv("data/data_full_r2_FR.csv")
data_r2_DK<-read.csv("data/data_full_r2_DK.csv")
data_r2_LV<-read.csv("data/data_full_r2_LV.csv")
data_r2_IT<-read.csv("data/data_full_r2_IT.csv")

#Creating country variable
data_DE$country <- "Germany"
data_FR$country <- "France"
data_DK$country <- "Denmark"
data_LV$country <- "Latvia"
data_IT$country <- "Italy"

data_r2_DE$country <- "Germany"
data_r2_FR$country <- "France"
data_r2_DK$country <- "Denmark"
data_r2_LV$country <- "Latvia"
data_r2_IT$country <- "Italy"

#Setting the year for each round
data_DE$year <- 2021
data_FR$year <- 2021
data_DK$year <- 2021
data_LV$year <- 2021
data_IT$year <- 2021

data_r2_DE$year <- 2022
data_r2_FR$year <- 2022
data_r2_DK$year <- 2022
data_r2_LV$year <- 2022
data_r2_IT$year <- 2022

# Removing several not used variables prior to joining

for (data_name in c("data_DK",  "data_FR","data_DE","data_IT","data_LV",
                    "data_r2_DK", "data_r2_FR", "data_r2_DE", "data_r2_IT", "data_r2_LV")){
  data<-get(data_name)
  
  data <- data %>% 
    select(-starts_with("T"),-D1,-starts_with("DN"),-starts_with("DT"), -starts_with("M"),-starts_with("SD14"),-starts_with("SP5"), -starts_with("DA"),-starts_with("EID"), -starts_with("PO"),
           -c(E1, H2,SP4,WB1,WB2,v_442,v_338,v_339,v_340,v_341,v_342,v_343,v_344))
  
  assign(data_name, data)
}



data_long <- bind_rows(data_DE, data_FR, data_DK, data_LV, data_IT, data_r2_DE, data_r2_FR, data_r2_DK, data_r2_LV, data_r2_IT)


data_long<-data_long%>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()%>%
  filter(year==2021 | F1_new==1)#only respondents who did not move between 2021 and 2022


## Section 0.2 Data cleaning ----

#Removing accents as they are problematic later
data_long$H1_other <-stri_trans_general(data_long$H1_other, id = "Latin-ASCII")
data_long$GQ1_other <-stri_trans_general(data_long$GQ1_other, id = "Latin-ASCII")

#Manually recoding heating fuel source where necessary for the robustness check
data_long <- data_long %>%
  mutate(H1_other = sub("\\s*$", "", H1_other))%>%#remove empty space at end
  mutate(H1_other = sub("^\\s*", "", H1_other))%>%#remove empty space at beginning
  mutate(H1_other = tolower(H1_other)) %>%
  mutate(
    H1 = case_when(
      H1_other %in% c( "gas, holz","gas, holz ", "zentralheizung","gas til luft pumpe", "naturgas og elvarmepumpe", "centralfyr","butano",  "gas metano", "misto gas e pompa di calore","boiler","caldaia", "impianto riscaldamento centralizzato/condominiale","bombola",
                       "caldaia a gas", "metano", "gas condominiale",  "stufa a gas", "propano in bombole", "aria propanata", "stufa gas","condominio","riscaldamento a pavimento","zentralheizung mit warmwasserbereitung",
                       "chauffage collectif au gaz", "chauffage collectif gaz", "gas metano", "gas strom", "chaudiere au gaz", "gaza de ville", "chauffage collectif au gaz ","caldaia condominiale, non so da cosa sia alimentata",
                       "gaz et electricite","dabasgaze + skelda + saules kolektors (diversificeta centralizeta apkure)","zentral","gaz et electricite ","gaza de ville ", "mes izmantojam centralo apkuri", "anschluss an efh familie") ~ 1,
      H1_other == "bouteille butane" ~ 2,
      H1_other == "bio ethanol" ~ 3,
      H1_other %in% c("gasolio", "gasolio per riscaldamento", "kerosene", "carosene","portable a petrole ", "petrolio zibro camin",  "?alka","portable a petrole", "petrole", 
                      "fioul et bois","fioul et bois ", "fioul et electricite",  "?????","dizeldegviela", "мазут") ~ 4,
      H1_other %in% c("nachtstrom", "nachtspeicherheizung", "nachtspeicher (strom)", "dachs", "strom und holz","pellets pour incert ", "infrarot","el", "elvarme", "radiator", "el og braende", 
                      "el og vi har en vindmolle som ca. daekker 50% af vores forbrug", "elpaneler","stufa elettrica", "stufe elettriche", "termoventilatori", "termosifoni", 
                      "stufe elettriche", "el olievarmer","climatizzatore", "condizionatore","chauffage electrique", "termoventilatori", "clim reversible ","climatisation reversible ",
                      "electricite en complement ma chaudiere a ete un petit moment en panne", "electricite","collectif electrique ","nachtspeicherheizung ", "collectif electrique","stufe elettriche ", "chauffage electrique ","stufa elettrica ",
                      "electricite","elpaneler ", "chauffages personnels", "chaudiere", "chaudiere collective", "chauffage au sol") ~ 5,
      H1_other %in% c( "ibrido tra caldaia a condensazione e pompa di calore","el, varmepumpe og braendeovn", "el og varmepumpe","luft til vand ","varmepumpe ",
                       "solartermine und warmepumpe","varmepumpe", "varmepumpe i sommerhus", "luft til vand varmepumpe", "varmepumpe", "jordvarme", "jordvarme ", "luft til vand",
                       "erdwarme", "solartermine und warmepumpe","climatisation reversible", "clims reversibles", "clim reversible", "climatisation reversible", "geothermie", 
                       "bade varmepumpen og braendeovn", "solartermine und warmepumpe ",
                       "nachtspeicherheizung", "climatiseur reversible", "jordvarme", "luft til  vand", "climatiseur", "clim reversible", "climatisation reversible", 
                       "climatisation reversible gainable", "climatisation reversible", "clim reversible", "chauffage collectif geothermie", "geothermie",  "geothermie ",
                       "aerothermie", "geothermie", "geothermie", "climatisation reversible") ~ 6,
      H1_other %in% c("chauffage collectif urbain", "urbain", "chauffage urbain", "chauffage urbaine", "chauffage collectif", "chauffage collectif", "chauffage collectif",
                      "chauffage colletif","chauffage  collectif","collective", "collectif",  "collective",  "collectif","fjernvarme hjemme og el og braende i sommerhuset",
                      "fjernvarme", "centralizeta apkure","zentrale heizung fur ca. 30 hauser","riscaldamento centralizzato","chauffage urbain ","riscaldamento autonomo",
                      "riscaldamento condominiale","centralapkure", "chauffage urbain ","fernwarme", "centralizetais siltums un elektriskais siltumsuknis") ~ 7, 
      H1_other %in% c("stufa pellet", "pellet","pelletofen ","pellets ","poele a granule ","poele a granules ","poele a pellets ","pelletts und holz ", "stufa a pellet", "stufa a pellets", "stufa a legna",  "riscaldamento pellet",  "pellet ","granules de bois ","granulas ","stufa a pellet ","stufa pellet ",
                      "stufe", "stufa pellets","pellets", "stufa", "stufe", "braendeovm","braendeovn ", "traepiller", "braendeovn", "pelletts und holz","pelletofen", "skaidu granulas", "skelda",
                      "briketes","briketes", "kokskaidu granulas", "kamins - briketes", "granulu katls", "granulas un saules kolektori", "soltumsuknis","braende", "halmfyr", 
                      "braendeovn og luft til luft varmepumpe", "braendeovn", "traepiller", "traepiller", "trae piller", "braendeovn og varmepumpe", 
                      "og pilleovn", "flis / savsmuld", "halm fyr", "trae og flaskegas", "braendeovn + luft-vand varmepumpe","bois",  "legna o bucce di mandorla", "poele a pellets",
                      "malka + elektriba", "poele a granules", "poele a pellets", "koka granulas", "granula","granulu apkure","malka", "granulas", "poele a granules", 
                      "poele a pellets","pillrovn tim traepiller, luft-luft varmepumpe, gas", "poil a bois", "skaidu briketes","poele a pelets",  "poele a granules", "granules de bois", "peles", 
                      "poele a granules", "poele a granules", "cheminee bois", "pellets pour incert",  "cheminee", "bois recuperation", "chauffage collectif au bois", 
                      "chaudiere granules bois", "poele a bois", "poil a granule", "granules de bois", "poele a pellets", "poele a granule", "poele a granules", "insere a pellet", "malka un akmenogles",
                      "chaudiere granules bois", "pelets","kamin", "akmenogles un malka") ~ 8,
      H1_other %in% c("solar", "passivhaus", "windgas") ~ 9,
      H1_other %in% c("ogles", "ogle", "kohle", "kohlenofen") ~ 11,
      T ~ as.numeric(H1)))
  

#Manually recoding building type where necessary
data_long <- data_long %>%
  mutate(GQ1_other = sub("\\s*$", "", GQ1_other))%>%#remove empty space at end
  mutate(GQ1_other = sub("^\\s*", "", GQ1_other))%>%#remove empty space at beginning
  mutate(GQ1_other = tolower(GQ1_other)) %>%
  mutate(GQ1 = case_when(
    GQ1_other %in% c("hus pa landet", "privat leje kun en lejilghed",  "landejendo.", "parcelhus",  "et hus i 3 etager men kun et lejemal", "bungalow","land ejderdom",
                     "landbrugsejendom",  "lejlighed i tilbygning til udlejers hus", "nedlagt landbrug", "fritidshus med tilladelse", "nedlagt landbrug a", 
                     "mit hus", "gard ude pa landet med husdyrhold (heste)", "land ejendom", "er der forskel pa enfamiliehus og parcelhus?", "kolonihave hus pa andelsgrund", 
                     "kaedevilla", "stuehus, landbrug", "ejendom pa landet", "landbrugsejendom", "egen hus", "deltidslandbrug", "hus til leje", "nedlagt landejendom", 
                     "landbrug", "landejendom", "sommerhus", "ejendom.",  "lejet hus", "landbrug","et hus i 3 etager men kun et lejemal", "villalejlighed", "udlejning hus",
                     "gard pa landet", "hus", "villa", "gard","casa unifamiliare con sopra ed a fianco altri appartamenti con entrate separate", "villetta su due piani", 
                     "palazzo storico", "cascinale", "einfamilienhaus bungalow",  "ein bewohntes einfamilienhaus", "einfamilienhaus bungalow","maison de village", 
                     "maison individuelle sans terrain", "maison rurale", "ein klein haus", "mit sommerhus", "maja ar 3 istabam","maja laukos", "lauku maja", 
                     "viengimenes maja atseviski stavosa", "ireta privatmaja", "privatmaja", "2 stavu sencu celta maja (kopipasums)", "parbuveta vasarnica", "personiga maja", 
                     "privata maja", "maja", "neparcclos", "neparcelos","neparcelos", "privatmaja 2 stavi","4 stavu eka", "divstavu privatmaja","einfamilienhaus bungalow ohne  wohnung",
                     "vienseta") ~ 1,
    GQ1_other %in% c("dobbelt hus med 2 halve huse", "anneks", "nedlagt ejendom", "raekkehus leje", "appartement dans une maison", 
                     "casa bifamiliare confinante con altre unita abitative",  "rindu dzivoklis","villetta bifamiliare", "bifamiliare", "edificio bifamiliare",
                     "bifamiliare", "casa famigliare su 2 appartamenti", "casa singola bifamiliare", "condominio d'epoca", "borgo medioevale", "villetta bifamiliare","zweifamilienhaus",
                     "divgimenes maja", "rindu dzivoklis", "2 stavu sencu celta maja (kopipasums)", "privata maja, kur dziivoja 2 gimenes", "2 stavl", "2 stavi","casa bifamiliare") ~ 2,
    GQ1_other %in% c("2 stavi, 6dzivokli", "daudzdzivoklu maja 2 stavi", "rindu maja (5 dzivokli pec kartas)", "daudzdzivoklu maja, tris stavi", "daudzivoklu maja ( lidz 3 saviem)", 
                     "4 dzivoklu maja", "daudzdzivoklu maja ar 3 staviem", "daudzdzivoklu maja 3 staviem", "laiku maja", "tris istabu dzivoklis", "irets dzivoklis", "dzivoklis",  
                     "mietwohnung", "wohnung mit 2 zimmer", "wohnung",  "eigentumswohnung","appartamento in condominio di 6 unita abitative", "condominio con 8 appartamenti", 
                     "piccolo condominio", "palazzina con 3 appartamenti","unica casa ma con tre unita abitative", "edificio di 4 unita abitative","appartement dans une ancienne maison bourgeoise avec jardin, dont nous sommes proprietaires","mehrfamilienhaus (6)", "appartamento", "condominio",  
                     "piccolo condominio di campagna", "piccolo condomini di campagna", "biappartamento", "appartamennto", "aeldrebolig", "ejendom blandet erhverv og privat") ~ 3,
    GQ1_other %in% c("enfamilieshu omdannet til 8 lejligheder", "raekkehus kompleks ialt 98 boliger", "bofaellesskab", "gammel skole", "vaerelses udlejning", "15 kvadratmeter vaerelse", 
                     "andelsbolig", "andelsboligforening", "mindre gard med 4 bo enheder som jeg ejer","lejlighed","ejerlejlighed", "ældrebolig", 
                     "i en ejerlejlighed, det har jag da vist skrevet tidligere :-)","ejerlejlighed", "lejlighed", "andelsbolig lejlighed", "kollegie", "kollegium", "en studiebolig", "mietswohnung","wohngruppe",
                     "privat leje kun en lejlighed", "lycee", "batiment de 4 grands etages, 8 logements", "appartement", "immeuble classique", "raekkehus kompleks ialt 98 boliger", 
                     "ejerlejlighed", "lejebolig", "andelsbolig", "rbnb", "logement de fonction", "centre d'herbergement", "appartement duplex", "appartement avec 2 etages") ~ 3,
    GQ1_other %in% c("hlm", "locataire d'un logement dans un immeuble de 19 appartements", "appartement dans une copropriete", "appartement social","appartamento 10 unita", 
                     "casa in affitto dell'agec", "palazzina di 5 piani e 23 appartamenti", "edifici popolari", "caserma",  "hochhaus 13 etagen","eine zwei-zimmer-wohnung","miet-wohnung im mehrfamilienhaus","eine wohnung","eine mietwohnung","eine 3 zimmer wohnung" ) ~ 4,
    T ~ GQ1
  ))

 

# Replace missing values in 2022 with the corresponding 2021 values for specified variables
vars_to_replace <- c("GQ1", "GQ3", "GQ4", "urban", "SD12", "SD4", "HW1", "HW3")
data_long <- data_long %>% 
  arrange(PersistantID, year) %>%
  group_by(PersistantID) %>%
  mutate(across(all_of(vars_to_replace), ~ ifelse(year == 2022 & is.na(.), lag(.), .))) %>%
  ungroup()

# Replace missing values in 2022 with the corresponding 2021 values for specified variables
vars_to_replace <- c("H10")
data_long <- data_long %>% 
  arrange(PersistantID, year) %>%
  group_by(PersistantID) %>%
  mutate(across(all_of(vars_to_replace), ~ ifelse(year == 2021 & is.na(.), lead(.), .))) %>%
  ungroup()

# Recode specific numeric missing codes to NA for all numeric variables
numeric_vars <- data_long %>% select(where(is.numeric)) %>% names()
for (var in numeric_vars) {
  data_long[[var]][data_long[[var]] %in% c(-99, -77, -66)] <- NA
}

# For a specific list of variables, also recode 99 and 66 to NA
vars_specific <- c("SD4", "Income",  
                   "H1", "HW2",  "HW1", "HW3")
for (var in vars_specific) {
  if (var %in% names(data_long) && is.numeric(data_long[[var]])) {
    data_long[[var]][data_long[[var]] %in% c(99, 66)] <- NA
  }
}

data_long <- data_long %>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()


# Remove if failed both attention checks
data_long <- data_long %>%
  mutate(ac1_cor = ifelse((year == 2021 & AC1 == 6) | (year == 2022 & AC1 == 8), 1, 0),
         ac2_cor = ifelse(SO1_3 == 4, 1, 0))%>%
  filter(ac1_cor == 1, ac2_cor == 1)%>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()

# Remove inconsistent age
data_long <- data_long %>% 
  arrange(year)%>%
  arrange(PersistantID) %>%
   group_by(PersistantID) %>%
   mutate(prev_age = lag(Age),
          flag = if_else(year == 2022 & (Age == prev_age | Age == prev_age + 1 | Age == prev_age + 2), 1, 0)) %>%
  select(-prev_age) %>%  # Remove temporary column
  ungroup()%>%
  filter(!(year == 2022 & flag == 0)) %>% 
  select(-flag)%>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()



# Remove change in gender
data_long <- data_long %>% 
  arrange(year)%>%
  arrange(PersistantID) %>%
  group_by(PersistantID) %>%
  mutate(flag = ifelse(year == 2022 & (Gender == lag(Gender)), 1, 0)) %>%
  ungroup() %>% 
  filter(!(year == 2022 & flag == 0)) %>% 
  select(-flag)%>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()


# Setting unrealistic values to NA:
# If heated area (H9) is larger than living area (GQ4), set H9 to NA
data_long <- data_long %>% 
  arrange(PersistantID, year) %>%
  group_by(PersistantID) %>%
  mutate(H9 = ifelse(H9 > GQ4, NA_real_, H9)) %>%
  ungroup()

# For SP3 and H9, set unrealistic values to NA
data_long <- data_long %>%
  mutate(SP3 = ifelse(SP3 > 21 | SP3 < 1, NA_real_, SP3),
         H9 = ifelse(H9 < 9, NA_real_, H9))

# Create variable 'female': 0 if Gender equals 1, 1 if Gender equals 2
data_long <- data_long %>%
  mutate(female = case_when(
    Gender == 1 ~ 0,
    Gender == 2 ~ 1
  ))

# Remove if no heating in either year

data_long <- data_long %>%
  filter(!H1_other%in%c("indossando una maglia in piu'","non abbiamo usato il riscaldamento perche il reddito non e sufficiente","nessun riscaldamento","non abbiamo riscaldamento","nulla",
                        "pas de chauffage"))%>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()

          
# Create variable 'big_price_incr'
data_long <- data_long %>%
  arrange(PersistantID, year) %>%
  group_by(PersistantID) %>%
  mutate(H10=ifelse(H10==6,NA,H10))%>% #setting "I don't know" to missing
  mutate(big_price_incr = ifelse(H10 == 5 , 1, 0)) %>%
  ungroup()


# Rename variables
data_long <- data_long %>%
  rename(worry = v_271,
         cold = v_273)

# Create dummy variables for household size variables (SD9_1, SD9_2, SD9_3): replace missing with 0
HH_vars <- c("SD9_1", "SD9_2", "SD9_3")
for (var in HH_vars) {
  if (var %in% names(data_long)) {
    data_long[[var]][is.na(data_long[[var]])] <- 0
  }
}

# Generate heated area per person using OECD weights
data_long <- data_long %>%
  mutate(H9_pp_OECD = H9 / (SD9_1 * 0.3 + SD9_2 * 0.3 + 1 + (SD9_3 - 1) * 0.5))

# Generate 'room_temp': use H6_1 (thermometer) if available; otherwise, use H6_2 (estimate)
data_long <- data_long %>%
  mutate(room_temp = ifelse(!is.na(H6_1), H6_1, H6_2))

# For energy deptivation variables: recode such that 1 becomes 0 and values 2,3,4 become 1
vars <- c("worry", "cold")
for (var in vars) {
  data_long[[var]] <- ifelse(data_long[[var]] == 1, 0,
                              ifelse(data_long[[var]] %in% c(2, 3, 4), 1, data_long[[var]]))
}

# Rename Income to income_detailed
data_long <- data_long %>% rename(income_detailed = Income)

# Create income_detailed_eur based on country-specific mappings
data_long <- data_long %>% mutate(
  income_detailed_eur = case_when(
    country == "Germany" & income_detailed == 1 ~ 3600,
    country == "Germany" & income_detailed == 2 ~ 5400,
    country == "Germany" & income_detailed == 3 ~ 9600,
    country == "Germany" & income_detailed == 4 ~ 18100,
    country == "Germany" & income_detailed == 5 ~ 29300,
    country == "Germany" & income_detailed == 6 ~ 38100,
    country == "Germany" & income_detailed == 7 ~ 45400,
    country == "Germany" & income_detailed == 8 ~ 52850,
    country == "Germany" & income_detailed == 9 ~ 60950,
    country == "Germany" & income_detailed == 10 ~ 70200,
    country == "Germany" & income_detailed == 11 ~ 81600,
    country == "Germany" & income_detailed %in% c(12, 13) ~ 88000,
    
    country == "France" & income_detailed == 1 ~ 3600,
    country == "France" & income_detailed == 2 ~ 5400,
    country == "France" & income_detailed == 3 ~ 9600,
    country == "France" & income_detailed == 4 ~ 18100,
    country == "France" & income_detailed == 5 ~ 29300,
    country == "France" & income_detailed == 6 ~ 38100,
    country == "France" & income_detailed == 7 ~ 45400,
    country == "France" & income_detailed == 8 ~ 52850,
    country == "France" & income_detailed == 9 ~ 60950,
    country == "France" & income_detailed == 10 ~ 70200,
    country == "France" & income_detailed == 11 ~ 81600,
    country == "France" & income_detailed %in% c(12, 13) ~ 88000,
    
    country == "Italy" & income_detailed == 1 ~ 3600,
    country == "Italy" & income_detailed == 2 ~ 5400,
    country == "Italy" & income_detailed == 3 ~ 9600,
    country == "Italy" & income_detailed == 4 ~ 18100,
    country == "Italy" & income_detailed == 5 ~ 29300,
    country == "Italy" & income_detailed == 6 ~ 38100,
    country == "Italy" & income_detailed == 7 ~ 45400,
    country == "Italy" & income_detailed == 8 ~ 52850,
    country == "Italy" & income_detailed == 9 ~ 60950,
    country == "Italy" & income_detailed == 10 ~ 70200,
    country == "Italy" & income_detailed == 11 ~ 81600,
    country == "Italy" & income_detailed %in% c(12, 13) ~ 88000,
    
    country == "Latvia" & income_detailed == 1 ~ 3000,
    country == "Latvia" & income_detailed == 2 ~ 4500,
    country == "Latvia" & income_detailed == 3 ~ 6750,
    country == "Latvia" & income_detailed == 4 ~ 8250,
    country == "Latvia" & income_detailed == 5 ~ 10500,
    country == "Latvia" & income_detailed == 6 ~ 13500,
    country == "Latvia" & income_detailed == 7 ~ 16500,
    country == "Latvia" & income_detailed == 8 ~ 19500,
    country == "Latvia" & income_detailed == 9 ~ 22500,
    country == "Latvia" & income_detailed %in% c(10, 11) ~ 24000,
    
    country == "Denmark" & income_detailed == 1 ~ 63700 * 0.134243,
    country == "Denmark" & income_detailed == 2 ~ ((62700 + 127400) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 3 ~ ((191100 + 127400) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 4 ~ ((250000 + 127400) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 5 ~ ((250000 + 308000) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 6 ~ ((419600 + 308000) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 7 ~ ((419600 + 530200) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 8 ~ ((650000 + 530200) / 2) * 0.134243,
    country == "Denmark" & income_detailed == 9 ~ ((650000 + 800000) / 2) * 0.134243,
    country == "Denmark" & income_detailed %in% c(10, 11) ~ 800000 * 0.134243,
    TRUE ~ NA
  )
)

# Create income_pp and income_pp_1000 variables
data_long <- data_long %>%
  mutate(
    income_pp = income_detailed_eur / (SD9_1 * 0.3 + SD9_2 * 0.3 + 1 + (SD9_3 - 1) * 0.5),
    income_pp_1000 = income_pp / 1000
  )

# Create additional variables: children, urban type, education dummies, housing and employment status
data_long <- data_long %>%
  mutate(
    city = if_else(urban == 1, 1, 0),
    high_educ = if_else(SD4 == 5, 1, 0),
    owner = case_when(
      SD12 == 2 ~ 1,
      SD12 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    sfh = ifelse(GQ1 %in% c(1, 2), 1, 0)
  )


# Create country dummy variables
data_long <- data_long %>%
  mutate(
    DK = ifelse(country == "Denmark", 1, 0),
    FR = ifelse(country == "France", 1, 0),
    DE = ifelse(country == "Germany", 1, 0),
    IT = ifelse(country == "Italy", 1, 0),
    LV = ifelse(country == "Latvia", 1, 0)
  )

# Heating fuel recoding
data_long <- data_long %>%
  mutate(
    heat_fuel = case_when(
      H1 == 1 ~ "NG",
      H1 == 2 ~ "LPG",
      H1 == 3 ~ "Biogas",
      H1 == 4 ~ "oil",
      H1 %in% c(5, 6) ~ "electricity",
      H1 == 7 ~ "district",
      H1 == 8 ~ "wood",
      H1 == 9 ~ "solar",
      TRUE ~ "other"
    )
  )


# Rename dependent variables: H9_pp_OECD becomes pp_area, SP3 becomes shower_freq
data_long <- data_long %>% 
  rename(pp_area = H9_pp_OECD,
         shower_freq = SP3)


# If one year is missing for a variable, then set both years to NA
for (var in vars_outcome) {
  data_long <- data_long %>% 
    group_by(PersistantID) %>%
    mutate(flagmissing = ifelse(is.na(.data[[var]]), 1, 0)) %>%
    mutate(flagmissing = ifelse(lag(flagmissing, default = 0) == 1 & year == 2021 | 
                                   lead(flagmissing, default = 0) == 1 & year == 2022, 1, 0)) %>%
    ungroup() %>%
    mutate(!!sym(var) := ifelse(flagmissing == 1, NA_real_, .data[[var]])) %>%
    select(-flagmissing)
}

# Calculate difference variables for the outcome variables
for (var in vars_outcome) {
  diff_var <- paste0("diff_", var)
  data_long <- data_long %>% 
    arrange(PersistantID, year) %>%
    group_by(PersistantID) %>%
    mutate(!!sym(diff_var) := ifelse(year == 2022, .data[[var]] - lag(.data[[var]]),
                                      lead(.data[[var]]) - .data[[var]])) %>%
    ungroup()
}


# Create dummy variables for energy deprivation variables: diff_worry and diff_cold
for (var in c("diff_worry", "diff_cold")) {
  new_var <- paste0(var, "_dum")
  data_long[[new_var]] <- ifelse(data_long[[var]] %in% c(-1, 0), 0, 
                                  ifelse(data_long[[var]] == 1, 1, NA_real_))
}


data<-data_long%>%
  group_by(PersistantID) %>%
  filter(n_distinct(year) == 2) %>%  # Keep only IDs with both years
  ungroup()%>%
  filter(year==2021)#so as to keep each respondent only once in the dataset


## Section 0.3 saving the data ----

write_csv(data, "data/clean_data.csv")






