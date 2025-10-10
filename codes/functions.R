polish_to_iso3c <- function(countries) {
  
  # Mapping of Polish names to ISO3C codes
  map <- c(
    "afganistan" = "AFG", "albania" = "ALB", "algieria" = "DZA",
    "andora" = "AND", "arabia saudyjska" = "SAU", "argentyna" = "ARG", 
    "armenia" = "ARM", "australia" = "AUS", "austria" = "AUT", 
    "austia" = "AUT", "azerbejdżan" = "AZE",
    "bahamy" = "BHS", "bangladesz" = "BGD", "belgia" = "BEL",
    "benin" = "BEN", "bezpaństwowiec" = NA, "białoruś" = "BLR", 
    "birma" = "MMR", "boliwia" = "BOL",
    "bośnia i hercegowina" = "BIH", "brazylia" = "BRA", "bułgaria" = "BGR",
    "butgaria" = "BGR",
    "chiny" = "CHN", "chorwacja" = "HRV", "czarnogóra" = "MNE",
    "czechy" = "CZE", "cypr" = "CYP", "dania" = "DNK", 
    "demokratyczna republika konga" = "COD",
    "egipt" = "EGY", "ekwador" = "ECU", "estonia" = "EST", 
    "etiopia" = "ETH", "etipia" = "ETH", 
    "filipiny" = "PHL", "finlandia" = "FIN", "finlandaia" = "FIN",
    "francja" = "FRA", "gambia" = "GMB", "ghana" = "GHA",
    "grecja" = "GRC", "gruzja" = "GEO", "gwinea" = "GIN",
    "hiszpania" = "ESP", "holandia" = "NLD", 
    "indie" = "IND", "indonezja" = "IDN",
    "irak" = "IRQ", "iran" = "IRN", "irlandia" = "IRL",
    "islandia" = "ISL", "izrael" = "ISR", 
    "jamajka" = "JAM", "japonia" = "JPN",
    "jemen" = "YEM", "jordania" = "JOR", "jugosławia" = NA,
    "kamerun" = "CMR", "kanada" = "CAN", "kazachstan" = "KAZ",
    "kenia" = "KEN", "kirgistan" = "KGZ", "kolumbia" = "COL",
    "kongo" = "COG", "korea południowa" = "KOR", "kosowo" = "XKX",
    "kostaryka" = "CRI", "kuba" = "CUB",
    "liban" = "LBN", "liberia" = "LBR",
    "libia" = "LBY", "litwa" = "LTU", "luksemburg" = "LUX",
    "macedonia" = "MKD", "malawi" = "MWI",
    "malezja" = "MYS", "mali" = "MLI", "malta" = "MLT",
    "maroko" = "MAR", "meksyk" = "MEX", "mongolia" = "MNG",
    "mołdawia" = "MDA", "nepal" = "NPL", "niemcy" = "DEU",
    "nieustalone" = NA, "nigeria" = "NGA", "norwegia" = "NOR",
    "nowa zelandia" = "NZL", 
    "pakistan" = "PAK", "palestyna" = "PSE",
    "panama" = "PAN", "papua nowa gwinea" = "PNG", "paragwaj" = "PRY", 
    "peru" = "PER", "polinezja francuska" = "PYF", "portugalia" = "PRT",
    "powrót do spisu treści" = NA, "powrót do spisu" = NA,
    "rep.bośni i hercegowiny" = "BIH", 
    "rep.południowej afryki" = "ZAF",
    "republika bośni i hercegowiny" = "BIH", 
    "republika południowej afryki" = "ZAF",
    "rosja" = "RUS", "rpa" = "ZAF", "ruanda" = "RWA", "rumunia" = "ROU",
    "rwanda" = "RWA", "senegal" = "SEN", "serbia" = "SRB",
    "somalia" = "SOM", "sri lanka" = "LKA", 
    "stany zjednoczone ameryki" = "USA",
    "sudan" = "SDN", "syria" = "SYR", "szwajcaria" = "CHE",
    "szwecja" = "SWE", "słowacja" = "SVK", "słowenia" = "SVN",
    "tadżykistan" = "TJK", "tajlandia" = "THA", "tajwan" = "TWN", 
    "tanzania" = "TZA", "tunezja" = "TUN", "turcja" = "TUR", 
    "turkmenistan" = "TKM",
    "uganda" = "UGA", "ukraina" = "UKR", "urugwaj" = "URY",
    "usa" = "USA", "uzbekistan" = "UZB", 
    "wenezuela" = "VEN", "wielka brytania" = "GBR",
    "wietnam" = "VNM", "wybrzeże kości słoniowej" = "CIV", 
    "węgry" = "HUN", "włochy" = "ITA", 
    "zambia" = "ZMB", "zimbabwe" = "ZWE", 
    "zjednoczone emiraty arabskie" = "ARE",
    "łotwa" = "LVA"
  )
  
  map[str_to_lower(str_trim(countries))]
}

select_months <- function(months) {
  base <- str_remove(months, "\\s*korekta tabl")
  has_korekta <- str_detect(months, "korekta tabl")
  
  # Group by base and keep only one per group (prioritize korekta tabl)
  keep <- !duplicated(base) | (has_korekta & !duplicated(base[has_korekta]))
  
  # Better approach: for each base, if korekta exists, remove the non-korekta version
  to_remove <- !has_korekta & base %in% base[has_korekta]
  
  months[!to_remove]
}

month_to_number <- function(months) {
  setNames(1:12, c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec",
                   "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień")
  )[str_extract(months, "\\w+(?=\\s+\\d{4})")]
}
