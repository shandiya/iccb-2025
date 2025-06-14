# GBIF table --------
# table of all GBIF participants copied and pasted in here from
# https://www.gbif.org/the-gbif-network on 5 June 2025. Scraping with rvest
# would probably have been better practice but this was quicker

gbif_tbl <- data.frame(
  Participant = c(
    "Albertine Rift Conservation Society",
    "Amazon Cooperation Treaty Organization",
    "Andean Network of BioNET-INTERNATIONAL",
    "Andorra",
    "Angola, Republic of",
    "Argentina",
    "Armenia, Republic of",
    "ASEAN Centre for Biodiversity",
    "Australia",
    "Belgium",
    "Benin",
    "Biodiversity Heritage Library",
    "Bioversity International",
    "Botanic Gardens Conservation International",
    "Brazil",
    "Burundi",
    "Cambodia",
    "Cameroon",
    "Canada",
    "Canadensys",
    "Central African Republic",
    "Chile",
    "Chinese Academy of Sciences",
    "Chinese Taipei",
    "Ciencia y Tecnología para el Desarrollo",
    "Colombia",
    "Consortium of European Taxonomic Facilities",
    "Costa Rica",
    "Croatia, Republic of",
    "Denmark",
    "Discover Life",
    "Distributed System of Scientific Collections",
    "East Asia Biodiversity Conservation Network",
    "Ecuador",
    "Encyclopedia of Life",
    "Endangered Wildlife Trust",
    "Estonia",
    "European Environment Agency",
    "Finland",
    "France",
    "Georgia",
    "Germany",
    "Guatemala, Republic of",
    "Guinea",
    "Horn of Africa Regional Environment Centre and Network",
    "Iceland",
    "ICLEI – Local Governments for Sustainability",
    "iDigBio",
    "Integrated Taxonomic Information System",
    "International Barcode of Life Consortium",
    "International Centre for Insect Physiology and Ecology",
    "International Centre for Integrated Mountain Development",
    "International Long Term Ecological Research",
    "Ireland",
    "Korea, Republic of",
    "Liberia",
    "LifeWatch ERIC",
    "Luxembourg",
    "Madagascar",
    "Malawi",
    "Mauritania",
    "Mexico",
    "Mongolia",
    "Namibia",
    "Natural Science Collections Alliance",
    "NatureServe",
    "Netherlands",
    "New Zealand",
    "Nigeria, Federal Republic of",
    "Nordic Genetic Resource Center",
    "Norway",
    "Observation International",
    "Panama",
    "Peru",
    "Plazi",
    "Poland",
    "Portugal",
    "Scientific Committee on Antarctic Research",
    "Secretariat of the Pacific Regional Environment Programme",
    "Sierra Leone",
    "Slovakia",
    "Slovenia",
    "Society for the Preservation of Natural History Collections",
    "South Africa",
    "South Sudan",
    "Spain",
    "Species 2000",
    "Specify Collections Consortium",
    "Suriname, Republic of",
    "Sweden",
    "Switzerland",
    "Symbiota Support Hub",
    "Tajikistan, Republic of",
    "Tanzania, United Republic of",
    "Taxonomic Databases Working Group",
    "Timor-Leste, Democratic Republic of",
    "Togo",
    "Tonga, Kingdom of",
    "UN Environment World Conservation Monitoring Centre (UNEP-WCMC)",
    "United Kingdom",
    "United States",
    "Uruguay",
    "Uzbekistan",
    "VertNet",
    "World Federation for Culture Collections",
    "Zimbabwe"
  ),
  Membership = c(
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Associate country participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Associate country participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Associate country participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Associate country participant",
    "Voting participant",
    "Associate country participant",
    "Associate country participant",
    "Other associate participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Associate country participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Associate country participant",
    "Voting participant",
    "Associate country participant",
    "Associate country participant",
    "Associate country participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Voting participant",
    "Other associate participant",
    "Associate country participant",
    "Voting participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Associate country participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Associate country participant",
    "Associate country participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Voting participant",
    "Other associate participant",
    "Voting participant",
    "Voting participant",
    "Associate country participant",
    "Voting participant",
    "Other associate participant",
    "Other associate participant",
    "Voting participant"
  ))

dplyr::filter(gbif_tbl, 
              Membership == "Voting participant" | 
                Membership == "Associate country participant") |> 
  saveRDS("data/gbif_tbl.RDS")
