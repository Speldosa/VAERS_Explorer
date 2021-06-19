# MedDRA key terms that we will be looking for here (not case sensitive, so it doesn't matter if you use uppercase or lowercase letters). If one of these words/phrases are found within a symptom listed for a person, he/she is considered a hit (for example "deafness" will be considered a hit in the MedDRA term "sudden deafness"). Otherwise, he/she is considered a miss.
keywords = c("deafnesss", "hearing", "tinnitus", "hyperacusis")

# Load necessary packages.
library(readr)

# Imort datasets. Note that you have to download these yourself and place them in the folder "2021VAERSData". You can download these at: https://vaers.hhs.gov/data/datasets.html?. I've been using the data contained in the 2021 Zip File.
dataset2021VAERSDATA <- read_csv("2021VAERSData/2021VAERSDATA.csv")
dataset2021VAERSSYMPTOMS <- read_csv("2021VAERSData/2021VAERSSYMPTOMS.csv")
dataset2021VAERSVAX <- read_csv("2021VAERSData/2021VAERSVAX.csv")

# Only keep rows in the vaccine data dataset that pertains to COVID19.
dataset2021VAERSVAX <- dataset2021VAERSVAX[dataset2021VAERSVAX$VAX_TYPE == "COVID19",]

# The Symptoms file lists symptoms in one of five columns (SYMPTOM1, SYMPTOM2, et cetera) using terms from the MedDRA dictionary. If a person experiences more than five symptoms, he/she can be represented using several rows. We'll start by combining all SYMPTOM columns for each row into a single column.

dataset2021VAERSSYMPTOMS$SYMPTOMS <- paste(dataset2021VAERSSYMPTOMS$SYMPTOM1, dataset2021VAERSSYMPTOMS$SYMPTOM2, dataset2021VAERSSYMPTOMS$SYMPTOM3, dataset2021VAERSSYMPTOMS$SYMPTOM4, dataset2021VAERSSYMPTOMS$SYMPTOM5, sep=", ")

# Then, we'll go through each cell of the combined symptoms all look for a match with regard to our keywords (specified further up).
dataset2021VAERSSYMPTOMS$KEYWORD_MATCH <- rep(FALSE, nrow(dataset2021VAERSSYMPTOMS))
dataset2021VAERSSYMPTOMS$VAX_MANU <- rep(NA, nrow(dataset2021VAERSSYMPTOMS))
for(a in 1:nrow(dataset2021VAERSSYMPTOMS))
{
  if(TRUE %in% sapply(toupper(keywords), grepl, toupper(dataset2021VAERSSYMPTOMS$SYMPTOMS[a])))
  {
    dataset2021VAERSSYMPTOMS$KEYWORD_MATCH[a] <- TRUE
  }
}

# Then, we'll create a datafile of symptoms where only the hits are kept.
dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH <- dataset2021VAERSSYMPTOMS[dataset2021VAERSSYMPTOMS$KEYWORD_MATCH,]

# Create vaccine data datasets for the four different type of vaccines (including Covid19).
dataset2021VAERSVAX_MODERNA <- dataset2021VAERSVAX[dataset2021VAERSVAX$VAX_MANU == "MODERNA",]
dataset2021VAERSVAX_PFIZER <- dataset2021VAERSVAX[dataset2021VAERSVAX$VAX_MANU == "PFIZER\\BIONTECH",]
dataset2021VAERSVAX_JANSSEN <- dataset2021VAERSVAX[dataset2021VAERSVAX$VAX_MANU == "JANSSEN",]
dataset2021VAERSVAX_UNKNOWN <- dataset2021VAERSVAX[dataset2021VAERSVAX$VAX_MANU == "UNKNOWN MANUFACTURER",]

# Pick out all rows in the symptoms data file pertaining to the four different vaccine types.
dataset2021VAERSSYMPTOMS_MODERNA <- dataset2021VAERSSYMPTOMS[(dataset2021VAERSSYMPTOMS$VAERS_ID %in% dataset2021VAERSVAX_MODERNA$VAERS_ID),]
dataset2021VAERSSYMPTOMS_PFIZER <- dataset2021VAERSSYMPTOMS[(dataset2021VAERSSYMPTOMS$VAERS_ID %in% dataset2021VAERSVAX_PFIZER$VAERS_ID),]
dataset2021VAERSSYMPTOMS_JANSSEN <- dataset2021VAERSSYMPTOMS[(dataset2021VAERSSYMPTOMS$VAERS_ID %in% dataset2021VAERSVAX_JANSSEN$VAERS_ID),]
dataset2021VAERSSYMPTOMS_UNKNOWN <- dataset2021VAERSSYMPTOMS[(dataset2021VAERSSYMPTOMS$VAERS_ID %in% dataset2021VAERSVAX_UNKNOWN$VAERS_ID),]

# Do the same thing as above but for the vaccine dataset containins only hits.
dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___MODERNA <- dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH[(dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH$VAERS_ID %in% dataset2021VAERSVAX_MODERNA$VAERS_ID),]
dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___PFIZER <- dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH[(dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH$VAERS_ID %in% dataset2021VAERSVAX_PFIZER$VAERS_ID),]
dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___JANSSEN <- dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH[(dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH$VAERS_ID %in% dataset2021VAERSVAX_JANSSEN$VAERS_ID),]
dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___UNKNOWN <- dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH[(dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH$VAERS_ID %in% dataset2021VAERSVAX_UNKNOWN$VAERS_ID),]

# Now, we can print some ratios of hits divided by total amount of cases.

# All cases
Hits_Total <- length(unique(dataset2021VAERSSYMPTOMS_WITH_KEYWORD_MATCH$VAERS_ID))
All_cases_Total <- length(unique(dataset2021VAERSSYMPTOMS$VAERS_ID))
cat(paste("Number of all cases hits: ", Hits_Total, sep=""))
cat(paste("Number of all cases: ", All_cases_Total, sep=""))
cat(paste("Percentage of all cases involving at least one of the keywords: ", Hits_Total/All_cases_Total, sep=""))

# Moderna
Hits_Moderna <- length(unique(dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___MODERNA$VAERS_ID))
All_cases_Moderna <- length(unique(dataset2021VAERSSYMPTOMS_MODERNA$VAERS_ID))
cat(paste("Number of Moderna hits: ", Hits_Moderna, sep=""))
cat(paste("Number of Moderna cases: ", All_cases_Moderna, sep=""))
cat(paste("Percentage of Moderna cases involving at least one of the keywords: ", Hits_Moderna/All_cases_Moderna, sep=""))

# Pfizer
Hits_Pfizer <- length(unique(dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___PFIZER$VAERS_ID))
All_cases_Pfizer <- length(unique(dataset2021VAERSSYMPTOMS_PFIZER$VAERS_ID))
cat(paste("Number of Pfizer hits: ", Hits_Pfizer, sep=""))
cat(paste("Number of Pfizer cases: ", All_cases_Pfizer, sep=""))
cat(paste("Percentage of Pfizer cases involving at least one of the keywords: ", Hits_Pfizer/All_cases_Pfizer, sep=""))

# Janssen
Hits_Janssen <- length(unique(dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___JANSSEN$VAERS_ID))
All_cases_Janssen <- length(unique(dataset2021VAERSSYMPTOMS_JANSSEN$VAERS_ID))
cat(paste("Number of Janssen hits: ", Hits_Janssen, sep=""))
cat(paste("Number of Janssen cases: ", All_cases_Janssen, sep=""))
cat(paste("Percentage of Janssen cases involving at least one of the keywords: ", Hits_Janssen/All_cases_Janssen, sep=""))

# Unknown
Hits_Unknown <- length(unique(dataset2021VAERSSYMPTOMS___WITH_KEYWORD_MATCH___UNKNOWN$VAERS_ID))
All_cases_Unknown <- length(unique(dataset2021VAERSSYMPTOMS_UNKNOWN$VAERS_ID))
cat(paste("Number of unknown hits: ", Hits_Unknown, sep=""))
cat(paste("Number of unknown cases: ", All_cases_Unknown, sep=""))
cat(paste("Percentage of Unknown cases involving at least one of the keywords: ", Hits_Unknown/All_cases_Unknown, sep=""))

# If you want to compare these numbers to total number of doses given in the USA for different vaccine manufacturers, you can find this data here: https://covid.cdc.gov/covid-data-tracker/#vaccinations