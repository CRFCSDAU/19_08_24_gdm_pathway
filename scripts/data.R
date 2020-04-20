
  library(readxl)
  library(tidyverse)
  library(growthstandards)

# Data -------------------------------------------------------------------------

# There are 3 datasets. This is to get them all into one file.

# Function for removing blank columns and rows
  remove_blank <- function(data){
    data <- data[, unlist(lapply(data, function(x) !all(is.na(x))))] # blank cols
    data <- data[rowSums(is.na(data)) != ncol(data),] # Remove blank rows
    return(data)
  }

# Read in the 3 datasets, 1 for each of 3 years.
  gdm_1 <- read_excel("data/GDM 2014.xls", na = c("99", "N/A"),
                      .name_repair = ~ make.names(., unique = TRUE)) %>%
    remove_blank() %>% mutate(year = 2014)
  gdm_2 <- read_excel("data/GDM 2015.xlsx", na = c("99", "N/A"),
                      .name_repair = ~ make.names(., unique = TRUE))[1:393, ] %>%
    remove_blank() %>% mutate(year = 2015)
  gdm_3 <- read_excel("data/GDM 2016.xlsx", na = c("99", "N/A"),
                      .name_repair = ~ make.names(., unique = TRUE)) %>%
    remove_blank() %>% mutate(year = 2016)

# Make nicer names, with no effort
  names(gdm_1) <- tolower(names(gdm_1))
  names(gdm_2) <- tolower(names(gdm_2))
  names(gdm_3) <- tolower(names(gdm_3))

# Based on earlier work, change these names to match up across datasets.

  names(gdm_3)[which(names(gdm_3) == "feeding.on.discharge")] <-
    "breastfeeding.on.discharge"

  gdm_1$attended.post.partum.clinic <- NA

  gdm_3$education.level <- NA

  gdm_2$hypoglycemia.in.infant <- NA
  gdm_3$hypoglycemia.in.infant <- NA

# In 2016, the numbers and note in the sheet suggest that ODV is really
# indicating ID
  gdm_3$instrumental.delivery <- gdm_3$operative.vaginal.delivery - 1
  gdm_3$operative.vaginal.delivery <- NA

  names(gdm_1)[which(names(gdm_1) == "insulin.dose.at.end")] <-
    "final.insulin.dose"

  names(gdm_1)[which(names(gdm_1) == "operative.delivery")] <-
    "cesarean.section"
  names(gdm_2)[which(names(gdm_2) == "operative.delivery")] <-
    "cesarean.section"

# table(gdm_3$cesarean.section, gdm_3$category.cs.operation)
  gdm_3$cesarean.section[gdm_3$cesarean.section == 2 &
                         !is.na(gdm_3$cesarean.section)] <- 0

  gdm_3$cesarean.section[gdm_3$category.cs.operation %in% c(3, 4) &
                         !is.na(gdm_3$cesarean.section)] <- 1

  gdm_3$cesarean.section[gdm_3$category.cs.operation %in% c(1, 2) &
                           !is.na(gdm_3$cesarean.section)] <- 2

  gdm_2$trauma.to.baby__1 <- NA

  gdm_1$baby.sex.1 <- NA
  gdm_1$baby.sex.2 <- NA

  gdm_1$body.fat.mass <- NA
  gdm_1$body.fat.percentage <- NA
  gdm_1$body.muscle.mass <- NA
  gdm_1$body.muscle.percentage <- NA
  gdm_1$body.weight <- NA
  gdm_1$ategory.cs.operation <- NA
  gdm_1$days.from.dx.to.clinic <- NA
  gdm_2$eason.for.admission__1 <- NA
  gdm_1$distance.travelled.km <- NA
  gdm_1$viceral.fat.rating <- NA
  gdm_1$vitamin.d <- NA
  gdm_1$note <- NA
  gdm_1$other <- NA


  names(gdm_2)[which(names(gdm_2) == "total.in.km")] <-
    "distance.travelled.km"

  names(gdm_2)[which(names(gdm_2) == "x__2")] <-
    "note"

  names(gdm_3)[which(names(gdm_3) == "x__1")] <-
    "note"

  names(gdm_3)[which(names(gdm_3) == "days.between.dx.to.clinic")] <-
    "days.from.dx.to.clinic"

  names(gdm_2)[which(names(gdm_2) == "eason.for.admission__1")] <-
    "reason.for.admission__1"

  names(gdm_1)[which(names(gdm_1) == "ategory.cs.operation")] <-
    "category.cs.operation"

  names(gdm_3)[which(names(gdm_3) == "indication..cs")] <-
    "indication_cs"
  names(gdm_2)[which(names(gdm_2) == "reason.for.elective.cs")] <-
    "indication_cs"
  gdm_1$indication_cs <- NA

# Other errors, caught below
  gdm_3$patient.type <- gdm_3$patient.type - 1 # to match the other 2 years
  gdm_3$folic.acid.intake <- gdm_3$folic.acid.intake - 1
  gdm_3$folic.acid.intake <- ifelse(
    gdm_3$folic.acid.intake == -1, 0, gdm_3$folic.acid.intake
  )

  gdm_3$phx.gdm <- ifelse(gdm_3$phx.gdm == 0, 0, 1)
  gdm_2$phx.gdm <- ifelse(gdm_2$phx.gdm == 2, 0, 1)
  gdm_1$phx.gdm <- ifelse(gdm_1$phx.gdm == 2, 0, 1)

  gdm_3$phx.pcos <- ifelse(gdm_3$phx.pcos == 0, 0, 1)
  gdm_2$phx.pcos <- ifelse(gdm_2$phx.pcos == 2, 0, 1)
  gdm_1$phx.pcos <- ifelse(gdm_1$phx.pcos == 2, 0, 1)


# Skip centiles...I can recalcuate as needed
# Skip jaundice; respiratory distress syndrome;
# hypoglycemia in gdm_1 is just a repeat of the matched "hypoglycaemia";
# bmi_1;

# names(gdm_match)[order(names(gdm_match))]

# What's the longest dataset?
  max_names <- max(c(length(names(gdm_1)), length(names(gdm_2)),
                     length(names(gdm_3))))


# # View all the names lined up in a dataframe to get a sense of how bad it is
  all_names <- data_frame(
    "2014" = c(names(gdm_1), rep("", max_names - length(names(gdm_1)))),
    "2015" = c(names(gdm_2), rep("", max_names - length(names(gdm_2)))),
    "2016" = c(names(gdm_3), rep("", max_names - length(names(gdm_3))))
    ); rm(max_names)

# # View(all_names)
# FML

# Put them all together
# Step 1: split off all the columns for each dataframe where they share a name
# gdm2 is the longest
  n1 <- names(gdm_1)[names(gdm_1) %in% names(gdm_2)]
  n2 <- names(gdm_3)[names(gdm_3) %in% names(gdm_2)]
  n_match <- n1[n1 %in% n2]; rm(n1, n2)

# Set everything to character so it can be rbound (needs same class),
# and everything can be character without risking being set to NA
  gdm_1_match <- gdm_1[n_match] %>% mutate_all(as.character)
  gdm_2_match <- gdm_2[n_match] %>% mutate_all(as.character)
  gdm_3_match <- gdm_3[n_match] %>% mutate_all(as.character)

# bind_rows can handle the different column orders
  gdm_match <- bind_rows(gdm_1_match, gdm_2_match, gdm_3_match)

# Check they are all character
# table(sapply(gdm_match, class))

# Convert  to numeric all those that can be set as numeric without any missings
  gdm_match <- mutate_if(
    gdm_match,
    function(x){all(is.na(x) == is.na(as.numeric(x)))},
    as.numeric
  )

# Check to see that some are now numeric
# table(sapply(gdm_match, class))

# Look at names that aren't in the matched datasets

# other_1 <- names(gdm_1)[!names(gdm_1) %in% names(gdm_match)]
# other_1 <- other_1[order(other_1)]
#
# other_2 <- names(gdm_2)[!names(gdm_2) %in% names(gdm_match)]
# other_2 <- other_2[order(other_2)]
#
# other_3 <- names(gdm_3)[!names(gdm_3) %in% names(gdm_match)]
# other_3 <- other_3[order(other_3)]
#
#
# max_other <- max(c(length(other_1), length(other_2), length(other_3)))
#
# other_names <- data_frame(
#   "2014" = c(other_1, rep("", max_other - length(other_1))),
#   "2015" = c(other_2, rep("", max_other - length(other_2))),
#   "2016" = c(other_3, rep("", max_other - length(other_3)))
# )

# View(other_names)
# View(gdm_1[other_1]) # Don't need this.
# View(gdm_2[other_2]) # A mess.
# View(gdm_3[other_3]) # Don't need this.
# names(gdm_match)[order(names(gdm_match))]
#
# names(gdm_1)[grepl("date", names(gdm_1))]
# names(gdm_2)[grepl("date", names(gdm_2))]
# names(gdm_3)[grepl("date", names(gdm_3))]

# View(select(gdm_match, starts_with("date")))

  data <- gdm_match
  names(data) <- gsub("\\.", "_", gsub("__", "_", names(data)))

# Variables --------------------------------------------------------------------

  data <- rename(data,
    basal_insulin_start_date = date_commenced,
    prandial_insulin_start_date = date_commeced
    )

# id -----
# TODO not unique
  data <- rename(data, id = patient_identifier)

# Twins/live infants

  data$live_infant_2[data$live_infant_2 == 0] <- NA
  data$live_infant_2 <- data$live_infant_2 - 1

  data$live_infant_1[data$live_infant_1 == 0 & !is.na(data$live_infant_1)] <- 2

  data$live_infant_1 <- factor(data$live_infant_1, labels = c("Alive", "Died"))
  data$live_infant_2 <- factor(data$live_infant_2, labels = c("Alive", "Died"))

# age ----
# TODO - unreasonable values
# View(filter(data, age < 15 | age >50))

# county - these aren't counties, but cities
  data$county <- tolower(data$county)
  data$county[grepl("cork", data$county)] <- "cork"
  target <- c("blarney", "dungarvan", "macroom", "mallow", "waterford")
  data$county[data$county %in% target] <- "cork"
  data$county <- factor(data$county)

# nationality TODO - fix this mess
  data$nationality <- tolower(data$nationality)

  tar <- data$nationality[grepl("rish|rsh", data$nationality)]
  data$irish_nat <- ifelse(grepl("rish", data$nationality), "Irish", "Not Irish")
  data$irish_nat <- str_to_title(data$irish_nat)
  data$irish_nat[is.na(data$nationality)] <- NA


# ethnicity TODO - fix this mess
  data$ethnicity <- tolower(data$ethnicity)

  tar <- data$ethnicity[grepl("rish", data$ethnicity)]
  data$irish_eth <- ifelse(grepl("rish", data$ethnicity), "Irish", "Not Irish")
  data$irish_eth <- str_to_title(data$irish_eth)
  data$irish_eth[is.na(data$ethnicity)] <- NA
  data$irish_eth <- factor(data$irish_eth)

  data$ethnicity[grepl("rish|rsh", data$ethnicity)] <- "Irish"
  data$ethnicity[data$ethnicity == "bangladesh"] <- "bangladeshi"
  data$ethnicity[data$ethnicity == "bangladeshian"] <- "bangladeshi"
  data$ethnicity[data$ethnicity == "bulgaria"] <- "bulgarian"
  data$ethnicity[data$ethnicity == "black african"] <- "african"
  data$ethnicity[data$ethnicity == "british iranian"] <- "iranian"
  data$ethnicity[data$ethnicity == "brtish"] <- "british"
  data$ethnicity[data$ethnicity == "congo"] <- "congalese"
  data$ethnicity[data$ethnicity == "germany"] <- "german"
  data$ethnicity[data$ethnicity == "ghana"] <- "ghanaian"
  data$ethnicity[data$ethnicity == "ghaniau"] <- "ghanaian"
  data$ethnicity[data$ethnicity == "iirsh"] <- "irish"
  data$ethnicity[data$ethnicity == "inidan"] <- "indian"
  data$ethnicity[data$ethnicity == "ghaniau"] <- "ghanaian"
  data$ethnicity[data$ethnicity == "ghaniau"] <- "ghanaian"
  data$ethnicity[data$ethnicity == "irish/pakistani"] <- "pakistani"
  data$ethnicity[data$ethnicity == "islam"] <- "muslim"
  data$ethnicity[data$ethnicity == "kenya"] <- "kenyan"
  data$ethnicity[data$ethnicity == "liberia"] <- "liberian"
  data$ethnicity[data$ethnicity == "lithuania"] <- "lithuanian"
  data$ethnicity[data$ethnicity == "oman"] <- "omani"
  data$ethnicity[data$ethnicity == "pakistan"] <- "pakistani"
  data$ethnicity[data$ethnicity == "poland"] <- "polish"
  data$ethnicity[data$ethnicity == "portuges"] <- "portuguese"
  data$ethnicity[data$ethnicity == "portugese"] <- "portuguese"
  data$ethnicity[data$ethnicity == "romanian"] <- "roamanian"
  data$ethnicity[data$ethnicity == "russia"] <- "russian"
  data$ethnicity[data$ethnicity == "saudi"] <- "saudi arabian"
  data$ethnicity[data$ethnicity == "saudi arabia"] <- "saudi arabian"
  data$ethnicity[data$ethnicity == "slavakian"] <- "slovakian"
  data$ethnicity[data$ethnicity == "south afrixcan"] <- "south african"
  data$ethnicity[data$ethnicity == "sri lanka"] <- "sri lankan"
  data$ethnicity[data$ethnicity == "sirilankan"] <- "sri lankan"
  data$ethnicity[data$ethnicity == "sudan"] <- "sudanese"
  data$ethnicity[data$ethnicity == "ukrania"] <- "ukranian"
  data$ethnicity[data$ethnicity == "uzbackistan"] <- "uzbekistani"
  data$ethnicity[data$ethnicity == "white irish"] <- "irish"
  data$ethnicity[data$ethnicity == "somalian"] <- "somali"

  data$ethnicity <- str_to_title(data$ethnicity)


# table(paste(data$nationality, data$ethnicity))

# From https://www.hse.ie/eng/health/az/d/diabetes,-gestational/diagnosing-gestational-diabetes-.html
# your family origins are South Asian (specifically India, Pakistan or
# Bangladesh), black Caribbean or Middle Eastern (specifically Saudi Arabia,
# United Arab Emirates, Iraq, Jordan, Syria, Oman, Qatar, Kuwait, Lebanon or
# Egypt)

  tar <- c("Black", "Indian", "Pakistani", "Bangladeshi", "Jamaican",
           "Sri Lankan", "Jamaican", "Angolan", "Congalese", "Ghanaian",
           "Kenyan", "Malawian", "Nigerian", "Sudanese", "Angolan", "Ethiopian",
           "Liberian", "Moroccan", "Nepali", "Saudi Arabian", "Omani",
           "African", "South African")

  data$risky_eth <- ifelse(
    data$ethnicity %in% tar,
    "Yes",
    "No"
  )

  data$risky_eth[is.na(data$ethnicity)] <- NA
  data$risky_eth <- factor(data$risky_eth)

# occupation TODO - fix this mess
  data$occupation <- tolower(data$occupation)

# education - TODO - why so many missings

# occupation.partner TODO - fix this mess
  data <- rename(data, occupation_partner = occupation_of_partner)
  data$occupation_partner <- tolower(data$occupation_partner)

# marital_status
  data$marital_status <- factor(
    data$marital_status, levels = c(0:5),
    labels = c("unknown", "married", "single", "seperated", "widow", "divorced")
  )

# patient_type
  data$patient_type <- factor(
    data$patient_type, levels = c(0:1),
    labels = c("public", "private")
  )

# consultant - TODO - fix this mess

# medical_hx_of_relevance -Medical history - TODO fix this mess
# data$medical_hx_of_relevance

# diabetes - not enough to worry about type of diabetes
  data$diabetes[data$diabetes> 1 & !is.na(data$diabetes)] <- 1
  data$diabetes <- factor(as.numeric(data$diabetes), levels = c(0:1),
                          labels = c("Not diabetic", "Diabetic"))

# thyroid - not enough to worry about type
   data$thyroid[data$thyroid == "overactive" & !is.na(data$thyroid)] <- 1
   data$thyroid[data$thyroid > 1 & !is.na(data$thyroid)] <- 1
   data$thyroid <- factor(
     as.numeric(data$thyroid),
     levels = c(0:1),
     labels = c("Normal Thyroid", "Abnormal Thyroid")
     )

# hypertension - too few to worry about type
   data <- mutate(data,
     hypertension = case_when(
     hypertension == "0" ~ 0,
     hypertension != "0" ~ 1
     )
   )

 # depression - too few to worry about type
  data <- mutate(data,
    depression = case_when(
    depression == "0" ~ 0,
    depression != "0" ~ 1
    )
  )

# previous_bariatric_surgery
  data <- rename(data, bariatric = previous_bariatric_surgery)
  data <- mutate(data,
    bariatric = case_when(
    bariatric == 0 ~ 0,
    bariatric != 0 ~ 1
    )
  )

# smoking
  data$smoking[data$smoking == 3 & !is.na(data$smoking)] <- 2
  data$smoking <- factor(
    data$smoking, levels = c(0:2),
    labels = c("never", "pregnancy", "previous/not in pregnancy")
    )

# cigs_per_day
  data <- rename(data, cigs_per_day = no_of_ciggarettes_per_day)
  miss <- !is.na(data$cigs_per_day)
  data$cigs_per_day[data$cigs_per_day == "1/day" & miss] <- 1
  data$cigs_per_day[data$cigs_per_day == "5-10" & miss] <- 10
  data$cigs_per_day[data$cigs_per_day == "5/day" & miss] <- 5
  data$cigs_per_day <- as.numeric(data$cigs_per_day)

# alcohol
  data$alcohol[data$alcohol == 3 & !is.na(data$alcohol)] <- 2
  data$alcohol <- factor(
    data$alcohol, levels = c(0:2),
    labels = c("never", "pregnancy", "previous/not in pregnancy")
  )

# units_per_week
  data <- rename(data, units_per_week = no_of_units__week)
  miss <- !is.na(data$units_per_week)
  data$units_per_week[data$units_per_week == "2-3 units" & miss] <- 2
  data$units_per_week[data$units_per_week == "4 to 5" & miss] <- 5
  data$units_per_week[data$units_per_week == "5-6 units prior ." & miss] <- 6
  data$units_per_week[data$units_per_week == "occasional" & miss] <- 7
  data$units_per_week[data$units_per_week == "one-two" & miss] <- 1
  data$units_per_week[data$units_per_week == "Rarley" & miss] <- NA
  data$units_per_week[data$units_per_week == "unknown" & miss] <- NA
  data$units_per_week <- as.numeric(data$units_per_week)

# gravida - TODO check outlier (17)

# parity - TODO make sure removing the +X was ok. Tidy up implausible values.
  miss <- !is.na(data$parity)
  data$parity[data$parity == "o" & miss] <- 0
  data$parity <- gsub("\\+.*", "", data$parity) # remove + and everything after
# ggplot(data, aes(parity, gravida)) + geom_jitter() # Not bad

# multiple_pregnancy_currently
  data <- rename(data, singleton = multiple_pregnancy_currently)
  data <- mutate(data,
    singleton = case_when(
    singleton == 2 ~ "Yes",
    singleton != 2 ~ "No"
    )
  ) %>% mutate(singleton = factor(singleton))

# multiple_preg TODO not a perfect match with singleton
  data <- rename(data, multiple_preg = type)
  data <- mutate(data,
    multiple_preg = case_when(
    multiple_preg == 0 ~ "No",
    multiple_preg == 1 ~ "Twins",
    multiple_preg == 2 ~ "Triplets"
    )
  ) %>% mutate(multiple_preg = factor(multiple_preg))

# edd
  data$edd <- as.POSIXct(data$edd, format = "%Y-%m-%d", tz = "GMT")

# data$weight
# data$height
  data$weight <- as.numeric(data$weight) # + 1 missing

  target <- data$weight < 10 & !is.na(data$weight) # 4 heights and weights swapped
  x <- data$weight[target]
  y <- data$height[target]
  data$weight[target] <- y
  data$height[target] <- x; rm(x, y)

# target <- data$height > 3 & !is.na(data$height) # Are others swapped?
# data$weight[target]
# data$height[target] # No
  data$height <- ifelse(data$height > 3, data$height/100, data$height)
# height TODO correct implausible values. Set to missing for now.
  data$height[data$height < 1.4] <- NA

# ggplot(data, aes(height, weight)) + geom_jitter()

# bmi
  data$bmi_2 <- data$weight / (data$height^2)
# ggplot(data, aes(bmi, bmi_2)) + geom_point() # Close, solves outlier, replace
  data$bmi <- data$bmi_2
  data <- select(data, -bmi_2)
# ggplot(data, aes(height, bmi)) + geom_jitter()
# ggplot(data, aes(weight, bmi)) + geom_jitter() # Looks good

# bmi.class replace the existing

  levs <- c("Underweight", "Normal", "Overweight", "Obese", "Very obese")
  data <- mutate(data,
    bmi_class = case_when(
      bmi <  19            ~ levs[1],
      bmi >= 19 & bmi < 25 ~ levs[2],
      bmi >= 25 & bmi < 30 ~ levs[3],
      bmi >= 30 & bmi < 40 ~ levs[4],
      bmi >= 40            ~ levs[5]
    )
  ) %>% mutate(bmi_class = factor(bmi_class, levels = levs)); rm(levs)

# ggplot(data, aes(x = bmi, fill = bmi_class)) +
#   geom_histogram()


# folic_acid ----
  data <- rename(data, folic_acid = folic_acid_intake)
  data$folic_acid <- factor(
    data$folic_acid, levels = c(0:2),
    labels = c("none", "preconceptual", "postconception")
  )

# age ----

  data$age[data$age < 15 | data$age > 50] <- NA

  data$over_40 <- factor(ifelse(data$age >= 40, "Yes", "No"))

# Gestational age at GDM ----

  data <- rename(data, gdm_date = date__u_2206__gdm,
                 gdm_gstwks = gestation_age__u_2206_)

# table(data$gdm_gstwks)

  data$gdm_gstwks <- gsub("\\s+", "", data$gdm_gstwks)
  data$gdm_gstwks[data$gdm_gstwks == "28/30" &
                  !is.na(data$gdm_gstwks)] <- "29"
  data$gdm_gstwks[data$gdm_gstwks == "Educatedat35weeks" &
                  !is.na(data$gdm_gstwks)] <- "35"
  data$gdm_gstwks[data$gdm_gstwks == "misseddx37" &
                  !is.na(data$gdm_gstwks)] <- "37"
  data$gdm_gstwks[data$gdm_gstwks == "Misseddx38+2" &
                  !is.na(data$gdm_gstwks)] <- "38"
  data$gdm_gstwks[data$gdm_gstwks == "8+" &
                    !is.na(data$gdm_gstwks)] <- "8"
  data$gdm_gstwks[data$gdm_gstwks == "n/a"] <- NA

  data$gdm_weeks <- as.numeric(data$gdm_gstwks) # Move over those without "+"
# data$gdm_gstwks[!is.na(data$Weeks)] <- NA # Set to missing if moved over
  tar <- is.na(data$gdm_weeks) & !is.na(data$gdm_gstwks)
  data$gdm_weeks[tar] <- sapply(strsplit(data$gdm_gstwks[tar], "\\+"), `[`, 1) %>%
    unlist() %>% as.numeric()
  data$gdm_days <- sapply(strsplit(data$gdm_gstwks, "\\+"), `[`, 2) %>%
    unlist() %>% as.numeric()
  tar <- is.na(data$gdm_days) & !is.na(data$gdm_gstwks)
  data$gdm_days[tar] <- 0

  data$gdm_gstdays <- data$gdm_days + (data$gdm_weeks * 7)

# Move just the pre + part

  data$gdm_over30wks <- factor(ifelse(data$gdm_gstdays/7 > 30, "Yes", "No"))

  data <- mutate(data, gdm_gest_cat = case_when(
    gdm_gstdays/7 >= 30 ~ "30+",
    gdm_gstdays/7 < 24 ~ "< 24",
    gdm_gstdays/7 >=24 & gdm_gstdays/7 < 30 ~ "24 to 30"
  ))

  data$gtt_gstweeks <- data$gdm_gstdays/7

  data$gtt_location[grepl("[[:alpha:]]", data$gtt_location)] <- 4
  data$gtt_location <- factor(
    as.numeric(data$gtt_location),
    labels = c("CUMH", "GP", "Private Rooms", "Other")
  )

# People who do and dont meet any GTT criteria

  data$gtt_criteria <- factor(ifelse(
    data$x1hr_pp >= 10 | data$x2hr_pp >= 8.5 | data$fasting >= 5.1,
    "Met any criteria",
    "Did not meet any criteria"
    ))

  data$gtt_criteria[is.na(data$gtt_criteria)] <- "Did not meet any criteria"

# Each criteria

  data$fasting_criteria <- factor(ifelse(data$fasting >= 5.1, "Yes", "No"))
  data$onehour_criteria <- factor(ifelse(data$x1hr_pp >= 10,  "Yes", "No"))
  data$towhour_criteria <- factor(ifelse(data$x2hr_pp >= 8.5, "Yes", "No"))

# previous gdm

  data$no_of_previous_gdm_episodes[data$no_of_previous_gdm_episodes >1 &
                                   !is.na(data$no_of_previous_gdm_episodes)] <- 2

  data$no_of_previous_gdm_episodes <- factor(data$no_of_previous_gdm_episodes,
                                             labels = c("0", "1", "2+"))

# Biggest baby

  nm <- !is.na(data$weight_of_biggest_baby)
  data$weight_of_biggest_baby[data$weight_of_biggest_baby == ">4500" & nm] <- 4500
  data$weight_of_biggest_baby[data$weight_of_biggest_baby == "0"] <- NA
  data$weight_of_biggest_baby <- as.numeric(data$weight_of_biggest_baby)



# Key dates --------------------------------------------------------------------
# GDM date
# table(data$gdm_date)[1:2]

  data$gdm_date <- as.POSIXct(as.numeric(data$gdm_date) * 60 * 60 * 24,
             origin = "1899-12-30") # A few additional missing with this.

# table(data$date_1st_visit_to_clinic)

  data$first_clinic_date <- as.POSIXct(
    as.numeric(data$date_1st_visit_to_clinic) * 60 * 60 * 24,
    origin = "1899-12-30"
    )

  data$date_1st_visit_to_clinic <- as.POSIXct(
    data$date_1st_visit_to_clinic,
    format = "%Y-%m-%d", tz = "GMT"
    )

  data$first_clinic_date[is.na(data$first_clinic_date) &
                         !is.na(data$date_1st_visit_to_clinic)] <-
    data$date_1st_visit_to_clinic[is.na(data$first_clinic_date) &
                                    !is.na(data$date_1st_visit_to_clinic)]

  data$first_clinic_date[data$first_clinic_date < "2000-01-01"] <- NA

# View(filter(data, first_clinic_date > gdm_date))
# View(filter(data, first_clinic_date <= gdm_date))

  data$edd[data$edd < "2000-01-01"] <- NA
  data$edd[data$edd == "2104-11-13 GMT" & !is.na(data$edd)] <- "2014-11-13 GMT"
  data$edd[data$edd == "3015-03-21 GMT" & !is.na(data$edd)] <- "2015-03-21 GMT"

  data$delivery_date_ <- as.POSIXct(
    as.numeric(data$delivery_date) * 60 * 60 * 24,
    origin = "1899-12-30"
  )

  data$delivery_date <- as.POSIXct(
    data$delivery_date,
    format = "%Y-%m-%d", tz = "GMT"
  )

  data$delivery_date[is.na(data$delivery_date) &
                           !is.na(data$delivery_date_)] <-
    data$delivery_date_[is.na(data$delivery_date) &
                                    !is.na(data$delivery_date_)]

  data$delivery_date[data$delivery_date < "2013-01-01"] <- NA
  data$delivery_date[data$delivery_date == "3015-03-19 GMT" &
                     !is.na(data$delivery_date)] <- "2015-03-19 GMT"


# table(data$date_of_scan)

  data$scan_date <- as.POSIXct(
    as.numeric(data$date_of_scan) * 60 * 60 * 24,
    origin = "1899-12-30"
  )

  data <- select(data, -delivery_date_, -date_1st_visit_to_clinic,
                 -date_of_scan)


  data$date_seen_by_endocrinology[as.numeric(data$date_seen_by_endocrinology)
                                %in% c(0, 2)] <- NA
  data$date_seen_by_endocrinology <- as.POSIXct(
    as.numeric(data$date_seen_by_endocrinology) * 60 * 60 * 24,
    origin = "1899-12-30"
  )

  data$basal_insulin_start_date[as.numeric(data$basal_insulin_start_date)
                                   %in% c(0, 2)] <- NA
  data$basal_insulin_start_date <- as.POSIXct(
    as.numeric(data$basal_insulin_start_date) * 60 * 60 * 24,
    origin = "1899-12-30"
  )


  data$prandial_insulin_start_date[data$prandial_insulin_start_date == 0] <- NA
  data$prandial_insulin_start_date <- as.POSIXct(
    as.numeric(data$prandial_insulin_start_date) * 60 * 60 * 24,
    origin = "1899-12-30"
  )
  data$prandial_insulin_start_date[data$prandial_insulin_start_date < "2013-01-01"] <- NA

# select(data, id, year, edd, contains("date")) %>%
#   gather(event, time, edd, contains("date")) %>%
#   filter(time < max(time, na.rm = TRUE)) %>%
#   ggplot(aes(x = time, color = event, y = factor(id))) +
#     geom_point() +
#    facet_wrap(~year, ncol = 1)



# Gestation at delivery

  data$del_gstwks <- gsub("\\s+", "", data$gestation_at_delivery) # remove spaces
# table(data$del_gstwks)

  data$del_gstwks[data$del_gstwks == "42500" &
                    !is.na(data$del_gstwks)] <- "42+5"
  data$del_gstwks[data$del_gstwks == "37(BIBApostdelivery)" &
                    !is.na(data$del_gstwks)] <- "37"
  data$del_gstwks[data$del_gstwks == "38/40" &
                    !is.na(data$del_gstwks)] <- "39"
  data$del_gstwks[data$del_gstwks == "39/40" &
                    !is.na(data$del_gstwks)] <- "39"
  data$del_gstwks[data$del_gstwks == "t+1" &
                    !is.na(data$del_gstwks)] <- "40+1"
  data$del_gstwks[data$del_gstwks == "T+1" &
                    !is.na(data$del_gstwks)] <- "40+1"
  data$del_gstwks[data$del_gstwks == "t+5" &
                    !is.na(data$del_gstwks)] <- "40+5"
  data$del_gstwks[data$del_gstwks == "term" &
                    !is.na(data$del_gstwks)] <- "40"
  data$del_gstwks[data$del_gstwks == "39=1" &
                    !is.na(data$del_gstwks)] <- "39+1"
  data$del_gstwks[data$del_gstwks %in% c("40+11", "40+10") &
                    !is.na(data$del_gstwks)] <- "40+1"

  data$del_gstwks[data$del_gstwks == "0"] <- NA
  data$del_gstwks[data$del_gstwks == "32/40"] <- NA
  data$del_gstwks[data$del_gstwks == "DELIVEREDINCOOMBE"] <- NA
  data$del_gstwks[data$del_gstwks == "didnotdeliverincumh"] <- NA

  data$del_weeks <- as.numeric(data$del_gstwks) # Move over those without "+"
  # data$del_gstwks[!is.na(data$Weeks)] <- NA # Set to missing if moved over
  tar <- is.na(data$del_weeks) & !is.na(data$del_gstwks)
  data$del_weeks[tar] <- sapply(strsplit(data$del_gstwks[tar], "\\+"), `[`, 1) %>%
    unlist() %>% as.numeric()
  data$del_days <- sapply(strsplit(data$del_gstwks, "\\+"), `[`, 2) %>%
    unlist() %>% as.numeric()
  tar <- is.na(data$del_days) & !is.na(data$del_gstwks)
  data$del_days[tar] <- 0

  data$del_gstdays <- data$del_days + (data$del_weeks * 7)
  data$del_gstweeks <- data$del_gstdays / 7

# Hypoglecemia

# table(data$hypoglycaemia_in_infant)
# table(data$hypoglycemia_in_infant)
# table(data$hypoglycaemia_in_infant, data$hypoglycemia_in_infant) # they overlap

  data <- select(data, -hypoglycemia_in_infant)

# Remove 2 with notes (not = 1 | 2)
  data$hypoglycaemia_in_infant[nchar(data$hypoglycaemia_in_infant) > 2] <- NA

  data$hypoglycaemia_in_infant <- factor(
    as.numeric(data$hypoglycaemia_in_infant),
    labels = c("Yes", "No")
    )

# Macrosomic

# table(data$large_baby__4_5kg)
# table(nchar(data$large_baby__4_5kg))
# data$birth_weight_1[nchar(data$large_baby__4_5kg) == 4] There don't match
# table(data$year, data$large_baby__4_5kg)

 data$macrosomic_4.5 <- ifelse(data$birth_weight_1 >= 4500, "Yes", "No")
 data$macrosomic_4.5[is.na(data$birth_weight_1)] <- NA
 data$macrosomic_4.5 <- factor(data$macrosomic_4.5)
# table(data$macrosomic_4.5)

 data$macrosomic_4 <- ifelse(data$birth_weight_1 >= 4000, "Yes", "No")
 data$macrosomic_4[is.na(data$birth_weight_1)] <- NA
 data$macrosomic_4 <- factor(data$macrosomic_4)

# table(data$macrosomic_4)

# Induction

  data$induction <- factor(
   as.numeric(data$induction),
   labels = c("Yes", "No")
  )

# Instrumental delivery
  data$instrumental_delivery <- factor(
    as.numeric(data$instrumental_delivery),
    labels = c("Nothing", "Vacuum", "Forceps", "Combined")
  )

# Cesarian

  data$cesarean_section <- factor(data$cesarean_section,
                                  labels = c("None", "Elective", "Emergency"))

# table(data$operative_vaginal_delivery, data$instrumental_delivery)
# table(data$instrumental_delivery, data$year)
# table(data$operative_vaginal_delivery, data$year)

# Shoulder dytocia

  data$shoulder_dystocia <- factor(
    as.numeric(data$shoulder_dystocia),
    labels = c("Yes", "No")
  )

# PPH

# table(data$pph)

  data$pph[data$pph %in% c(0, 3)] <- NA

  data$pph <- factor(
    as.numeric(data$pph),
    labels = c("Yes", "No")
  )

# Maternal trauma - too messy so make binary

# table(data$maternal_trauma)

  data$any_maternal_trauma <- data$maternal_trauma
  data$any_maternal_trauma[data$any_maternal_trauma != 0] <- 1

  data$any_maternal_trauma <- factor(
    as.numeric(data$any_maternal_trauma),
    labels = c("No", "Yes")
  )

# NICU - too messy so make binary

# table(data$nicu_admission, data$year)

  data$nicu_admission[data$nicu_admission == 2 & data$year == 2016] <- NA
  data$nicu_admission[data$nicu_admission == 0 & data$year == 2015] <- NA
  data$nicu_admission[data$nicu_admission == 0 & data$year == 2016 &
                        !is.na(data$nicu_admission)] <- 2

  data$nicu_admission <- factor(
    data$nicu_admission,
    labels = c("Yes", "No")
  )

# Body fat

  data$body_fat_percentage[data$body_fat_percentage == 0] <- NA

# Previous history GDM

  data$phx_gdm <- factor(data$phx_gdm, labels = c("No", "Yes"))

# Previous history PCOS

  data$phx_pcos <- factor(data$phx_pcos, labels = c("No", "Yes"))

# Parity

  data$parity <- as.numeric(data$parity)

  data <- mutate(data, parity2 = case_when(
    parity == 0 ~ "0",
    parity == 1 ~ "1",
    parity == 2 ~ "2",
    parity == 3 ~ "3",
    parity > 3 ~ "4+"
    )
  )




# Customized centiles ---------------------------------------------------------

# Ethnicity score
  data <- mutate(data, grow_eth_score = case_when(
    ethnicity == "Indian" ~ -149.4,
    ethnicity == "Nepali" ~ -149.4,
    ethnicity == "Pakistani" ~ -187.3,
    ethnicity == "Bangladeshi" ~ -79.3,
    ethnicity == "Jamaican" ~ -129.9,
    ethnicity == "Chinese" ~ -0.3,
    ethnicity %in% c("African", "Angolan", "Congalese", "Ethiopian", "Ghanaian",
                     "Malawian", "Nigerian", "Somali", "Sudanese", "Kenyan",
                     "Liberian", "South African") ~ -218.5,
    ethnicity %in% c("Filipino", "Phillipino", "Sri Lankan", "Thai",
                     "Fijian") ~  +56.4,
    ethnicity %in% c("Moroccan", "Saudi Arabian",  "Muslim", "Omani") ~ -89.9
    )
  )

  data$grow_eth_score[is.na(data$grow_eth_score) & !is.na(data$ethnicity)] <- 0

# Parity
  data <- mutate(data, grow_parity_score = case_when(
    parity == 0 ~ 0,
    parity == 1 ~ 101.9,
    parity == 2 ~ 133.7,
    parity == 3 ~ 140.2,
    parity >  3 ~ 162.7
    )
  )

# Sex
  data <- mutate(data, grow_sex_score = case_when(
    baby_sex_1 == 0 ~ 0,
    baby_sex_1 == 1 ~ 48.9,
    baby_sex_1 == 2 ~ -48.9
    )
  )
  data$grow_sex_score[is.na(data$baby_sex_1)] <- 0

  data$baby_sex_1[data$baby_sex_1 == 0] <- NA
  data$sex <- factor(data$baby_sex_1, labels = c("Male", "Female"))

# Weight (capped)

  data$weight_grow <- data$weight
  target <- data$bmi >= 30 & !is.na(data$weight_grow) & !is.na(data$height)
  data$weight_grow[target] <-
    (30 * data$height[target]^2)

  data$weight_grow <- data$weight_grow - 64

# Score
   data$tow <-
     3455.6 +
     ((data$height*100 - 163) * 6.7) +
     data$weight_grow +
     data$grow_eth_score +
     data$grow_parity_score +
     data$grow_sex_score +
     (9.1733 * data$weight_grow) + (-0.151 * data$weight_grow^2) +
       (-0.001* data$weight_grow^3)

# Correct for GA
   data$grow_pct_weight <- (298.8 +
     (-31.85 * data$del_gstweeks) +
     (1.094 * data$del_gstweeks^2) +
     (-0.01055 * data$del_gstweeks^3)) / 100

# Calc corrected grow weight

   data$grow <- data$tow * data$grow_pct_weight

   data$grow_diff <- data$birth_weight_1 - data$grow

# Grow centile

   data$tow_z <- (((data$birth_weight_1/data$tow) - 1) * 100) / 11 # z score from TOW
   data$tow_centile <- pnorm(data$tow_z)


# z score from GROW - Get the % diff of observed weight from GROW. Divide by 11,
# the CV, to get the z-score. Then get the centile for that z-score.
   data$grow_z <- (((data$birth_weight_1/data$grow) - 1) * 100) / 11 #
#  plot(data$grow_z,    pnorm(data$grow_z))
   data$grow_centile <- pnorm(data$grow_z) * 100

   data <- mutate(data, grow_size_ga = case_when(
     grow_centile <= 10 ~ "SGA",
     grow_centile > 10 & grow_centile < 90 ~ "Normal",
     grow_centile > 90 ~ "LGA"
    )
   )

   data$grow_size_ga <- factor(data$grow_size_ga)



# WHO centiles

   tar <- data$sex == "Male" & !is.na(data$sex)
   data$igb_wt_centile[tar] <- igb_wtkg2centile(data$del_gstdays[tar],
                                                data$birth_weight_1[tar]/1000,
                                                sex = "Male")
   tar <- data$sex == "Female" & !is.na(data$sex)
   data$igb_wt_centile[tar] <- igb_wtkg2centile(data$del_gstdays[tar],
                                                data$birth_weight_1[tar]/1000,
                                                sex = "Female")

   data <- mutate(data, igb_size_ga = case_when(
     igb_wt_centile <= 10 ~ "SGA",
     igb_wt_centile > 10 & igb_wt_centile < 90 ~ "Normal",
     igb_wt_centile > 90 ~ "LGA"
    )
   )

   data$igb_size_ga <- factor(data$igb_size_ga)

# hba1c

   data$hba1c_at_diagnosis <- as.numeric(data$hba1c_at_diagnosis)

# TX ---------------------------------------------------------------------------

  data$diet_only <- factor(as.numeric(data$diet_only), labels = c("Yes", "No"))
  table(data$referral_to_endocrinology)
  data$metformin <- factor(as.numeric(data$metformin), labels = c("Yes", "No"))
  data$basal_insulin[data$basal_insulin == 0] <- NA
  data$basal_insulin <- factor(data$basal_insulin,
                               labels = c("Yes", "No"))
  data$prandial_insulin[data$prandial_insulin == 0] <- NA
  data$prandial_insulin <- factor(data$prandial_insulin,
                               labels = c("Yes", "No"))

# Polish

   data <- select(data, year, everything()) %>% arrange(year)

# Save -------------------------------------------------------------------------

  # save(data, gdm_1, gdm_2, gdm_3, file = "data.RData")
  # rm(list = ls())
  # load("data.RData")

# End --------------------------------------------------------------------------

# # https://cran.r-project.org/web/packages/desctable/vignettes/desctable.html
#
#   describe(data)
#   precis(data)
#
# # https://github.com/hadley/precis
#
#   library(skimr)
#   skim(data)
#
# # https://ropensci.org/blog/blog/2017/07/11/skimr
#
# # https://github.com/sfirke/janitor
#   library(janitor)
#
#   head(data)
#   tail(data)
#   data
#   summary(data)
#   str(data)
#   table(unlist(lapply(data, class)))
#
#
#
# # Debugging
#
#   dput()
#
# # A quick way to look at all the data
#
#   plotData(data)
#
# # Compare
#
#   Compare(x, y)
#
# # Remove blank rows and columns
#
#   data <- data[, unlist(lapply(data, function(x) !all(is.na(x))))] # blank cols
#   data <- data[rowSums(is.na(data)) != ncol(data),] # Remove blank rows
#
#
# # Tidy variable names ####
#
# # find a given name
#
#   colnames(data)["xxxx", colnames(data))]
#
#
#
#   colnames(data)
#
#   colnames(data) <- tolower(colnames(data))
#   colnames(data) <- gsub("^\\s+|\\s+$", "", colnames(data)) # trailing/leading space
#
#   colnames(data) <- gsub("[[:punct:]]", "", colnames(data))
#   colnames(data) <- gsub(" ", ".", colnames(data))
#
#   colnames(data) <- gsub("^[[:digit:]]", "", colnames(data))
#
#   colnames(data) <- gsub(" ", ".", colnames(data))
#   colnames(data) <- gsub("\\/", ".", colnames(data))
#   colnames(data) <- gsub("\\,", "", colnames(data))
#   colnames(data) <- gsub("\\?", "", colnames(data))
#
#   as.character(gsub("^\\s+|\\s+$", "", colnames(data))) # lead, trailing white space
#   colnames(data) <- make.names(colnames(data), unique = TRUE)
#
#   sub("^([0-9])(.+)", "\\2\\1", colnames(data)) # Move digit from front to end
#
#   data$x[grepl("XXXXX",  data$x)]    <- "xxxxxxxx"
#
# # This captures a number of any length \\d+ in the () as \\1, and then puts . in
# # front what was captured.
#   colnames(data) <- sub("(\\d+)", "\\.\\1", colnames(data))
#
# # Remove . at end of string
#   colnames(data) <- gsub("\\.$", "", colnames(data))
#
# # keep only the digit from a string
#   gregexpr("[[:digit:]]+", data$x) %>%
#   regmatches(data$x, .) %>%
#   unlist() %>%
#   as.numeric()
#
#   readr::parse_number() # "Parse numbers, flexibly."
#
# # select values that match string in column
#   data$x[grepl("x",  data$x)]    <- "X"
#
# # Keep only the first digit
#
#   data$x <- sub("([0-9]{1}).*", "\\1", data$x)
#
# # Keep a match
# # Keep everything after a given character
#
#   data$x <-  regmatches(data$x, regexpr("([^XXX]*$)", data$x))
#
# # Add a separator to a pattern and then use separate to split it
#   ecg1$bp_exam <- sub("(BP:\\s*\\d+/\\d{,2})", "\\1\\SEPP", ecg1$bp_exam)
#   ecg1 <- separate(ecg1, bp_exam, c("bp_exam", "exam"), sep = "SEPP")
#
# # Keep just the pattern you want
#   ecg1$bp_exam <-  sub(".*?(\\d+/\\d{,2}).*", "\\1", ecg1$bp_exam)
#
# # Remove digit from position
#
# # Rouding while keeping trailing zeros
#
#   formatC(round(x, 2), format = "f", 2)
#
# # Finding duplicated column names
#
#   full.1 <- full[, colnames(full) %in%
#                    unique(colnames(full)[ duplicated(colnames(full))])]
#
#   full.2 <- full[, !(colnames(full) %in%
#                        unique(colnames(full)[ duplicated(colnames(full))]))]
#
# # Repeats values to fill in NAs
# # http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
#   repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
#     ind = which(!is.na(x))        # get positions of nonmissing values
#     if(is.na(x[1]))               # if it begins with a missing, add the
#       ind = c(1,ind)              # first position to the indices
#     rep(x[ind], times = diff(     # repeat the values at these indices
#       c(ind, length(x) + 1) ))    # diffing the indices + length yields how often
#   }
#
# # Find the matching set of colnames among a set of files
#
#   form.cols <- function(target = getwd()) {
#
#   	require(readxl)
#
#   	form <- read_excel(target, skip = 1)
#
#   	colnames(form)  <- gsub("[[:punct:]]", "", colnames(form))
#
#   	return(colnames(form))
#
#   }
#
#   forms <- list("data/final.splitsets/senato40.xls",
#   							"data/final.splitsets/senato41.xls",
#   							"data/final.splitsets/senato42.xls",
#   							"data/final.splitsets/senato43.xls",
#   							"data/final.splitsets/senato44.xls",
#   							"data/final.splitsets/senato45.xls",
#   							"data/final.splitsets/senato46.xls",
#   							"data/final.splitsets/senato47.xls",
#   							"data/final.splitsets/senato48.xls",
#   							"data/final.splitsets/senato49.xls",
#   							"data/final.splitsets/senato50.xls",
#   							"data/final.splitsets/senato51.xls",
#   							"data/final.splitsets/senato52.xls")
#
#   colnamesList <- lapply(forms, form.cols)
#
# # Get the column names shared across datasets, and add the new one for type
#   shared.vars <- c(Reduce(intersect, colnamesList), "adr.type")
#
# # Keep columns with less missing data
#
#   data <- data[colSums(!is.na(data)) > x]
#
# # Index columns whose name includes a given string
#   x <- data[, grepl("xxx", names(data))]
#
#   data[, grepl("xxxx", names(data))] <-  x[colSums(!is.na(x)) > 0]
#
#
# # Use attributes in a factor label
#
#   data$x <- factor(data$x,
#                   labels = attributes(attributes(data$x)$labels)$names)
#
# # Add variable labels ####
#
#   label(data) <- lapply(names(varlabs),
#                         function(x) label(data[, x]) = varlabs[x])
#
# # Save labels from Haven
#
#   label.list <- list()
#
#   for (i in seq_along(data)){
#     label.list[[i]] <- attributes(data[[i]])$label
#   }
#
# # Remove labelled class from Haven
#   is_labelled <- function(x) {
#     if (length(class(x)) > 1) return(any(class(x) == "labelled"))
#     return(class(x) == "labelled")
#   }
#
#   unlabel <- function(x) {
#     if (is.data.frame(x) || is.matrix(x)) {
#       for (i in 1:ncol(x)) {
#         if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
#       }
#     }
#     else {
#       # remove labelled class
#       if (is_labelled(x)) x <- unclass(x)
#     }
#     return(x)
#   }
#
#
#
# # Tidy character values ####
#
#   # View(data[, sapply(data, class) == 'character'])
#
#
# # Remove leading and trailing white space from characters
#
#   trim <- function(x) {
#     if (is.character(x) == TRUE) {
#       x <- as.character(gsub("^\\s+|\\s+$", "", x))
#     }
#     else {
#       x <- x
#     }
#   }
#
#   data <- as.data.frame(lapply(data, trim), stringsAsFactors = FALSE)
#
#   data[, c()] <- apply(data[, c()] , 2,
#                        function(x) gsub("\\\"", "", x))
#
#
# # Gets rid of characters from what should be numeric values. (Surely an easier
# # way?)
#
#   x <- gregexpr("[[:digit:]]+", x) %>%
# 		   regmatches(x, .) %>%
# 		   unlist() %>%
# 		   as.numeric()
# # Tidy factors ####
#
#   bi.factor <- function(x, ...){
#     x <- factor(x, levels = c(1, 2), labels = c("Yes", "No"))
#   }
#
# # Find variables that look like factors because min = 1 and max = 2
#   lapply(data[, sapply(data, max, na.rm = TRUE) == 2 &
#                 sapply(data, min, na.rm = TRUE) == 1 ],
#          bi.factor,
#          stringsAsFactors = FALSE) %>%
#     as.data.frame() -> data[, sapply(data, max, na.rm = TRUE) == 2 &
#                               sapply(data, min, na.rm = TRUE) == 1 ]
#
#
# # Tidy time ####
#
#   data$x <- paste(data$date, data$time, sep = " ") %>%
#             as.POSIXct(format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
#
# # Add ":" from 2 spots back
#   data$time <- gsub("(.{2})$", ":\\1",  data$time)
#
# # Time from excel sheep (e.g. 41202)
#
#   data$x <- as.POSIXct(as.numeric(data$x) * 60 * 60 * 24,
#              origin = "1899-12-30")
#
#   as.POSIXct(x, origin = "1970-01-01", tz = "GMT")
#
# # Add time to date variables
#
#   x <- unlist(lapply(x, function(x){
#   	                      return(as.character(seq(as.Date(x),
#   																								length = 2,
#   																				        by = "-100 years")[2]))
#   								      }))
#
# # Create decimel hours from HH:MM:SS
#
#   x <- sapply(strsplit(x, ":"), function(x) {
#   															  x <- as.numeric(x)
#   																x[1] + (x[2] / 60) + (x[3] / 3600)
#   															})
#
#
# # Duplicates ####
#
#   allDup <- function(value){
#     duplicated(value) | duplicated(value, fromLast = TRUE)
#   }
#
#   data[allDup(data_frame(A = data$A,
#                          B = data$B)), ]
#
#
# # Missing values ####
#
# # Recode missings to NA
#
#   replMiss <- function(x) {
#     if (is.character(x) == TRUE) {
#       ifelse(x == "99.000000" | x == "999.000000", NA, x)
#     }
#     else {
#       x <- x
#     }
#   }
#
#   data <- as.data.frame(lapply(data, replMiss), stringsAsFactors = FALSE)
#
#   propMiss(data)
#
# # Do these data match the existing data?
#   a <- select(clin, id, sex, age, surgery, arm, procedure) %>%
#        mutate(surgery = factor(surgery))
#   b <- select(rec,  id, sex, age, surgery, arm, procedure)
#
#   comparison <- compare(a, b)
#
#   # View(comparison$tM)
#
#   ids <- anti_join(a, b)$id
#
#   filter(a, id %in% ids) %>% # View()
#   filter(b, id %in% ids) %>% # View()
#
# # NaN
#
#   data[sapply(data, is.numeric)] <- apply(data[sapply(data, is.numeric)], 2,
#                                           function(x) ifelse(is.nan(x), NA, x))
#
#
#
# # Haven
#
# # Convert lablled to factors, or remove the labels
#
#   data <- mutate_if(data, is.labelled, as_factor)
#   data <- mutate_if(data, is.labelled, zap_label)
#
