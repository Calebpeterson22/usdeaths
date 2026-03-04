library(tidyverse)

data_mortality_multiple_2012 <- tribble(
  ~name, ~start, ~end, ~size, ~type, ~description, ~codes,
  # ... [Positions 1-164 same as 2010/2011] ...
  "reserved_1", 1, 19, 19, "str", "Reserved Positions", "Blank",
  "resident_status", 20, 20, 1, "int", "Resident status", "1=Residents|2=Intrastate Nonresidents|3=Interstate Nonresidents|4=Foreign Residents",
  "reserved_2", 21, 60, 40, "str", "Reserved Positions", "Blank",
  # ... [Skipping ahead to the 2012 Year Marker] ...
  "current_data_year", 102, 105, 4, "int", "Current data year", "2012",
  # ... [Condition Blocks 165-443] ...
  "num_entity_axis_conditions", 163, 164, 2, "int", "Number of entity-axis conditions", "00-20",
  "condition_1", 165, 171, 7, "str", "Entity-axis condition 1", "pos1=part/line, pos2=seq, pos3-6=ICD",
  # [Note: Rows for conditions 2-20 and record conditions 1-20 are identical to 2011]
  "record_condition_20", 439, 443, 5, "str", "Record-axis condition 20", "",
  "reserved_10", 444, 444, 1, "str", "Reserved Position", "Blank",
  # ... [Demographics] ...
  "race", 445, 446, 2, "int", "Race", "01=White|02=Black...78=Combined Asian/PI",
  "bridged_race_flag", 447, 447, 1, "int", "Bridged race flag", "1=Bridged|blank=Not bridged",
  "race_imputation_flag", 448, 448, 1, "int", "Race imputation flag", "1=Unknown imputed|2=All other imputed",
  "race_recode_3", 449, 449, 1, "int", "Race recode 3", "1=White|2=Other|3=Black",
  "race_recode_5", 450, 450, 1, "int", "Race recode 5", "1=White|2=Black|3=AIAN|4=Asian/PI",
  "reserved_11", 451, 483, 33, "str", "Reserved Positions", "Blank",
  "hispanic_origin", 484, 486, 3, "int", "Hispanic origin", "100-199=Non-Hispanic|200-299=Hispanic",
  "reserved_12", 487, 487, 1, "str", "Reserved Position", "Blank",
  "hispanic_origin_race_recode", 488, 488, 1, "int", "Hispanic origin/race recode", "1-5=Hispanic|6=NH White|7=NH Black|8=Other|9=Unknown"
)

usethis::use_data(data_mortality_multiple_2012, overwrite = TRUE)