data_birth_cohort_1988 <- tribble(
  ~name,                   ~start, ~end, ~size, ~type,  ~description,                                       ~codes,
 
  # -- General (positions 1-11) ----------------------------------------------
  "match_status",               1,   1,   1,  "int",  "Match Status",
    "1=Matched Birth/Infant Death Record|2=Late Filed Matched Birth/Infant Death Record|3=Surviving infant record",
  "dob_yy",                     2,   5,   4,  "int",  "Year of Birth",
    "1988=Born in 1988",
  "reserved_1",                 6,   9,   4,  "str",  "Reserved positions",
    "",
  "record_type",               10,  10,   1,  "int",  "Record Type",
    "1=Residents (state and county of occurrence and residence are the same)|2=Nonresidents (state and/or county of occurrence and residence are different)",
  "resident_status",           11,  11,   1,  "int",  "Resident Status",
    "1=Residents|2=Intrastate Nonresidents (state same, county different)|3=Interstate Nonresidents (state different, both in U.S.)|4=Foreign Residents",
 
  # -- Place of Occurrence (positions 12-21) ---------------------------------
  "occ_region",                12,  12,   1,  "int",  "Region of Occurrence",
    "1=Northeast|2=Midwest|3=South|4=West",
  "occ_div_state",             13,  14,   2,  "str",  "Division and State Subcode of Occurrence",
    "1-1=Maine|1-2=New Hampshire|1-3=Vermont|1-4=Massachusetts|1-5=Rhode Island|1-6=Connecticut|2-1=New York|2-2=New Jersey|2-3=Pennsylvania|3-1=Ohio|3-2=Indiana|3-3=Illinois|3-4=Michigan|3-5=Wisconsin|4-1=Minnesota|4-2=Iowa|4-3=Missouri|4-4=North Dakota|4-5=South Dakota|4-6=Nebraska|4-7=Kansas|5-1=Delaware|5-2=Maryland|5-3=District of Columbia|5-4=Virginia|5-5=West Virginia|5-6=North Carolina|5-7=South Carolina|5-8=Georgia|5-9=Florida|6-1=Kentucky|6-2=Tennessee|6-3=Alabama|6-4=Mississippi|7-1=Arkansas|7-2=Louisiana|7-3=Oklahoma|7-4=Texas|8-1=Montana|8-2=Idaho|8-3=Wyoming|8-4=Colorado|8-5=New Mexico|8-6=Arizona|8-7=Utah|8-8=Nevada|9-1=Washington|9-2=Oregon|9-3=California|9-4=Alaska|9-5=Hawaii",
  "occ_state_exp",             15,  16,   2,  "int",  "Expanded State of Occurrence (identifies New York City separately)",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=New York City|35=North Carolina|36=North Dakota|37=Ohio|38=Oklahoma|39=Oregon|40=Pennsylvania|41=Rhode Island|42=South Carolina|43=South Dakota|44=Tennessee|45=Texas|46=Utah|47=Vermont|48=Virginia|49=Washington|50=West Virginia|51=Wisconsin|52=Wyoming",
  "occ_state",                 17,  18,   2,  "int",  "State of Occurrence",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming",
  "occ_county",                19,  21,   3,  "str",  "County of Occurrence",
    "001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population",
 
  # -- Place of Residence (positions 22-35) ----------------------------------
  "res_region",                22,  22,   1,  "int",  "Region of Residence",
    "000=Foreign Resident|1=Northeast|2=Midwest|3=South|4=West",
  "res_div_state",             23,  24,   2,  "str",  "Division and State Subcode of Residence",
    "000=Foreign Resident|1-1=Maine|1-2=New Hampshire|1-3=Vermont|1-4=Massachusetts|1-5=Rhode Island|1-6=Connecticut|2-1=New York|2-2=New Jersey|2-3=Pennsylvania|3-1=Ohio|3-2=Indiana|3-3=Illinois|3-4=Michigan|3-5=Wisconsin|4-1=Minnesota|4-2=Iowa|4-3=Missouri|4-4=North Dakota|4-5=South Dakota|4-6=Nebraska|4-7=Kansas|5-1=Delaware|5-2=Maryland|5-3=District of Columbia|5-4=Virginia|5-5=West Virginia|5-6=North Carolina|5-7=South Carolina|5-8=Georgia|5-9=Florida|6-1=Kentucky|6-2=Tennessee|6-3=Alabama|6-4=Mississippi|7-1=Arkansas|7-2=Louisiana|7-3=Oklahoma|7-4=Texas|8-1=Montana|8-2=Idaho|8-3=Wyoming|8-4=Colorado|8-5=New Mexico|8-6=Arizona|8-7=Utah|8-8=Nevada|9-1=Washington|9-2=Oregon|9-3=California|9-4=Alaska|9-5=Hawaii",
  "res_state_exp",             25,  26,   2,  "int",  "Expanded State of Residence (identifies New York City separately)",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=New York City|35=North Carolina|36=North Dakota|37=Ohio|38=Oklahoma|39=Oregon|40=Pennsylvania|41=Rhode Island|42=South Carolina|43=South Dakota|44=Tennessee|45=Texas|46=Utah|47=Vermont|48=Virginia|49=Washington|50=West Virginia|51=Wisconsin|52=Wyoming|53=Puerto Rico|54=Virgin Islands|55=Guam|56=Canada|57=Cuba|58=Mexico|60=Remainder of the world",
  "res_state",                 27,  28,   2,  "int",  "State of Residence",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming|52=Puerto Rico|53=Virgin Islands|54=Guam|55=Canada|56=Cuba|57=Mexico|59=Remainder of the world",
  "res_county",                29,  31,   3,  "str",  "County of Residence",
    "001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population|222=Foreign residents",
  "res_city",                  32,  34,   3,  "str",  "City of Residence",
    "001-nnn=City code (numbered alphabetically within state; population 250,000+)|999=Entire county/balance of county/city less than 250,000|222=Foreign residents",
  "reserved_2",                35,  35,   1,  "str",  "Reserved position",
    "",
 
  # -- Infant (positions 36-54) -----------------------------------------------
  "race_child",                36,  36,   1,  "int",  "Detail Race of Child",
    "1=White|2=Black|3=American Indian (includes Aleuts and Eskimos)|4=Chinese|5=Japanese|6=Hawaiian (includes Part-Hawaiian)|7=Filipino|8=Other Asian or Pacific Islander|0=Other races",
  "race_child_r3",             37,  37,   1,  "int",  "Race of Child Recode 3",
    "1=White|2=Races other than White or Black|3=Black",
  "csex",                      38,  38,   1,  "int",  "Sex of Child",
    "1=Male|2=Female",
  "gestat",                    39,  40,   2,  "int",  "Detail Gestation in Weeks",
    "17-52=17th through 52nd week of gestation|99=Gestation not stated",
  "gestat_r10",                41,  42,   2,  "int",  "Gestation Recode 10",
    "01=Under 20 weeks|02=20-27 weeks|03=28-31 weeks|04=32-35 weeks|05=36 weeks|06=37-39 weeks|07=40 weeks|08=41 weeks|09=42 weeks and over|10=Gestation not stated",
  "dbirwt",                    43,  46,   4,  "int",  "Birth Weight Detail in Grams",
    "0227-8165=Number of grams|9999=Birth weight not stated",
  "birwt_r14",                 47,  48,   2,  "int",  "Birth Weight Recode 14",
    "01=499 grams or less|02=500-749 grams|03=750-999 grams|04=1000-1249 grams|05=1250-1499 grams|06=1500-1999 grams|07=2000-2499 grams|08=2500-2999 grams|09=3000-3499 grams|10=3500-3999 grams|11=4000-4499 grams|12=4500-4999 grams|13=5000-8165 grams|14=Birth weight not stated",
  "birwt_r3",                  49,  49,   1,  "int",  "Birth Weight Recode 3",
    "1=2499 grams or less|2=2500 grams or more|3=Birth weight not stated",
  "dplural",                   50,  50,   1,  "int",  "Plurality Detail",
    "1=Single Birth|2=Twin|3=Other Multiple Births",
  "omaps",                     51,  52,   2,  "int",  "One Minute Apgar Score",
    "00-10=A score of 0-10|99=Unknown or not stated",
  "fmaps",                     53,  54,   2,  "int",  "Five Minute Apgar Score",
    "00-10=A score of 0-10|99=Unknown or not stated",
 
  # -- Mother (positions 55-67) -----------------------------------------------
  "origin_moth",               55,  56,   2,  "int",  "Origin or Descent of Mother",
    "00=Non-Hispanic|01=Mexican|02=Puerto Rican|03=Cuban|04=Central or South American|05=Other and Unknown Hispanic|06=American|07=American Indian|08=British/Scottish/Welsh/Scotch-Irish|09=Irish|10=German|11=French|12=Norwegian/Swedish/Danish|13=Polish|14=Italian|15=Other North/Central/South American|16=Other Western European|17=Other Northern European|18=Other Eastern European|19=Other Southern European (excl. Spain)|20=Southeast Asian and Pacific Islander|21=South Central Asian|22=Other Asian|23=North African|24=Other African|88=Origin not reported|99=Origin not classifiable",
  "race_moth",                 57,  57,   1,  "int",  "Detail Race of Mother",
    "1=White|2=Black|3=American Indian (includes Aleuts and Eskimos)|4=Chinese|5=Japanese|6=Hawaiian (includes Part-Hawaiian)|7=Filipino|8=Other Asian or Pacific Islander|0=Other races|9=Race not stated",
  "dmage",                     58,  59,   2,  "int",  "Detail Age of Mother",
    "10-49=Age in single years",
  "mage_r12",                  60,  61,   2,  "int",  "Age of Mother Recode 12",
    "01=Under 15 years|03=15 years|04=16 years|05=17 years|06=18 years|07=19 years|08=20-24 years|09=25-29 years|10=30-34 years|11=35-39 years|12=40-44 years|13=45-49 years",
  "dmeduc",                    62,  63,   2,  "int",  "Mother's Education Detail",
    "00=No formal education|01-08=Years of elementary school|09=1 year of high school|10=2 years of high school|11=3 years of high school|12=4 years of high school|13=1 year of college|14=2 years of college|15=3 years of college|16=4 years of college|17=5 or more years of college|99=Not stated",
  "meduc_r6",                  64,  64,   1,  "int",  "Mother's Education Recode 6",
    "1=0-8 years|2=9-11 years|3=12 years|4=13-15 years|5=16 years and over|6=Not stated",
  "dmar",                      65,  65,   1,  "int",  "Marital Status",
    "1=Married|2=Unmarried",
  "mplbir",                    66,  67,   2,  "int",  "Mother's Place of Birth",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming|52=Puerto Rico|53=Virgin Islands|54=Guam|55=Canada|56=Cuba|57=Mexico|59=Remainder of the world|99=Not classifiable",
 
  # -- Father (positions 68-74) -----------------------------------------------
  "origin_fath",               68,  69,   2,  "int",  "Origin or Descent of Father",
    "00=Non-Hispanic|01=Mexican|02=Puerto Rican|03=Cuban|04=Central or South American|05=Other and Unknown Hispanic|06=American|07=American Indian|08=British/Scottish/Welsh/Scotch-Irish|09=Irish|10=German|11=French|12=Norwegian/Swedish/Danish|13=Polish|14=Italian|15=Other North/Central/South American|16=Other Western European|17=Other Northern European|18=Other Eastern European|19=Other Southern European (excl. Spain)|20=Southeast Asian and Pacific Islander|21=South Central Asian|22=Other Asian|23=North African|24=Other African|88=Origin not reported|99=Origin not classifiable",
  "race_fath",                 70,  70,   1,  "int",  "Detail Race of Father",
    "1=White|2=Black|3=American Indian (includes Aleuts and Eskimos)|4=Chinese|5=Japanese|6=Hawaiian (includes Part-Hawaiian)|7=Filipino|8=Other Asian or Pacific Islander|0=Other races|9=Race not stated",
  "dfage",                     71,  72,   2,  "int",  "Detail Age of Father",
    "10-98=Age in single years|99=Age not stated",
  "dfeduc",                    73,  74,   2,  "int",  "Father's Education Detail",
    "00=No formal education|01-08=Years of elementary school|09=1 year of high school|10=2 years of high school|11=3 years of high school|12=4 years of high school|13=1 year of college|14=2 years of college|15=3 years of college|16=4 years of college|17=5 or more years of college|99=Not stated",
 
  # -- Pregnancy Items (positions 75-88) -------------------------------------
  "illb",                      75,  75,   1,  "int",  "Interval Since Last Live Birth",
    "0=Not applicable (no previous live birth)|1=Zero months (plural birth)|2=1-11 months|3=12-23 months|4=24-35 months|5=36-47 months|6=48-71 months|7=72 months and over|9=Not stated",
  "outcome_last_preg",         76,  76,   1,  "int",  "Outcome of Last Pregnancy",
    "0=Not applicable (no previous pregnancy)|1=Last pregnancy was a live birth|2=Last pregnancy was some other termination|9=Unknown",
  "ilp",                       77,  77,   1,  "int",  "Interval Since Termination of Last Pregnancy",
    "0=Not applicable (no previous pregnancy)|1=Zero months (plural delivery)|2=1-11 months|3=12-17 months|4=18-23 months|5=24-35 months|6=36-47 months|7=48-59 months|8=60 months and over|9=Not stated",
  "monpre",                    78,  79,   2,  "int",  "Detail Month of Pregnancy Prenatal Care Began",
    "00=No prenatal care|01=1st month|02=2nd month|03=3rd month|04=4th month|05=5th month|06=6th month|07=7th month|08=8th month|09=9th month|99=Not stated",
  "mpre_r6",                   80,  80,   1,  "int",  "Month Prenatal Care Began Recode 6",
    "1=1st-2nd month|2=3rd month|3=4th-6th month|4=7th-9th month|5=No prenatal care|6=Not stated",
  "nprevist",                  81,  82,   2,  "int",  "Total Number of Prenatal Visits",
    "00=No prenatal visits|01-49=Stated number of visits|99=Not stated",
  "dtotord",                   83,  84,   2,  "int",  "Detail Total Birth Order",
    "01-50=Total number of live births and other terminations|99=Unknown or not stated",
  "totord_r9",                 85,  85,   1,  "int",  "Total Birth Order Recode 9",
    "1=First Child|2=Second Child|3=Third Child|4=Fourth Child|5=Fifth Child|6=Sixth Child|7=Seventh Child|8=Eighth Child and over|9=Not stated",
  "dlivord",                   86,  87,   2,  "int",  "Detail Live Birth Order",
    "01-50=Number of children ever born alive to mother|99=Unknown or not stated",
  "livord_r9",                 88,  88,   1,  "int",  "Live Birth Order Recode 9",
    "1=First Child|2=Second Child|3=Third Child|4=Fourth Child|5=Fifth Child|6=Sixth Child|7=Seventh Child|8=Eighth Child and over|9=Not stated",
 
  # -- Medical / Delivery Items (positions 89-91) ----------------------------
  "pldel",                     89,  89,   1,  "int",  "Place of Delivery",
    "1=Hospital Births|2=Nonhospital Births|3=En route or born on arrival (BOA)|9=Not classifiable",
  "attend",                    90,  90,   1,  "int",  "Attendant at Birth",
    "1=Physician|2=Midwife|3=Attendant other than physician or midwife|9=Unknown",
  "mrace_imp",                 91,  91,   1,  "int",  "Imputed Race of Mother",
    "1=White|2=Black|3=American Indian (includes Aleuts and Eskimos)|4=Chinese|5=Japanese|6=Hawaiian (includes Part-Hawaiian)|7=Filipino|8=Other Asian or Pacific Islander|0=Other races",
 
  # -- Reserved (positions 92-193, Numerator only) ---------------------------
  "reserved_3",                92, 193, 102,  "str",  "Reserved positions (Numerator file only; reserved for possible additional data)",
    "",
 
  # --------------------------------------------------------------------------
  # MORTALITY SECTION - Numerator (Linked) File only, positions 194-500
  # --------------------------------------------------------------------------
 
  "dth_yy",                   194, 197,   4,  "int",  "Year of Death",
    "1988=Death occurred in 1988|1989=Death occurred in 1989",
  "dth_record_type",          198, 198,   1,  "int",  "Record Type - Death",
    "1=Residents (state and county of occurrence and residence are the same)|2=Nonresidents (state and/or county of occurrence and residence are different)",
  "dth_resstat",              199, 199,   1,  "int",  "Resident Status - Death",
    "1=Residents|2=Intrastate Nonresidents (state same, county different)|3=Interstate Nonresidents (state different, both in U.S.)|4=Foreign Residents",
 
  # Place of Occurrence - Death (positions 200-209)
  "dth_occ_region",           200, 200,   1,  "int",  "Region of Occurrence - Death",
    "1=Northeast|2=Midwest|3=South|4=West",
  "dth_occ_div_state",        201, 202,   2,  "str",  "Division and State Subcode of Occurrence - Death",
    "1-1=Maine|1-2=New Hampshire|1-3=Vermont|1-4=Massachusetts|1-5=Rhode Island|1-6=Connecticut|2-1=New York|2-2=New Jersey|2-3=Pennsylvania|3-1=Ohio|3-2=Indiana|3-3=Illinois|3-4=Michigan|3-5=Wisconsin|4-1=Minnesota|4-2=Iowa|4-3=Missouri|4-4=North Dakota|4-5=South Dakota|4-6=Nebraska|4-7=Kansas|5-1=Delaware|5-2=Maryland|5-3=District of Columbia|5-4=Virginia|5-5=West Virginia|5-6=North Carolina|5-7=South Carolina|5-8=Georgia|5-9=Florida|6-1=Kentucky|6-2=Tennessee|6-3=Alabama|6-4=Mississippi|7-1=Arkansas|7-2=Louisiana|7-3=Oklahoma|7-4=Texas|8-1=Montana|8-2=Idaho|8-3=Wyoming|8-4=Colorado|8-5=New Mexico|8-6=Arizona|8-7=Utah|8-8=Nevada|9-1=Washington|9-2=Oregon|9-3=California|9-4=Alaska|9-5=Hawaii",
  "dth_occ_state_exp",        203, 204,   2,  "int",  "Expanded State of Occurrence - Death (identifies New York City separately)",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=New York City|35=North Carolina|36=North Dakota|37=Ohio|38=Oklahoma|39=Oregon|40=Pennsylvania|41=Rhode Island|42=South Carolina|43=South Dakota|44=Tennessee|45=Texas|46=Utah|47=Vermont|48=Virginia|49=Washington|50=West Virginia|51=Wisconsin|52=Wyoming",
  "dth_occ_state",            205, 206,   2,  "int",  "State of Occurrence - Death",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming",
  "dth_occ_county",           207, 209,   3,  "str",  "County of Occurrence - Death",
    "001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population",
 
  # Place of Residence - Death (positions 210-222)
  "dth_res_region",           210, 210,   1,  "int",  "Region of Residence - Death",
    "000=Foreign Resident|1=Northeast|2=Midwest|3=South|4=West",
  "dth_res_div_state",        211, 212,   2,  "str",  "Division and State Subcode of Residence - Death",
    "1-1=Maine|1-2=New Hampshire|1-3=Vermont|1-4=Massachusetts|1-5=Rhode Island|1-6=Connecticut|2-1=New York|2-2=New Jersey|2-3=Pennsylvania|3-1=Ohio|3-2=Indiana|3-3=Illinois|3-4=Michigan|3-5=Wisconsin|4-1=Minnesota|4-2=Iowa|4-3=Missouri|4-4=North Dakota|4-5=South Dakota|4-6=Nebraska|4-7=Kansas|5-1=Delaware|5-2=Maryland|5-3=District of Columbia|5-4=Virginia|5-5=West Virginia|5-6=North Carolina|5-7=South Carolina|5-8=Georgia|5-9=Florida|6-1=Kentucky|6-2=Tennessee|6-3=Alabama|6-4=Mississippi|7-1=Arkansas|7-2=Louisiana|7-3=Oklahoma|7-4=Texas|8-1=Montana|8-2=Idaho|8-3=Wyoming|8-4=Colorado|8-5=New Mexico|8-6=Arizona|8-7=Utah|8-8=Nevada|9-1=Washington|9-2=Oregon|9-3=California|9-4=Alaska|9-5=Hawaii",
  "dth_res_state_exp",        213, 214,   2,  "int",  "Expanded State of Residence - Death (identifies New York City separately)",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=New York City|35=North Carolina|36=North Dakota|37=Ohio|38=Oklahoma|39=Oregon|40=Pennsylvania|41=Rhode Island|42=South Carolina|43=South Dakota|44=Tennessee|45=Texas|46=Utah|47=Vermont|48=Virginia|49=Washington|50=West Virginia|51=Wisconsin|52=Wyoming|53=Puerto Rico|54=Virgin Islands|55=Guam|56=Canada|57=Cuba|58=Mexico|60=Remainder of the world",
  "dth_res_state",            215, 216,   2,  "int",  "State of Residence - Death",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming|52=Puerto Rico|53=Virgin Islands|54=Guam|55=Canada|56=Cuba|57=Mexico|59=Remainder of the world",
  "dth_res_county",           217, 219,   3,  "str",  "County of Residence - Death",
    "001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population|222=Foreign residents",
  "dth_res_city",             220, 222,   3,  "str",  "City of Residence - Death",
    "001-nnn=City code (numbered alphabetically within state; population 250,000+)|999=Entire county/balance of county/city less than 250,000|222=Foreign residents",
 
  # Infant Age at Death (positions 223-227)
  "ager5",                    223, 223,   1,  "int",  "Infant Age Recode 5",
    "1=Under 1 hour|2=1-23 hours|3=1-6 days|4=7-27 days (late neonatal)|5=28 days and over (postneonatal)",
  "ager76",                   224, 225,   2,  "int",  "Infant Age Recode 76",
    "00=Less than 1 day|01-27=1-27 days|28=4th week|29=5th week|30=6th week|31-76=7th-52nd weeks",
  "ager3b",                   226, 227,   2,  "int",  "Infant Age Recode 3B",
    "00=Less than 1 day|01-27=1-27 days|28=1 month|29=2 months|30=3 months|31=4 months|32=5 months|33=6 months|34=7 months|35=8 months|36=9 months|37=10 months|38=11 months",
 
  # Death Certificate Items (positions 228-237)
  "hosp_pat_status",          228, 228,   1,  "int",  "Hospital and Patient Status",
    "1=Hospital/Clinic/Medical Center - Inpatient|2=Hospital/Clinic/Medical Center - Outpatient or admitted to Emergency Room|3=Hospital/Clinic/Medical Center - Dead on Arrival|4=Hospital/Clinic/Medical Center - Patient status unknown|5=Hospital/Clinic/Medical Center - Patient status not on certificate|6=Other institution providing patient care|7=All other reported entries|8=Dead on Arrival - Hospital name not given|9=Hospital and patient status not stated",
  "autopsy",                  229, 229,   1,  "int",  "Autopsy Performed",
    "1=Yes|2=No|8=Not on certificate|9=Not stated",
  "accidpl",                  230, 230,   1,  "str",  "Place of Accident for Causes E850-E869 and E880-E928",
    "blank=Causes other than E850-E869 and E880-E928|0=Home|1=Farm|2=Mine and Quarry|3=Industrial Place and Premises|4=Place for Recreation and Sport|5=Street and Highway|6=Public Building|7=Resident Institution|8=Other Specified Places|9=Place of accident not specified",
  "ucod",                     231, 234,   4,  "str",  "ICD Code (9th Revision) - Underlying Cause of Death",
    "format=ICD-9 9th revision|external_cause=E800 through E999 coded without letter E|4th_digit=blank when cause has no 4th digit",
 
  "ucodr61",                  235, 237,   3,  "int",  "Infant Cause of Death Recode 61 Groups",
    "010-680=Code range (not inclusive); see cause-of-death list in documentation",
 
  # Multiple Conditions (positions 238-481)
  "ea_num",                   238, 239,   2,  "int",  "Number of Entity-Axis Conditions",
    "00-20=Code range",
  "entity_axis_01",           240, 246,   7,  "str",  "Entity-Axis Condition 1 (part/line, sequence, ICD-9 x4, injury flag)",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_02",           247, 253,   7,  "str",  "Entity-Axis Condition 2",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_03",           254, 260,   7,  "str",  "Entity-Axis Condition 3",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_04",           261, 267,   7,  "str",  "Entity-Axis Condition 4",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_05",           268, 274,   7,  "str",  "Entity-Axis Condition 5",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_06",           275, 281,   7,  "str",  "Entity-Axis Condition 6",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_07",           282, 288,   7,  "str",  "Entity-Axis Condition 7",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_08",           289, 295,   7,  "str",  "Entity-Axis Condition 8",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_09",           296, 302,   7,  "str",  "Entity-Axis Condition 9",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_10",           303, 309,   7,  "str",  "Entity-Axis Condition 10",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_11",           310, 316,   7,  "str",  "Entity-Axis Condition 11",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_12",           317, 323,   7,  "str",  "Entity-Axis Condition 12",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_13",           324, 330,   7,  "str",  "Entity-Axis Condition 13",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_14",           331, 337,   7,  "str",  "Entity-Axis Condition 14",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_15",           338, 344,   7,  "str",  "Entity-Axis Condition 15",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_16",           345, 351,   7,  "str",  "Entity-Axis Condition 16",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_17",           352, 358,   7,  "str",  "Entity-Axis Condition 17",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_18",           359, 365,   7,  "str",  "Entity-Axis Condition 18",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_19",           366, 372,   7,  "str",  "Entity-Axis Condition 19",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_20",           373, 379,   7,  "str",  "Entity-Axis Condition 20",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "ra_num",                   380, 381,   2,  "int",  "Number of Record-Axis Conditions",
    "00-20=Code range",
  "record_axis_01",           382, 386,   5,  "str",  "Record-Axis Condition 1 (ICD-9 x4, injury flag)",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_02",           387, 391,   5,  "str",  "Record-Axis Condition 2",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_03",           392, 396,   5,  "str",  "Record-Axis Condition 3",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_04",           397, 401,   5,  "str",  "Record-Axis Condition 4",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_05",           402, 406,   5,  "str",  "Record-Axis Condition 5",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_06",           407, 411,   5,  "str",  "Record-Axis Condition 6",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_07",           412, 416,   5,  "str",  "Record-Axis Condition 7",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_08",           417, 421,   5,  "str",  "Record-Axis Condition 8",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_09",           422, 426,   5,  "str",  "Record-Axis Condition 9",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_10",           427, 431,   5,  "str",  "Record-Axis Condition 10",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_11",           432, 436,   5,  "str",  "Record-Axis Condition 11",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_12",           437, 441,   5,  "str",  "Record-Axis Condition 12",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_13",           442, 446,   5,  "str",  "Record-Axis Condition 13",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_14",           447, 451,   5,  "str",  "Record-Axis Condition 14",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_15",           452, 456,   5,  "str",  "Record-Axis Condition 15",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_16",           457, 461,   5,  "str",  "Record-Axis Condition 16",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_17",           462, 466,   5,  "str",  "Record-Axis Condition 17",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_18",           467, 471,   5,  "str",  "Record-Axis Condition 18",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_19",           472, 476,   5,  "str",  "Record-Axis Condition 19",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_20",           477, 481,   5,  "str",  "Record-Axis Condition 20",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "reserved_4",               482, 500,  19,  "str",  "Reserved positions",
    ""
)

usethis::use_data(data_birth_cohort_1988, overwrite = TRUE)