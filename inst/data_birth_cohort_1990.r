data_birth_cohort_1990 <- tribble(
  ~name,               ~start, ~end, ~size, ~type,  ~description,                                     ~codes,
 
  # -- General (positions 1-6) -----------------------------------------------
  "matchs",                   1,   1,   1,  "int",  "Match Status",
    "1=Matched Birth/Infant Death Record|2=Late Filed Matched Birth/Infant Death Record|3=Surviving infant record|4=Unmatched infant death record (unlinked file only)",
 
  "idnumber",                 2,   6,   5,  "int",  "Infant Death Number (unique ID linking files)",
    "unique_integer=unique identifier linking numerator and denominator-plus files",
 
 
  # -- Birth Certificate Data (positions 7-212) ------------------------------
  "biryr",                    7,  10,   4,  "int",  "Year of Birth",
    "1990=Born in 1990",
 
  "resstatb",                11,  11,   1,  "int",  "Resident Status - Birth",
    "1=Residents|2=Intrastate Nonresidents (state same, county different)|3=Interstate Nonresidents (state different, both in 50 States/D.C.)|4=Foreign Residents",
 
  # -- Place of Occurrence - FIPS (positions 12-18) -------------------------
  "pldel",                   12,  12,   1,  "int",  "Place or Facility of Delivery",
    "1=Hospital|2=Freestanding Birthing Center|3=Clinic or Doctor's Office|4=A Residence|5=Other|9=Unknown or Not Stated",
 
  "birattnd",                13,  13,   1,  "int",  "Attendant at Delivery",
    "1=Doctor of Medicine (M.D.)|2=Doctor of Osteopathy (D.O.)|3=Certified Nurse Midwife (C.N.M.)|4=Other Midwife|5=Other|9=Unknown or not stated",
 
  "stoccfipb",               14,  15,   2,  "int",  "State of Occurrence (FIPS) - Birth",
    "01=Alabama|02=Alaska|04=Arizona|05=Arkansas|06=California|08=Colorado|09=Connecticut|10=Delaware|11=District of Columbia|12=Florida|13=Georgia|15=Hawaii|16=Idaho|17=Illinois|18=Indiana|19=Iowa|20=Kansas|21=Kentucky|22=Louisiana|23=Maine|24=Maryland|25=Massachusetts|26=Michigan|27=Minnesota|28=Mississippi|29=Missouri|30=Montana|31=Nebraska|32=Nevada|33=New Hampshire|34=New Jersey|35=New Mexico|36=New York|37=North Carolina|38=North Dakota|39=Ohio|40=Oklahoma|41=Oregon|42=Pennsylvania|44=Rhode Island|45=South Carolina|46=South Dakota|47=Tennessee|48=Texas|49=Utah|50=Vermont|51=Virginia|53=Washington|54=West Virginia|55=Wisconsin|56=Wyoming",
 
  "cntocfipb",               16,  18,   3,  "str",  "County of Occurrence (FIPS) - Birth",
    "001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population",
 
  # -- Place of Residence - FIPS (positions 19-23) ---------------------------
  "stresfipb",               19,  20,   2,  "int",  "State of Residence (FIPS) - Birth",
    "00=Foreign residents|01=Alabama|02=Alaska|04=Arizona|05=Arkansas|06=California|08=Colorado|09=Connecticut|10=Delaware|11=District of Columbia|12=Florida|13=Georgia|15=Hawaii|16=Idaho|17=Illinois|18=Indiana|19=Iowa|20=Kansas|21=Kentucky|22=Louisiana|23=Maine|24=Maryland|25=Massachusetts|26=Michigan|27=Minnesota|28=Mississippi|29=Missouri|30=Montana|31=Nebraska|32=Nevada|33=New Hampshire|34=New Jersey|35=New Mexico|36=New York|37=North Carolina|38=North Dakota|39=Ohio|40=Oklahoma|41=Oregon|42=Pennsylvania|44=Rhode Island|45=South Carolina|46=South Dakota|47=Tennessee|48=Texas|49=Utah|50=Vermont|51=Virginia|53=Washington|54=West Virginia|55=Wisconsin|56=Wyoming",
 
  "cntyrfpb",                21,  23,   3,  "str",  "County of Residence (FIPS) - Birth",
    "000=Foreign residents|001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population",
 
  # -- Place of Residence - NCHS Codes (positions 24-28) --------------------
  "brstate",                 24,  25,   2,  "int",  "State of Residence - NCHS Codes - Birth",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming|52=Puerto Rico|53=Virgin Islands|54=Guam|55=Canada|56=Cuba|57=Mexico|59=Remainder of the World",
 
  "cityresb",                26,  28,   3,  "str",  "City of Residence - NCHS Codes - Birth",
    "001-nnn=City code (alphabetical within state; population 250,000+ in 1980)|999=Entire county/balance of county/city less than 250,000|ZZZ=Foreign residents",
 
  # -- Mother's Age (positions 29-32) ----------------------------------------
  "mageflg",                 29,  29,   1,  "str",  "Age of Mother Flag",
    "blank=Not imputed and reported age not used|1=Reported age is used|2=Age is imputed",
 
  "dmage",                   30,  31,   2,  "int",  "Age of Mother",
    "10-49=Age in single years",
 
  "mager8",                  32,  32,   1,  "int",  "Age of Mother Recode 8",
    "1=Under 15 years|2=15-19 years|3=20-24 years|4=25-29 years|5=30-34 years|6=35-39 years|7=40-44 years|8=45-49 years",
 
  # -- Mother's Hispanic Origin and Race (positions 33-38) -------------------
  "ormoth",                  33,  33,   1,  "int",  "Hispanic Origin of Mother",
    "0=Non-Hispanic|1=Mexican|2=Puerto Rican|3=Cuban|4=Central or South American|5=Other and unknown Hispanic|9=Origin unknown or not stated",
 
  "orracem",                 34,  34,   1,  "int",  "Hispanic Origin and Race of Mother Recode",
    "1=Mexican|2=Puerto Rican|3=Cuban|4=Central or South American|5=Other and unknown Hispanic|6=Non-Hispanic White|7=Non-Hispanic Black|8=Non-Hispanic other races|9=Origin unknown or not stated",
 
  "mraceimp",                35,  35,   1,  "str",  "Race of Mother Imputation Flag",
    "blank=Race is not imputed|1=Race is imputed",
 
  "mrace",                   36,  37,   2,  "int",  "Race of Mother",
    "01=White|02=Black|03=American Indian (includes Aleuts and Eskimos)|04=Chinese|05=Japanese|06=Hawaiian (includes Part-Hawaiian)|07=Filipino|08=Other Asian or Pacific Islander|09=All other races",
 
  "mrace3",                  38,  38,   1,  "int",  "Race of Mother Recode",
    "1=White|2=Races other than White or Black|3=Black",
 
  # -- Mother's Education (positions 39-41) ----------------------------------
  "dmeduc",                  39,  40,   2,  "int",  "Education of Mother Detail",
    "00=No formal education|01-08=Years of elementary school|09=1 year of high school|10=2 years of high school|11=3 years of high school|12=4 years of high school|13=1 year of college|14=2 years of college|15=3 years of college|16=4 years of college|17=5 or more years of college|99=Not stated",
 
  "meduc6",                  41,  41,   1,  "int",  "Education of Mother Recode",
    "1=0-8 years|2=9-11 years|3=12 years|4=13-15 years|5=16 years and over|6=Not stated",
 
  # -- Marital Status (positions 42-43) --------------------------------------
  "dmarimp",                 42,  42,   1,  "str",  "Marital Status of Mother Imputation Flag",
    "blank=Marital status not imputed|1=Marital status is imputed",
 
  "dmar",                    43,  43,   1,  "int",  "Marital Status of Mother",
    "1=Married|2=Unmarried",
 
  # -- Mother's Place of Birth (positions 44-46) -----------------------------
  "mplbir",                  44,  45,   2,  "int",  "Place of Birth of Mother",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming|52=Puerto Rico|53=Virgin Islands|54=Guam|55=Canada|56=Cuba|57=Mexico|59=Remainder of the world|99=Not classifiable",
 
  "mplbirr",                 46,  46,   1,  "int",  "Place of Birth of Mother Recode",
    "1=Born in the 50 States and D.C.|2=Born outside the 50 States and D.C.|3=Unknown or not stated",
 
  # -- Birth Order (positions 47-50) -----------------------------------------
  "dtotord",                 47,  48,   2,  "int",  "Detail Total Birth Order",
    "01-40=Total number of live births and other terminations of pregnancy|99=Unknown",
 
  "dlivord",                 49,  50,   2,  "int",  "Detail Live Birth Order",
    "00-31=Number of children born alive to mother|99=Unknown",
 
  # -- Prenatal Care (positions 51-56) ---------------------------------------
  "monpre",                  51,  52,   2,  "int",  "Detail Month of Pregnancy Prenatal Care Began",
    "00=No prenatal care|01=1st month|02=2nd month|03=3rd month|04=4th month|05=5th month|06=6th month|07=7th month|08=8th month|09=9th month|99=Unknown or not stated",
 
  "mpre5",                   53,  53,   1,  "int",  "Month Prenatal Care Began Recode 5",
    "1=1st Trimester (1st-3rd month)|2=2nd Trimester (4th-6th month)|3=3rd Trimester (7th-9th month)|4=No prenatal care|5=Unknown or not stated",
 
  "nprevist",                54,  55,   2,  "int",  "Total Number of Prenatal Visits",
    "00=No prenatal visits|01-48=Stated number of visits|49=49 or more visits|99=Unknown or not stated",
 
  "adequacy",                56,  56,   1,  "int",  "Adequacy of Care Recode (Kessner Index)",
    "1=Adequate|2=Intermediate|3=Inadequate|4=Unknown",
 
  # -- Birth Interval (positions 57-59) --------------------------------------
  "disllb",                  57,  59,   3,  "int",  "Interval Since Last Live Birth (months)",
    "777=No previous live birth|000=Zero months (plural birth)|001-468=One to 468 months|999=Unknown",
 
  # -- Father (positions 60-68) ----------------------------------------------
  "fagerflg",                60,  60,   1,  "str",  "Reported Age of Father Used Flag",
    "blank=Reported age not used|1=Reported age is used",
 
  "dfage",                   61,  62,   2,  "int",  "Age of Father",
    "10-98=Age in single years|99=Unknown or not stated",
 
  "orfath",                  63,  63,   1,  "int",  "Hispanic Origin of Father",
    "0=Non-Hispanic|1=Mexican|2=Puerto Rican|3=Cuban|4=Central or South American|5=Other and unknown Hispanic|9=Origin unknown or not stated",
 
  "orracef",                 64,  64,   1,  "int",  "Hispanic Origin and Race of Father Recode",
    "1=Mexican|2=Puerto Rican|3=Cuban|4=Central or South American|5=Other and unknown Hispanic|6=Non-Hispanic White|7=Non-Hispanic Black|8=Non-Hispanic other or unknown race|9=Origin unknown or not stated",
 
  "frace",                   65,  66,   2,  "int",  "Race of Father",
    "01=White|02=Black|03=American Indian (includes Aleuts and Eskimos)|04=Chinese|05=Japanese|06=Hawaiian (includes Part-Hawaiian)|07=Filipino|08=Other Asian or Pacific Islander|09=All other races|99=Unknown or not stated",
 
  "dfeduc",                  67,  68,   2,  "int",  "Education of Father Detail",
    "00=No formal education|01-08=Years of elementary school|09=1 year of high school|10=2 years of high school|11=3 years of high school|12=4 years of high school|13=1 year of college|14=2 years of college|15=3 years of college|16=4 years of college|17=5 or more years of college|99=Not stated",
 
  # -- Child's Birth Month (positions 69-71) ---------------------------------
  "cdobmimp",                69,  69,   1,  "str",  "Month of Birth of Child Imputation Flag",
    "blank=Month not imputed|1=Month is imputed",
 
  "birmon",                  70,  71,   2,  "int",  "Month of Birth",
    "01=January|02=February|03=March|04=April|05=May|06=June|07=July|08=August|09=September|10=October|11=November|12=December",
 
  # -- Gestation (positions 72-76) -------------------------------------------
  "gestflg",                 72,  72,   1,  "str",  "Clinical Estimate of Gestation Used or Gestation Imputed Flag",
    "blank=Not imputed and clinical estimate not used|1=Clinical estimate is used|2=Gestation is imputed",
 
  "gestat",                  73,  74,   2,  "int",  "Gestation Detail in Weeks",
    "17-47=17th through 47th week of gestation|99=Unknown",
 
  "gestat_r10",              75,  76,   2,  "int",  "Gestation Recode 10",
    "01=Under 20 weeks|02=20-27 weeks|03=28-31 weeks|04=32-35 weeks|05=36 weeks|06=37-39 weeks|07=40 weeks|08=41 weeks|09=42 weeks and over|10=Not stated",
 
  # -- Sex and Birthweight (positions 77-85) ---------------------------------
  "cseximp",                 77,  77,   1,  "str",  "Sex Imputation Flag",
    "blank=Sex not imputed|1=Sex is imputed",
 
  "csex",                    78,  78,   1,  "int",  "Sex",
    "1=Male|2=Female",
 
  "dbirwt",                  79,  82,   4,  "int",  "Birth Weight Detail in Grams",
    "0227-8165=Number of grams|9999=Not stated",
 
  "birwt12",                 83,  84,   2,  "int",  "Birth Weight Recode 12",
    "01=499 grams or less|02=500-999 grams|03=1000-1499 grams|04=1500-1999 grams|05=2000-2499 grams|06=2500-2999 grams|07=3000-3499 grams|08=3500-3999 grams|09=4000-4499 grams|10=4500-4999 grams|11=5000-8165 grams|12=Unknown or not stated",
 
  "birwt4",                  85,  85,   1,  "int",  "Birth Weight Recode 4",
    "1=1499 grams or less|2=1500-2499 grams|3=2500 grams or more|4=Unknown or not stated",
 
  # -- Plurality and Apgar (positions 86-91) ---------------------------------
  "plurimp",                 86,  86,   1,  "str",  "Plurality Imputation Flag",
    "blank=Plurality not imputed|1=Plurality is imputed",
 
  "dplural",                 87,  87,   1,  "int",  "Plurality",
    "1=Single|2=Twin|3=Triplet|4=Quadruplet|5=Quintuplet or higher",
 
  "omaps",                   88,  89,   2,  "int",  "One Minute Apgar Score",
    "00-10=A score of 0-10|99=Unknown or not stated",
 
  "fmaps",                   90,  91,   2,  "int",  "Five Minute Apgar Score",
    "00-10=A score of 0-10|99=Unknown or not stated",
 
  # -- Medical and Health Data (positions 92-186) ----------------------------
  # Method of Delivery (positions 92-99)
  "vaginal",                 92,  92,   1,  "int",  "Method of Delivery: Vaginal",
    "1=Used|2=Not used|8=Not on certificate|9=Unknown or not stated",
 
  "vbac",                    93,  93,   1,  "int",  "Method of Delivery: Vaginal Birth After Previous C-Section",
    "1=Used|2=Not used|8=Not on certificate|9=Unknown or not stated",
 
  "primac",                  94,  94,   1,  "int",  "Method of Delivery: Primary C-Section",
    "1=Used|2=Not used|8=Not on certificate|9=Unknown or not stated",
 
  "repeac",                  95,  95,   1,  "int",  "Method of Delivery: Repeat C-Section",
    "1=Used|2=Not used|8=Not on certificate|9=Unknown or not stated",
 
  "forcep",                  96,  96,   1,  "int",  "Method of Delivery: Forceps",
    "1=Used|2=Not used|8=Not on certificate|9=Unknown or not stated",
 
  "vacuum",                  97,  97,   1,  "int",  "Method of Delivery: Vacuum",
    "1=Used|2=Not used|8=Not on certificate|9=Unknown or not stated",
 
  "reserved_3",              98,  98,   1,  "str",  "Reserved position",
    "",
 
  "delmeth5",                99,  99,   1,  "int",  "Method of Delivery Recode",
    "1=Vaginal (excl. vaginal after previous C-section)|2=Vaginal birth after previous C-section|3=Primary C-section|4=Repeat C-section|5=Not stated",
 
  "reserved_4",             100, 100,   1,  "str",  "Reserved position",
    "",
 
  # Medical Risk Factors (positions 101-117)
  "anemia",                 101, 101,   1,  "int",  "Medical Risk: Anemia (Hct.<30/Hgb.<10)",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "cardiac",                102, 102,   1,  "int",  "Medical Risk: Cardiac Disease",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "lung",                   103, 103,   1,  "int",  "Medical Risk: Acute or Chronic Lung Disease",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "diabetes",               104, 104,   1,  "int",  "Medical Risk: Diabetes",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "herpes",                 105, 105,   1,  "int",  "Medical Risk: Genital Herpes",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "hydra",                  106, 106,   1,  "int",  "Medical Risk: Hydramnios/Oligohydramnios",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "hemo",                   107, 107,   1,  "int",  "Medical Risk: Hemoglobinopathy",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "chyper",                 108, 108,   1,  "int",  "Medical Risk: Hypertension, Chronic",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "phyper",                 109, 109,   1,  "int",  "Medical Risk: Hypertension, Pregnancy-Associated",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "eclamp",                 110, 110,   1,  "int",  "Medical Risk: Eclampsia",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "incervix",               111, 111,   1,  "int",  "Medical Risk: Incompetent Cervix",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "pre4000",                112, 112,   1,  "int",  "Medical Risk: Previous Infant 4000+ Grams",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "preterm",                113, 113,   1,  "int",  "Medical Risk: Previous Preterm or Small-for-Gestational-Age Infant",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "renal",                  114, 114,   1,  "int",  "Medical Risk: Renal Disease",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "rh_sensitization",       115, 115,   1,  "int",  "Medical Risk: Rh Sensitization",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "uterine",                116, 116,   1,  "int",  "Medical Risk: Uterine Bleeding",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  "othermr",                117, 117,   1,  "int",  "Medical Risk: Other Medical Risk Factors",
    "1=Factor reported|2=Factor not reported|8=Not on certificate|9=Not classifiable",
 
  # Other Risk Factors (positions 118-128)
  "tobacco",                118, 118,   1,  "int",  "Tobacco Use During Pregnancy",
    "1=Yes|2=No|9=Unknown or not stated",
 
  "cigar",                  119, 120,   2,  "int",  "Average Number of Cigarettes Per Day",
    "00-97=As stated|98=98 or more cigarettes per day|99=Unknown or not stated",
 
  "cigar6",                 121, 121,   1,  "int",  "Average Number of Cigarettes Per Day Recode",
    "0=Nonsmoker|1=1-5 cigarettes per day|2=6-10 cigarettes per day|3=11-20 cigarettes per day|4=21-40 cigarettes per day|5=41 or more cigarettes per day|6=Unknown or not stated",
 
  "alcohol",                122, 122,   1,  "int",  "Alcohol Use During Pregnancy",
    "1=Yes|2=No|9=Unknown or not stated",
 
  "drink",                  123, 124,   2,  "int",  "Average Number of Drinks Per Week",
    "00-97=As stated|98=98 or more drinks per week|99=Unknown or not stated",
 
  "drink5",                 125, 125,   1,  "int",  "Average Number of Drinks Per Week Recode",
    "0=Non drinker|1=1 drink per week|2=2 drinks per week|3=3-4 drinks per week|4=5 or more drinks per week|5=Unknown or not stated",
 
  "wtgain",                 126, 127,   2,  "int",  "Weight Gain During Pregnancy (pounds)",
    "00-97=Stated number of pounds|98=98 pounds or more|99=Unknown or not stated",
 
  "wtgain9",                128, 128,   1,  "int",  "Weight Gain Recode",
    "1=Less than 16 pounds|2=16-20 pounds|3=21-25 pounds|4=26-30 pounds|5=31-35 pounds|6=36-40 pounds|7=41-45 pounds|8=46 or more pounds|9=Unknown or not stated",
 
  "reserved_5",             129, 129,   1,  "str",  "Reserved position",
    "",
 
  # Obstetric Procedures (positions 130-137)
  "amnio",                  130, 130,   1,  "int",  "Obstetric Procedure: Amniocentesis",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "monitor",                131, 131,   1,  "int",  "Obstetric Procedure: Electronic Fetal Monitoring",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "induct",                 132, 132,   1,  "int",  "Obstetric Procedure: Induction of Labor",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "stimula",                133, 133,   1,  "int",  "Obstetric Procedure: Stimulation of Labor",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "tocol",                  134, 134,   1,  "int",  "Obstetric Procedure: Tocolysis",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "ultras",                 135, 135,   1,  "int",  "Obstetric Procedure: Ultrasound",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "otherob",                136, 136,   1,  "int",  "Obstetric Procedure: Other Obstetric Procedures",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "reserved_6",             137, 137,   1,  "str",  "Reserved position",
    "",
 
  # Complications of Labor and/or Delivery (positions 138-154)
  "febrile",                138, 138,   1,  "int",  "Labor Complication: Febrile (>100-F or 38-C)",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "meconium",               139, 139,   1,  "int",  "Labor Complication: Meconium, Moderate/Heavy",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "rupture",                140, 140,   1,  "int",  "Labor Complication: Premature Rupture of Membrane (>12 hours)",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "abruptio",               141, 141,   1,  "int",  "Labor Complication: Abruptio Placenta",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "preplace",               142, 142,   1,  "int",  "Labor Complication: Placenta Previa",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "excebld",                143, 143,   1,  "int",  "Labor Complication: Other Excessive Bleeding",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "seizure",                144, 144,   1,  "int",  "Labor Complication: Seizures During Labor",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "precip",                 145, 145,   1,  "int",  "Labor Complication: Precipitous Labor (<3 hours)",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "prolong",                146, 146,   1,  "int",  "Labor Complication: Prolonged Labor (>20 hours)",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "dysfunc",                147, 147,   1,  "int",  "Labor Complication: Dysfunctional Labor",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "breech",                 148, 148,   1,  "int",  "Labor Complication: Breech/Malpresentation",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "cephalo",                149, 149,   1,  "int",  "Labor Complication: Cephalopelvic Disproportion",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "cord",                   150, 150,   1,  "int",  "Labor Complication: Cord Prolapse",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "anesthe",                151, 151,   1,  "int",  "Labor Complication: Anesthetic Complications",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "distress",               152, 152,   1,  "int",  "Labor Complication: Fetal Distress",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "otherlb",                153, 153,   1,  "int",  "Labor Complication: Other Complication of Labor and/or Delivery",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "reserved_7",             154, 154,   1,  "str",  "Reserved position",
    "",
 
  # Abnormal Conditions of the Newborn (positions 155-164)
  "nanemia",                155, 155,   1,  "int",  "Newborn Condition: Anemia (Hct.>39/Hgb.<13)",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "injury",                 156, 156,   1,  "int",  "Newborn Condition: Birth Injury",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "alcosyn",                157, 157,   1,  "int",  "Newborn Condition: Fetal Alcohol Syndrome",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "hyaline",                158, 158,   1,  "int",  "Newborn Condition: Hyaline Membrane Disease",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "meconsyn",               159, 159,   1,  "int",  "Newborn Condition: Meconium Aspiration Syndrome",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "ven_lt30",               160, 160,   1,  "int",  "Newborn Condition: Assisted Ventilation, Less Than 30 Minutes",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "ven_ge30",               161, 161,   1,  "int",  "Newborn Condition: Assisted Ventilation, 30 Minutes or More",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "nseiz",                  162, 162,   1,  "int",  "Newborn Condition: Seizures",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "otherab",                163, 163,   1,  "int",  "Newborn Condition: Other Abnormal Conditions",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "reserved_8",             164, 164,   1,  "str",  "Reserved position",
    "",
 
  # Congenital Anomalies (positions 165-186)
  "anen",                   165, 165,   1,  "int",  "Congenital Anomaly: Anencephalus",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "spina",                  166, 166,   1,  "int",  "Congenital Anomaly: Spina Bifida/Meningocele",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "hydro",                  167, 167,   1,  "int",  "Congenital Anomaly: Hydrocephalus",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "microce",                168, 168,   1,  "int",  "Congenital Anomaly: Microcephalus",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "nervous",                169, 169,   1,  "int",  "Congenital Anomaly: Other Central Nervous System Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "heart",                  170, 170,   1,  "int",  "Congenital Anomaly: Heart Malformations",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "circul",                 171, 171,   1,  "int",  "Congenital Anomaly: Other Circulatory/Respiratory Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "rectal",                 172, 172,   1,  "int",  "Congenital Anomaly: Rectal Atresia/Stenosis",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "tracheo",                173, 173,   1,  "int",  "Congenital Anomaly: Tracheo-Esophageal Fistula/Esophageal Atresia",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "omphalo",                174, 174,   1,  "int",  "Congenital Anomaly: Omphalocele/Gastroschisis",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "gastro",                 175, 175,   1,  "int",  "Congenital Anomaly: Other Gastrointestinal Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "genital",                176, 176,   1,  "int",  "Congenital Anomaly: Malformed Genitalia",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "renalage",               177, 177,   1,  "int",  "Congenital Anomaly: Renal Agenesis",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "urogen",                 178, 178,   1,  "int",  "Congenital Anomaly: Other Urogenital Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "cleftlp",                179, 179,   1,  "int",  "Congenital Anomaly: Cleft Lip/Palate",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "adactyly",               180, 180,   1,  "int",  "Congenital Anomaly: Polydactyly/Syndactyly/Adactyly",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "clubfoot",               181, 181,   1,  "int",  "Congenital Anomaly: Club Foot",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "hernia",                 182, 182,   1,  "int",  "Congenital Anomaly: Diaphragmatic Hernia",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "musculo",                183, 183,   1,  "int",  "Congenital Anomaly: Other Musculoskeletal/Integumental Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "downs",                  184, 184,   1,  "int",  "Congenital Anomaly: Down's Syndrome",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "chromo",                 185, 185,   1,  "int",  "Congenital Anomaly: Other Chromosomal Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  "othercon",               186, 186,   1,  "int",  "Congenital Anomaly: Other Congenital Anomalies",
    "1=Reported|2=Not reported|8=Not on certificate|9=Not classifiable",
 
  # Reporting Flags for Place of Residence (positions 187-206)
  "origm_rf",               187, 187,   1,  "int",  "Reporting Flag: Origin of Mother",
    "0=Not reported|1=Reported or partially reported",
 
  "origf_rf",               188, 188,   1,  "int",  "Reporting Flag: Origin of Father",
    "0=Not reported|1=Reported or partially reported",
 
  "educm_rf",               189, 189,   1,  "int",  "Reporting Flag: Education of Mother",
    "0=Not reported|1=Reported or partially reported",
 
  "educf_rf",               190, 190,   1,  "int",  "Reporting Flag: Education of Father",
    "0=Not reported|1=Reported or partially reported",
 
  "geste_rf",               191, 191,   1,  "int",  "Reporting Flag: Clinical Estimate of Gestation",
    "0=Not reported|1=Reported or partially reported",
 
  "omapsrf",                192, 192,   1,  "int",  "Reporting Flag: 1-Minute Apgar Score",
    "0=Not reported|1=Reported or partially reported",
 
  "fmapsrf",                193, 193,   1,  "int",  "Reporting Flag: 5-Minute Apgar Score",
    "0=Not reported|1=Reported or partially reported",
 
  "delmetrf",               194, 194,   1,  "int",  "Reporting Flag: Method of Delivery",
    "0=Not reported|1=Reported or partially reported",
 
  "medrsk_rf",              195, 195,   1,  "int",  "Reporting Flag: Medical Risk Factors",
    "0=Not reported|1=Reported or partially reported",
 
  "tobuse_rf",              196, 196,   1,  "int",  "Reporting Flag: Tobacco Use",
    "0=Not reported|1=Reported or partially reported",
 
  "alcuse_rf",              197, 197,   1,  "int",  "Reporting Flag: Alcohol Use",
    "0=Not reported|1=Reported or partially reported",
 
  "wtgn_rf",                198, 198,   1,  "int",  "Reporting Flag: Weight Gain",
    "0=Not reported|1=Reported or partially reported",
 
  "obstrc_rf",              199, 199,   1,  "int",  "Reporting Flag: Obstetric Procedures",
    "0=Not reported|1=Reported or partially reported",
 
  "clabor_rf",              200, 200,   1,  "int",  "Reporting Flag: Complications of Labor and/or Delivery",
    "0=Not reported|1=Reported or partially reported",
 
  "abnml_rf",               201, 201,   1,  "int",  "Reporting Flag: Abnormal Conditions of Newborn",
    "0=Not reported|1=Reported or partially reported",
 
  "congan_rf",              202, 202,   1,  "int",  "Reporting Flag: Congenital Anomalies",
    "0=Not reported|1=Reported or partially reported",
 
  "reserved_9",             203, 203,   1,  "str",  "Reserved position",
    "",
 
  "educsmsa_rf",            204, 204,   1,  "int",  "Reporting Flag: Education of Mother (Based on SMSA)",
    "0=Not reported|1=Reported or partially reported",
 
  "reserved_10",            205, 206,   2,  "str",  "Reserved positions",
    "",
 
  # -- Day of Week and Race of Child (positions 207-212) ---------------------
  "weekdayb",               207, 207,   1,  "int",  "Day of Week Child Born",
    "1=Sunday|2=Monday|3=Tuesday|4=Wednesday|5=Thursday|6=Friday|7=Saturday",
 
  "reserved_11",            208, 208,   1,  "str",  "Reserved position",
    "",
 
  "crace",                  209, 210,   2,  "int",  "Race of Child",
    "01=White|02=Black|03=American Indian (includes Aleuts and Eskimos)|04=Chinese|05=Japanese|06=Hawaiian (includes Part-Hawaiian)|07=Filipino|08=Other Asian or Pacific Islander|09=All other races",
 
  "reserved_12",            211, 212,   2,  "str",  "Reserved positions",
    "",
 
 
  # --------------------------------------------------------------------------
  # MORTALITY SECTION - positions 213-535
  # Data in 213-225: present on BOTH Denominator-Plus and Numerator files
  # Data in 226-535: Numerator (Linked) file only
  # --------------------------------------------------------------------------
 
  # Infant Age at Death (positions 213-215; shared with Denominator-Plus)
  "aged",                     213, 215,   3,  "int",  "Age at Death in Days (computed from dates of birth/death; reported age used if <2 days or uncomputable)",
    "000-364=Number of days",
  "ager5",                    216, 216,   1,  "int",  "Infant Age Recode 5",
    "1=Under 1 hour|2=1-23 hours|3=1-6 days|4=7-27 days (late neonatal)|5=28 days and over (postneonatal)",
  "autopsy",                  217, 217,   1,  "int",  "Autopsy Performed",
    "1=Yes|2=No|8=Not on certificate|9=Not stated",
  "accidpl",                  218, 218,   1,  "str",  "Place of Accident for Causes E850-E869 and E880-E928",
    "blank=Causes other than E850-E869 and E880-E928|0=Home|1=Farm|2=Mine and quarry|3=Industrial place and premises|4=Place for recreation and sport|5=Street and highway|6=Public building|7=Resident institution|8=Other specified places|9=Place of accident not specified",
  "ucod",                     219, 222,   4,  "str",  "ICD Code (9th Revision) - Underlying Cause of Death",
    "format=ICD-9 9th revision|external_cause=E800 through E999 coded without letter E|4th_digit=blank when cause has no 4th digit",
 
  "ucodr61",                  223, 225,   3,  "int",  "Infant Cause of Death Recode 61 Groups (shared with Denominator-Plus)",
    "010-680=Code range (not inclusive); see cause-of-death list in documentation",
 
  # Numerator-only mortality data (positions 226-535)
  "reserved_mort",            226, 260,  35,  "str",  "Reserved positions (Numerator only)",
    "",
 
  # Multiple Conditions (positions 261-504)
  "ea_num",                   261, 262,   2,  "int",  "Number of Entity-Axis Conditions",
    "00-20=Code range",
  "entity_axis_01",           263, 269,   7,  "str",  "Entity-Axis Condition 1 (part/line, sequence, ICD-9 x4, injury flag)",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_02",           270, 276,   7,  "str",  "Entity-Axis Condition 2",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_03",           277, 283,   7,  "str",  "Entity-Axis Condition 3",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_04",           284, 290,   7,  "str",  "Entity-Axis Condition 4",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_05",           291, 297,   7,  "str",  "Entity-Axis Condition 5",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_06",           298, 304,   7,  "str",  "Entity-Axis Condition 6",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_07",           305, 311,   7,  "str",  "Entity-Axis Condition 7",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_08",           312, 318,   7,  "str",  "Entity-Axis Condition 8",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_09",           319, 325,   7,  "str",  "Entity-Axis Condition 9",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_10",           326, 332,   7,  "str",  "Entity-Axis Condition 10",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_11",           333, 339,   7,  "str",  "Entity-Axis Condition 11",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_12",           340, 346,   7,  "str",  "Entity-Axis Condition 12",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_13",           347, 353,   7,  "str",  "Entity-Axis Condition 13",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_14",           354, 360,   7,  "str",  "Entity-Axis Condition 14",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_15",           361, 367,   7,  "str",  "Entity-Axis Condition 15",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_16",           368, 374,   7,  "str",  "Entity-Axis Condition 16",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_17",           375, 381,   7,  "str",  "Entity-Axis Condition 17",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_18",           382, 388,   7,  "str",  "Entity-Axis Condition 18",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_19",           389, 395,   7,  "str",  "Entity-Axis Condition 19",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "entity_axis_20",           396, 402,   7,  "str",  "Entity-Axis Condition 20",
    "part_or_line_1=Part I line a|part_or_line_2=Part I line b|part_or_line_3=Part I line c|part_or_line_4=Part I line d|part_or_line_5=Part I line e|part_or_line_6=Part II|sequence=1 through 7 within part/line|icd9_code=ICD-9 condition code (positions 3-6 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "ra_num",                   403, 404,   2,  "int",  "Number of Record-Axis Conditions",
    "00-20=Code range",
  "record_axis_01",           405, 409,   5,  "str",  "Record-Axis Condition 1 (ICD-9 x4, injury flag)",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_02",           410, 414,   5,  "str",  "Record-Axis Condition 2",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_03",           415, 419,   5,  "str",  "Record-Axis Condition 3",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_04",           420, 424,   5,  "str",  "Record-Axis Condition 4",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_05",           425, 429,   5,  "str",  "Record-Axis Condition 5",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_06",           430, 434,   5,  "str",  "Record-Axis Condition 6",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_07",           435, 439,   5,  "str",  "Record-Axis Condition 7",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_08",           440, 444,   5,  "str",  "Record-Axis Condition 8",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_09",           445, 449,   5,  "str",  "Record-Axis Condition 9",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_10",           450, 454,   5,  "str",  "Record-Axis Condition 10",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_11",           455, 459,   5,  "str",  "Record-Axis Condition 11",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_12",           460, 464,   5,  "str",  "Record-Axis Condition 12",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_13",           465, 469,   5,  "str",  "Record-Axis Condition 13",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_14",           470, 474,   5,  "str",  "Record-Axis Condition 14",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_15",           475, 479,   5,  "str",  "Record-Axis Condition 15",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_16",           480, 484,   5,  "str",  "Record-Axis Condition 16",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_17",           485, 489,   5,  "str",  "Record-Axis Condition 17",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_18",           490, 494,   5,  "str",  "Record-Axis Condition 18",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_19",           495, 499,   5,  "str",  "Record-Axis Condition 19",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
  "record_axis_20",           500, 504,   5,  "str",  "Record-Axis Condition 20",
    "icd9_code=ICD-9 condition code (positions 1-4 of sub-field)|injury_flag_0=not a nature-of-injury code|injury_flag_1=nature of injury code",
 
 
 
  # Mortality Geographic and Admin (positions 505-528)
  "dth_resstat",              505, 505,   1,  "int",  "Resident Status - Death",
    "1=Residents|2=Intrastate Nonresidents (state same, county different)|3=Interstate Nonresidents (state different, both in 50 States/D.C.)|4=Foreign Residents",
  "dth_stoccfipd",            506, 507,   2,  "int",  "State of Occurrence (FIPS) - Death",
    "01=Alabama|02=Alaska|04=Arizona|05=Arkansas|06=California|08=Colorado|09=Connecticut|10=Delaware|11=District of Columbia|12=Florida|13=Georgia|15=Hawaii|16=Idaho|17=Illinois|18=Indiana|19=Iowa|20=Kansas|21=Kentucky|22=Louisiana|23=Maine|24=Maryland|25=Massachusetts|26=Michigan|27=Minnesota|28=Mississippi|29=Missouri|30=Montana|31=Nebraska|32=Nevada|33=New Hampshire|34=New Jersey|35=New Mexico|36=New York|37=North Carolina|38=North Dakota|39=Ohio|40=Oklahoma|41=Oregon|42=Pennsylvania|44=Rhode Island|45=South Carolina|46=South Dakota|47=Tennessee|48=Texas|49=Utah|50=Vermont|51=Virginia|53=Washington|54=West Virginia|55=Wisconsin|56=Wyoming",
  "dth_cntocfipd",            508, 510,   3,  "str",  "County of Occurrence (FIPS) - Death",
    "001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population",
  "dth_stresfipd",            511, 512,   2,  "int",  "State of Residence (FIPS) - Death",
    "00=Foreign residents|01=Alabama|02=Alaska|04=Arizona|05=Arkansas|06=California|08=Colorado|09=Connecticut|10=Delaware|11=District of Columbia|12=Florida|13=Georgia|15=Hawaii|16=Idaho|17=Illinois|18=Indiana|19=Iowa|20=Kansas|21=Kentucky|22=Louisiana|23=Maine|24=Maryland|25=Massachusetts|26=Michigan|27=Minnesota|28=Mississippi|29=Missouri|30=Montana|31=Nebraska|32=Nevada|33=New Hampshire|34=New Jersey|35=New Mexico|36=New York|37=North Carolina|38=North Dakota|39=Ohio|40=Oklahoma|41=Oregon|42=Pennsylvania|44=Rhode Island|45=South Carolina|46=South Dakota|47=Tennessee|48=Texas|49=Utah|50=Vermont|51=Virginia|53=Washington|54=West Virginia|55=Wisconsin|56=Wyoming",
  "dth_cntyrfpd",             513, 515,   3,  "str",  "County of Residence (FIPS) - Death",
    "000=Foreign residents|001-nnn=County code (numbered alphabetically within state)|999=County with less than 250,000 population",
  "dth_drstate",              516, 517,   2,  "int",  "State of Residence - NCHS Codes - Death",
    "01=Alabama|02=Alaska|03=Arizona|04=Arkansas|05=California|06=Colorado|07=Connecticut|08=Delaware|09=District of Columbia|10=Florida|11=Georgia|12=Hawaii|13=Idaho|14=Illinois|15=Indiana|16=Iowa|17=Kansas|18=Kentucky|19=Louisiana|20=Maine|21=Maryland|22=Massachusetts|23=Michigan|24=Minnesota|25=Mississippi|26=Missouri|27=Montana|28=Nebraska|29=Nevada|30=New Hampshire|31=New Jersey|32=New Mexico|33=New York|34=North Carolina|35=North Dakota|36=Ohio|37=Oklahoma|38=Oregon|39=Pennsylvania|40=Rhode Island|41=South Carolina|42=South Dakota|43=Tennessee|44=Texas|45=Utah|46=Vermont|47=Virginia|48=Washington|49=West Virginia|50=Wisconsin|51=Wyoming|52=Puerto Rico|53=Virgin Islands|54=Guam|55=Canada|56=Cuba|57=Mexico|59=Remainder of the World",
  "dth_cityresd",             518, 520,   3,  "str",  "City of Residence - NCHS Codes - Death",
    "001-nnn=City code (alphabetical within state; population 250,000+ in 1980)|999=Balance of county|ZZZ=Foreign residents",
  "hospd",                    521, 521,   1,  "int",  "Hospital and Patient Status - Death",
    "1=Hospital/Clinic/Medical Center - Inpatient|2=Hospital/Clinic/Medical Center - Outpatient or admitted to Emergency Room|3=Hospital/Clinic/Medical Center - Dead on arrival|4=Hospital/Clinic/Medical Center - Patient status unknown|5=Nursing home|6=Residence|7=Other|9=Place of death unknown",
  "dth_yy",                   522, 525,   4,  "int",  "Year of Death",
    "1990=Death occurred in 1990|1991=Death occurred in 1991",
  "dth_mon",                  526, 527,   2,  "int",  "Month of Death",
    "01=January|02=February|03=March|04=April|05=May|06=June|07=July|08=August|09=September|10=October|11=November|12=December",
  "weekdayd",                 528, 528,   1,  "int",  "Day of Week of Death",
    "1=Sunday|2=Monday|3=Tuesday|4=Wednesday|5=Thursday|6=Friday|7=Saturday|9=Unknown",
  "reserved_end",             529, 535,   7,  "str",  "Reserved positions",
    ""
 
)

usethis::use_data(data_birth_cohort_1990, overwrite = TRUE)