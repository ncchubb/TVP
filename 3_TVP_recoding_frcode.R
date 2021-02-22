## The Violence Project: R Translation (socsci package)
# Make sure the (socsci) package is installed:

library(socsci)
library(dplyr)
library(lubridate)
install.packages(remotes)
install.packages("devtools")
devtools::install_github("ryanburge/socsci")

# See https://rdrr.io/github/ryanburge/socsci/f/README.Rmd for details on the package.

tvp4 <- tvp3 %>% 
  mutate(state = frcode(state == 1 ~ "Alabama",
                               state == 2 ~ "Alaska",
                               state == 3 ~ "Arizona",
                               state == 4 ~ "Arkansas",
                               state == 5 ~ "California",
                               state == 6 ~ "Colorado",
                               state == 7 ~ "Connecticut",
                               state == 8 ~ "Delaware",
                               state == 9 ~ "Florida",
                               state == 10 ~ "Georgia",
                               state == 11 ~ "Hawaii",
                               state == 12 ~ "Idaho",
                               state == 13 ~ "Illinois",
                               state == 14 ~ "Indiana",
                               state == 15 ~ "Iowa",
                               state == 16 ~ "Kansas",
                               state == 17 ~ "Kentucky",
                               state == 18 ~ "Louisiana",
                               state == 19 ~ "Maine",
                               state == 20 ~ "Maryland",
                               state == 21 ~ "Massacusetts",
                               state == 22 ~ "Michigan",
                               state == 23 ~ "Minnesota",
                               state == 24 ~ "Mississippi",
                               state == 25 ~ "Missouri",
                               state == 26 ~ "Montana",
                               state == 27 ~ "Nebraska",
                               state == 28 ~ "Nevada",
                               state == 29 ~ "New Hampshire",
                               state == 30 ~ "New Jersey",
                               state == 31 ~ "New Mexico",
                               state == 32 ~ "New York",
                               state == 33 ~ "North Carolina",
                               state == 34 ~ "North Dakota",
                               state == 35 ~ "Ohio",
                               state == 36 ~ "Oklahoma",
                               state == 37 ~ "Oregon",
                               state == 38 ~ "Pennsylvania",
                               state == 39 ~ "Rhode Island",
                               state == 40 ~ "South Carolina",
                               state == 41 ~ "South Dakota",
                               state == 42 ~ "Tennessee",
                               state == 43 ~ "Texas",
                               state == 44 ~ "Utah",
                               state == 45 ~ "Vermont",
                               state == 46 ~ "Virginia",
                               state == 47 ~ "Washington",
                               state == 48 ~ "West Virginia",
                               state == 49 ~ "Wisconsin",
                               state == 50 ~ "Wyoming"
                               )) %>% 
  mutate(region = frcode(region == 0 ~ "South",
                                region == 1 ~ "Midwest",
                                region == 2 ~ "Northeast",
                                region == 3 ~ "West"
                                )) %>% 
  mutate(urban_type = frcode(urban_type == 0 ~ "Urban",
                                    urban_type == 1 ~ "Suburban",
                                    urban_type == 2 ~ "Rural"
                                    )) %>% 
  mutate(location_type = frcode(location == 0 ~ "K-12 School",
                                       location == 1 ~ "College/University",
                                       location == 2 ~ "Government Building/Place of Civic Importance",
                                       location == 4 ~ "House of Worship",
                                       location == 5 ~ "Restaurant/Bar/Night Club",
                                       location == 6 ~ "Workplace",
                                       location == 7 ~ "Place of Residence",
                                       location == 8 ~ "Outdoors"
                                       )) %>% 
  mutate(bifurcated = frcode(bifurcated == 0 ~ "Single Location",
                                    bifurcated == 1 ~ "Multiple Location"
                                    )) %>% 
  mutate(if_bifurcated = frcode(if_bifurcated == 0 ~ "K-12 School",
                                       if_bifurcated == 1 ~ "College/University",
                                       if_bifurcated == 2 ~ "Government Building/Place of Civic Importance",
                                       if_bifurcated == 4 ~ "House of Worship",
                                       if_bifurcated == 5 ~ "Restaurant/Bar/Night Club",
                                       if_bifurcated == 6 ~ "Workplace",
                                       if_bifurcated == 7 ~ "Place of Residence",
                                       if_bifurcated == 8 ~ "Outdoors"
                                       )) %>% 
  mutate(specify_presence = frcode(specify_presence == 0 ~ "None",
                                          specify_presence == 1 ~ "LEO/Public Safety Professional",
                                          specify_presence == 2 ~ "Civilian"
                                          )) %>% 
  mutate(armed_presence = frcode(armed_presence == 0 ~ "No",
                                        armed_presence == 1 ~ "Yes"
                                        )) %>% 
  mutate(kidnapping = frcode(kidnapping == 0 ~ "No",
                                    kidnapping == 1 ~ "Yes"
                                    )) %>% 
  mutate(gender = frcode(gender == 0 ~ "Male",
                            gender == 1 ~ "Female"
                            )) %>% 
  mutate(race = frcode(race == 0 ~ "White",
                          race == 1 ~ "Black",
                          race == 2 ~ "Latinx",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Native American",
                          race == 6 ~ "Other"
                          )) %>% 
  mutate(immigrant = frcode(immigrant == 0 ~ "Not An Immigrant",
                               immigrant == 1 ~ "Immigrant"
                               )) %>% 
  mutate(sex_orient = frcode(sex_orient == 0 ~ "Heterosexual",
                                sex_orient == 1 ~ "LGB"
                                )) %>% 
  mutate(religion = frcode(religion == 0 ~ "None",
                              religion == 1 ~ "Christian",
                              religion == 2 ~ "Muslim",
                              religion == 3 ~ "Atheist",
                              religion == 4 ~ "Cultural Spirituality/Other",
                              religion == 5 ~ "Jewish"
                              )) %>% 
  mutate(education = frcode(education == 0 ~ "None",
                               education == 1 ~ "High School/GED",
                               education == 2 ~ "Bachelor's Degree",
                               education == 3 ~ "Graduate School/Advanced Degree"
                               )) %>% 
  mutate(birth_order = frcode(birth_order == 0 ~ "Only Child",
                                 birth_order == 1 ~ "Oldest Child",
                                 birth_order == 2 ~ "Middle Child",
                                 birth_order == 3 ~ "Youngest Child",
                                 birth_order == 4 ~ "Twin"
                                 )) %>% 
  mutate(relationship = frcode(relationship_status == 0 ~ "Single",
                                  relationship_status == 1 ~ "Boyfriend/Girlfriend",
                                  relationship_status == 2 ~ "Married",
                                  relationship_status == 3 ~ "Divorced/Separated/Widowed"
                                  )) %>% 
  mutate(parent = frcode(Children == 0 ~ "No children",
                         Children == 1 ~ "children"
                            )) %>% 
  mutate(employment_s = frcode(employment_s == 0 ~ "Working",
                               employment_s == 1 ~ "Not Working"
                                  )) %>% 
  mutate(employment_t = frcode(employment_t == 0 ~ "Blue Collar",
                                  employment_t == 1 ~ "White Collar",
                                  employment_t == 2 ~ "In Between"
                                  )) %>% 
  mutate(military_s = frcode(military_s == 0 ~ "No Military Service",
                                military_s == 1 ~ "Military Service",
                                military_s == 2 ~ "Joined, but did not make it through training"
                                )) %>% 
  mutate(military_b = frcode(military_b == 0 ~ "Army",
                                military_b == 1 ~ "Navy",
                                military_b == 2 ~ "Air Force",
                                military_b == 3 ~ "Marines",
                                military_b == 4 ~ "Coast Guard",
                                military_b == 5 ~ "National Guard"
                                )) %>% 
  mutate(community_inv = frcode(community_inv == 0 ~ "No Community Involvement",
                                   community_inv == 1 ~ "Somewhat Involved",
                                   community_inv == 2 ~ "Heavily Involved",
                                   community_inv == 3 ~ "Formerly involved but recently withdrawn"
                                   )) %>% 
  mutate(crim_r = frcode(crim_r == 0 ~ "No Criminal / Prior Police Contact",
                             crim_r == 1 ~ "Criminal/Prior Police Contact"
                                )) %>% 
  mutate(previous_homicide = frcode(previous_homicide == 0 ~ "No Previous Homicides",
                                    previous_homicide == 1 ~ "At Least One Previous Homicide"
                                            )) %>% 
  mutate(history_of_domestic_abuse = frcode(history_of_domestic_abuse == 0 ~ "No History of Domestic Abuse",
                                   history_of_domestic_abuse == 1 ~ "Abused Romantic Partner",
                                   history_of_domestic_abuse == 2 ~ "Abused other family member(s)",
                                   history_of_domestic_abuse == 3 ~ "Abused romantic partner and other family member(s)"
                                   )) %>% 
  mutate(phys_abuse = frcode(phys_abuse == 0 ~ "No History of Physical Violence",
                             phys_abuse == 1 ~ "Yes, History of Physical Violence"
                             )) %>% 
  mutate(dom_abuse_t1 = frcode(dom_abuse_t1 == 0 ~ "Non-sexual Physical Violence",
                                      dom_abuse_t1 == 1 ~ "Sexual Violence",
                                      dom_abuse_t1 == 2 ~ "Threats / Coercive Control",
                                      dom_abuse_t1 == 3 ~ "Threats with a Deadly Weapon"
                                      )) %>% 
  mutate(dom_abuse_t2 = frcode(dom_abuse_t2 == 0 ~ "Non-sexual Physical Violence",
                                      dom_abuse_t2 == 1 ~ "Sexual Violence",
                                      dom_abuse_t2 == 2 ~ "Threats / Coercive Control",
                                      dom_abuse_t2 == 3 ~ "Threats with a Deadly Weapon"
                                      )) %>% 
  mutate(dom_abuse_t3 = frcode(dom_abuse_t3 == 0 ~ "Non-sexual Physical Violence",
                                      dom_abuse_t3 == 1 ~ "Sexual Violence",
                                      dom_abuse_t3 == 2 ~ "Threats / Coercive Control",
                                      dom_abuse_t3 == 3 ~ "Threats with a Deadly Weapon"
                                      )) %>% 
  mutate(sex_offense = frcode(sex_offense == 0 ~ "No History of Sexual Offenses",
                                  sex_offense == 1 ~ "History of Sexual Offenses"
                                     )) %>% 
  mutate(gang_association = frcode(gang_association == 0 ~ "No Gang Association",
                           gang_association == 1 ~ "Gang Association"
                              )) %>% 
  mutate(hate_group = frcode(hate_group == 0 ~ "No Hate Group Association",
                                    hate_group == 1 ~ "Hate Group Community Association",
                                    hate_group == 2 ~ "Other radical group association",
                                    hate_group == 3 ~ "Inspired by a hate group, but no direct connection",
                                    hate_group == 4 ~ "Website or chat room postings relating to hate or hate groups"
                                    )) %>% 
  mutate(terror_group_association = frcode(terror_group_association == 0 ~ "No Association with Terror Group",
                                terror_group_association == 1 ~ "Associated with Terror Group"
                             )) %>% 
  mutate(violent_game = frcode(violent_game == 0 ~ "No Violent Video Games",
                                      violent_game == 1 ~ "Played Violent Video Games",
                                      violent_game == 2 ~ "Played unspecified video Games",
                                      violent_game == 3 ~ "NA (pre-1992)"
                                      )) %>% 
  mutate(bully = frcode(bully == 0 ~ "Not A Bully",
                               bully == 1 ~ "Bully"
                               )) %>% 
  mutate(bullied = frcode(bullied == 0 ~ "Not Bullied",
                                 bullied == 1 ~ "Bullied"
                                 )) %>% 
  mutate(single_parent = frcode(single_parent == 0 ~ "Parent Not Single",
                                       single_parent == 1 ~ "Single Parent"
                                       )) %>% 
  mutate(parent_suicide = frcode(parent_suicide == 0 ~ "No Parental Suicide",
                                        parent_suicide == 1 ~ "Father",
                                        parent_suicide == 2 ~ "Mother"
                                        )) %>% 
  mutate(childhood_trauma = frcode(childhood_trauma == 0 ~ "No Childhood Trauma",
                                          childhood_trauma == 1 ~ "Abused by Father",
                                          childhood_trauma == 2 ~ "Abused by Mother",
                                          childhood_trauma == 3 ~ "Other Major Trauma",
                                          childhood_trauma == 4 ~ "Abused by other family members",
                                          childhood_trauma == 5 ~ "Abused by other Party",
                                          childhood_trauma == 6 ~ "Abused by both Parents"
                                          )) %>% 
  mutate(childhood_ses = frcode(childhood_ses == 0 ~ "Lower Class",
                                       childhood_ses == 1 ~ "Middle Class",
                                       childhood_ses == 2 ~ "Higher Class"
                                       )) %>% 
  mutate(adult_trauma = frcode(adult_trauma == 0 ~ "No Adult Trauma",
                                      adult_trauma == 1 ~ "Adult Trauma"
                                      )) %>% 
  mutate(breakup = frcode(breakup == 0 ~ "No Recent Breakup",
                                 breakup == 1 ~ "Recent Breakup"
                                 )) %>% 
  mutate(traumatic_work = frcode(traumatic_work == 0 ~ "No Recent Employment Trouble",
                                        traumatic_work == 1 ~ "Recent Employment Trouble"
                                        )) %>% 
  mutate(crisis_signs = frcode(crisis_signs == 0 ~ "No Signs of Crisis",
                                crisis_signs == 1 ~ "Signs of Crisis"
                                )) %>% 
  mutate(crisis_time = frcode(crisis_time == 0 ~ "Days before Shooting",
                                     crisis_time == 1 ~ "Weeks before Shooting",
                                     crisis_time == 2 ~ "Months before Shooting",
                                     crisis_time == 3 ~ "Years before Shooting"
                                     )) %>% 
  mutate(stressor = frcode(stressor == 0 ~ "No Ongoing stressor",
                                  stressor == 1 ~ "Ongoing Stressor"
                                  )) %>% 
  mutate(daily_task = frcode(daily_task == 0 ~ "Normal Daily Tasks",
                             daily_task == 1 ~ "Inability to Perform Daily Tasks"
                                    )) %>% 
  mutate(depressed_mood = frcode(depressed_mood == 0 ~ "Not Noticeably Depressed",
                                        depressed_mood == 1 ~ "Noticeably Depressed"
                                        )) %>% 
  mutate(unusual_mood = frcode(unusual_mood == 0 ~ "Not Unusually Calm or Happy",
                                      unusual_mood == 1 ~ "Unusually Calm or Happy"
                                      )) %>% 
  mutate(mood_swings = frcode(mood_swings == 0 ~ "No Rapid Mood Swings",
                              mood_swings == 1 ~ "Rapid Mood Swings"
                                     )) %>% 
  mutate(agitation = frcode(agitation == 0 ~ "No Increased Agitation",
                                   agitation == 1 ~ "Increased Agitation"
                                   )) %>% 
  mutate(abusive_behavior = frcode(abusive_behavior == 0 ~ "No Abusive Behavior",
                                   abusive_behavior == 1 ~ "Abusive Behavior"
                                          )) %>% 
  mutate(isolation = frcode(isolation == 0 ~ "No isolation",
                                isolation == 1 ~ "isolation"
                                   )) %>% 
  mutate(reality = frcode(reality == 0 ~ "Lucid",
                          reality == 1 ~ "Losing Touch with Reality"
                                 )) %>% 
  mutate(paranoia = frcode(paranoia == 0 ~ "No paranoia",
                                  paranoia == 1 ~ "Paranoid"
                                  )) %>% 
  mutate(perp_suicidality = frcode(perp_suicidality == 0 ~ "Not Suicidal",
                                      perp_suicidality == 1 ~ "Suicidal"
                                      )) %>% 
  mutate(psych_hosp = frcode(psych_hosp == 0 ~ "No Prior Psychiatric Hospitalization",
                                    psych_hosp == 1 ~ "Prior Psychiatric Hospitalization"
                                    )) %>% 
  mutate(counseling = frcode(counseling == 0 ~ "No Prior Counseling",
                             counseling == 1 ~ "Prior Counseling"
                                    )) %>% 
  mutate(counseling_type = frcode(counseling_type == 0 ~ "Voluntary Counseling",
                                  counseling_type == 1 ~ "Mandatory (imposed by court or job) Counseling",
                                  counseling_type == 2 ~ "Both Voluntary/Mandatory Counseling"
                                         )) %>% 
  mutate(psych_med = frcode(psych_med == 0 ~ "No Psychiatric Medication",
                                   psych_med == 1 ~ "Psychiatric"
                                   )) %>% 
  mutate(treatment = frcode(treatment == 0 ~ "No Treatment 6 Months Prior to Shooting",
                                   treatment == 1 ~ "Treatment 6 Months Prior to Shooting"
                                   )) %>% 
  mutate(mental_illness = frcode(mental_illness == 0 ~ "No signs of Psychiatric Diagnosis",
                                        mental_illness == 1 ~ "Mood Disorder",
                                        mental_illness == 2 ~ "Thought Disorder",
                                        mental_illness == 3 ~ "Other psychiatric diagnosis",
                                        mental_illness == 4 ~ "Both mood and thought disorders",
                                        mental_illness == 5 ~ "Signs of mental illness but no diagnosis"
                                    )) %>% 
  mutate(family_mental = frcode(family_mental == 0 ~ "No published/Known History",
                                       family_mental == 1 ~ "Parents had mental health issues",
                                       family_mental == 2 ~ "Other close relatives had mental health issues"
                                       )) %>% 
  mutate(autism_spectrum = frcode(autism_spectrum == 0 ~ "Not on Autism Spectrum",
                                         autism_spectrum == 1 ~ "On Autism Spectrum"
                                         )) %>% 
  mutate(substance_use = frcode(substance_use == 0 ~ "No Substance Use",
                                       substance_use == 1 ~ "Drank alcohol occasionally",
                                       substance_use == 2 ~ "Marijuana",
                                       substance_use == 3 ~ "Other drugs",
                                       substance_use == 4 ~ "Problem with alcohol and drugs",
                                       substance_use == 5 ~ "Problem with alcohol"
                                       )) %>% 
  mutate(health_issues = frcode(health_issues == 0 ~ "No Health Issues",
                                health_issues == 1 ~ "Health Issues"
                                )) %>% 
  mutate(prejudice1 = frcode(prejudice1 == 0 ~ "None Known",
                                    prejudice1 == 1 ~ "Racism",
                                    prejudice1 == 2 ~ "Misogyny",
                                    prejudice1 == 3 ~ "Homophobia",
                                    prejudice1 == 4 ~ "Religious hatred"
                                    )) %>% 
  mutate(prejudice2 = frcode(prejudice2 == 0 ~ "None Known",
                                 prejudice2 == 1 ~ "Racism",
                                 prejudice2 == 2 ~ "Misogyny",
                                 prejudice2 == 3 ~ "Homophobia",
                                 prejudice2 == 4 ~ "Religious hatred"
                                    )) %>% 
  mutate(prejudice3 = frcode(prejudice3 == 0 ~ "None Known",
                                    prejudice3 == 1 ~ "Racism",
                                    prejudice3 == 2 ~ "Misogyny",
                                    prejudice3 == 3 ~ "Homophobia",
                                    prejudice3 == 4 ~ "Religious hatred"
                                    )) %>% 
  mutate(motive_race = frcode(motive_race == 0 ~ "No Racist Motive",
                                     motive_race == 1 ~ "Targeting people of color",
                                     motive_race == 2 ~ "Targeting white people"
                                     )) %>% 
  mutate(motive_religion = frcode(motive_religion == 0 ~ "No Religious Motive",
                                         motive_religion == 1 ~ "Antisemitism",
                                         motive_religion == 2 ~ "Islamaphobia",
                                         motive_religion == 3 ~ "Angry with Christianity/Christian God",
                                         motive_religion == 4 ~ "Both Antisemitism and Islamaphobia"
                                         )) %>% 
  mutate(motive_misogyny = frcode(motive_misogyny == 0 ~ "No Misogynistic Motive",
                                         motive_misogyny == 1 ~ "Misogynistic Motive"
                                         )) %>% 
  mutate(motive_homophobia = frcode(motive_homophobia == 0 ~ "No Homophobic Motive",
                                           motive_homophobia == 1 ~ "Homophobic Motive"
                                           )) %>% 
  mutate(motive_employment = frcode(motive_employment == 0 ~ "No Employment Motive",
                                           motive_employment == 1 ~ "Employment Motive (fired, lost promotion)"
                                           )) %>% 
  mutate(motive_economic = frcode(motive_economic == 0 ~ "No Economic Motive",
                                         motive_economic == 1 ~ "Economic Motive (issues with money)"
                                         )) %>% 
  mutate(motive_legal = frcode(motive_legal == 0 ~ "No Legal Motive",
                               motive_legal == 1 ~ "Legal Motive"
                                       )) %>% 
  mutate(motive_relationship = frcode(motive_relationship == 0 ~ "No Relationship Issue",
                                      motive_relationship == 1 ~ "Relationship Motive (break-up, separation)"
                                             )) %>% 
  mutate(motive_interpersonal = frcode(motive_interpersonal == 0 ~ "No Interpersonal Motive",
                                       motive_interpersonal == 1 ~ "Interpersonal Motive (non-domestic, with coworkers, friends, family"
                                              )) %>% 
  mutate(motive_other = frcode(motive_other == 0 ~ "No 'Other' Motive",
                               motive_other == 1 ~ "Other Motive",
                               motive_other == 2 ~ "Generalized anger (angry at a group, society, world, carries out symbolic killing)"
                                      )) %>% 
  mutate(motive_unknown = frcode(motive_unknown == 0 ~ "No",
                                 motive_unknown == 1 ~ "Yes, Unknown Motive"
                                 )) %>% 
  mutate(domestic_spillage = frcode(domestic_spillage == 0 ~ "No Domestic Spillage",
                                        domestic_spillage == 1 ~ "Yes, targeted partner or ex-partner"
                                           )) %>% 
  mutate(psychosis_role = frcode(psychosis_role == 0 ~ "Psychotic Symptoms Played No Role",
                                        psychosis_role == 1 ~ "Psychotic symptoms played a small role in the crime",
                                        psychosis_role == 2 ~ "Psychotic symptoms played a significant role in the crime, but was not the only cause",
                                        psychosis_role == 3 ~ "Psychotic symptoms completely caused the crime"
                                        )) %>% 
  mutate(social_media = frcode(social_media == 0 ~ "No Social Media Usage",
                                      social_media == 1 ~ "Social Media Usage",
                                      social_media == 2 ~ "NA/Pre-1999"
                                      )) %>% 
  mutate(leakage = frcode(leakage == 0 ~ "No Leakage",
                                 leakage == 1 ~ "Leakage"
                                 )) %>% 
  mutate(leakage_how1 = frcode(leakage_how1 == 0 ~ "In Person",
                                      leakage_how1 == 1 ~ "Letter",
                                      leakage_how1 == 2 ~ "Other Writing",
                                      leakage_how1 == 3 ~ "Phone/Text",
                                      leakage_how1 == 4 ~ "Internet/Social Media",
                                      leakage_how1 == 5 ~ "Other Leakage"
                                      )) %>% 
  mutate(leakage_how2 = frcode(leakage_how2 == 0 ~ "In Person",
                                      leakage_how2 == 1 ~ "Letter",
                                      leakage_how2 == 2 ~ "Other Writing",
                                      leakage_how2 == 3 ~ "Phone/Text",
                                      leakage_how2 == 4 ~ "Internet/Social Media",
                                      leakage_how2 == 5 ~ "Other Leakage"
                                      )) %>% 
  mutate(leakage_who1 = frcode(leakage_who1 == 0 ~ "Mental Health Professional",
                                      leakage_who1 == 1 ~ "Immediate Family",
                                      leakage_who1 == 2 ~ "Wife/Girlfriend",
                                      leakage_who1 == 3 ~ "Police",
                                      leakage_who1 == 4 ~ "Coworkerks/Supervisors",
                                      leakage_who1 == 5 ~ "Friend/Neighbor",
                                      leakage_who1 == 6 ~ "Classmate",
                                      leakage_who1 == 7 ~ "Teacher/School Staff",
                                      leakage_who1 == 8 ~ "Waitress/Bartender/Clerk",
                                      leakage_who1 == 9 ~ "Other"
                                      )) %>% 
  mutate(leakage_who2 = frcode(leakage_who2 == 0 ~ "Mental Health Professional",
                                      leakage_who2 == 1 ~ "Immediate Family",
                                      leakage_who2 == 2 ~ "Wife/Girlfriend",
                                      leakage_who2 == 3 ~ "Police",
                                      leakage_who2 == 4 ~ "Coworkerks/Supervisors",
                                      leakage_who2 == 5 ~ "Friend/Neighbor",
                                      leakage_who2 == 6 ~ "Classmate",
                                      leakage_who2 == 7 ~ "Teacher/School Staff",
                                      leakage_who2 == 8 ~ "Waitress/Bartender/Clerk",
                                      leakage_who2 == 9 ~ "Other"
                                      )) %>% 
  mutate(leakage_specific1 = frcode(leakage_specific1 == 0 ~ "Nonspecific (Threatened Violence)",
                                   leakage_specific1 == 1 ~ "Specific (threatened shooting)"
                                          )) %>% 
  mutate(leakage_specific2 = frcode(leakage_specific2 == 0 ~ "Nonspecific (Threatened Violence)",
                                    leakage_specific2 == 1 ~ "Specific (threatened shooting)"
                                           )) %>% 
  mutate(mass_viol_int = frcode(mass_viol_int == 0 ~ "No Interest in Past Mass Violence",
                                       mass_viol_int == 1 ~ "Interest in Past Mass Violence"
                                       )) %>% 
  mutate(other_shooting = frcode(other_shooting == 0 ~ "No Relationship with Other Shootings",
                                        other_shooting == 1 ~ "Relationship with Other Shootings"
                                        )) %>% 
  mutate(legacy_token = frcode(legacy_token == 0 ~ "No Legacy Token",
                                      legacy_token == 1 ~ "Legacy Token (left something behind"
                                      )) %>% 
  mutate(pop_culture = frcode(pop_culture == 0 ~ "No Connection to Pop Culture",
                                     pop_culture == 1 ~ "Explicit Reference to Pop Culture",
                                     pop_culture == 2 ~ "Tangential Reference to Pop Culture"
                                     )) %>% 
  mutate(well_planned = frcode(well_planned == 0 ~ "Not Well Planned",
                                      well_planned == 1 ~ "Well Planned"
                                      )) %>% 
  mutate(performance = frcode(performance == 0 ~ "No ('will to representation')",
                                     performance == 1 ~ "Yes ('will to representation')"
                                     )) %>% 
  mutate(firearms_interest = frcode(firearms_interest == 0 ~ "No Interest in Firearms",
                                       firearms_interest == 1 ~ "Interest in Firearms"
                                       )) %>% 
  mutate(firearms_total = frcode(firearms_total == 0 ~ "No Interest in Firearms",
                                 firearms_total == 1 ~ "Interest in Firearms"
                                 )) %>% 
  mutate(firearms_proficiency = frcode(firearms_proficiency == 0 ~ "No Firearm Experience",
                                          firearms_proficiency == 1 ~ "Some Experience",
                                          firearms_proficiency == 2 ~ "More Expierenced (held a license, took classes)",
                                          firearms_proficiency == 3 ~ "Very Expierenced (Military, years of practice)"
                                          )) %>% 
  mutate(other_weapons = frcode(other_weapons == 0 ~ "No Other Weapons or Gear",
                                   other_weapons == 1 ~ "Other Weapons/Gear"
                                  )) %>% 
  mutate(scene_outcome = frcode(scene_outcome == 0 ~ "Killed Self",
                                       scene_outcome == 1 ~ "Killed on Scene",
                                       scene_outcome == 2 ~ "Apprehended",
                                       scene_outcome == 3 ~ "Apprehended, then Suicide Before Trial"
                                       )) %>% 
  mutate(flee = frcode(flee == 0 ~ "No attempt, resigned to die",
                              flee == 1 ~ "Tried to escape/keep living freely"
                           )) %>% 
  mutate(insanity = frcode(insanity == 0 ~ "No Insanity Defense",
                                  insanity == 1 ~ "Insanity Defense",
                                  insanity == 2 ~ "N/A (dead before trial)",
                                  insanity == 3 ~ "Trial Pending Life"
                                  )) %>% 
  mutate(sentence = frcode(sentence == 0 ~ "N/A",
                                  sentence == 1 ~ "Death Penalty",
                                  sentence == 2 ~ "Life without Parole",
                                  sentence == 3 ~ "Life Imprisonment w/possibility of Parole",
                                  sentence == 4 ~ "Hospitalization",
                                  sentence == 5 ~ "Juvenile Detention"
                                  )) %>% 

view(tvp3)

