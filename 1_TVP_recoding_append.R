
## The Violence Project: R Translation (case_when)

library(dplyr)

tvp <- read.csv("C:/Data/The Violence Project/TVP_copy.csv")

tvp2 <- tvp %>% 
  mutate(state_txt = case_when(State == 1 ~ "Alabama",
                               State == 2 ~ "Alaska",
                               State == 3 ~ "Arizona",
                               State == 4 ~ "Arkansas",
                               State == 5 ~ "California",
                               State == 6 ~ "Colorado",
                               State == 7 ~ "Connecticut",
                               State == 8 ~ "Delaware",
                               State == 9 ~ "Florida",
                               State == 10 ~ "Georgia",
                               State == 11 ~ "Hawaii",
                               State == 12 ~ "Idaho",
                               State == 13 ~ "Illinois",
                               State == 14 ~ "Indiana",
                               State == 15 ~ "Iowa",
                               State == 16 ~ "Kansas",
                               State == 17 ~ "Kentucky",
                               State == 18 ~ "Louisiana",
                               State == 19 ~ "Maine",
                               State == 20 ~ "Maryland",
                               State == 21 ~ "Massacusetts",
                               State == 22 ~ "Michigan",
                               State == 23 ~ "Minnesota",
                               State == 24 ~ "Mississippi",
                               State == 25 ~ "Missouri",
                               State == 26 ~ "Montana",
                               State == 27 ~ "Nebraska",
                               State == 28 ~ "Nevada",
                               State == 29 ~ "New Hampshire",
                               State == 30 ~ "New Jersey",
                               State == 31 ~ "New Mexico",
                               State == 32 ~ "New York",
                               State == 33 ~ "North Carolina",
                               State == 34 ~ "North Dakota",
                               State == 35 ~ "Ohio",
                               State == 36 ~ "Oklahoma",
                               State == 37 ~ "Oregon",
                               State == 38 ~ "Pennsylvania",
                               State == 39 ~ "Rhode Island",
                               State == 40 ~ "South Carolina",
                               State == 41 ~ "South Dakota",
                               State == 42 ~ "Tennessee",
                               State == 43 ~ "Texas",
                               State == 44 ~ "Utah",
                               State == 45 ~ "Vermont",
                               State == 46 ~ "Virginia",
                               State == 47 ~ "Washington",
                               State == 48 ~ "West Virginia",
                               State == 49 ~ "Wisconsin",
                               State == 50 ~ "Wyoming",
                               TRUE ~ "NA"
                               )) %>% 
  mutate(region_txt = case_when(Region == 0 ~ "South",
                                Region == 1 ~ "Midwest",
                                Region == 2 ~ "Northeast",
                                Region == 3 ~ "West",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(urban_type_txt = case_when(Urban.Suburban.Rural == 0 ~ "Urban",
                                    Urban.Suburban.Rural == 1 ~ "Suburban",
                                    Urban.Suburban.Rural == 2 ~ "Rural",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(location_type_txt = case_when(Location == 0 ~ "K-12 School",
                                       Location == 1 ~ "College/University",
                                       Location == 2 ~ "Government Building/Place of Civic Importance",
                                       Location == 4 ~ "House of Worship",
                                       Location == 5 ~ "Restaurant/Bar/Night Club",
                                       Location == 6 ~ "Workplace",
                                       Location == 7 ~ "Place of Residence",
                                       Location == 8 ~ "Outdoors",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(bifurcated_txt = case_when(Bifurcated == 0 ~ "Single Location",
                                    Bifurcated == 1 ~ "Multiple Location",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(if_bifurcated_txt = case_when(If.Bifurcated..Other.Location == 0 ~ "K-12 School",
                                       If.Bifurcated..Other.Location == 1 ~ "College/University",
                                       If.Bifurcated..Other.Location == 2 ~ "Government Building/Place of Civic Importance",
                                       If.Bifurcated..Other.Location == 4 ~ "House of Worship",
                                       If.Bifurcated..Other.Location == 5 ~ "Restaurant/Bar/Night Club",
                                       If.Bifurcated..Other.Location == 6 ~ "Workplace",
                                       If.Bifurcated..Other.Location == 7 ~ "Place of Residence",
                                       If.Bifurcated..Other.Location == 8 ~ "Outdoors",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(specify_presence_txt = case_when(Specify.Armed.Person == 0 ~ "None",
                                          Specify.Armed.Person == 1 ~ "LEO/Public Safety Professional",
                                          Specify.Armed.Person == 2 ~ "Civilian",
                                          TRUE ~ "NA"
                                          )) %>% 
  mutate(armed_presence_txt = case_when(Armed.Person.on.Scene == 0 ~ "No",
                                        Armed.Person.on.Scene == 1 ~ "Yes",
                                        TRUE ~ "NA"
                                        )) %>% 
  mutate(kidnapping_txt = case_when(Kidnapping.or.Hostage.Situation == 0 ~ "No",
                                    Kidnapping.or.Hostage.Situation == 1 ~ "Yes",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(gender_txt = case_when(Gender == 0 ~ "Male",
                            Gender == 1 ~ "Female",
                            TRUE ~ "NA"
                            )) %>% 
  mutate(race_txt = case_when(Race == 0 ~ "White",
                          Race == 1 ~ "Black",
                          Race == 2 ~ "Latinx",
                          Race == 4 ~ "Asian",
                          Race == 5 ~ "Native American",
                          Race == 6 ~ "Other",
                          TRUE ~ "NA"
                          )) %>% 
  mutate(immigrant_txt = case_when(Immigrant == 0 ~ "Not An Immigrant",
                               Immigrant == 1 ~ "Immigrant",
                               TRUE ~ "NA"
                               )) %>% 
  mutate(sex_orient_txt = case_when(Sexual.Orientation == 0 ~ "Heterosexual",
                                Sexual.Orientation == 1 ~ "LGB",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(religion_txt = case_when(Religion == 0 ~ "None",
                              Religion == 1 ~ "Christian",
                              Religion == 2 ~ "Muslim",
                              Religion == 3 ~ "Atheist",
                              Religion == 4 ~ "Cultural Spirituality/Other",
                              Religion == 5 ~ "Jewish",
                              TRUE ~ "NA"
                              )) %>% 
  mutate(education_txt = case_when(Education == 0 ~ "None",
                               Education == 1 ~ "High School/GED",
                               Education == 2 ~ "Bachelor's Degree",
                               Education == 3 ~ "Graduate School/Advanced Degree",
                               TRUE ~ "NA"
                               )) %>% 
  mutate(birth_order_txt = case_when(Birth.Order == 0 ~ "Only Child",
                                 Birth.Order == 1 ~ "Oldest Child",
                                 Birth.Order == 2 ~ "Middle Child",
                                 Birth.Order == 3 ~ "Youngest Child",
                                 Birth.Order == 4 ~ "Twin",
                                 TRUE ~ "NA"
                                 )) %>% 
  mutate(relationship_txt = case_when(Relationship.Status == 0 ~ "Single",
                                  Relationship.Status == 1 ~ "Boyfriend/Girlfriend",
                                  Relationship.Status == 2 ~ "Married",
                                  Relationship.Status == 3 ~ "Divorced/Separated/Widowed",
                                  TRUE ~ "NA"
                                  )) %>% 
  mutate(parent_txt = case_when(Children == 0 ~ "No Children",
                            Children == 1 ~ "Children",
                            TRUE ~ "NA"
                            )) %>% 
  mutate(employment_s_txt = case_when(Employment.Status == 0 ~ "Working",
                                  Employment.Status == 1 ~ "Not Working",
                                  TRUE ~ "NA"
                                  )) %>% 
  mutate(employment_t_txt = case_when(Employment.Type == 0 ~ "Blue Collar",
                                  Employment.Type == 1 ~ "White Collar",
                                  Employment.Type == 2 ~ "In Between",
                                  TRUE ~ "NA"
                                  )) %>% 
  mutate(military_s_txt = case_when(Military.Service == 0 ~ "No Military Service",
                                Military.Service == 1 ~ "Military Service",
                                Military.Service == 2 ~ "Joined, but did not make it through training",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(military_b_txt = case_when(Miltary.Branch == 0 ~ "Army",
                                Miltary.Branch == 1 ~ "Navy",
                                Miltary.Branch == 2 ~ "Air Force",
                                Miltary.Branch == 3 ~ "Marines",
                                Miltary.Branch == 4 ~ "Coast Guard",
                                Miltary.Branch == 5 ~ "National Guard",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(community_inv_txt = case_when(Community.Involvement == 0 ~ "No Community Involvement",
                                   Community.Involvement == 1 ~ "Somewhat Involved",
                                   Community.Involvement == 2 ~ "Heavily Involved",
                                   Community.Involvement == 3 ~ "Formerly involved but recently withdrawn",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(crim_r_txt = case_when(Criminal.Record.Police.Contact == 0 ~ "No Criminal / Prior Police Contact",
                                Criminal.Record.Police.Contact == 1 ~ "Criminal/Prior Police Contact",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(previous_homicides_txt = case_when(Previous.Homicide.s. == 0 ~ "No Previous Homicides",
                                            Previous.Homicide.s. == 1 ~ "At Least One Previous Homicide",
                                            TRUE ~ "NA"
                                            )) %>% 
  mutate(phys_abuse_txt = case_when(History.of.Physical.Violence == 0 ~ "No History of Physical Violence",
                                  History.of.Physical.Violence == 1 ~ "Yes, History of Physical Violence",
                                  TRUE ~ "NA")) %>% 
  mutate(dom_abuse_txt = case_when(History.of.Domestic.Abuse == 0 ~ "No History of Domestic Abuse",
                                   History.of.Domestic.Abuse == 1 ~ "Abused Romantic Partner",
                                   History.of.Domestic.Abuse == 2 ~ "Abused other family member(s)",
                                   History.of.Domestic.Abuse == 3 ~ "Abused romantic partner and other family member(s)",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(dom_abuse_t1_txt = case_when(Domestic.Abuse.Type.1 == 0 ~ "Non-sexual Physical Violence",
                                      Domestic.Abuse.Type.1 == 1 ~ "Sexual Violence",
                                      Domestic.Abuse.Type.1 == 2 ~ "Threats / Coercive Control",
                                      Domestic.Abuse.Type.1 == 3 ~ "Threats with a Deadly Weapon",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(dom_abuse_t2_txt = case_when(Domestic.Abuse.Type.2 == 0 ~ "Non-sexual Physical Violence",
                                      Domestic.Abuse.Type.2 == 1 ~ "Sexual Violence",
                                      Domestic.Abuse.Type.2 == 2 ~ "Threats / Coercive Control",
                                      Domestic.Abuse.Type.2 == 3 ~ "Threats with a Deadly Weapon",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(dom_abuse_t3_txt = case_when(Domestic.Abuse.Type.3 == 0 ~ "Non-sexual Physical Violence",
                                      Domestic.Abuse.Type.3 == 1 ~ "Sexual Violence",
                                      Domestic.Abuse.Type.3 == 2 ~ "Threats / Coercive Control",
                                      Domestic.Abuse.Type.3 == 3 ~ "Threats with a Deadly Weapon",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(sex_offense_txt = case_when(History.of.Sexual.Offenses == 0 ~ "No History of Sexual Offenses",
                                     History.of.Sexual.Offenses == 1 ~ "History of Sexual Offenses",
                                     TRUE ~ "NA"
                                     )) %>% 
  mutate(gang_association_txt = case_when(Gang.Association == 0 ~ "No Gang Association",
                              Gang.Association == 1 ~ "Gang Association",
                              TRUE ~ "NA"
                              )) %>% 
  mutate(terror_group_association_txt = case_when(Terror.Group.Association == 0 ~ "No Association with Terror Group",
                                Terror.Group.Association == 1 ~ "Associated with Terror Group",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(hate_group_txt = case_when(Hate.Group.Association == 0 ~ "No Hate Group Association",
                                    Hate.Group.Association == 1 ~ "Hate Group Community Association",
                                    Hate.Group.Association == 2 ~ "Other radical group association",
                                    Hate.Group.Association == 3 ~ "Inspired by a hate group, but no direct connection",
                                    Hate.Group.Association == 4 ~ "Website or chat room postings relating to hate or hate groups",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(violent_game_txt = case_when(Violent.Video.Games == 0 ~ "No Violent Video Games",
                                      Violent.Video.Games == 1 ~ "Played Violent Video Games",
                                      Violent.Video.Games == 2 ~ "Played unspecified video Games",
                                      Violent.Video.Games == 3 ~ "NA (pre-1992)",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(bully_txt = case_when(Bully == 0 ~ "Not A Bully",
                               Bully == 1 ~ "Bully",
                               TRUE ~ "NA"
                               )) %>% 
  mutate(bullied_txt = case_when(Bullied == 0 ~ "Not Bullied",
                                 Bullied == 1 ~ "Bullied",
                                 TRUE ~ "NA"
                                 )) %>% 
  mutate(single_parent_txt = case_when(Raised.by.Single.Parent == 0 ~ "Parent Not Single",
                                       Raised.by.Single.Parent == 1 ~ "Single Parent",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(parent_suicide_txt = case_when(Parental.Suicide == 0 ~ "No Parental Suicide",
                                        Parental.Suicide == 1 ~ "Father",
                                        Parental.Suicide == 2 ~ "Mother",
                                        TRUE ~ "NA"
                                        )) %>% 
  mutate(childhood_trauma_txt = case_when(Childhood.Trauma == 0 ~ "No Childhood Trauma",
                                          Childhood.Trauma == 1 ~ "Abused by Father",
                                          Childhood.Trauma == 2 ~ "Abused by Mother",
                                          Childhood.Trauma == 3 ~ "Other Major Trauma",
                                          Childhood.Trauma == 4 ~ "Abused by other family members",
                                          Childhood.Trauma == 5 ~ "Abused by other Party",
                                          Childhood.Trauma == 6 ~ "Abused by both Parents",
                                          TRUE ~ "NA"
                                          )) %>% 
  mutate(childhood_ses_txt = case_when(Childhood.SES == 0 ~ "Lower Class",
                                       Childhood.SES == 1 ~ "Middle Class",
                                       Childhood.SES == 2 ~ "Higher Class",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(adult_trauma_txt = case_when(Adult.Trauma == 0 ~ "No Adult Trauma",
                                      Adult.Trauma == 1 ~ "Adult Trauma",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(breakup_txt = case_when(Recent.Breakup == 0 ~ "No Recent Breakup",
                                 Recent.Breakup == 1 ~ "Recent Breakup",
                                 TRUE ~ "NA"
                                 )) %>% 
  mutate(traumatic_work_txt = case_when(Recent.Employment.Change.or.Trouble == 0 ~ "No Recent Employment Trouble",
                                        Recent.Employment.Change.or.Trouble == 1 ~ "Recent Employment Trouble",
                                        TRUE ~ "NA"
                                        )) %>% 
  
  mutate(crisis_txt = case_when(Signs.of.Crisis == 0 ~ "No Signs of Crisis",
                                Signs.of.Crisis == 1 ~ "Signs of Crisis",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(crisis_time_txt = case_when(Timeline.of.Signs.of.Crisis == 0 ~ "Days before Shooting",
                                     Timeline.of.Signs.of.Crisis == 1 ~ "Weeks before Shooting",
                                     Timeline.of.Signs.of.Crisis == 2 ~ "Months before Shooting",
                                     Timeline.of.Signs.of.Crisis == 3 ~ "Years before Shooting",
                                     TRUE ~ "NA"
                                     )) %>% 
  mutate(stressor_txt = case_when(Recent.or.Ongoing.Stressor == 0 ~ "No Ongoing stressor",
                                  Recent.or.Ongoing.Stressor == 1 ~ "Ongoing Stressor",
                                  TRUE ~ "NA"
                                  )) %>% 
  mutate(daily_task_txt = case_when(Inability.to.Perform.Daily.Tasks == 0 ~ "Normal Daily Tasks",
                                    Inability.to.Perform.Daily.Tasks == 1 ~ "Inability to Perform Daily Tasks",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(depressed_mood_txt = case_when(Notably.Depressed.Mood == 0 ~ "Not Noticeably Depressed",
                                        Notably.Depressed.Mood == 1 ~ "Noticeably Depressed",
                                        TRUE ~ "NA"
                                        )) %>% 
  mutate(unusual_mood_txt = case_when(Unusually.Calm.or.Happy == 0 ~ "Not Unusually Calm or Happy",
                                      Unusually.Calm.or.Happy == 1 ~ "Unusually Calm or Happy",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(mood_swings_txt = case_when(Rapid.Mood.Swings == 0 ~ "No Rapid Mood Swings",
                                     Rapid.Mood.Swings == 1 ~ "Rapid Mood Swings",
                                     TRUE ~ "NA"
                                     )) %>% 
  mutate(agitation_txt = case_when(Increased.Agitation == 0 ~ "No Increased Agitation",
                                   Increased.Agitation == 1 ~ "Increased Agitation",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(abusive_behavior_txt = case_when(Abusive.Behavior == 0 ~ "No Abusive Behavior",
                                          Abusive.Behavior == 1 ~ "Abusive Behavior",
                                          TRUE ~ "NA"
                                          )) %>% 
  mutate(isolation_txt = case_when(Isolation == 0 ~ "No Isolation",
                                   Isolation == 1 ~ "Isolation",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(reality_txt = case_when(Losing.Touch.with.Reality == 0 ~ "Lucid",
                                 Losing.Touch.with.Reality == 1 ~ "Losing Touch with Reality",
                                 TRUE ~ "NA"
                                 )) %>% 
  mutate(paranoia_txt = case_when(Paranoia == 0 ~ "No Paranoia",
                                  Paranoia == 1 ~ "Paranoid",
                                  TRUE ~ "NA"
                                  )) %>% 
  mutate(perp_suicidality_txt = case_when(Suicidality == 0 ~ "Not Suicidal",
                                      Suicidality == 1 ~ "Suicidal",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(psych_hosp_txt = case_when(Prior.Hospitalization == 0 ~ "No Prior Psychiatric Hospitalization",
                                    Prior.Hospitalization == 1 ~ "Prior Psychiatric Hospitalization",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(counseling_txt = case_when(Prior.Counseling == 0 ~ "No Prior Counseling",
                                    Prior.Counseling == 1 ~ "Prior Counseling",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(counseling_type_txt = case_when(Voluntary.or.Mandatory.Counseling == 0 ~ "Voluntary Counseling",
                                         Voluntary.or.Mandatory.Counseling == 1 ~ "Mandatory (imposed by court or job) Counseling",
                                         Voluntary.or.Mandatory.Counseling == 2 ~ "Both Voluntary/Mandatory Counseling",
                                         TRUE ~ "NA"
                                         )) %>% 
  mutate(psych_med_txt = case_when(Psychiatric.Medication == 0 ~ "No Psychiatric Medication",
                                   Psychiatric.Medication == 1 ~ "Psychiatric",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(treatment_txt = case_when(Treatment.6.Months.Prior.to.Shooting == 0 ~ "No Treatment 6 Months Prior to Shooting",
                                   Treatment.6.Months.Prior.to.Shooting == 1 ~ "Treatment 6 Months Prior to Shooting",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(mental_illness_txt = case_when(Mental.Illness == 0 ~ "No signs of Psychiatric Diagnosis",
                                        Mental.Illness == 1 ~ "Mood Disorder",
                                        Mental.Illness == 2 ~ "Thought Disorder",
                                        Mental.Illness == 3 ~ "Other psychiatric diagnosis",
                                        Mental.Illness == 4 ~ "Both mood and thought disorders",
                                        Mental.Illness == 5 ~ "Signs of mental illness but no diagnosis",
                                        TRUE ~ "NA"
                                    )) %>% 
  mutate(family_mental_txt = case_when(Known.Family.Mental.Health.History == 0 ~ "No published/Known History",
                                       Known.Family.Mental.Health.History == 1 ~ "Parents had mental health issues",
                                       Known.Family.Mental.Health.History == 2 ~ "Other close relatives had mental health issues",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(autism_spectrum_txt = case_when(Autism.Spectrum == 0 ~ "Not on Autism Spectrum",
                                         Autism.Spectrum == 1 ~ "On Autism Spectrum",
                                         TRUE ~ "NA"
                                         )) %>% 
  mutate(substance_use_txt = case_when(Substance.Use == 0 ~ "No Substance Use",
                                       Substance.Use == 1 ~ "Drank alcohol occasionally",
                                       Substance.Use == 2 ~ "Marijuana",
                                       Substance.Use == 3 ~ "Other drugs",
                                       Substance.Use == 4 ~ "Problem with alcohol and drugs",
                                       Substance.Use == 5 ~ "Problem with alcohol",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(health_txt = case_when(Health.Issues == 0 ~ "No Health Issues",
                                Health.Issues == 1 ~ "Health Issues",
                                TRUE ~ "NA"
                                )) %>% 
  mutate(prejudice1_txt = case_when(Known.Prejudice.2 == 0 ~ "None Known",
                                    Known.Prejudice.2 == 1 ~ "Racism",
                                    Known.Prejudice.2 == 2 ~ "Misogyny",
                                    Known.Prejudice.2 == 3 ~ "Homophobia",
                                    Known.Prejudice.2 == 4 ~ "Religious hatred",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(prejudice2_txt = case_when(Known.Prejudice.2 == 0 ~ "None Known",
                                    Known.Prejudice.2 == 1 ~ "Racism",
                                    Known.Prejudice.2 == 2 ~ "Misogyny",
                                    Known.Prejudice.2 == 3 ~ "Homophobia",
                                    Known.Prejudice.2 == 4 ~ "Religious hatred",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(prejudice3_txt = case_when(Known.Prejudice.3 == 0 ~ "None Known",
                                    Known.Prejudice.3 == 1 ~ "Racism",
                                    Known.Prejudice.3 == 2 ~ "Misogyny",
                                    Known.Prejudice.3 == 3 ~ "Homophobia",
                                    Known.Prejudice.3 == 4 ~ "Religious hatred",
                                    TRUE ~ "NA"
                                    )) %>% 
  mutate(motive_race_txt = case_when(Motive..Racism.Xenophobia == 0 ~ "No Racist Motive",
                                     Motive..Racism.Xenophobia == 1 ~ "Targeting people of color",
                                     Motive..Racism.Xenophobia == 2 ~ "Targeting white people",
                                     TRUE ~ "NA"
                                     )) %>% 
  mutate(motive_religion_txt = case_when(Motive..Religious.Hate == 0 ~ "No Religious Motive",
                                         Motive..Religious.Hate == 1 ~ "Antisemitism",
                                         Motive..Religious.Hate == 2 ~ "Islamaphobia",
                                         Motive..Religious.Hate == 3 ~ "Angry with Christianity/Christian God",
                                         Motive..Religious.Hate == 4 ~ "Both Antisemitism and Islamaphobia",
                                         TRUE ~ "NA"
                                         )) %>% 
  mutate(motive_misogyny_txt = case_when(Motive..Misogyny == 0 ~ "No Misogynistic Motive",
                                         Motive..Misogyny == 1 ~ "Misogynistic Motive",
                                         TRUE ~ "NA"
                                         )) %>% 
  mutate(motive_homophobia_txt = case_when(Motive..Homophobia. == 0 ~ "No Homophobic Motive",
                                           Motive..Homophobia. == 1 ~ "Homophobic Motive",
                                           TRUE ~ "NA"
                                           )) %>% 
  mutate(motive_employment_txt = case_when(Motive..Employment.Issue == 0 ~ "No Employment Motive",
                                           Motive..Employment.Issue == 1 ~ "Employment Motive (fired, lost promotion)",
                                           TRUE ~ "NA"
                                           )) %>% 
  mutate(motive_economic_txt = case_when(Motive..Economic.Issue == 0 ~ "No Economic Motive",
                                         Motive..Economic.Issue == 1 ~ "Economic Motive (issues with money)",
                                         TRUE ~ "NA"
                                         )) %>% 
  mutate(motive_legal_txt = case_when(Motive..Legal.Issue == 0 ~ "No Legal Motive",
                                      Motive..Legal.Issue == 1 ~ "Legal Motive",
                                      TRUE ~ "NA")) %>% 
  mutate(motive_relationship_txt = case_when(Motive..Relationship.Issue == 0 ~ "No Relationship Issue",
                                             Motive..Relationship.Issue == 1 ~ "Relationship Motive (break-up, separation)",
                                             TRUE ~ "NA"
                                             )) %>% 
  mutate(motive_interpersonal_txt = case_when(Motive..Interpersonal.Conflict == 0 ~ "No Interpersonal Motive",
                                              Motive..Interpersonal.Conflict == 1 ~ "Interpersonal Motive (non-domestic, with coworkers, friends, family",
                                              TRUE ~ "NA"
                                              )) %>% 
  mutate(motive_other_txt = case_when(Motive..Other == 0 ~ "No 'Other' Motive",
                                      Motive..Other == 1 ~ "Other Motive",
                                      Motive..Other == 2 ~ "Generalized anger (angry at a group, society, world, carries out symbolic killing)",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(motive_fame_txt = case_when(Motive..Fame.Seeking == 0 ~ "No Fame Motive",
                                 Motive..Fame.Seeking == 1 ~ "Fame Seeking",
                                 TRUE ~ "NA")) %>% 
  mutate(motive_unknown_txt = case_when(Motive..Unknown == 0 ~ "No",
                                        Motive..Unknown == 1 ~ "Yes, Unknown Motive",
                                        TRUE ~ "NA"
                                        )) %>% 
  mutate(domestic_spillage_txt = case_when(Domestic.Spillage == 0 ~ "No Domestic Spillage",
                                           Domestic.Spillage == 1 ~ "Yes, targeted partner or ex-partner",
                                           TRUE ~ "NA"
                                           )) %>% 
  mutate(psychosis_role_txt = case_when(Role.of.Psychosis.in.the.Shooting == 0 ~ "Psychotic Symptoms Played No Role",
                                        Role.of.Psychosis.in.the.Shooting == 1 ~ "Psychotic symptoms played a small role in the crime",
                                        Role.of.Psychosis.in.the.Shooting == 2 ~ "Psychotic symptoms played a significant role in the crime, but was not the only cause",
                                        Role.of.Psychosis.in.the.Shooting == 3 ~ "Psychotic symptoms completely caused the crime",
                                        TRUE ~ "NA"
                                        )) %>% 
  mutate(social_media_txt = case_when(Social.Media.Use == 0 ~ "No Social Media Usage",
                                      Social.Media.Use == 1 ~ "Social Media Usage",
                                      Social.Media.Use == 2 ~ "NA/Pre-1999",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(leakage_txt = case_when(Leakage == 0 ~ "No Leakage",
                                 Leakage == 1 ~ "Leakage",
                                 TRUE ~ "NA"
                                 )) %>% 
  mutate(leakage_how1_txt = case_when(Leakage.1.How == 0 ~ "In Person",
                                      Leakage.1.How == 1 ~ "Letter",
                                      Leakage.1.How == 2 ~ "Other Writing",
                                      Leakage.1.How == 3 ~ "Phone/Text",
                                      Leakage.1.How == 4 ~ "Internet/Social Media",
                                      Leakage.1.How == 5 ~ "Other Leakage",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(leakage_how2_txt = case_when(Leakage.2.How == 0 ~ "In Person",
                                      Leakage.2.How == 1 ~ "Letter",
                                      Leakage.2.How == 2 ~ "Other Writing",
                                      Leakage.2.How == 3 ~ "Phone/Text",
                                      Leakage.2.How == 4 ~ "Internet/Social Media",
                                      Leakage.2.How == 5 ~ "Other Leakage",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(leakage_who1_txt = case_when(Leakage.1.Who == 0 ~ "Mental Health Professional",
                                      Leakage.1.Who == 1 ~ "Immediate Family",
                                      Leakage.1.Who == 2 ~ "Wife/Girlfriend",
                                      Leakage.1.Who == 3 ~ "Police",
                                      Leakage.1.Who == 4 ~ "Coworkerks/Supervisors",
                                      Leakage.1.Who == 5 ~ "Friend/Neighbor",
                                      Leakage.1.Who == 6 ~ "Classmate",
                                      Leakage.1.Who == 7 ~ "Teacher/School Staff",
                                      Leakage.1.Who == 8 ~ "Waitress/Bartender/Clerk",
                                      Leakage.1.Who == 9 ~ "Other",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(leakage_who2_txt = case_when(Leakage.2.Who == 0 ~ "Mental Health Professional",
                                      Leakage.2.Who == 1 ~ "Immediate Family",
                                      Leakage.2.Who == 2 ~ "Wife/Girlfriend",
                                      Leakage.2.Who == 3 ~ "Police",
                                      Leakage.2.Who == 4 ~ "Coworkerks/Supervisors",
                                      Leakage.2.Who == 5 ~ "Friend/Neighbor",
                                      Leakage.2.Who == 6 ~ "Classmate",
                                      Leakage.2.Who == 7 ~ "Teacher/School Staff",
                                      Leakage.2.Who == 8 ~ "Waitress/Bartender/Clerk",
                                      Leakage.2.Who == 9 ~ "Other",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(leakage_specific_txt = case_when(Leakage.1.Specific.Nonspecific == 0 ~ "Nonspecific (Threatened Violence)",
                                          Leakage.1.Specific.Nonspecific == 1 ~ "Specific (threatened shooting)",
                                          TRUE ~ "NA"
                                          )) %>% 
  mutate(leakage_specific2_txt = case_when(Leakage.2.Specific.Nonspecific == 0 ~ "Nonspecific (Threatened Violence)",
                                           Leakage.2.Specific.Nonspecific == 1 ~ "Specific (threatened shooting)",
                                           TRUE ~ "NA"
                                           )) %>% 
  mutate(mass_viol_int_txt = case_when(Interest.in.Past.Mass.Violence == 0 ~ "No Interest in Past Mass Violence",
                                       Interest.in.Past.Mass.Violence == 1 ~ "Interest in Past Mass Violence",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(other_shooting_txt = case_when(Relationship.with.Other.Shooting.s. == 0 ~ "No Relationship with Other Shootings",
                                        Relationship.with.Other.Shooting.s. == 1 ~ "Relationship with Other Shootings",
                                        TRUE ~ "NA"
                                        )) %>% 
  mutate(legacy_token_txt = case_when(Legacy.Token == 0 ~ "No Legacy Token",
                                      Legacy.Token == 1 ~ "Legacy Token (left something behind",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(pop_culture_txt = case_when(Pop.Culture.Connection == 0 ~ "No Connection to Pop Culture",
                                     Pop.Culture.Connection == 1 ~ "Explicit Reference to Pop Culture",
                                     Pop.Culture.Connection == 2 ~ "Tangential Reference to Pop Culture",
                                     TRUE ~ "NA"
                                     )) %>% 
  mutate(well_planned_txt = case_when(Planning == 0 ~ "Not Well Planned",
                                      Planning == 1 ~ "Well Planned",
                                      TRUE ~ "NA"
                                      )) %>% 
  mutate(performance_txt = case_when(Performance == 0 ~ "No ('will to representation')",
                                     Performance == 1 ~ "Yes ('will to representation')",
                                     TRUE ~ "NA"
                                     )) %>% 
  mutate(firearms_interest_txt = case_when(Interest.in.Firearms == 0 ~ "No Interest in Firearms",
                                       Interest.in.Firearms == 1 ~ "Interest in Firearms",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(firearms_proficiency_txt = case_when(Firearm.Proficiency == 0 ~ "No Firearm Experience",
                                          Firearm.Proficiency == 1 ~ "Some Experience",
                                          Firearm.Proficiency == 2 ~ "More Expierenced (held a license, took classes)",
                                          Firearm.Proficiency == 3 ~ "Very Expierenced (Military, years of practice)",
                                          TRUE ~ "NA"
                                          )) %>% 
  mutate(other_weapons_txt = case_when(Other.Weapons.or.Gear == 0 ~ "No Other Weapons or Gear",
                                   Other.Weapons.or.Gear == 1 ~ "Other Weapons/Gear",
                                   TRUE ~ "NA"
                                   )) %>% 
  mutate(scene_outcome_txt = case_when(On.Scene.Outcome == 0 ~ "Killed Self",
                                       On.Scene.Outcome == 1 ~ "Killed on Scene",
                                       On.Scene.Outcome == 2 ~ "Apprehended",
                                       On.Scene.Outcome == 3 ~ "Apprehended, then Suicide Before Trial",
                                       TRUE ~ "NA"
                                       )) %>% 
  mutate(flee_txt = case_when(Attempt.to.Flee == 0 ~ "No attempt, resigned to die",
                              Attempt.to.Flee == 1 ~ "Tried to escape/keep living freely",
                              TRUE ~ "NA"
                              )) %>% 
  mutate(insanity_txt = case_when(Insanity.Defense == 0 ~ "No Insanity Defense",
                                  Insanity.Defense == 1 ~ "Insanity Defense",
                                  Insanity.Defense == 2 ~ "N/A (dead before trial)",
                                  Insanity.Defense == 3 ~ "Trial Pending Life",
                                  TRUE ~ "NA"
                                  )) %>% 
  mutate(sentence_txt = case_when(Criminal.Sentence == 0 ~ "N/A",
                                  Criminal.Sentence == 1 ~ "Death Penalty",
                                  Criminal.Sentence == 2 ~ "Life without Parole",
                                  Criminal.Sentence == 3 ~ "Life Imprisonment w/possibility of Parole",
                                  Criminal.Sentence == 4 ~ "Hospitalization",
                                  Criminal.Sentence == 5 ~ "Juvenile Detention",
                                  TRUE ~ "NA"
                                  )) %>%  
  mutate(date_col = make_datetime(Year, Month, Day))

## FIX: 


legal_issue
leakage.2. (fix)