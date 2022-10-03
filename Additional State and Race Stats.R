
```{r Printing Demographics by State and Race, eval = FALSE, include = FALSE}

kable(aianStats, col.names = c("State", "Number of American Indians/Alaska Natives Applicants", "N", "%"), align = "cccc")

kable(asianStats, col.names = c("State", "Number of Asian Applicants", "N", "%"), align = "cccc")

kable(blackStats, col.names = c("State", "Number of Black Applicants", "N", "%"), align = "cccc")

kable(nhopiStats, col.names = c("State", "Number of Native Hawaiian & OPI Applicants", "N", "%"), align = "cccc")

kable(whiteStats, col.names = c("State", "Number of WhiteApplicants", "N", "%"), align = "cccc")

kable(multiRaceStats, col.names = c("State", "Number of Multirace Applicants", "N", "%"), align = "cccc")


race = if_else(applicant_race.1 == 1 & (is.na(applicant_ethnicity.2)|is.na(applicant_ethnicity.3)|is.na(applicant_ethnicity.4)|is.na(applicant_ethnicity.5)), 1, #aian
               if_else(applicant_race.1 %in% c(2,21:27) 
               & (is.na(applicant_ethnicity.2)|is.na(applicant_ethnicity.3)|is.na(applicant_ethnicity.4)|is.na(applicant_ethnicity.5)), 2, #asian
                
               if_else(applicant_race.1 == 3 
                & (is.na(applicant_ethnicity.2)|is.na(applicant_ethnicity.3)|is.na(applicant_ethnicity.4)|is.na(applicant_ethnicity.5)), 3, #black

                if_else(applicant_race.1 %in% c(4, 41:44) 
                & (is.na(applicant_ethnicity.2)|is.na(applicant_ethnicity.3)|is.na(applicant_ethnicity.4)|is.na(applicant_ethnicity.5)), 4, #nhopiß

                if_else(applicant_race.1 == 5  
                & (is.na(applicant_ethnicity.2)|is.na(applicant_ethnicity.3)|is.na(applicant_ethnicity.4)|is.na(applicant_ethnicity.5)), 5, #white
                
                if_else(applicant_race.1 == 1 
                                    & (applicant_race.2 %in% c(2,21:27, 3, 4, 41:44, 5) |
                                         applicant_race.3 %in% c(2,21:27, 3, 4, 41:44, 5) |
                                         applicant_race.4 %in% c(2,21:27, 3, 4, 41:44, 5) |
                                         applicant_race.5 %in% c(2,21:27, 3, 4, 41:44, 5))|
                                      
                                      
                                      
                                      applicant_race.1 %in% c(2, 21:27) 
                                    & (applicant_race.2 %in% c(1, 3, 4, 41:44, 5)|
                                         applicant_race.3 %in% c(1, 3, 4, 41:44, 5)|
                                         applicant_race.4 %in% c(1, 3, 4, 41:44, 5)|
                                         applicant_race.5 %in% c(1, 3, 4, 41:44, 5))|
                                      
                                      applicant_race.1 %in% c(3) 
                                    & (applicant_race.2 %in% c(1, 2, 21:27, 4, 41:44, 5)|
                                         applicant_race.3 %in% c(1, 2, 21:27, 4, 41:44, 5)|
                                         applicant_race.4 %in% c(1, 2, 21:27, 4, 41:44, 5)|
                                         applicant_race.5 %in% c(1, 2, 21:27, 4, 41:44, 5))|
                                      
                                      applicant_race.1 %in% c(4, 41:44) 
                                    & (applicant_race.2 %in% c(1, 2, 21:27, 3, 5)|
                                         applicant_race.3 %in% c(1, 2, 21:27, 3, 5)|
                                         applicant_race.4 %in% c(1, 2, 21:27, 3, 5)|
                                         applicant_race.5 %in% c(1, 2, 21:27, 3, 5))|
                                      
                                      applicant_race.1 %in% c(4) 
                                    & (applicant_race.2 %in% c(1, 2, 21:27, 3, 4, 41:44)|
                                         applicant_race.3 %in% c(1, 2, 21:27, 3, 4, 41:44)|
                                         applicant_race.4 %in% c(1, 2, 21:27, 3, 4, 41:44)|
                                         applicant_race.5 %in% c(1, 2, 21:27, 3, 4, 41:44))  
                                    ,6, 0)) 

                
                
                ```{r}
                
              
                aianStats = stateCrosstabFxn(race)
                
                asianStats = stateCrosstabFxn(asian)
                
                blackStats = stateCrosstabFxn(black)
                
                nhopiStats = stateCrosstabFxn(nhopi)
                
                whiteStats = stateCrosstabFxn(white)
                
                multiRaceStats =  stateCrosstabFxn(multiRace)
                
                
                ```
                # aianActiontakenStats =  stateActiontakenFxn(aian)
                # 
                # asianActiontakenStats = stateActiontakenFxn(asian)
                # 
                # blackActiontakenStats = stateActiontakenFxn(black)
                # 
                # nhopiActiontakenStats = stateActiontakenFxn(nhopi)
                # 
                # whiteActiontakenStats = stateActiontakenFxn(white)
                # 
                # multiRaceactionTakenstats = stateActiontakenFxn(multiRace)
                
                
                
                ```{r Printing Race, State, and Actions Taken Statistics}
                
                kable(aianActiontakenStats, col.names = c("State", "Number of AI/AN Applicants", "Action Taken", "N", "%"), align =  "cccc")
                
                kable(asianActiontakenStats, col.names = c("State", "Number of Asiann Applicants", "Action Taken", "N", "%"), align =  "cccc")
                
                kable(blackActiontakenStats, col.names = c("State", "Number of Black Applicants", "Action Taken", "N", "%"), align =  "cccc")
                
                kable(nhopiActiontakenStats, col.names = c("State", "Number of Native Hawaiian & OPI Applicants", "Action Taken", "N", "%"), align =  "cccc")
                
                kable(whiteActiontakenStats, col.names = c("State", "Number of White Applicants", "Action Taken", "N", "%"), align =  "cccc")
                
                kable(multiRaceactionTakenstats, col.names = c("State", "Number of MultiRace Applicants", "Action Taken", "N", "%"), align = "cccc")
                
                ```
                
                ```{r Statistically Testing the Difference in Actions Taken by Ethnicity}
                
                chisq.test(data2$hispanicLatino, data2$action_taken)
                
                chisq.test(data2$aian, data2$action_taken)
                
                
                chisq.test(data2$asian, data2$action_taken)
                
                chisq.test(data2$black, data2$action_taken)
                
                chisq.test(data2$nhopi, data2$action_taken)
                
                chisq.test(data2$white, data2$action_taken)
                
                chisq.test(data2$multiRace, data2$action_taken)
                
                
                ```
                
                All chi-square tests for action taken by race are statistically significant. However, we need to take caution in interpretation with race groups with small numbers (e.g., American Indian/Alaska Natives)
                
                ```{r  Statistically Testing the Difference in Actions Taken by Ethnicity and State, include = FALSE}
                
                
                mantelhaen.test(x = multiRaceactionTakenstats$multiRace, y = multiRaceactionTakenstats$action_taken, z = multiRaceactionTakenstats$state_code)
                
                multiRaceactionTakenstats$multiRace
                
                ```
                
                ```{r Logistic Regression - Getting the Training and Testing Data, include = FALSE}
                
                data3 = 
                  data2 %>%
                  group_by(state_code) %>%
                  tibble::rowid_to_column("id") %>%
                  relocate(id) %>%
                  mutate(actionTakenbinomal = if_else(action_taken == "5", 0, 1))
                
                head(data3)
                
                table(data3$action_taken, data3$actionTakenbinomal)
                
                trainingData = slice_sample(data3, prop = 0.80)
                
                testData = data3 %>% anti_join(trainingData, by = "id")
                
                nrow(trainingData) +nrow(testData) == nrow(data3)
                ```
                
                ```{r Running the Training Model}
                
                model = glm(actionTakenbinomal ~ state_code, family=binomial (link = 'logit'), data = trainingData)
                
                summary(model)
                
                sum(trainingData$white, na.rm = TRUE)
                
                ```
                stateActiontakenFxn = function(var) {
                  raceDataset %>%            
                    group_by(state_code, {{var}}, action_taken) %>%
                    summarise(countVar = n()) %>%
                    mutate(pctVar = scales::percent(countVar/sum(countVar), 0.1),
                           countVar = scales::comma(countVar))
                  
                }
                