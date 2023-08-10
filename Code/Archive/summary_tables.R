## Load In Data
rm(list = ls())
library(haven)
library(survey)
library(tidyverse)
library(margins)
library(gtsummary)

CaltechClimate_Nov2022 <- read_sav("Data/CalTech_November_2022.sav")

data = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% 
                                               select(pid3, Q18, weight) %>% 
                                               mutate(across(!weight, as_factor))), 
                 weights = ~weight,id = ~1)


# Vote by Party ID
out <- tbl_svysummary(data = data,
                        by = "Q18",
                        percent = "row", 
                        label = list(pid3 ~ "Party ID"), 
                        missing = "no",
                      statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                     all_categorical() ~ "{p}% ({p.std.error})"))

gt::as_latex(as_gt(out) )

out <- tbl_svysummary(data = data,
                      by = "Q18",
                      percent = "row", 
                      label = list(pid3 ~ "Party ID"), 
                      missing = "no",
                      statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                     all_categorical() ~ "{n_unweighted} ({p_unweighted}%)"))

gt::as_latex(as_gt(out) )

demo_data_srvy = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% 
                                               filter(Q18 %in% c(1,2)) %>% 
                                               select(gender3, educ, region, race, Q85,age4, Q18, weight) %>% 
                                               mutate(across(!weight, as_factor))), 
                            weights = ~weight, id = ~1)

demos <- tbl_svysummary(demo_data_srvy,
                        by = "Q18",
                        percent = "row", 
                        label = list(gender3 ~ "Gender",
                                     region ~ "Region",
                                     educ ~ "Educational Attainment",
                                     race ~ "Race",
                                     Q85 ~ "Religion",
                                     age4 ~ "Age"), 
                        missing = "no",
                        statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                       all_categorical() ~ "{p}% ({p.std.error})"))


gt::as_latex(as_gt(demos) )



demos <- tbl_svysummary(demo_data_srvy,
                    by = "Q18",
                    percent = "row", 
                    label = list(gender3 ~ "Gender",
                                 region ~ "Region",
                                 educ ~ "Educational Attainment",
                                 race ~ "Race",
                                 Q85 ~ "Religion",
                                 age4 ~ "Age"), 
                    missing = "no",
                    statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                   all_categorical() ~ "{n_unweighted} ({p_unweighted}%)"))


gt::as_latex(as_gt(demos) )




import_data_srvy = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% 
                                                 filter(Q18 %in% c(1,2)) %>% 
                                               select(Q31_1,
                                                      Q31_2,
                                                      Q31_3,
                                                      Q31_4,
                                                      Q31_5,
                                                      Q31_6,
                                                      Q31_7,
                                                      Q31_8,
                                                      Q31_9,
                                                      Q31_10,
                                                      Q31_11,
                                                      Q31_12,
                                                      Q31_13,
                                                      Q18, 
                                                      weight) %>% 
                                               mutate(across(!weight, as_factor))), 
                           weights = ~weight, id = ~1)

import <- tbl_svysummary(import_data_srvy,
                        by = "Q18",
                        list(Q31_1 ~ "Immigration",
                             Q31_2 ~  " Abortion",
                             Q31_3 ~ "Foreign Policy",
                             Q31_4 ~ "Economic Inequality",
                             Q31_5 ~ "COVID-19",
                             Q31_6 ~ "Violent Crime",
                             Q31_7 ~ "Health Care",
                             Q31_8 ~ "The Economy",
                             Q31_9 ~ "Racial and Ethnic Inequality",
                             Q31_10 ~ "Climate Change",
                             Q31_11 ~ "Inflation",
                             Q31_12 ~ "Gun Policy",
                             Q31_13 ~ "Supreme Court Appointments"),
                        percent = "row", 
                        missing = "no",
                        statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                       all_categorical() ~ "{p}% ({p.std.error})"))


gt::as_latex(as_gt(import) )



import <- tbl_svysummary(import_data_srvy,
                         by = "Q18",
                         list(Q31_1 ~ "Immigration",
                              Q31_2 ~  " Abortion",
                              Q31_3 ~ "Foreign Policy",
                              Q31_4 ~ "Economic Inequality",
                              Q31_5 ~ "COVID-19",
                              Q31_6 ~ "Violent Crime",
                              Q31_7 ~ "Health Care",
                              Q31_8 ~ "The Economy",
                              Q31_9 ~ "Racial and Ethnic Inequality",
                              Q31_10 ~ "Climate Change",
                              Q31_11 ~ "Inflation",
                              Q31_12 ~ "Gun Policy",
                              Q31_13 ~ "Supreme Court Appointments"),
                         percent = "row", 
                         missing = "no",
                         statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                        all_categorical() ~ "{n_unweighted} ({p_unweighted}%)"))


gt::as_latex(as_gt(import) )




better_data_srvy = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% 
                                                 filter(Q18 %in% c(1,2)) %>% 
                                                 select(Q51_1,
                                                        Q51_2,
                                                        Q51_3,
                                                        Q51_4,
                                                        Q51_5,
                                                        Q51_6,
                                                        Q51_7,
                                                        Q51_8,
                                                        Q51_9,
                                                        Q51_10,
                                                        Q18, 
                                                        weight) %>% 
                                                 mutate(across(!weight, as_factor))), 
                             weights = ~weight, id = ~1)

better <- tbl_svysummary(better_data_srvy,
                         by = "Q18",
                         list(Q51_1 ~ "Preventing Terrorism",
                              Q51_2 ~  "Mitigating Climate Change",
                              Q51_3 ~ "Abortion Policy",
                              Q51_4 ~ "Criminal justice reform",
                              Q51_5 ~ "COVID-19",
                              Q51_6 ~ "Deficit",
                              Q51_7 ~ "The Economy",
                              Q51_8 ~ "Health Care",
                              Q51_9 ~ "Foreign Policy",
                              Q51_10 ~ "Inflation"),
                         percent = "row", 
                         missing = "no",
                         statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                        all_categorical() ~ "{p}% ({p.std.error})"))


gt::as_latex(as_gt(better) )

better <- tbl_svysummary(better_data_srvy,
                         by = "Q18",
                         list(Q51_1 ~ "Preventing Terrorism",
                              Q51_2 ~  "Mitigating Climate Change",
                              Q51_3 ~ "Abortion Policy",
                              Q51_4 ~ "Criminal justice reform",
                              Q51_5 ~ "COVID-19",
                              Q51_6 ~ "Deficit",
                              Q51_7 ~ "The Economy",
                              Q51_8 ~ "Health Care",
                              Q51_9 ~ "Foreign Policy",
                              Q51_10 ~ "Inflation"),
                         percent = "row", 
                         missing = "no",
                         statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                        all_categorical() ~ "{n_unweighted} ({p_unweighted}%)"))


gt::as_latex(as_gt(better) )


economy = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% 
                                                 filter(Q18 %in% c(1,2)) %>% 
                                                 select(Q75,
                                                        Q74,
                                                        Q18, 
                                                        weight) %>% 
                                                 mutate(across(!weight, as_factor))), 
                             weights = ~weight, id = ~1)

economy_table <- tbl_svysummary(economy,
                         by = "Q18",
                         list(Q75 ~ "Economic Situation",
                              Q74 ~ "Financial Situation"),
                         percent = "row", 
                         missing = "no",
                         statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                        all_categorical() ~ "{p}% ({p.std.error})"))


gt::as_latex(as_gt(economy_table) )


economy_table <- tbl_svysummary(economy,
                                by = "Q18",
                                list(Q75 ~ "Economic Situation",
                                     Q74 ~ "Financial Situation"),
                                percent = "row", 
                                missing = "no",
                                statistic=list(all_continuous() ~ "{median} ({p25}, {p75})", 
                                               all_categorical() ~ "{n_unweighted} ({p_unweighted}%)"))


gt::as_latex(as_gt(economy_table) )


news = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% 
                                        filter(Q18 %in% c(1,2)) %>% 
                                        select(news_1,
                                               news_2,
                                               news_3,
                                               news_4,
                                               Q66_1,
                                               Q66_2,
                                               Q66_3,
                                               Q66_4,
                                               Q67,
                                               Q69,
                                               Q18, 
                                               weight) %>% 
                                        mutate(across(!weight, as_factor))), 
                    weights = ~weight, id = ~1)

news_table <- tbl_svysummary(news,
                                by = "Q18",
                                list(news_1 ~ "Television",
                                     news_2 ~ "Radio",
                                     news_3 ~ "Print Publications",
                                     news_4 ~ "Online",
                                     Q66_1 ~ "News websites",
                                     Q66_2 ~ "Social Media",
                                     Q66_3 ~"Podcasts",
                                     Q66_4 ~ "Google",
                                     Q67 ~ "Social Media",
                                     Q69 ~ "Follow Goverment"),
                                percent = "row", 
                                missing = "no")

gt::as_latex(as_gt(news_table) )

