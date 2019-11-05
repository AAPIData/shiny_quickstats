universal_formatter <- function(tablename, geo, year){
  
  v16 <- load_variables(year, "acs5", cache = TRUE)
  label <- v16 %>%
    select(-concept) %>% 
    mutate(name = sub('E$', '',name), # get rid of E at the end of each name
           label = sub('Estimate!!', '',label)) %>%
    rename(variable = name)
  
  ########################
  #population by race
  ######################## 
  if(tablename == "alone" || tablename == "combo"){
    
    if(tablename == "alone"){
      var_list <- c("B02001_005", "B02001_006", "B03002_003",
                    "B02001_003", "B03002_012")
    } else{
      var_list <- c("B02011_001", "B02012_001", "B03002_003",
                    "B02001_003", "B03002_012")    
    }
    table <- get_acs(variables = var_list, geography = geo, year = year)
    raw_dta <- table %>% select(NAME,variable, estimate,moe)
    tbl_final <- raw_dta %>% left_join(label) %>% 
      select(label, everything()) %>% # join table
      mutate(label = case_when(
        variable == "B02011_001" ~"Asian alone or in combo",
        variable == "B02001_005" ~"Asian alone",
        variable == "B02001_006" ~"NHPI alone",
        variable == "B02012_001" ~"NHPI alone or in combo",
        variable == "B02001_003" ~"Black alone",
        variable == "B03002_003" ~"Non-Hispanic White alone",
        variable == "B03002_012" ~"Hispanic (any race) alone",
        TRUE ~label)) %>% 
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      mutate(`moe check` = case_when(
        moe < estimate * .25 ~" ",
        TRUE ~"Uncertain Eestimates"
      )) %>% select(-moe,-variable) %>% 
      rename(Population = estimate,
             `Racial Group` = label) %>% 
      select(NAME, `Racial Group`, Population, `moe check`)
    #return(tbl_final)
  } 
  ########################
  #Detailed groups
  ######################## 
  else if(tablename == "B02015" || tablename == "B02016" ||
          tablename == "B02018" || tablename == "B02019"){
    
    
    table <- get_acs(table = tablename, geography = geo, year = year)
    raw_dta <- table %>% select(NAME,variable, estimate,moe)
    v16 <- load_variables(year, "acs5", cache = TRUE)
    # groups <- c("B02015", "B02018", "B02016", "B02019")
    tbl <- raw_dta %>% left_join(label) %>% select(label, everything()) # join table
    
    if(tablename == "B02015") {
      total <- c("Total")
      race <- c("Total!!Asian Indian", "Total!!Bangladeshi","Total!!Bhutanese", 
                "Total!!Burmese", "Total!!Cambodian", "Total!!Chinese, except Taiwanese",
                "Total!!Filipino", "Total!!Hmong", "Total!!Indonesian", 
                "Total!!Japanese", "Total!!Korean", "Total!!Laotian",
                "Total!!Malaysian", "Total!!Mongolian", "Total!!Nepalese",
                "Total!!Okinawan", "Total!!Pakistani", "Total!!Sri Lankan",
                "Total!!Taiwanese", "Total!!Thai", "Total!!Vietnamese") 
      
    } else if(tablename == "B02016"){
      total <- c("Total")
      race <- c("Total!!Polynesian!!Native Hawaiian", "Total!!Polynesian!!Samoan",
                "Total!!Micronesian!!Guamanian or Chamorro", "Total!!Micronesian!!Marshallese",
                "Total!!Melanesian!!Fijian")
      
    } else if(tablename == "B02018") {
      total <- c("Total Groups Tallied")
      race <- c("Total Groups Tallied!!Asian Indian", "Total Groups Tallied!!Bangladeshi",
                "Total Groups Tallied!!Bhutanese", "Total Groups Tallied!!Burmese",
                "Total Groups Tallied!!Cambodian", "Total Groups Tallied!!Chinese, except Taiwanese",
                "Total Groups Tallied!!Filipino", "Total Groups Tallied!!Hmong", 
                "Total Groups Tallied!!Indonesian", "Total Groups Tallied!!Japanese",
                "Total Groups Tallied!!Korean", "Total Groups Tallied!!Laotian", 
                "Total Groups Tallied!!Malaysian", "Total Groups Tallied!!Mongolian", 
                "Total Groups Tallied!!Nepalese", "Total Groups Tallied!!Okinawan",
                "Total Groups Tallied!!Pakistani", "Total Groups Tallied!!Sri Lankan",
                "Total Groups Tallied!!Taiwanese", "Total Groups Tallied!!Thai", "Total Groups Tallied!!Vietnamese")
      
    } else {
      total <- c("Total Groups Tallied")
      race <- c("Total Groups Tallied!!Polynesian!!Native Hawaiian",
                "Total Groups Tallied!!Polynesian!!Samoan", "Total Groups Tallied!!Polynesian!!Tongan",
                "Total Groups Tallied!!Micronesian!!Guamanian or Chamorro", 
                "Total Groups Tallied!!Micronesian!!Marshallese", "Total Groups Tallied!!Melanesian!!Fijian")
    }
    
    
    tbl_tot <- tbl %>% 
      filter(label %in% total) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      mutate(`moe check` = case_when(
        moe < estimate *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate, `moe check`) %>%
      rename(`Total Population` = estimate)
    
    tbl_race <- tbl %>% 
      filter(label %in% race) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      mutate(`moe check 1` = case_when(
        moe < estimate *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      separate(label,into = c("lab1","label"),sep="!!") %>%
      select(NAME, label, estimate, `moe check 1`) %>% 
      rename(`Population` = estimate, `Detailed Group` = label)
    
    #merge two together
    tbl_final <- tbl_race %>% left_join(tbl_tot) %>% 
      mutate(`percent Population` = `Population` / `Total Population`)
  } 
  
  ########################
  #cvap by race
  ######################## 
  else if(tablename =="cvap: B05003B" || tablename =="cvap: B05003D" 
          || tablename =="cvap: B05003E" || tablename =="cvap: B05003H" 
          || tablename =="cvap: B05003I"){
    tablename <- sapply(strsplit(tablename, split=': ', fixed=TRUE), function(x) (x[2]))
    
    table <- get_acs(table = tablename, geography = geo, year = year)
    raw_dta <- table %>% select(NAME,variable, estimate,moe)
    tbl <- raw_dta %>% left_join(label) %>% select(label, everything()) # join table
    
    total <- c("Total")
    cvap <- c("Total!!Male!!18 years and over!!Native",
              "Total!!Male!!18 years and over!!Foreign born!!Naturalized U.S. citizen",
              "Total!!Female!!18 years and over!!Native",
              "Total!!Female!!18 years and over!!Foreign born!!Naturalized U.S. citizen")
    
    tbl_tot <- tbl %>% 
      filter(label %in% total) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      mutate(`moe check` = case_when(
        moe < estimate *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate, `moe check`) %>%
      rename(`Total Population` = estimate)
    
    tbl_cvap <- tbl %>%
      filter(label %in% cvap) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      group_by(NAME) %>% 
      mutate(moe2 = sum(moe),
             estimate2 = sum(estimate)) %>% 
      mutate(`moe check 2` = case_when(
        moe2 < estimate2 *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate2, `moe check 2`) %>%
      rename(`CVAP Population` = estimate2)
    #merge two together
    tbl_final <- tbl_tot %>%
      left_join(tbl_cvap) %>%
      mutate(pct_cvap = `CVAP Population` / `Total Population`) %>% 
      rename (`%CVAP` = pct_cvap)
    
  }
  
  ########################
  #Nativity by race
  ########################
  else if(tablename =="nativity: B05003B" || tablename =="nativity: B05003D" 
          || tablename =="nativity: B05003E" || tablename =="nativity: B05003H" 
          || tablename =="nativity: B05003I"){
    tablename <- sapply(strsplit(tablename, split=': ', fixed=TRUE), function(x) (x[2]))
    table <- get_acs(table = tablename, geography = geo, year = year)
    raw_dta <- table %>% select(NAME, variable, estimate,moe)
    tbl <- raw_dta %>% left_join(label) %>% select(label, everything()) # join table
    
    total <- c("Total")
    fb <- c("Total!!Male!!Under 18 years!!Foreign born" ,
            "Total!!Male!!18 years and over!!Foreign born",
            "Total!!Female!!Under 18 years!!Foreign born",
            "Total!!Female!!18 years and over!!Foreign born")
    
    tbl_tot <- tbl %>% filter(label %in% total) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0, TRUE ~moe)) %>%
      mutate(`moe check` = case_when(
        moe < estimate *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate, `moe check`) %>%
      rename(`Total Population` = estimate)
    
    tbl_fb <- tbl %>% filter(label %in% fb) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0, TRUE ~moe)) %>%
      group_by(NAME) %>% 
      mutate(moe1 = sum(moe),
             estimate1 = sum(estimate)) %>% 
      mutate(`moe check 1` = case_when(
        moe1 < estimate1 *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate1, `moe check 1`) %>%
      rename(`Foreign-born Population` = estimate1) %>% unique()
    
    tbl_final <- tbl_tot %>% left_join(tbl_fb) %>% 
      mutate(`Percent Foreign-born` =
               `Foreign-born Population` / `Total Population`)
  } 
  ########################
  #Educational Attainment by race
  ########################   
  else if(tablename =="C15002B" || tablename =="C15002D" 
          || tablename =="C15002E" || tablename =="C15002H" || tablename =="C15002I") {
    
    table <- get_acs(table = tablename, geography = geo, year = year)
    raw_dta <- table %>% select(NAME,variable, estimate,moe)
    tbl <- raw_dta %>% left_join(label) %>% select(label, everything()) # join table
    
    total <- c("Total")
    
    edu1 <- c("Total!!Male!!Less than high school diploma",
              "Total!!Female!!Less than high school diploma")
    
    edu2 <- c("Total!!Male!!High school graduate (includes equivalency)",
              "Total!!Female!!High school graduate (includes equivalency)") 
    
    edu3 <- c("Total!!Male!!Some college or associate's degree",
              "Total!!Female!!Some college or associate's degree")
    
    edu4 <- c("Total!!Male!!Bachelor's degree or higher",
              "Total!!Female!!Bachelor's degree or higher")
    
    tbl_edu1 <- tbl %>% 
      filter(label %in% edu1) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      group_by(NAME) %>% 
      mutate(moe1 = sum(moe),
             estimate1 = sum(estimate)) %>% 
      mutate(`moe check 1` = case_when(
        moe1 < estimate1 *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate1, `moe check 1`) %>%
      rename(`Less than HS` = estimate1) %>% unique()
    
    tbl_edu2 <- tbl %>% 
      filter(label %in% edu2) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      group_by(NAME) %>% 
      mutate(moe2 = sum(moe),
             estimate2 = sum(estimate)) %>% 
      mutate(`moe check 2` = case_when(
        moe2 < estimate2 *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate2, `moe check 2`) %>%
      rename(`HS degree (includes equivalency)` = estimate2) %>% unique()
    
    tbl_edu3 <- tbl %>% 
      filter(label %in% edu3) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      group_by(NAME) %>% 
      mutate(moe3 = sum(moe),
             estimate3 = sum(estimate)) %>% 
      mutate(`moe check 3` = case_when(
        moe3 < estimate3 *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate3, `moe check 3`) %>%
      rename(`Some college or AA degree` = estimate3) %>% unique()
    
    tbl_edu4 <- tbl %>% 
      filter(label %in% edu4) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      group_by(NAME) %>% 
      mutate(moe4 = sum(moe),
             estimate4 = sum(estimate)) %>% 
      mutate(`moe check 4` = case_when(
        moe4 < estimate4 *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate4, `moe check 4`) %>%
      rename(`BA degree or higher` = estimate4) %>% unique()
    
    tbl_tot <- tbl %>% 
      filter(label %in% total) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>%
      mutate(`moe check` = case_when(
        moe < estimate *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate, `moe check`) %>%
      rename(`Total Population` = estimate)
    
    tbl_final <- tbl_tot %>% 
      left_join(tbl_edu1) %>% 
      left_join(tbl_edu2) %>% 
      left_join(tbl_edu3) %>% 
      left_join(tbl_edu4) %>% 
      mutate(`% Less than HS` = `Less than HS` / `Total Population`,
             `% HS degree (includes equivalency)` = `HS degree (includes equivalency)` / `Total Population`,
             `% Some college or AA degree` = `Some college or AA degree` / `Total Population`,
             `% BA degree or higher` = `BA degree or higher` / `Total Population`)
  }
  
  ########################
  #LEP by race
  ########################   
  else if(tablename =="B16005B" || tablename =="B16005D" 
          || tablename =="B16005E" || tablename =="B16005H" || tablename =="B16005I"){
    table <- get_acs(table = tablename, geography = geo, year = year)
    raw_dta <- table %>% select(NAME,variable, estimate,moe)
    tbl <- raw_dta %>% left_join(label) %>% select(label, everything()) # join table
    
    other_lang <- c("Total!!Native!!Speak another language",
                    "Total!!Foreign born!!Speak another language")
    
    lep <- c("Total!!Native!!Speak another language!!Speak English less than \"very well\"",
             "Total!!Foreign born!!Speak another language!!Speak English less than \"very well\"")
    
    tbl_lang <- tbl %>% 
      filter(label %in% other_lang) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>% 
      group_by(NAME) %>%
      mutate(other_lang = sum(estimate)) %>%
      mutate(other_lang_moe = sum(moe)) %>%
      mutate(`moe check` = case_when(
        other_lang_moe< other_lang *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, other_lang, `moe check`) %>%
      unique()
    
    tbl_lep <- tbl %>% 
      filter(label %in% lep) %>%
      mutate(moe = case_when(
        is.na(moe) ~ 0,
        TRUE ~moe)) %>% 
      group_by(NAME) %>%
      mutate(lep = sum(estimate)) %>%
      mutate(lep_moe = sum(moe)) %>%
      mutate(`moe check 1` = case_when(
        lep_moe < lep * 0.25 ~"",
        TRUE ~"Uncertain Estimate")) %>%
      select(NAME, lep, `moe check 1`) %>%
      unique()
    
    #merge two together
    tbl_final <- tbl_lep %>%
      left_join(tbl_lang) %>%
      mutate(pct_lep = lep / other_lang) %>% 
      rename(`Speak other language` = other_lang,
             `Limited English Proficiency` = lep,
             `%LEP` = pct_lep)
  }
  
  ####################
  #poverty by race
  ####################
  else if(tablename =="B17001B" || tablename =="B17001D" 
          || tablename =="B17001E" || tablename =="B17001H" || tablename =="B17001I"){
    table <- get_acs(table = tablename, geography = geo, year = year)
    raw_dta <- table %>% select(NAME,variable, estimate,moe)
    tbl <- raw_dta %>% left_join(label) %>% select(label, everything()) # join table
    
    tot <- c("Total")
    pov <- c("Total!!Income in the past 12 months below poverty level")
    
    tbl_total <- tbl %>% 
      filter(label %in% tot) %>%
      mutate(`moe check` = case_when(
        moe < estimate *.25   ~"",
        TRUE ~ "Uncertain Estimate")) %>%
      select(NAME, estimate, `moe check`) %>%
      rename(`Total Population` = estimate)
    
    
    tbl_pov <- tbl %>% 
      filter(label %in% pov) %>%
      mutate(`moe check 1` = case_when(
        moe < estimate * 0.25 ~"",
        TRUE ~"Uncertain Estimate")) %>%
      select(NAME, estimate, `moe check 1`) %>%
      rename(`Below poverty` = estimate)
    
    #merge two together
    tbl_final <- tbl_total %>%
      left_join(tbl_pov) %>%
      mutate(pct_pov = `Below poverty` / `Total Population`) %>%
      rename(`% below poverty` = pct_pov)
    
    
  } 
  ###################
  #leave this blank
  ###################  
  else{
    table <- get_acs(table = tablename, geography = geo, year = year)
  }
  ###################
  #re-arrange geo
  ################### 
  if (geo == "county") {
    tbl_final <- tbl_final %>%
      separate(NAME, into = c("County", "State"), sep = ", ") %>%
      arrange(State)
  } else if (geo == "congressional district") {
    tbl_final <- tbl_final %>%
      separate(NAME, into = c("Congressional District", "State"), sep = ", ") %>%
      arrange(State)
  } else if (geo == "state") {
    tbl_final <- tbl_final %>%
      rename(State = NAME) %>%
      arrange(State)
  } else {
    tbl_final <- tbl_final %>%
      rename(Geography = NAME) %>%
      arrange(Geography) %>%
      select(-Geography)
  }
  
  return(tbl_final)
  
}
