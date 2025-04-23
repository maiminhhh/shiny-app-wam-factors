#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(stringr)
library(readr)
library(plotly)
library(DT)

dataset = read.csv("dataset.csv")

#modify column names
new_col_names = c("time", "n_units", "task_approach", "age", "respond_habit", "fass_units", "fass_major", "novel", "lib", "health_insurance", 
                  "sweet_days", "rent", "post_code", "haircut_freq", "laptop", "urinal_position", "stall_position", "n_weetbix", "money_fnb", 
                  "pineapple", "living_arrangements", "height", "uni_transport", "anxious_freq", "study_time", "work", "social_media", "gender", 
                  "sleep_time", "diet", "random_num", "steak", "dominant_hand", "unit_stream", "exercise_time", "employment_time", "asm_submit", 
                  "r_exp", "group_role", "social_media_time", "uni_year", "sports", "wam", "shoe_size")
colnames(dataset) = new_col_names

#clean height
dataset$height = case_when(
  readr::parse_number(dataset$height) <= 2.5 ~ readr::parse_number(dataset$height)*100,
  readr::parse_number(dataset$height) <= 9 ~ NA_real_,
  readr::parse_number(dataset$height) < 90 ~ readr::parse_number(dataset$height)*2.54,
  readr::parse_number(dataset$height) > 100 ~ readr::parse_number(dataset$height),
  TRUE ~ as.numeric(dataset$height)
)
dataset$height = if_else(as.numeric(dataset$height) > 100, dataset$height, NA)

#clean gender
dataset$gender = gendercoder::recode_gender(dataset$gender)

#clean social media
dataset$social_media = dataset$social_media %>% 
  tolower() 
dataset$social_media = str_replace_all(dataset$social_media, '[[:punct:]]',' ')
dataset$social_media = stringr::word(dataset$social_media)
dataset$social_media = case_when(
  stringr::str_starts(dataset$social_media, "in") ~ "instagram",
  stringr::str_starts(dataset$social_media, "ig") ~ "instagram",
  stringr::str_starts(dataset$social_media, "tik") ~ "tiktok",
  stringr::str_starts(dataset$social_media, "we") ~ "wechat",
  stringr::str_starts(dataset$social_media, "x") ~ "twitter",
  stringr::str_starts(dataset$social_media, "mess") ~ "facebook",
  stringr::str_starts(dataset$social_media, "bil") ~ "bilibili",
  is.na(dataset$social_media) | dataset$social_media == "" ~ NA,
  TRUE ~ dataset$social_media
)
dataset$social_media = tools::toTitleCase(dataset$social_media)
dataset$social_media = forcats::fct_lump_min(dataset$social_media, min = 10)

#clean age
dataset$age = if_else(dataset$age > 16 & dataset$age < 100, dataset$age, NA)

#clean number of units taken this sem
dataset$n_units = if_else(dataset$n_units > 0 & dataset$n_units < 11, dataset$n_units, NA)

#clean responding yes or no through life
dataset$respond_habit = if_else(dataset$respond_habit == "", NA, dataset$respond_habit)

#clean have ever taken fass units
dataset$fass_units = if_else(dataset$fass_units == "", NA, dataset$fass_units)

#clean read novel this year
dataset$novel = if_else(dataset$novel == "", NA, dataset$novel)

#clean fav library
dataset$lib = forcats::fct_lump_min(dataset$lib, min = 10)
dataset$lib = if_else(dataset$lib == "", NA, dataset$lib)

#clean private health insurance
dataset$health_insurance = if_else(dataset$health_insurance == "", NA, dataset$health_insurance)

#clean number of days eating sweet
dataset$sweet_days = if_else(readr::parse_number(dataset$sweet_days) >= 0 & 
                               readr::parse_number(dataset$sweet_days) <= 7,
                             readr::parse_number(dataset$sweet_days),
                             NA)

#clean pay rent
dataset$rent = if_else(dataset$rent == "" | 
                         (dataset$rent != "Yes" & 
                            dataset$rent != "No"), 
                       NA, dataset$rent)

#clean post code
dataset$post_code = if_else(readr::parse_number(dataset$post_code) > 2000, 
                            readr::parse_number(dataset$post_code), NA)

#clean haircut frequency
dataset$haircut_freq = dplyr::case_when(
  readr::parse_number(dataset$haircut_freq) <= 8 ~ readr::parse_number(dataset$haircut_freq) * 30,
  readr::parse_number(dataset$haircut_freq) > 8 ~ readr::parse_number(dataset$haircut_freq),
  is.na(dataset$haircut_freq) | dataset$haircut_freq == "" ~ NA,
  TRUE ~ as.numeric(dataset$haircut_freq)
)

#clean laptop type
dataset$laptop = tolower(dataset$laptop) %>% trimws() 
dataset$laptop = if_else(dataset$laptop == "", NA, dataset$laptop)
dataset$laptop = if_else(grepl("mac", dataset$laptop) | 
                           grepl("apple", dataset$laptop) | 
                           grepl("macbook", dataset$laptop), "Mac OS", "Other OS")

#clean urinal position
dataset$urinal_position = case_when(
  dataset$urinal_position == "C: Furthest from the entrance" ~ "furthest",
  dataset$urinal_position == "B: Middle" ~ "middle",
  dataset$urinal_position == "A: Closest to the entrance" ~ "closest",
  dataset$urinal_position == "" ~ NA,
  TRUE ~ dataset$urinal_position
)

#clean stall position
dataset$stall_position = case_when(
  dataset$stall_position == "C: Furthest from the entrance" ~ "furthest",
  dataset$stall_position == "B: Middle" ~ "middle",
  dataset$stall_position == "A: Closest to the entrance" ~ "closest",
  dataset$stall_position == "" ~ NA,
  TRUE ~ dataset$stall_position
)

#clean pineapple on pizza
dataset$pineapple = case_when(
  dataset$pineapple == "Yes" | dataset$pineapple == "No" ~ dataset$pineapple,
  is.na(dataset$pineapple) | dataset$pineapple == "" | (dataset$pineapple != "Yes" & dataset$pineapple != "No") ~ NA,
  TRUE ~ dataset$pineapple
)

#clean living arrangements
dataset$living_arrangements = case_when(
  dataset$living_arrangements == "With parent(s) and/or sibling(s)" ~ "family",
  dataset$living_arrangements == "With partner" ~ "partner",
  dataset$living_arrangements == "College or student accomodation" ~ "students",
  dataset$living_arrangements == "Alone" ~ "alone",
  dataset$living_arrangements == "Share house" ~ "shared",
  TRUE ~ dataset$living_arrangements
)
dataset$living_arrangements = forcats::fct_lump_min(dataset$living_arrangements, min = 10)
dataset$living_arrangements = stringr::word(dataset$living_arrangements) %>% 
  tools::toTitleCase()

#clean study time
dataset$study_time = if_else(dataset$study_time >= 168, NA, dataset$study_time)

#clean working
dataset$work = if_else(dataset$work %in% c("Casual","Contractor","Full time","I don't currently work","Part time", "Self employed"), 
                       dataset$work, NA)

#clean sleep time
dataset$sleep_time = case_when(
  readr::parse_number(dataset$sleep_time) < 20 ~ readr::parse_number(dataset$sleep_time),
  readr::parse_number(dataset$sleep_time) >= 20 ~ readr::parse_number(dataset$sleep_time) / 60,
  TRUE ~ as.numeric(dataset$sleep_time)
)
dataset$sleep_time = if_else(dataset$sleep_time > 24, NA, dataset$sleep_time)

#clean diet
dataset$diet = tools::toTitleCase(dataset$diet)
dataset$diet = if_else(dataset$diet %in% c("Omnivorous", "Pescatarian", "Vegan", "Vegetarian"), dataset$diet, NA)

#clean steak preference
dataset$steak = if_else(dataset$steak %in% c("I don't eat beef", "Medium", "Medium-rare", "Medium-well done", "Rare", "Well done"), 
                        dataset$steak, NA)

#clean exercise time
dataset$exercise_time = if_else(dataset$exercise_time > 5*7, NA, dataset$exercise_time)

#clean social media time
dataset$social_media_time = if_else(dataset$social_media_time >= 24, NA, dataset$social_media_time)

#clean current uni year
dataset$uni_year = if_else(dataset$uni_year == "" | is.na(dataset$uni_year), NA, dataset$uni_year)

#clean wam
dataset$wam = if_else(dataset$wam >= 50 & dataset$wam <= 100, dataset$wam, NA)

#clean dominant hand
dataset$dominant_hand = if_else(dataset$dominant_hand == "" | is.na(dataset$dominant_hand), NA, dataset$dominant_hand)

#clean 2002 or 2902
dataset$unit_stream = if_else(dataset$unit_stream == "" | is.na(dataset$unit_stream), NA, dataset$unit_stream)

#clean asm submission pattern
dataset$asm_submit = if_else(dataset$asm_submit == "" | is.na(dataset$asm_submit), NA, dataset$asm_submit)

#clean experience with r
dataset$r_exp = if_else(dataset$r_exp == "" | is.na(dataset$r_exp), NA, dataset$r_exp)

#clean major/minor in FASS
dataset$fass_major = if_else(dataset$fass_major == "" | is.na(dataset$fass_major), NA, dataset$fass_major)

#clean shoe size
dataset$shoe_size = if_else(dataset$shoe_size < 3 | dataset$shoe_size > 300 | dataset$shoe_size == "" | is.na(dataset$shoe_size), 
                            NA, dataset$shoe_size)
dataset$shoe_size = case_when(
  dataset$shoe_size %in% c(35, 22.8, 3) ~ 228,
  dataset$shoe_size %in% c(35.5, 23.1, 3.5) ~ 231,
  dataset$shoe_size %in% c(36, 23.5, 4, 34) ~ 235,
  dataset$shoe_size %in% c(37, 23.8, 4.5) ~ 238,
  dataset$shoe_size %in% c(37.5, 24.1, 5) ~ 241,
  dataset$shoe_size %in% c(38, 24.5, 5.5) ~ 245,
  dataset$shoe_size %in% c(38.5, 24.8, 6, 24) ~ 248,
  dataset$shoe_size %in% c(39, 25.1, 6.5) ~ 251,
  dataset$shoe_size %in% c(40, 25.4, 7, 9) ~ 254,
  dataset$shoe_size %in% c(41, 25.7, 7.5, 9.5) ~ 257,
  dataset$shoe_size %in% c(42,26, 8) ~ 260,
  dataset$shoe_size %in% c(43, 26.7, 8.5, 10.5, 27) ~ 267,
  dataset$shoe_size %in% c(44, 27.3, 10) ~ 273,
  dataset$shoe_size %in% c(45, 27.9, 11, 13) ~ 279,
  dataset$shoe_size %in% c(46.5,  28.6, 12, 14) ~ 286,
  dataset$shoe_size %in% c(48.5, 29.2, 13.5, 11.5) ~ 292,
  TRUE ~ dataset$shoe_size
)
dataset$shoe_size = if_else(dataset$shoe_size < 228 | dataset$shoe_size > 300 | dataset$shoe_size == "" | is.na(dataset$shoe_size), 
                            NA, dataset$shoe_size)

#clean task_approach
dataset$task_approach = case_when(
  dataset$task_approach == "" ~ NA,
  dataset$task_approach == "cram at the last second" ~ "last minute",
  dataset$task_approach == "do them immediately" ~ "immediately",
  dataset$task_approach == "draw up a schedule and work through it in planned stages" ~ "plan",
  TRUE ~ dataset$task_approach
)

#clean uni_year
dataset$uni_year = case_when(
  dataset$uni_year == "First year" ~ "1st year",
  dataset$uni_year == "Second year" ~ "2nd year",
  dataset$uni_year == "Third year" ~ "3rd year",
  dataset$uni_year == "Fourth year" ~ "4th year",
  dataset$uni_year == "Fifth or higher year" ~ "5th+ year",
  dataset$uni_year == "" ~ NA,
  TRUE ~ dataset$uni_year
)

cat_var_name = c("Assignment approach", "Often say \"yes\" or \"no\"", "1+ unit(s) from FASS", 
                 "Major/minor in FASS", "Read a novel this year", "Favorite USYD library", "Private health insurance",
                 "Pay rent", "Laptop brand", "Urinal position preference", "Stall position preference", "Pineapple on pizza", 
                 "Current living arrangements", "Employment status", "Favorite social media platform", "Gender",
                 "Diet style", "Steak preference", "Dominant hand", "Data unit stream", "Assignment submission", "Experience with R",
                 "Current uni year")
num_var_name = c("Number of units this sem", "Age", "Sugar-consuming days", "Post code", "Haircut frequency", "Weetbix", "Money on food & beverage",
                 "Height", "Anxious frequency", "Hours of Study", "Hours of Sleep", "Random number", "Hours of Exercise", "Hours of Work",
                 "Active level in group work", "Hours of Social media", "Weighted Average Mark (WAM)", "Shoe size")

cat_var = c("task_approach", "respond_habit", "fass_units", "fass_major", "novel", "lib", "health_insurance", "rent", "laptop", "urinal_position",
            "stall_position", "pineapple", "living_arrangements", "work", "social_media", "gender", "diet", "steak", 
            "dominant_hand", "unit_stream", "asm_submit", "r_exp", "uni_year")
num_var = c("n_units", "age", "sweet_days", "post_code", "haircut_freq", "n_weetbix", "money_fnb", "height", "anxious_freq", "study_time", 
            "sleep_time", "random_num", "exercise_time", "employment_time", "group_role", "social_media_time", "wam", "shoe_size")

dict_cat_var = data.frame(cat_var_name, cat_var)
dict_cat_var = dplyr::arrange(dict_cat_var, cat_var_name)
cat_var_name = dict_cat_var$cat_var_name
cat_var = dict_cat_var$cat_var

dict_num_var = data.frame(num_var_name, num_var)
dict_num_var = dplyr::arrange(dict_num_var, num_var_name)
num_var_name = dict_num_var$num_var_name
num_var = dict_num_var$num_var

dict_time_var = filter(dict_num_var, startsWith(dict_num_var$num_var_name, "Hours"))
dict_time_var = filter(dict_time_var, num_var_name != "Hours of Social media")
time_var_name = dict_time_var$num_var_name
time_var = dict_time_var$num_var

dict_cat_mean = filter(dict_cat_var, cat_var_name %in% c("Often say \"yes\" or \"no\"","1+ unit(s) from FASS","Major/minor in FASS",
                                                         "Read a novel this year","Private health insurance","Pay rent","Pineapple on pizza",
                                                         "Data unit stream","Experience with R"))
cat_mean_name = dict_cat_mean$cat_var_name
cat_mean = dict_cat_mean$cat_var

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "minty"),
  
  # Application title
  navbarPage(
    title = "Get To Know DATA2X02 - Semester 2/2023 Students",
    id = "navbar_page",
    tabPanel("Data Visualization",
             sidebarLayout(
               
               sidebarPanel = sidebarPanel(
                 
                 #side bar for data table
                 conditionalPanel(
                   condition = "input.tab_viz == 1",
                   
                   #selecting categorical variables to be displayed in the table
                   selectInput(inputId = "table_cat_var",
                               label = "Which categorical variable(s) do you wish to see in your table?",
                               choices = cat_var_name,
                               multiple = TRUE),
                   
                   #selecting numerical variables to be displayed in the table
                   selectInput(inputId = "table_num_var",
                               label = "Which numerical variable(s) do you wish to see in your table?",
                               choices = num_var_name,
                               multiple = TRUE),
                   
                   #whether to throw away NA
                   checkboxInput(inputId = "throw_na_tb",
                                 label = "Wanna get rid of empty variables? Click here!")
                 ),
                 
                 #side bar for bar chart
                 conditionalPanel(
                   condition = "input.tab_viz == 2",
                   
                   #special option: how students spend time
                   radioButtons(inputId = "bar_time",
                                label = "Are you curious about how different groups of students in the cohort spend their time on a weekly basis?",
                                choices = c("Tell me more",
                                            "Nah, more into other aspects")),
                   
                   #if user is curious about the time variables
                   conditionalPanel(condition = "input.bar_time == 'Tell me more'",
                                    #selecting numerical time activities
                                    selectInput(inputId = "time_by_activities",
                                                label = "Which students' activities are you interested in?",
                                                multiple = TRUE,
                                                choices = time_var_name,
                                                selected = time_var_name[1]),
                                    
                                    #selecting categorical variables to group students
                                    selectInput(inputId = "time_by_group",
                                                label = "By which category do you want to group your students? (this would be your x-axis later on)",
                                                choices = cat_var_name)
                   ),
                   
                   #usual bar plot: choosing 1 categorical variable and an optional categorical variable as color fill
                   conditionalPanel(condition = "input.bar_time == 'Nah, more into other aspects'",
                                    #selecting a main categorical variable to be displayed
                                    selectInput(inputId = "bar_cat_main",
                                                label = "Which categorical variable do you wish to see in your Bar chart?",
                                                choices = cat_var_name),
                                    
                                    #selecting an optional categorical variables as color fill
                                    selectInput(inputId = "bar_cat_color",
                                                label = "Do you want to color your variable with another categorical variable?",
                                                choices = NULL),
                                    
                                    #select a categorical variable for faceting
                                    conditionalPanel(condition = "input.bar_cat_color != 'Nah, all good with black and white'",
                                                     selectInput(inputId = "facet_var_bar",
                                                                 label = "Interested in comparing? Choose a category below to group our data!",
                                                                 choices = NULL))),
                   
                   #percentage option
                   checkboxInput(inputId = "percent",
                                 label = "Wanna have a look at the data in percentage? Click here! (Not recommended if you are only looking into 1 variable)"),
                   
                   #throw away NA
                   checkboxInput(inputId = "throw_na_bar",
                                 label = "Wanna get rid of empty variables? Click here!")),
                 
                 #side bar for histogram
                 conditionalPanel(
                   condition = "input.tab_viz == 3",
                   
                   #selecting a numerical variable to be displayed in the histogram
                   selectInput(inputId = "hist_main_var",
                               label = "Choose a numerical variable that you are interested in",
                               choices = num_var_name),
                   
                   #selecting a categorical variable to color the histogram
                   selectInput(inputId = "hist_color_var",
                               label = "Is there any categorical variable by which you want to color your Histogram?",
                               choices = c("None", cat_var_name)),
                   
                   #choose a variable for faceting
                   conditionalPanel(condition = "input.hist_color_var != 'None'",
                                    selectInput(inputId = "facet_var_hist",
                                                label = "Interested in comparing? Choose a category below to group our data!",
                                                choices = NULL)),
                   
                   #whether to throw away NA
                   checkboxInput(inputId = "throw_na_hist",
                                 label = "Wanna get rid of empty variables? Click here!")
                 ),
                 
                 #side bar for box plot
                 conditionalPanel(
                   condition = "input.tab_viz == 4",
                   
                   #select a numerical variable
                   selectInput(inputId = "box_num_var",
                               label = "Choose one of the numerical variables below to draw a box plot with",
                               choices = num_var_name),
                   
                   #select a categorical variable
                   selectInput(inputId = "box_cat_var1",
                               label = "Want some comparisons? Choose a categorical variable below!",
                               choices = c("Nah, all good!", cat_var_name)),
                   
                   #throw away NA
                   checkboxInput(inputId = "throw_na_box",
                                 label = "Wanna get rid of empty variables? Click here!")
                 ),
                 
                 #side bar for scatter plot
                 conditionalPanel(
                   condition = "input.tab_viz == 5",
                   
                   #select numerical variable 1
                   selectInput(inputId = "dot_num1",
                               label = "Select a numerical variable that you are interested in",
                               choices = num_var_name),
                  
                   #select numerical variable 2
                   selectInput(inputId = "dot_num2",
                               label = "Select another numerical variable that you are interested in",
                               choices = num_var_name),
                   
                   #optionally select a categorical variable for coloring
                   selectInput(inputId = "dot_cat1",
                               label = "Would you like to colour your scatter plot? Choose a categorical variable to do so!",
                               choices = c("Nah, no need to", cat_var_name)),
                   
                   #optionally select another categorical variable for facet
                   conditionalPanel(condition = "input.dot_cat1 != 'Nah, no need to'",
                                    selectInput(inputId = "dot_cat2",
                                               label = "Want a comparison across different categorical groups? Choose a variable to do so!",
                                               choices = c("This should already be fine", cat_var_name))),
                   
                   #throw away NA
                   checkboxInput(inputId = "throw_na_dot",
                                 label = "Wanna get rid of empty variables? Click here!")
                 )
               ),
               
               mainPanel = mainPanel(
                 tabsetPanel(id = "tab_viz",
                             
                             tabPanel("Data Table", value = 1, DTOutput("table")),
                             
                             tabPanel("Bar Chart", value = 2, plotlyOutput("bar_chart")),
                             
                             tabPanel("Histogram", value = 3, plotlyOutput("histogram")),
                             
                             tabPanel("Box Plot", value = 4, plotlyOutput("boxplot")),
                             tabPanel("Scatter Plot", value = 5, plotlyOutput("scatter_plot"))
                 )
               )
             )
    ),
    
    tabPanel("Hypothesis Testing",
             sidebarLayout(
               
               sidebarPanel(
                 
                 #side bar for test for independence
                 conditionalPanel(
                   condition = "input.tab_test == 1",
                   
                   #first tested variable
                   selectInput(inputId = "idp_var1",
                               label = "Choose the first variable to perform your test for independence",
                               choices = cat_var_name),
                   
                   #second tested variable
                   selectInput(inputId = "idp_var2",
                               label = "Choose the second variable to perform your test for independence",
                               choices = NULL),
                   
                   #level of significance
                   sliderInput(inputId = "idp_sig_lv",
                               label = "Choose the significant level for your test",
                               min = 0,
                               max = 1,
                               value = 0.05),
                   
                   #whether to fix continuity correction or not
                   checkboxInput(inputId = "correction",
                                 label = "Would you like to adjust continuity correction?")
                 ),
                 
                 #side bar for test for means
                 conditionalPanel(
                   condition = "input.tab_test == 2",
                   
                   #select a numerical variable
                   selectInput("mean_num_var",
                               label = "Select a numerical variable that you are interested in",
                               choices = num_var_name),
                   
                   #select a categorical variable
                   selectInput("mean_cat_var",
                               label = "Choose a categorical variable to group your sample",
                               choices = cat_mean_name),
                   
                   #select alternative hypothesis
                   radioButtons("mean_h1",
                                label = "What would you want to be your alternative hypothesis?",
                                choices = c("greater than", "not equal to", "less than")),
                   
                   #select level of significance
                   sliderInput("mean_sig_lv",
                               label = "Choose the significant level for your test",
                               min = 0,
                               max = 1,
                               value = 0.05)
                 )
               ),
               
               mainPanel(
                 tabsetPanel (id = "tab_test",
                              
                              tabPanel("Test for Independence", value = 1, 
                                       
                                       #print out contingency table
                                       column(12, align = "center",
                                              div(br(),
                                                  h5("Observed contingency table"),
                                                  tableOutput("idp_cont_tabl"))),
                                       
                                       #print out the hypothesis and test assumptions
                                       uiOutput("idp_hypothesis"),
                                       
                                       #print out the expected values
                                       column(12, align = "center",
                                              div(br(),
                                                  h5("Expected contingency table"),
                                                  tableOutput("idp_ev_cont_tabl"))),
                                       
                                       #print out whether to use Monte Carlo simulation
                                       uiOutput("idp_test_type"),
                                       
                                       #print out the test statistic
                                       uiOutput("idp_test_stat"),
                                       
                                       #print out the heading of the ov test statistic
                                       uiOutput("idp_head_t"),
                                       
                                       #print out the observed test statistic
                                       uiOutput("idp_ov_test_stat"),
                                       
                                       #print out the heading of the p-value
                                       uiOutput("idp_head_p"),
                                       
                                       #print out the p value
                                       uiOutput("idp_pval"),
                                       
                                       #print out the rest of the test
                                       verbatimTextOutput("idp_test"),
                                       
                                       #print out the conclusion of the test
                                       uiOutput("idp_conclusion")),
                              
                              tabPanel("Two-sample Test for Means", value = 2, 
                                       
                                       #print out box plot
                                       plotlyOutput("mean_boxplot"),
                                       
                                       #print out the hypothesis
                                       uiOutput("mean_hypothesis"),
                                       
                                       #print out the qq plot to check for normality
                                       plotOutput("qqplot"),
                                       
                                       #print out the test type to test for means
                                       uiOutput("mean_test_type"),
                                       
                                       #print out the test statistic
                                       uiOutput("mean_test_stat"),
                                       
                                       #print out the heading of the observed test statistic
                                       uiOutput("mean_head_t"),
                                       
                                       #print out the ov test stat
                                       uiOutput("mean_ov_test_stat"),
                                       
                                       #print out the heading for the p-value
                                       uiOutput("mean_head_p"),
                                       
                                       #print out the p-value
                                       uiOutput("mean_pval"),
                                       
                                       #print out the mean test performed
                                       verbatimTextOutput("mean_test"),
                                       
                                       #print out the conclusion
                                       uiOutput("mean_conclusion")
                 )
               )
             )
          )
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output, session) {
  observe ({
    choices_bar_cat_color = setdiff(cat_var_name, input$bar_cat_main)
    updateSelectInput(session, "bar_cat_color", choices = c("Nah, all good with black and white", choices_bar_cat_color))
  })
  
  observe ({
    choices_facet_var_bar = setdiff(cat_var_name, input$bar_cat_color)
    choices_facet_var_bar = setdiff(choices_facet_var_bar, input$bar_cat_main)
    updateSelectInput(session, "facet_var_bar", choices = c("No, thanks 😊", choices_facet_var_bar))
  })
  
  observe ({
    choices_facet_var_hist = setdiff(cat_var_name, input$hist_color_var)
    updateSelectInput(session, "facet_var_hist", choices = c("No, thanks 😊", choices_facet_var_hist))
  })
  
  observe ({
    choices_box_cat_var2 = setdiff(cat_var_name, input$box_cat_var1)
    updateSelectInput(session, "box_cat_var2", choices = c("All good", choices_box_cat_var2))
  })
  
  observe ({
    choices_dot_num2 = setdiff(num_var_name, input$dot_num1)
    updateSelectInput(session, "dot_num2", choices = choices_dot_num2)
  })
  
  observe ({
    choices_dot_cat2 = setdiff(cat_var_name, input$dot_cat1)
    updateSelectInput(session, "dot_cat2", choices = c("This should already be fine", choices_dot_cat2))
  })
  
  observe ({
    choices_idp_var2 = setdiff(cat_var_name, input$idp_var1)
    updateSelectInput(session, "idp_var2", choices = choices_idp_var2)
  })
  
  #defining data table in data visualization
  output$table = DT::renderDataTable({
    table_var_name = c(input$table_cat_var, input$table_num_var)
    table_var = numeric()
    table_df = data.frame(numeric(dim(dataset)[1]))
    colnames(table_df) = "Empty"
    if (length(table_var_name != 0)) {
      for (i in 1:length(table_var_name)) {
        if (table_var_name[i] %in% cat_var_name) {
          index = which(cat_var_name == table_var_name[i])
          table_var = append(table_var, cat_var[index])
        }
        else {
          index = which(num_var_name == table_var_name[i])
          table_var = append(table_var, num_var[index])
        }
      }
      
      for (i in 1:length(table_var)) {
        table_df = mutate(table_df, dataset[[table_var[i]]])
        colnames(table_df) = c("Empty", table_var_name[1:i])
      }
    }
    table_df = subset(table_df, select = -Empty)
    colnames(table_df) = table_var_name
    
    if (input$throw_na_tb) {
      for (i in 1:length(colnames(table_df))) {
        table_df = filter(table_df, !is.na(table_df[[i]]))
      }
    }
    
    datatable(table_df, 
              filter = "top",
              selection = "multiple",
              style = "bootstrap4")
  })
  
  #defining bar chart in data visualization
  output$bar_chart = renderPlotly({
    #bar chart for time
    if (input$bar_time == "Tell me more") {
      bar_time_var_name = input$time_by_activities
      if (length(bar_time_var_name)!=0){
        bar_time_group_name = input$time_by_group
        bar_time_group_var = cat_var[which(cat_var_name == bar_time_group_name)]
        bar_time_var = numeric()
        for (i in 1:length(bar_time_var_name)) {
          index = which(time_var_name == bar_time_var_name[i])
          bar_time_var = append(bar_time_var, time_var[index])
        }
        
        bar_time_df = data.frame(dataset[[bar_time_group_var]])
        colnames(bar_time_df) = bar_time_group_name
        for (i in 1:length(bar_time_var)) {
          bar_time_df = mutate(bar_time_df, dataset[[bar_time_var[i]]])
          colnames(bar_time_df) = c(bar_time_group_name, bar_time_var_name[1:i])
        }
        
        if (input$throw_na_bar) {
          for (i in 1:length(colnames(bar_time_df))) {
            bar_time_df = filter(bar_time_df, !is.na(.data[[colnames(bar_time_df)[i]]]))
          }
        }
        
        bar_time_df_long = pivot_longer(bar_time_df, cols = starts_with("Hours"), names_to = "activities", values_to = "value")
        colnames(bar_time_df_long) = c(bar_time_group_name, "activities", "value")
        if (input$percent){
          bar = ggplot(bar_time_df_long) +
            aes(.data[[bar_time_group_name]], y = value, fill = activities) +
            geom_bar(stat = "identity", position = "fill") +
            labs(y = "Hours per week", fill = "Activities categories")
        }
        else {
          bar = ggplot(bar_time_df_long) +
            aes(.data[[bar_time_group_name]], y = value, fill = activities) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(y = "Hours per week", fill = "Activities categories")
        }
      }
    }
    
    #usual bar chart
    else{
      bar_main_name = input$bar_cat_main
      bar_main = cat_var[which(cat_var_name == bar_main_name)]
      bar_color_name = input$bar_cat_color
      if (bar_color_name == "Nah, all good with black and white" | is.na(bar_color_name)) {
        bar_color = NA
      }
      else {
        bar_color = cat_var[which(cat_var_name == bar_color_name)]
      }
      
      if (is.na(bar_color)) {
        bar_df = data.frame(dataset[[bar_main]])
        colnames(bar_df) = bar_main_name
        if (input$throw_na_bar) {
          bar_df = filter(bar_df, !is.na(.data[[bar_main_name]]))
        }
        if (input$percent) {
          bar = ggplot(bar_df) +
            aes(.data[[bar_main_name]]) + 
            geom_bar(position = "fill")
        }
        else {
          bar = ggplot(bar_df) + 
            aes(.data[[bar_main_name]]) +
            geom_bar(position = "dodge")
        }
      }
      else {
        bar_facet_name = input$facet_var_bar
        if(bar_facet_name == "No, thanks 😊" | is.na(bar_facet_name)) {
          bar_facet = NA
        }
        else {
          bar_facet = cat_var[which(cat_var_name == bar_facet_name)]
        }
        if (is.na(bar_facet)) {
          bar_df = data.frame(dataset[[bar_main]], dataset[[bar_color]])
          colnames(bar_df) = c(bar_main_name, bar_color_name)
          if (input$throw_na_bar) {
            bar_df = filter(bar_df, !is.na(.data[[bar_main_name]]) & !is.na(.data[[bar_color_name]]))
          }
          if (input$percent) {
            bar = ggplot(bar_df) +
              aes(.data[[bar_main_name]], fill = .data[[bar_color_name]]) +
              geom_bar(position = "fill")
          }
          else {
            bar = ggplot(bar_df) + 
              aes(.data[[bar_main_name]], fill = .data[[bar_color_name]]) + 
              geom_bar(position = "dodge")
          }
        }
        else {
          bar_df = data.frame(dataset[[bar_main]], dataset[[bar_color]], dataset[[bar_facet]])
          colnames(bar_df) = c(bar_main_name, bar_color_name, bar_facet_name)
          if (input$throw_na_bar) {
            bar_df = filter(bar_df, !is.na(.data[[bar_main_name]]) & !is.na(.data[[bar_color_name]]) & !is.na(.data[[bar_facet_name]]))
          }
          if (input$percent) {
            bar = ggplot(bar_df) +
              aes(x = .data[[bar_main_name]], fill = .data[[bar_color_name]]) + 
              geom_bar(position = "fill") +
              facet_grid(cols = vars(.data[[bar_facet_name]]), as.table = TRUE)
          }
          else {
            bar = ggplot(bar_df) + 
              aes(x = .data[[bar_main_name]], fill = .data[[bar_color_name]]) + 
              geom_bar(position = "dodge") + 
              facet_grid(cols = vars(.data[[bar_facet_name]]), as.table = TRUE)
          }
        }
      }
    }
    ggplotly(bar)
  })
  
  #defining histogram in data visualization
  output$histogram = renderPlotly({
    hist_var_name = input$hist_main_var
    hist_var = num_var[which(num_var_name == hist_var_name)]
    hist_color_name = input$hist_color_var
    if (hist_color_name == "None" | is.na(hist_color_name)) {
      hist_color = NA
    }
    else {
      hist_color = cat_var[which(cat_var_name == hist_color_name)]
    }
    
    if (is.na(hist_color)) {
      hist_df = data.frame(dataset[[hist_var]])
      colnames(hist_df) = hist_var_name
      if (input$throw_na_hist) {
        hist_df = filter(hist_df, !is.na(.data[[hist_var_name]]))
      }
      histogram = ggplot(hist_df) +
        aes(.data[[hist_var_name]]) +
        geom_histogram()
    }
    else {
      hist_facet_name = input$facet_var_hist
      if (hist_facet_name == "No, thanks 😊" | is.na(hist_facet_name)) {
        hist_facet = NA
      }
      else {
        hist_facet = cat_var[which(cat_var_name == hist_facet_name)]
      }
      if (is.na(hist_facet)){
        hist_df = data.frame(dataset[[hist_var]], dataset[[hist_color]])
        colnames(hist_df) = c(hist_var_name, hist_color_name)
        if (input$throw_na_hist) {
          hist_df = filter(hist_df, !is.na(.data[[hist_var_name]]) & !is.na(.data[[hist_color_name]]))
        }
        histogram = ggplot(hist_df) +
          aes(.data[[hist_var_name]], fill = .data[[hist_color_name]]) +
          geom_histogram()
      }
      else{
        hist_df = data.frame(dataset[[hist_var]], dataset[[hist_color]], dataset[[hist_facet]])
        colnames(hist_df) = c(hist_var_name, hist_color_name, hist_facet_name)
        if (input$throw_na_hist) {
          hist_df = filter(hist_df, !is.na(.data[[hist_var_name]]) & !is.na(.data[[hist_color_name]]) & !is.na(.data[[hist_facet_name]]))
        }
        histogram = ggplot(hist_df) +
          aes(.data[[hist_var_name]], fill = .data[[hist_color_name]]) +
          geom_histogram() +
          facet_grid(vars(.data[[hist_facet_name]]), as.table = TRUE)
      }
    }
    ggplotly(histogram)
  })
  
  #defining box plot in data visualization
  output$boxplot = renderPlotly({
    box_num_name = input$box_num_var
    box_cat_name = input$box_cat_var1
    if (box_cat_name == "Nah, all good!") {
      box_cat = NA
    }
    else {
      box_cat = cat_var[which(cat_var_name == box_cat_name)]
    }
    box_num = num_var[which(num_var_name == box_num_name)]
    
    if (is.na(box_cat)) {
      df_box = data.frame(dataset[[box_num]])
      colnames(df_box) = box_num_name
      if (input$throw_na_box) {
        df_box = filter(df_box, !is.na(.data[[box_num_name]]))
      }
      box = ggplot(df_box) +
        aes(x = "", y = .data[[box_num_name]]) + 
        labs(x = "") + 
        geom_boxplot()
    }
    else {
        df_box = data.frame(dataset[[box_num]], dataset[[box_cat]])
        colnames(df_box) = c(box_num_name, box_cat_name)
        if (input$throw_na_box) {
          df_box = filter(df_box, !is.na(.data[[box_num_name]]) & !is.na(.data[[box_cat_name]]))
        }
        box = ggplot(df_box) +
          aes(y = .data[[box_num_name]], x = .data[[box_cat_name]]) +
          geom_boxplot()
      }
    ggplotly(box)
  })

  #defining scatter plot in data visualization
  output$scatter_plot = renderPlotly({
    dot_num_name1 = input$dot_num1
    dot_num_name2 = input$dot_num2
    dot_color_name = input$dot_cat1
    dot_facet_name = input$dot_cat2
    if (dot_color_name == "Nah, no need to") {
      dot_color = NA
    }
    else {
      dot_color = cat_var[which(cat_var_name == dot_color_name)]
    }
    if (dot_facet_name == "This should already be fine") {
      dot_facet = NA
    }
    else {
      dot_facet = cat_var[which(cat_var_name == dot_facet_name)]
    }
    dot_num_var1 = num_var[which(num_var_name == dot_num_name1)]
    dot_num_var2 = num_var[which(num_var_name == dot_num_name2)]
    
    if (is.na(dot_color)) {
      dot_df = data.frame(dataset[[dot_num_var1]], dataset[[dot_num_var2]])
      colnames(dot_df) = c(dot_num_name1, dot_num_name2)
      if (input$throw_na_dot) {
        dot_df = filter(dot_df, !is.na(.data[[dot_num_name1]]) & !is.na(.data[[dot_num_name2]]))
      }
      dot_plot = ggplot(dot_df) +
        aes(.data[[dot_num_name1]], .data[[dot_num_name2]]) +
        geom_point()
    }
    else {
      if (is.na(dot_facet)) {
        dot_df = data.frame(dataset[[dot_num_var1]], dataset[[dot_num_var2]], dataset[[dot_color]])
        colnames(dot_df) = c(dot_num_name1, dot_num_name2, dot_color_name)
        if (input$throw_na_dot) {
          dot_df = filter(dot_df, !is.na(.data[[dot_num_name1]]) & !is.na(.data[[dot_num_name2]]) & !is.na(.data[[dot_color_name]]))
        }
        dot_plot = ggplot(dot_df) + 
          aes(.data[[dot_num_name1]], .data[[dot_num_name2]], col = .data[[dot_color_name]]) +
          geom_point()
      }
      else {
        dot_df = data.frame(dataset[[dot_num_var1]], dataset[[dot_num_var2]], dataset[[dot_color]], dataset[[dot_facet]])
        colnames(dot_df) = c(dot_num_name1, dot_num_name2, dot_color_name, dot_facet_name)
        if (input$throw_na_dot) {
          dot_df = filter(dot_df, !is.na(.data[[dot_num_name1]]) & !is.na(.data[[dot_num_name2]]) & !is.na(.data[[dot_color_name]]) & !is.na(.data[[dot_facet_name]]))
        }
        dot_plot = ggplot(dot_df) +
          aes(.data[[dot_num_name1]], .data[[dot_num_name2]], col = .data[[dot_color_name]]) +
          facet_grid(cols = vars(dot_df[[dot_facet_name]]),
                     as.table = TRUE) +
          geom_point()
      }
    }
    ggplotly(dot_plot)
  })
  
  #defining the data frame used in test for independence
  idp_df = reactive ({
    idp_var_name1 = input$idp_var1
    idp_var_name2 = input$idp_var2
    sig_lv = input$sig_lv
    idp_var1 = cat_var[which(cat_var_name == idp_var_name1)]
    idp_var2 = cat_var[which(cat_var_name == idp_var_name2)]
    idp_df = data.frame(dataset[[idp_var1]], dataset[[idp_var2]])
    colnames(idp_df) = c(idp_var_name1, idp_var_name2)
    idp_df = filter(idp_df, !is.na(.data[[idp_var_name1]]) & !is.na(.data[[idp_var_name2]]))
    return(idp_df)
  })
  
  #defining the contingency table of the test for independence
  output$idp_cont_tabl = renderTable ({
    idp_df = idp_df()
    as.data.frame.matrix(table(idp_df[[input$idp_var1]], idp_df[[input$idp_var2]]))
  }, align = "c", include.rownames = TRUE)
  
  #defining test for independence in hypothesis testing
  output$idp_hypothesis = renderUI({
    idp_var1 = input$idp_var1
    idp_var2 = input$idp_var2
    HTML(paste("<b>1. Hypotheses</b>:", "<br>", "- <b><i>Null hypothesis:</b></i> ", idp_var1, " is independent of ", idp_var2, "<br>", "- <b><i>Alternative hypothesis:</b></i> ", idp_var1, " and ", idp_var2, " are not independent.", "<br>", "<b>2. Assumptions:</b> independent observations and none of the expected frequencies are less than 5"))
  })
  
  #defining expected contingency table
  idp_exp_cont_tabl = reactive ({
    idp_df = idp_df()
    idp_ov_tabl = table(idp_df)
    idp_ov_matrix = matrix(as.integer(0), nrow = length(rownames(idp_ov_tabl)) + 1, ncol = length(colnames(idp_ov_tabl)) + 1)
    for (i in 1:length(rownames(idp_ov_tabl))) {
      for (j in 1:length(colnames(idp_ov_tabl))) {
        idp_ov_matrix[i,j] = (idp_ov_tabl[i,j])
      }
    }
    for (i in 1 : length(rownames(idp_ov_tabl))) {
      idp_ov_matrix[i,length(colnames(idp_ov_tabl)) + 1] = sum(idp_ov_matrix[i, ])
    }
    for (j in 1:length(colnames(idp_ov_tabl))) {
      idp_ov_matrix[length(rownames(idp_ov_tabl)) + 1,j] = sum(idp_ov_matrix[ ,j])
    }
    n = sum(idp_ov_matrix[,length(colnames(idp_ov_tabl)) + 1])
    idp_ov_matrix[length(rownames(idp_ov_tabl)) + 1,length(colnames(idp_ov_tabl)) + 1] = n
    colnames(idp_ov_matrix) = c(colnames(idp_ov_tabl), "row totals")
    rownames(idp_ov_matrix) = c(rownames(idp_ov_tabl), "Column totals")
    
    idp_ev_matrix = matrix(nrow = length(rownames(idp_ov_matrix))-1, ncol = length(colnames(idp_ov_matrix))-1)
    for (i in 1:length(rownames(idp_ov_matrix))-1) {
      for (j in 1:length(colnames(idp_ov_matrix))-1) {
        idp_ev_matrix[i,j] = idp_ov_matrix[i,length(colnames(idp_ov_matrix))] * idp_ov_matrix[length(rownames(idp_ov_matrix)), j]/n
      }
    }
    colnames(idp_ev_matrix) = colnames(idp_ov_tabl)
    rownames(idp_ev_matrix) = rownames(idp_ov_tabl)
    return (idp_ev_matrix)
  })
  
  #defining the test that is used
  idp_small_sample = reactive({
    ev_df = idp_exp_cont_tabl()
    if (all(ev_df > 5)) {
      return (FALSE)
    }
    return (TRUE)
  })
  
  #defining the printout of the expected table
  output$idp_ev_cont_tabl = renderTable ({
    idp_exp_cont_tabl()
  }, include.rownames = TRUE)
  
  #defining the printout of the test type
  output$idp_test_type = renderUI ({
    if (idp_small_sample()) {
      HTML(paste("Since not all of the expected frequencies are greater than or equal to 5, we would use the <b>Monte Carlo simulation</b> instead of the Chi-square test to check for Independence", "<br>","<b>3. Test statistic</b>:"))
    }
    else {
      HTML(paste("Since all of the assumptions have been satisfied, we would use the <b>Chi-square test</b> to check for Independence.", "<br>","<b>3. Test statistic</b>:"))
    }
  })
  
  #defining the information of the test performed
  idp_test = reactive ({
    var1 = idp_df()[[input$idp_var1]]
    var2 = idp_df()[[input$idp_var2]]
    if (input$correction) {
      if (idp_small_sample()) {
        set.seed(123)
        test = chisq.test(var1, var2, correct = TRUE, simulate.p.value = TRUE, B = 10000)
      }
      else {
        test = chisq.test(var1, var2, correct = TRUE)
      }
    }
    else {
      if(idp_small_sample()) {
        set.seed(123)
        test = chisq.test(var1, var2, correct = FALSE, simulate.p.value = TRUE, B = 10000)
      }
      else {
        test = chisq.test(var1, var2, correct = FALSE)
      }
    }
    return (test)
  })
  
  #defining the test statistic
  output$idp_test_stat = renderUI ({
    idp_ov_tabl = table(idp_df())
    withMathJax(sprintf("\\( T = \\sum_{i=1}^%.0f\\sum_{j=1}^%.0f\\frac{(Y_{ij} - e_{ij})^2}{e_{ij}} \\). Under \\( H_0 \\), \\( T \\sim \\chi_{%.0f}^2 \\) approximately", length(rownames(idp_ov_tabl)), length(colnames(idp_ov_tabl)), ((length(rownames(idp_ov_tabl))-1) * (length(colnames(idp_ov_tabl))-1))))
  })
  
  #defining the heading of the observed test statistic
  output$idp_head_t = renderUI ({
    HTML(paste("<b>4. Observed Test statistic</b>: "))
    })
  
  #defining the observed test statistic
  output$idp_ov_test_stat = renderUI ({
    idp_ov_tabl = table(idp_df())
    test_stat = idp_test()$statistic
    withMathJax(sprintf("\\( t_0 = \\sum_{i=1}^%.0f\\sum_{j=1}^%.0f\\frac{(y_{ij}-y_{i\\bullet}y_{\\bullet j}/n)^2}{y_{i\\bullet}y_{\\bullet j}/n} = %.04f \\)", length(rownames(idp_ov_tabl)), length(colnames(idp_ov_tabl)),test_stat))
  })
  
  #defining the heading of the p-value
  output$idp_head_p = renderUI ({
    HTML(paste("<b>5. p-value</b>: "))
  })
  
  #defining the p-val
  output$idp_pval = renderUI ({
    idp_ov_tabl = table(idp_df())
    pval = idp_test()$p.value
    withMathJax(sprintf("\\( P(T \\ge t_0) = P(\\chi_{%.0f}^2 \\ge t_0) = %.04f \\)", (length(rownames(idp_ov_tabl)) - 1)*(length(colnames(idp_ov_tabl))-1), pval))
  })
  
  #defining the independence test performed
  output$idp_test = renderPrint({
    print(idp_test())
  })
  
  #defining decision making
  idp_decision = reactive({
    pval = idp_test()$p.value
    sig_lv = input$idp_sig_lv
    if (pval > sig_lv) {
      return (TRUE)
    }
    else{
      return (FALSE)
    }
  })
  
  #defining the conclusion of the test
  output$idp_conclusion = renderUI({
    sig_lv = input$idp_sig_lv
    if (idp_decision()) {
      HTML(sprintf("<b>6. Decision</b>: Since the p-value is greater than <b>%.02f</b> <i>(which is chosen as the significant level)</i>, we would <b>retain the null hypothesis</b>. There is no evidence to suggest that %s is independent of %s", sig_lv, input$idp_var1, input$idp_var2))
    }
    else {
      HTML(sprintf("<b>6. Decision</b>: Since the p-value is less than <b>%.02f</b> <i>(which is chosen as the significant level)</i>, we would <b>reject the null hypothesis</b>. There is evidence to suggest that %s is independent of %s", sig_lv, input$idp_var1, input$idp_var2))
    }
  })
  
  #defining the data frame to be used in the test for mean
  mean_df = reactive({
    mean_cat_name = input$mean_cat_var
    mean_num_name = input$mean_num_var
    mean_cat_var = cat_var[which(cat_var_name == mean_cat_name)]
    mean_num_var = num_var[which(num_var_name == mean_num_name)]
    mean_df = data.frame(dataset[[mean_cat_var]], dataset[[mean_num_var]])
    colnames(mean_df) = c(mean_cat_name, mean_num_name)
    mean_df = filter(mean_df, !is.na(.data[[mean_cat_name]]) & !is.na(.data[[mean_num_name]]))
    return (mean_df)
  })
  
  #defining the box plot in the test for mean
  output$mean_boxplot = renderPlotly ({
    mean_df = mean_df()
    box = ggplot(mean_df) +
      aes(.data[[input$mean_cat_var]], .data[[input$mean_num_var]]) + 
      geom_boxplot()
    ggplotly(box)
  })
  
  #defining the hypothesis of the test for mean
  output$mean_hypothesis = renderUI ({
    HTML(paste("<b>1. Hypotheses</b>", "<br>", "- <b><i>Null hypothesis</b></i>: The means of DATA2X02 students' ", input$mean_num_var, " are the same across different ", input$mean_cat_var, ".","<br>", "- <b><i>Alternative hypothesis</b></i>: The mean of DATA2X02 students' ", input$mean_num_var, " in the group that answers ", unique(mean_df()[[input$mean_cat_var]])[1], " when being asked about ", input$mean_cat_var, " is ", input$mean_h1, " the group that responded ", unique(mean_df()[[input$mean_cat_var]])[2], " to the same question.", "<br>", "<b>2. Assumptions</b>: the 2 groups of samples are identically and independently distributed"))
  })
  
  #check if the samples are normally distributed
  norm_dis = reactive({
    mean_df = mean_df()
    t1 = shapiro.test(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]])$p.value
    t2 = shapiro.test(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]])$p.value
    if (t1 > 0.05 & t2 > 0.05) {
      return (TRUE)
    }
    return (FALSE)
  })
  
  #defining the qqplot for normal distribution
  output$qqplot = renderPlot({
    mean_df = mean_df()
    par(mfrow = c(1,2))
    qqnorm(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]]) ; qqline(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]])
    qqnorm(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]]) ; qqline(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]])
  })
  
  #defining the test type used to check the mean
  output$mean_test_type = renderUI({
    if (norm_dis()) {
      HTML(paste("Since the samples are normally distributed, all the assumptions for a two-sample t-test have been satisfied. Hence, we would use a <b>two-sample t-test</b> in this case.", "<br>", "<b>3. Test statistic</b>: "))
    }
    else {
      HTML(paste("Since the samples are not normally distributed, we would consider using <b>Wilcoxon rank sum test</b> in this case.", "<br>", "<b>3. Test statistic</b>: "))
    }
  })
  
  #defining if the standard deviation are the same across all samples
  mean_sd = reactive({
    mean_df = mean_df()
    sd1 = sd(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]])
    sd2 = sd(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]])
    if (abs(sd1-sd2) <= 1) {
      return (TRUE)
    }
    return (FALSE)
  })
  
  #defining the test for mean
  mean_test = reactive({
    if (input$mean_h1 == "not equal to") {
      alt = "two.sided"
    }
    else if (input$mean_h1 == "less than") {
      alt = "less"
    }
    else{
      alt = "greater"
    }
    mean_df = mean_df()
    if (norm_dis()) {
      mean_test = t.test(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]],
                         filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]],
                         var.equal = mean_sd(),
                         alternative = alt,
                         conf.level = 1 - input$mean_sig_lv)
    }
    else {
      mean_test = wilcox.test(filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]],
                              filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]],
                              alternative = alt,
                              conf.level = 1-input$mean_sig_lv)
    }
  })
  
  #defining the test statistic
  output$mean_test_stat = renderUI({
    mean_df = mean_df()
    samp1 = filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[1])[[input$mean_num_var]]
    samp2 = filter(mean_df, .data[[input$mean_cat_var]] == unique(mean_df[[input$mean_cat_var]])[2])[[input$mean_num_var]]
    if (norm_dis() & mean_sd()) {
      withMathJax(sprintf("\\( T = \\frac{{\\bar X} - {\\bar Y}}{S_p \\sqrt{\\frac{1}{n_x} + \\frac{1}{n_y}}} \\), where \\(S^2_p = \\dfrac{(n_x-1) S_{x}^2 + (n_y-1) S_{y}^2}{n_x+n_y-2} \\). Under \\(H_0\\), \\(T \\sim t_{%.0f}\\)", length(samp1) + length(samp2)-2))
    }
    else if (norm_dis() & !mean_sd()) {
      withMathJax(sprintf("\\(T = \\frac{\\bar X-\\bar Y}{\\sqrt{\\frac{S_x^2}{n_x}+\\frac{S_y^2}{n_y}}}\\), where \\(S_x^2\\) and \\(S_y^2\\) are the sample variance of the \\(X\\) and \\(Y\\) samples, respectively. Under \\(H_0\\), \\(T\\sim t_%.0f\\) approximately", length(samp1) + length(samp2)-2))
    }
    else {
      withMathJax(sprintf("\\(W = R_1 + R_2 + \\ldots + R_{n_A}\\) (the sum of the ranks of observations in group 1). Under \\(H_0\\), \\(W\\) follows the \\(WRS(%.0f, %.0f)\\) distribution.", length(samp1), length(samp2)))
    }
  })
  
  #defining the heading of the observed test statistic
  output$mean_head_t = renderUI({
    HTML(paste("<b>4. Observed test statistic</b>: "))
  })
  
  #defining the observed test statistic
  output$mean_ov_test_stat = renderUI ({
    mean_test = mean_test()
    withMathJax(sprintf("\\(t_0 \\) = %.02f",mean_test$statistic))
  })
  
  #defining the heading of the p-value
  output$mean_head_p = renderUI({
    HTML(paste("<b>5. p-value</b>: "))
  })
  
  
  #defining the p-value
  output$mean_pval = renderUI ({
    mean_test = mean_test()
    withMathJax(sprintf("\\(p = %.04f\\) ", mean_test$p.value))
  })
  
  #defining the test performed
  output$mean_test = renderPrint({
    print(mean_test())
  })
  
  #defining the decision made with the test for mean
  mean_decision = reactive({
    mean_test = mean_test()
    sig_lv = input$mean_sig_lv
    pval = mean_test$p.value
    if (pval > sig_lv) {
      return (TRUE)
    }
    return (FALSE)
  })
  
  #defining the conclusion of the test for mean
  output$mean_conclusion = renderUI ({
    mean_df = mean_df()
    if (mean_decision()) {
      HTML(sprintf("<b>6. Decision</b>: Since the p-value is greater than <b>%.02f</b> <i>(which is the chosen significant level)</i>, we would <b>retain the null hypothesis</b>. It does not appear to be evident that %s differ in groups of DATA2X02 students categorized by their response to the question about %s.", input$mean_sig_lv, input$mean_num_var, input$mean_cat_var))
    }
    else {
      HTML(sprintf("<b>6. Decision</b>: Since the p-value is less than <b>%.02f</b> <i>(which is the chosen significant level)</i>, we would <b>reject the null hypothesis</b>. It appears to be evident that %s in groups of student answering %s when being asked about %s is %s than that of their counterparts", input$mean_sig_lv, input$mean_num_var, unique(mean_df[[input$mean_cat_var]])[1], input$mean_cat_var, input$mean_h1))
    }
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)
