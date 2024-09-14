# Title: GET Cities Analysis
# Author: Alexander Zakrzeski
# Date: September 14, 2024

# Part 1: Setup and Configuration

# Load to import, clean, and wrangle data
library(dplyr)
library(forcats)
library(lubridate)
library(purrr)
library(readr) 
library(stringr)
library(tibble)
library(tidyr)

# Load to visualize data
library(ggplot2)
library(gt)
library(scales)

# Load to get marginal effects
library(margins) 

# Load for natural language processing
library(textstem)
library(tidytext)
library(vader)

# Define a function to customize the theme of a horizontal bar chart
theme_custom_horizontal <- function(margin_size) {   
  # Create an empty theme  
  empty <- theme_void() 
  
  # Add the various styling elements to the theme 
  custom <- empty + theme(  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  
    text = element_text(family = "Roboto"), 
    plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                              size = 17, face = "bold"),
    panel.grid.major.x = element_line(linetype = 3, linewidth = 0.3, 
                                      color = "#808080"), 
    axis.text.x = element_text(size = 15, color = "#000000"),
    axis.text.y = element_text(margin = margin(0, margin_size, 0, 0), size = 15,
                               color = "#000000", hjust = 1), 
    legend.position = "top", 
    legend.key.size = unit(0.55, "cm"), 
    legend.text = element_text(size = 15), 
    legend.spacing.x = unit(0.25, "cm"),
    legend.margin = margin(0, 0, 12.5, 0)   
    ) 
  
  # Return the custom theme 
  return(custom)  
}

# Define a function to customize the theme of a vertical bar chart
theme_custom_vertical <- function() { 
  # Create an empty theme  
  empty <- theme_void() 
  
  # Add the various styling elements to the theme 
  custom <- empty + theme( 
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
    text = element_text(family = "Roboto"),
    plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                              size = 17, face = "bold"), 
    panel.grid.major.y = element_line(linetype = 3, linewidth = 0.3, 
                                      color = "#808080"),  
    axis.text.x = element_text(margin = margin(-2.5, 0, 0, 0), size = 15, 
                               color = "#000000"), 
    axis.text.y = element_text(margin = margin(0, 5, 0, 0), size = 15, 
                               color = "#000000"), 
    legend.position = "top", 
    legend.key.size = unit(0.55, "cm"), 
    legend.text = element_text(size = 15), 
    legend.spacing.x = unit(0.25, "cm"),
    legend.margin = margin(0, 0, 10, 0) 
    )
  
  # Return the custom theme 
  return(custom)  
}

# Part 2: Data Preprocessing

# Load the data from the CSV file and convert to a tibble
df_quant_guide <- read_csv("GET-Cities-Quantitative-Data-Guide.csv") |> 
  as_tibble() |>
  # Rename columns, modify values in the columns, and filter appropriately
  rename_with(str_to_lower) |>
  mutate(question = str_squish(question), 
         id = str_to_lower(id)) |>
  filter(id %in% c("sq2", "sq4", "sq5", "sq11", "sq13", "sq16", "sq23", "sq24",
                   "sq25", "sq29", "sq33", "sq35", "sq37", "sq40")) 

# Load the data from the CSV file
df_quant <- read_csv("GET-Cities-Quantitative-Data.csv") |>  
  # Rename columns, filter, select the columns, and sort the rows 
  rename_with(str_to_lower) |> 
  filter(sq3 == "Woman" & sq9 == "Work full-time") |>
  select(df_quant_guide |> pull(id)) |>
  arrange(sq2) |>
  # Modify values in various existing columns 
  mutate(sq2 = ymd(str_sub(sq2, 1, 10)), 
         sq4 = case_when(  
           str_detect(sq4, "Biracial|Native American") ~ "Other",
           sq4 %in% c("Middle Eastern", "north african", "White or Caucasian", 
                      "White, but I am an immigrant") ~ "White", 
           sq4 == "Asian or Pacific Islander" ~ "Asian", 
           sq4 == "Black or African American" ~ "Black", 
           sq4 == "Hispanic or Latino" ~ "Hispanic", 
           sq4 == "Prefer not to reply" ~ NA_character_  
           ), 
         sq5 = str_extract(sq5, "(?<=\\().*?(?=\\))"),
         sq11 = case_when(  
           str_detect(sq11, "(?i)post-graduate degree") ~ "Advanced Degree", 
           !str_detect(sq11, "(?i)post-graduate degree") & 
           str_detect(sq11, "Bachelor’s degree") ~ "Undergraduate Degree", 
           .default = "Associate Degree or Less"  
           ), 
         sq13 = if_else(   
           sq13 %in% c("Data Analyst", "Graphic Designer", "IT Project Manager", 
                       "Product Manager", "Software Developer"), 
           sq13, "Other" 
           ), 
         sq16 = case_when(  
           str_detect(sq16, "stay") ~ "Plan to Stay",  
           str_detect(sq16, "exit") ~ "Plan to Leave", 
           .default = sq16 
           ), 
         sq24 = case_when(  
           str_detect(sq24, "11-20 years|Senior-level") ~ "Senior-Level",
           sq24 %in% c("3-5 years", "4 years, between entry and intermediate", 
                       "Intermediate level - 5-10 years") ~ "Mid-Level", 
           sq24 == "Entry-level - 1-3 years" ~ "Entry-Level", 
           .default = sq24  
           ), 
         across(c(sq25, sq29, sq33), ~ if_else(     
           .x == "Neutral / Neither agree nor disagree", "Neutral", .x    
           )),    
         sq35 = case_when( 
           str_detect(sq35, str_remove_all("(?i)better|cost|experience| 
                                                inflation|market|predetermined",  
                                           "\\s+")) ~ "Other", 
           str_detect(sq35, "Switching") ~ "Switching Companies", 
           sq35 == "A bonus" ~ "Bonus", 
           sq35 == "A promotion" ~ "Promotion",
           sq35 == "A raise" ~ "Raise",
           .default = NA_character_  
           ),   
         sq37 = case_when(   
           sq37 %in% c("41-50", "51-60", "61-70", "70+") ~ "More than 40 Hours", 
           is.na(sq37) ~ NA_character_, 
           .default = "40 Hours or Less" 
           ),  
         sq40 = case_when(  
           str_detect(sq40, "hybrid|remote day allowed") ~ "Hybrid", 
           str_detect(sq40, "remotely") ~ "Remote", 
           sq40 == "We all work in-person" ~ "In-Person", 
           .default = NA_character_  
           )) 

# Load the data from the CSV file
df_qual <- read_csv("GET-Cities-Qualitative-Data.csv") |>  
  # Filter appropriately, rename columns, and select columns
  filter(!if_all(everything(), is.na)) |> 
  rename_with(~ c("age", "experience"), .cols = c(2, 4)) |>
  select(age, experience) |>
  # Create new columns and change the position of a new column
  mutate(under_thirty = if_else( 
    age < 30, "Younger than 30", "30 and Older"   
    ),  
    score = vader_df(experience) |> pull(compound)) |>
  relocate(under_thirty, .after = age) 

# Remove objects from global environment
rm(incl_nt, neu_set) 

# Part 3: Demographics and Socioeconomics

# Drop rows, get proportions, and set factor levels
demographics <- df_quant |> 
  drop_na(sq4) |>
  count(sq4, sq11) |>
  group_by(sq4) |>
  mutate(prop = n / sum(n),
         sq4 = factor(sq4, levels = c("Other", "Hispanic", "White", "Black", 
                                      "Asian")),   
         sq11 = factor(sq11, levels = c("Advanced Degree", 
                                        "Undergraduate Degree", 
                                        "Associate Degree or Less"))) |> 
  ungroup() |>
  select(-n) 

# Create a stacked bar chart to display proportions
ggplot(demographics, aes(x = sq4, y = prop, fill = sq11)) +   
  geom_col(width = 0.8, position = "stack") +
  geom_text(aes(label = percent(prop, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 5, 
            color = "#ffffff") + 
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") + 
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_fill_manual(values = c("#005288", "#0078ae", "#828284")) +
  labs(title = "Figure 1: Technologists by Ethnicity and Education Level", 
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) + 
  coord_flip() + 
  theme_custom_horizontal(margin_size = -17.5) 

# Generate counts, calculate percentages, and sort in descending order
count(df_quant, sq5) |> mutate(pct = percent(n / sum(n))) |> arrange(desc(n))
count(df_quant, sq13) |> mutate(pct = percent(n / sum(n))) |> arrange(desc(n))

# Part 4: Experiences

# Iteratively tokenize text into n-grams with specified size "number"
experiences <- map_df(1:3, function(number) { 
  processed <- df_qual |> 
    unnest_tokens(ngram, experience, token = "ngrams", n = number) |>
    drop_na(ngram) 
  
  # Additional processing for unigrams 
  if (number == 1) {  
    processed <- processed |>
      filter(!str_detect(ngram, "^[0-9]+$")) |>
      anti_join(stop_words, by = c("ngram" = "word")) |> 
      mutate(ngram = lemmatize_words(ngram))  
  }
  
  # Add a column to indicate whether it is a unigram, bigram, or trigram
  processed <- processed |>
    mutate(type = case_when( 
      number == 1 ~ "unigram", 
      number == 2 ~ "bigram",  
      number == 3 ~ "trigram"    
      )) |> 
    relocate(ngram, .after = type) 
}) |>
  # Filter appropriately and modify values in the column
  filter(ngram %in% c("challenge", "discriminate", "discrimination", "dominate",
                      "gender", "hard", "learn", "opportunity", "positive", 
                      "respect", "team")) |> 
  mutate(ngram = if_else( 
    ngram == "discriminate", "discrimination", ngram  
    ) |>                     
                 str_to_title()) |>
  # Generate term frequencies
  count(under_thirty, ngram)

# Create a stacked bar chart to display term frequencies
ggplot(experiences, aes(x = reorder(ngram, n), y = n, fill = under_thirty)) +
  geom_col(width = 0.825) + 
  geom_text(aes(label = after_stat(y), group = ngram), stat = "summary", 
            fun = "sum", vjust = 0.25, hjust = -0.25, size = 5) +
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") + 
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 14)) + 
  scale_fill_manual(values = c("#005288", "#3d98c1")) +
  labs(title = str_squish("Figure 2: Term Frequencies from the Work Experiences 
                           of Technologists"), 
       x = "", y = "") +  
  guides(fill = guide_legend(title = "", reverse = TRUE)) + 
  coord_flip() + 
  theme_custom_horizontal(margin_size = -17.5) 

# Create a faceted histogram to display the distributions
ggplot(df_qual, aes(x = score, fill = under_thirty)) +  
  geom_histogram(bins = 18, fill = "#3d98c1", color = "#000000") + 
  geom_hline(yintercept = 0, linewidth = 1.15, color = "#000000") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5), 
                     labels = label_number(drop0trailing = TRUE)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 6)) +
  labs(title = str_squish("Figure 3: Distribution of Compound Sentiment Scores 
                           by Age Group"),  
       x = "Compound Sentiment Score", y = "Frequency") + 
  facet_grid(~ factor(under_thirty, 
                      levels = c("Younger than 30", "30 and Older"))) +
  theme_void() + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        panel.spacing.x = unit(2.25, "lines"), 
        text = element_text(family = "Roboto"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 17, face = "bold"), 
        strip.text = element_text(margin = margin(0, 0, 10, 0), size = 15),
        panel.grid.major.y = element_line(linetype = 3, linewidth = 0.3, 
                                          color = "#808080"), 
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 15),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, 
                                    size = 15),
        axis.text.x = element_text(margin = margin(-2.5, 0, 0, 0), size = 14, 
                                   color = "#000000"), 
        axis.text.y = element_text(margin = margin(0, 5, 0, 0), size = 14, 
                                   color = "#000000")) 

# Perform a two-sample two-tailed t-test
t.test(df_qual |> filter(under_thirty == "Younger than 30") |> pull(score), 
       df_qual |> filter(under_thirty == "30 and Older") |> pull(score)) 

# Generate the Pearson correlation coefficient
cor(df_qual$age, df_qual$score)

# Create a scatter plot to visualize the relationship between the variables
ggplot(df_qual, aes(x = age, y = score)) +
  geom_point(color = "#0078ae", size = 2.25) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.15, color = "black") +
  geom_hline(yintercept = -1, linewidth = 1.15, color = "black") +
  scale_x_continuous(limits = c(18, 68), breaks = seq(18, 68, by = 16)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5), 
                     labels = label_number(drop0trailing = TRUE)) +
  labs(title = str_squish("Figure 4: Relationship between Compound Sentiment 
                           Score and Age"),  
       x = "Age (years)", y = "Compound Sentiment Score") + 
  theme_void() + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        text = element_text(family = "Roboto"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 17, face = "bold"), 
        panel.grid.major = element_line(linetype = 3, linewidth = 0.3, 
                                        color = "#808080"), 
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 15),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, 
                                    size = 15),
        axis.text.x = element_text(margin = margin(-2.5, 0, 0, 0), size = 14, 
                                   color = "#000000"), 
        axis.text.y = element_text(margin = margin(0, 5, 0, 0), size = 14, 
                                   color = "#000000")) 

# Part 5: Advancement, Culture, and Compensation

# Define a function to process and plot the results of a Likert scale question
plot_likert <- function(column1, column2, value1) { 
  # Drop rows, get proportions, and set factor levels
  processed <- df_quant |> 
    drop_na(all_of(c(column1, column2))) |>
    count(!!sym(column1), !!sym(column2)) |>
    group_by(!!sym(column1)) |>
    mutate(prop = n / sum(n),
           pct = if_else(      
             prop >= 0.05, percent(prop, accuracy = 1), "" 
             ), 
           !!sym(column2) := factor(!!sym(column2), 
                                    levels = c("Strongly Agree", "Agree", 
                                               "Neutral", "Disagree", 
                                               "Strongly Disagree")))  
  
  # Dynamically set factor levels of columns
  if (column1 == "sq11") { 
    processed <- processed |> 
      mutate(!!sym(column1) := factor(!!sym(column1), 
                                      levels = c("Advanced Degree", 
                                                 "Undergraduate Degree", 
                                                 "Associate Degree or Less")))  
  } else if (column1 == "sq24") {  
    processed <- processed |> 
      mutate(!!sym(column1) := factor(!!sym(column1), 
                                      levels = c("Senior-Level", "Mid-Level", 
                                                 "Entry-Level"))) 
  } else if (column1 == "sq40") { 
    processed <- processed |> 
      mutate(!!sym(column1) := factor(!!sym(column1), levels = c("In-Person", 
                                                                 "Hybrid", 
                                                                 "Remote"))) 
  } 
  
  # Ungroup by the column and drop a column
  processed <- processed |> 
    ungroup() |> 
    select(-n) 
  
  # Create a stacked bar chart to display proportions 
  plot <- ggplot(processed, aes(x = !!sym(column1), y = prop, 
                                fill = !!sym(column2))) +   
    geom_col(width = 0.65, position = "stack") +
    geom_text(aes(label = pct), position = position_stack(vjust = 0.5), 
              size = 5, color = "#ffffff") + 
    geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") + 
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    scale_fill_manual(values = c("#005288", "#0078ae", "#828284", "#ffac1c", 
                                 "#c41230")) +  
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "", reverse = TRUE)) +
    coord_flip() 
  
  # Dynamically set the title and theme of the plot 
  if (column2 == "sq25") { 
    plot <- plot + 
      ggtitle(label = str_squish("Figure 5: Satisfaction with Available Career 
                                  Advancement Opportunities")) + 
      theme_custom_horizontal(margin_size = -12.5)
  } else if (column2 == "sq29") {  
    plot <- plot + 
      ggtitle(label = "Figure 6: Satisfaction with Current Workplace Culture") + 
      theme_custom_horizontal(margin_size = -17.5)
  } else if (column2 == "sq33") { 
    plot <- plot + 
      ggtitle(label = "Figure 7: Satisfaction with Overall Compensation") + 
      theme_custom_horizontal(margin_size = -15)
  }  
  
  # Return the plot 
  return(plot) 
}

# Output the stacked bar charts to display the proportions
figure3 <- plot_likert("sq11", "sq25")
figure4 <- plot_likert("sq40", "sq29")
figure5 <- plot_likert("sq24", "sq33")

# Drop rows and filter based on the set condition
compensation <- df_quant |>  
  drop_na(sq24, sq35) |>
  filter(sq35 != "Other") |>
  # Get proportions and set factor levels
  count(sq24, sq35) |>
  group_by(sq24) |>
  mutate(prop = round(n / sum(n), 2), 
         sq35 = factor(sq35, levels = c("Promotion", "Bonus", 
                                        "Switching Companies", "Raise"))) |>   
  ungroup() |>
  select(-n)

# Create a grouped bar chart to display proportions
ggplot(compensation, aes(x = sq24, y = prop, fill = sq35)) +  
  geom_col(width = 0.85, position = "dodge") + 
  geom_text(aes(label = percent(prop)), position = position_dodge(0.85), 
            vjust = -1, hjust = 0.5, size = 5.5) +
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.15), 
                     labels = percent) + 
  scale_fill_manual(values = c("#5e9732", "#ffac1c", "#c41230", "#0078ae")) + 
  labs(title = "Figure 8: Most Recent Source of Compensation Increase at Work", 
       x = "", y = "") +
  guides(fill = guide_legend(title = "")) +
  theme_custom_vertical()

# Drop rows and get proportions
work_hours <- df_quant |>
  drop_na(sq24, sq37) |>
  count(sq24, sq37) |>
  group_by(sq24) |>
  mutate(prop = round(n / sum(n), 2)) |>   
  ungroup() |>
  select(-n) 

# Create a grouped bar chart to display proportions
ggplot(work_hours, aes(x = sq24, y = prop, fill = sq37)) + 
  geom_col(width = 0.5, position = "dodge") + 
  geom_text(aes(label = percent(prop)), position = position_dodge(0.5), 
            vjust = -1, hjust = 0.5, size = 5.7) +
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.25), 
                     labels = percent) + 
  scale_fill_manual(values = c("#3d98c1", "#005288")) + 
  labs(title = "Figure 9: Perceived Required Hours to Succeed in Current Role", 
       x = "", y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_custom_vertical() 

# Part 6: Retention

# Select the necessary columns and drop rows
retention1 <- df_quant |>
  select(sq4, sq11, sq16, sq24, sq25, sq33) |>
  drop_na() |>
  # Modify values in existing columns and set factor levels
  mutate(sq4 = if_else( 
    sq4 != "White", "Non-White", "White"   
    ),  
    sq11 = fct_relevel(sq11, "Undergraduate Degree"),
    sq16 = if_else( 
      sq16 == "Plan to Stay", 1, 0 
      ), 
    across(c(sq25, sq33), ~ case_when( 
      str_detect(.x, "Agree") ~ str_glue("{cur_column()}_Satisfied"),  
      str_detect(.x, "Disagree") ~ str_glue("{cur_column()}_Dissatisfied"), 
      .x == "Neutral" ~ str_glue("{cur_column()}_Neutral")     
      ))) 

# Perform chi-square tests
chisq.test(retention1$sq4, retention1$sq16) 
chisq.test(retention1$sq11, retention1$sq16)
chisq.test(retention1$sq24, retention1$sq16)
chisq.test(retention1$sq25, retention1$sq16)
chisq.test(retention1$sq33, retention1$sq16)

# Run the multivariate logistic regression model
retention_lr <- glm(sq16 ~ sq4 + sq11 + sq24 + sq25 + sq33, family = binomial, 
                    data = retention1)

# Confirm no multicollinearity among predictors
car::vif(retention_lr)

# Generate the summary of the logistic regression model
retention_table <- as_tibble(margins_summary(retention_lr)) |> 
  # Modify values in the columns and create new columns
  mutate(across(c(AME, lower, upper), ~ if_else(    
    abs(.x) >= 0.005, as.character(round(.x, 2)), as.character(round(.x, 3))    
    )),   
    `p-value` = case_when(    
      p < 0.001 ~ "<0.001", 
      abs(p) >= 0.005 ~ as.character(round(p, 2)), 
      .default = as.character(round(p, 3))   
      ),   
    factor = str_remove(factor, "^sq\\d+")) |>    
  unite(`95% CI`, c(lower, upper), sep = ", ") |> 
  # Drop columns, rename a column, and add rows
  select(-c(SE, z, p)) |>
  rename(Variable = factor) |>
  bind_rows(tibble(Variable = c("Education Level", "Undergraduate Degree", 
                                "Career Level", "Entry-Level", 
                                "Career Advancement", "sq25_Dissatisfied", 
                                "Compensation", "sq33_Dissatisfied", 
                                "Ethnicity", "Non-White"), 
                   AME = c("", "—", "", "—", "", "—", "", "—", "", "—"), 
                   `95% CI` = c("", "—", "", "—", "", "—", "", "—", "", "—"), 
                   `p-value` = c("", "", "", "", "", "", "", "", "", ""))) |> 
  # Change the order of the rows and modify values of a column
  slice(18, 19, 9, 10, 11, 2, 1, 12, 13, 3, 4, 14, 15, 5, 6, 16, 17, 7, 8) |>
  mutate(Variable = if_else(
    !Variable %in% c("Ethnicity", "Education Level", "Career Level", 
                     "Career Advancement", "Compensation"), 
    paste0("\u00A0\u00A0\u00A0\u00A0", Variable), Variable 
    ) |>            
                    str_remove("sq\\d+_"), 
    AME = if_else( 
      AME == "0.1", "0.10", AME 
      ))

# Create a table to display the outputs from the regression model
gt(retention_table) |>
  tab_header(title = md("**Table 1: Results of the Multivariate Logistic 
                           Regression Model**")) |>
  cols_align(align = "center", columns = -Variable) |>
  cols_label(`AME` = "Average Marginal Effect", 
             `95% CI` = "95% Confidence Interval") |>
  tab_options(table.width = "95%", table.font.names = "Roboto", 
              table.font.size = px(17)) |>
  opt_stylize(style = 6)

# Drop rows, split values in a column, get counts, and drop a column 
retention2 <- df_quant |> 
  drop_na(sq23) |>
  separate_longer_delim(sq23, delim = ", ") |>
  add_count(sq23) |>
  mutate(sq23 = if_else( 
    n == 1, "Other", sq23   
    )) |>
  select(-n) |>
  count(sq23) |>
  # Create new columns and modify the values in an existing column
  mutate(prop = round(n / sum(n), 2), 
         label = if_else( 
           prop < 0.11, "10% or Less", "Greater than 10%"
           ), 
         sq23 = case_when(  
           str_detect(sq23, "people in my gender") ~ "Gender Representation", 
           str_detect(sq23, "people in my race") ~ "Racial Representation",
           sq23 == "Advancement in your role" ~ "Career Advancement", 
           sq23 == "Equitable opportunities for parents" ~ "Parental Equity", 
           sq23 == "Management support" ~ "Management Support", 
           sq23 == "Inclusion at your workplace" ~ "Workplace Inclusion",  
           .default = sq23 
           ))

# Create a stacked bar chart to display proportions
ggplot(retention2, aes(x = reorder(sq23, prop), y = prop, fill = label)) +  
  geom_col(width = 0.825) + 
  geom_text(aes(label = percent(prop)), vjust = 0.25, hjust = -0.25, size = 5) +
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") + 
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, by = 0.08), 
                     labels = percent) + 
  scale_fill_manual(values = c("#828284", "#005288")) +
  labs(title = str_squish("Figure 10: Percentage of Factors Influencing Staying 
                           in Technology"),  
       x = "", y = "") +  
  guides(fill = guide_legend(title = "")) + 
  coord_flip() + 
  theme_custom_horizontal(margin_size = -15)