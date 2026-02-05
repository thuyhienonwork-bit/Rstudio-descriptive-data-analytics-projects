install.packages("readxl")
install.packages("ggplot2")
install.packages("VIM")
install.packages("glm2")
install.packages("mice")
install.packages("dplyr")
install.packages("MASS")
install.packages("moments")
install.packages("ggcorrplot")
install.packages("openxlsx")
install.packages("ggrepel")

library(readxl)
library(ggplot2)
library(VIM)
library(glm2)
library(mice)
library(dplyr)
library(MASS)
library(moments)
library(ggcorrplot)
library(stringr)
library(broom)
library(purrr)
library(ggrepel)

library(readxl)
setwd("~/Downloads")
Marketing_Campaign_Dataset_A1_2_ <- read_excel("Marketing Campaign Dataset_A1 (2).xlsx")

#1. Adjust data columns
##ADJUST VALUE
Marketing_Campaign_Dataset_A1_2_ <- Marketing_Campaign_Dataset_A1_2_ %>%
  mutate(
    ##Create Gender Column
    Gender = case_when(
      Target_Audience == "All Ages" ~ "Both Gender",
      grepl("^Men", Target_Audience) ~ "Men",
      grepl("^Women", Target_Audience) ~ "Women",
      TRUE ~ NA_character_
    ),
    
    ##Create Age_Group
    Age_Group = case_when(
      Target_Audience == "All Ages" ~ "All Ages",
      TRUE ~ sub("^(Men|Women)\\s+","", Target_Audience)
    ),
    ## Categorical convert to factor
    Company          = as.factor(Company),
    Campaign_Type    = as.factor(Campaign_Type),
    Channel_Used     = as.factor(Channel_Used),
    Location         = as.factor(Location),
    Language         = as.factor(Language),
    Customer_Segment = as.factor(Customer_Segment),
    Gender           = as.factor(Gender),
    Age_Group        = as.factor(Age_Group),
    
    ## Duration
    Duration_days = as.numeric(str_remove(Duration, " days")),
    
    ## Acquisition_Cost: "$12,724.00" -> 12724
    Acquisition_Cost_num = Acquisition_Cost %>%
      str_remove_all("[$,]") %>%
      as.numeric()
  )

##Erase columns
Marketing_Campaign_Dataset_A1_2_$Duration <- NULL
Marketing_Campaign_Dataset_A1_2_$Acquisition_Cost <- NULL

##2. Data imputation
## 2.1. Data type checking: MAR vs MCAR
## 2.1.1.SAVE THE ORIGINAL DATA AFTER CLEANING TO COMPARE MEAN/MEDIAN
data_raw <- Marketing_Campaign_Dataset_A1_2_

##DATA MISSING CHECKING
colSums(is.na(Marketing_Campaign_Dataset_A1_2_))
sum(is.na(Marketing_Campaign_Dataset_A1_2_))
na_percent <- colSums(is.na(Marketing_Campaign_Dataset_A1_2_)) / nrow(Marketing_Campaign_Dataset_A1_2_) * 100
na_percent

##CHECKING DATA STRUCTURE
str(Marketing_Campaign_Dataset_A1_2_)

## 2.1.2. MISSING MECHANISM CHECK: MCAR HAY MAR
## 2.2.2.1. CALCULATE MISSING RATE FOR EACH VARIABLE
missing_percent <- sapply(Marketing_Campaign_Dataset_A1_2_,
                          function(x) mean(is.na(x)) * 100)

missing_table <- data.frame(
  variable        = names(missing_percent),
  missing_percent = missing_percent,
  row.names       = NULL
)

missing_table <- missing_table[order(-missing_table$missing_percent), ]

cat("===== Missing rate (%) for each variables =====\n")
# print(missing_table)

vars_with_missing <- missing_table$variable[missing_table$missing_percent > 0]

cat("\nVariables have missing values:\n")
# print(vars_with_missing)

## 2.1.2.2. PREPARING PREDICTOR FOR MAR
predictor_vars <- c("Gender",
                    "Age_Group",
                    "Channel_Used",
                    "Customer_Segment",
                    "Location",
                    "Language")

# Filter out predictors that are actually in the dataset
predictor_vars <- predictor_vars[predictor_vars %in% names(Marketing_Campaign_Dataset_A1_2_)]

cat("\nPredictors dùng để kiểm tra MAR:\n")
# print(predictor_vars)

## 2.1.2.3. LOGISTIC REGRESSION FOR EACH MISSING VARIABLE
results_list <- list()
idx <- 1

for (var_name in vars_with_missing) {
  
  cat("\n----- DATA PROCESSING:", var_name, "-----\n")
  
  #Create missingness variable
  tmp <- Marketing_Campaign_Dataset_A1_2_ %>%
    mutate(miss = ifelse(is.na(.data[[var_name]]), 1, 0))
  
  # Select the missing indicator and predictors safely.
  keep_cols <- c("miss", predictor_vars)
  keep_cols <- keep_cols[keep_cols %in% names(tmp)]
  
  tmp <- tmp[, keep_cols]
  
  # Remove NA values from predictors before running the logistic regression.
  tmp <- na.omit(tmp)
  
  # If there are not enough 0/1 cases to fit the model, skip it.
  if (nrow(tmp) == 0 || length(unique(tmp$miss)) < 2) {
    cat("Biến", var_name, ": không đủ dữ liệu để fit logistic (toàn 0 hoặc toàn 1), bỏ qua.\n")
    next
  }
  
  # Fit logistic model
  formula_string <- paste("miss ~", paste(predictor_vars, collapse = " + "))
  fit <- glm(as.formula(formula_string), data = tmp, family = binomial)
  
  summ <- summary(fit)$coef
  
  res_df <- data.frame(
    target_var = var_name,
    term       = rownames(summ),
    estimate   = summ[, "Estimate"],
    p.value    = summ[, "Pr(>|z|)"],
    row.names  = NULL
  )
  
  results_list[[idx]] <- res_df
  idx <- idx + 1
}

if (length(results_list) == 0) {
  cat("\n>>> No variables run logistic regressionn.\n")
} else {
  
  logit_results <- do.call(rbind, results_list)
  
  cat("\n===== LOGISTIC REGRESSION RESULTS (ENTIRE) =====\n")
  # print(logit_results)
  
  # Filter predictor with p < 0.05
  significant_missing <- subset(logit_results,
                                term != "(Intercept)" & p.value < 0.05)
  
  cat("\n===== PREDICTORS p < 0.05 (MAR SUGGESTION) =====\n")
  if (nrow(significant_missing) == 0) {
    cat(">>> No predictor has p < 0.05 → missing near MCAR.\n")
  } else {
    # print(significant_missing)
  }
}

## 2.2. PMM (Predictive Mean Matching)
library(mice)

## 2.2.0. Store the original dataset for comparison
data_raw <- Marketing_Campaign_Dataset_A1_2_

## 2.2.1. Create the initial mice object to extract default methods and predictor matrix
ini  <- mice(data_raw, maxit = 0)
meth <- ini$method
pred <- ini$predictorMatrix

## 2.2.2. Automatically identify all numeric variables that contain missing values
num_with_NA <- names(data_raw)[
  sapply(data_raw, is.numeric) & colSums(is.na(data_raw)) > 0
]

## Print the numeric variables selected for PMM
cat("Numeric variables imputed using PMM:\n")
# print(num_with_NA)

## 2.2.3. Assign the PMM method to those numeric variables
meth[num_with_NA] <- "pmm"

## 2.2.4. Run MICE with PMM
set.seed(123)
imp <- mice(data_raw,
            method = meth,
            predictorMatrix = pred,
            m = 5,
            maxit = 20,
            printFlag = TRUE)

## 3. Extract one fully imputed dataset for analysis
final_data <- complete(imp, 1)

## 4. Check remaining missing values
sapply(final_data, function(x) sum(is.na(x)))

## 5. Mean and median comparison (before vs. after)
num_with_NA <- num_with_NA[num_with_NA %in% names(final_data)]

mean_compare <- data.frame(
  Variable    = num_with_NA,
  Mean_before = sapply(num_with_NA, function(v)
    mean(data_raw[[v]], na.rm = TRUE)),
  Mean_after  = sapply(num_with_NA, function(v)
    mean(final_data[[v]], na.rm = TRUE))
)

median_compare <- data.frame(
  Variable      = num_with_NA,
  Median_before = sapply(num_with_NA, function(v)
    median(data_raw[[v]], na.rm = TRUE)),
  Median_after  = sapply(num_with_NA, function(v)
    median(final_data[[v]], na.rm = TRUE))
)

cat("\n===== Compare mean before and after PMM =====\n")
# print(mean_compare)

cat("\n===== Compare median before and after PMM =====\n")
# print(median_compare)

##2.3. Fixing NA
## 2.3.0. Choose the dataset to impute
df <- final_data   

## 2.3.1. Convert ALL possible text-missing to NA (for all types)
missing_strings <- c("NA", "N/A", "n/a", "NULL", "Null", "null",
                     "Missing", "missing", "None", "none", "",
                     " ", "  ")

df <- df %>% mutate(across(everything(), ~ {
  x <- as.character(.x)
  x[x %in% missing_strings] <- NA
  x
}))

## Convert back numeric columns that became character
num_cols <- names(df)[sapply(df, function(x) all(is.na(x) | str_detect(x, "^[0-9.]+$")))]
df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(x))

## 2.3.2. Impute numeric with mean, categorical with mode

## 2.3.1. Mode function for categorical variables
get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) return(NA)
  ux <- unique(x_no_na)
  ux[which.max(tabulate(match(x_no_na, ux)))]
}

## 2.3.2.1. Numeric imputation (mean)
numeric_cols <- names(df)[sapply(df, is.numeric)]

df[numeric_cols] <- lapply(df[numeric_cols], function(col) {
  m <- mean(col, na.rm = TRUE)
  col[is.na(col)] <- m
  col
})

## 2.3.2.2. Categorical imputation (mode)
categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

df[categorical_cols] <- lapply(df[categorical_cols], function(col) {
  m <- get_mode(col)
  col[is.na(col)] <- m
  col
})

## Check remaining missing values
sapply(df, function(x) sum(is.na(x)))

## 2.3.3. Check if all missing values are gone
sapply(df, function(x) sum(is.na(x)))

## 2.3.4. Write to Excel (fully imputed, NO NA left)
library(openxlsx)
write.xlsx(df, "Dataset_Fully_Imputed.xlsx")

## Gán dataset đã impute để dùng các bước sau
Marketing_Campaign_Dataset_A1_2_imp <- df

## ✅ Chỉ View 1 bảng duy nhất
View(Marketing_Campaign_Dataset_A1_2_imp)

### 1. CHECK DUPLICATE RECORDS
## 1.1. Check fully duplicated rows (all columns the same)
dup_flags <- duplicated(Marketing_Campaign_Dataset_A1_2_imp) | duplicated(Marketing_Campaign_Dataset_A1_2_imp, fromLast = TRUE)
duplicates_full <- Marketing_Campaign_Dataset_A1_2_imp[dup_flags, ]
cat("Number of fully duplicated rows:", sum(dup_flags), "\n")

## 1.2. Identify inconcistences
## 1.2.1. Clean basic whitespace and case for character columns
Marketing_Campaign_Dataset_A1_2_imp <- Marketing_Campaign_Dataset_A1_2_imp %>%
  mutate(across(
    .cols = where(is.character),
    .fns  = ~ str_squish(str_trim(.x))   # remove extra spaces
  ))

## 3.Detecting and Handling Outliners
## 3.1. Checking structure of data
sapply(Marketing_Campaign_Dataset_A1_2_imp, is.numeric)

## 3.2. Dectecting numeric outliners
boxplot(Marketing_Campaign_Dataset_A1_2_imp$Conversion_Rate, main = "Boxplot of Conversion_Rate", col = "blue", horizontal = TRUE)
boxplot(Marketing_Campaign_Dataset_A1_2_imp$ROI, main = "Boxplot of ROI", col = "red", horizontal = TRUE)
boxplot(Marketing_Campaign_Dataset_A1_2_imp$Clicks, main = "Boxplot of Clicks", col = "blue", horizontal = TRUE)
boxplot(Marketing_Campaign_Dataset_A1_2_imp$Impressions, main = "Boxplot of Impressions", col = "blue", horizontal = TRUE)
boxplot(Marketing_Campaign_Dataset_A1_2_imp$Engagement_Score, main = "Boxplot of Engagement_Score", col = "red", horizontal = TRUE)
boxplot(Marketing_Campaign_Dataset_A1_2_imp$Duration_days, main = "Boxplot of Duration_days", col = "blue", horizontal = TRUE)


##Task 2: Data Visualization 

# Scenario 1: Tagert Segment Identification
df$Conv_per_1000 <- (df$Conversion_Rate * df$Impressions) / 100

# Summary per segment
seg_summary <- df %>%
  group_by(Customer_Segment) %>%
  summarise(
    avg_conv   = mean(Conversion_Rate, na.rm = TRUE),
    avg_roi    = mean(ROI, na.rm = TRUE),
    avg_cost   = mean(Acquisition_Cost_num, na.rm = TRUE),
    conv1000   = mean(Conv_per_1000, na.rm = TRUE)
  )

# Bubble scatterplot
ggplot(seg_summary,
       aes(x = avg_conv,
           y = avg_roi,
           color = Customer_Segment,
           size = avg_cost)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(name = "Avg Acquisition Cost") +
  labs(
    title = "Conversion vs ROI by Customer Segment",
    x = "Average Conversion Rate",
    y = "Average ROI",
    color = "Customer Segment",
    caption = "Figure 1: Bubble chart showing ROI–Conversion–Cost relationships by segment."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    
    # ⭐ Căn giữa caption + in đậm + size lớn hơn
    plot.caption = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 11,
      margin = margin(t = 8)
    )
  )

# Scenario 2: Localize Strategy (Language & Location & Conversion rate)
df <- Marketing_Campaign_Dataset_A1_2_imp

heat_df <- df %>%
  group_by(Location, Language) %>%
  summarise(avg_conv = mean(Conversion_Rate, na.rm = TRUE), .groups = "drop")

ggplot(heat_df, aes(x = Language, y = Location, fill = avg_conv)) +
  geom_tile(color = "white") +
  
  # hiển thị số với 2 chữ số sau dấu .
  geom_text(aes(label = sprintf("%.3f", avg_conv)),
            color = "white", size = 4, fontface = "bold") +
  
  scale_fill_gradient(low = "#9fd7f9", high = "#1261A6",
                      name = "Avg Conversion") +
  labs(
    title = "Heatmap of Conversion Rate by Location and Language",
    x = "Language",
    y = "Location",
    caption = "Figure 2: Heatmap showing cross-market conversion performance."
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    
    plot.caption = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 11,
      margin = margin(t = 8)
    )
  )


## Scenario 3: Average Engagement by Age Group and Language
df <- Marketing_Campaign_Dataset_A1_2_imp

lang_age_summary <- df %>%
  group_by(Language, Age_Group) %>%
  dplyr::summarise(
    mean_eng  = mean(Engagement_Score, na.rm = TRUE),
    mean_conv = mean(Conversion_Rate,  na.rm = TRUE),
    n         = n()
  ) %>%
  arrange(Language, Age_Group)

# print(lang_age_summary)

# Clustered bar: mean engagement by Age_Group & Language
ggplot(lang_age_summary,
       aes(x = Age_Group,
           y = mean_eng,
           fill = Language)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Average Engagement by Age Group and Language",
    x = "Age Group",
    y = "Average Engagement Score",
    fill = "Language",
    caption = "Test 2 – Age–language interaction on engagement."
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Scenario 4: 
df <- Marketing_Campaign_Dataset_A1_2_imp

# 1. Summary: mean conversion per Language × Gender
lang_gender_summary <- df %>%
  group_by(Language, Gender) %>%
  summarise(
    mean_conv = mean(Conversion_Rate, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# print(lang_gender_summary)

# 2. Line Chart
ggplot(lang_gender_summary,
       aes(x = Language,
           y = mean_conv,
           color = Gender,
           group = Gender)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  scale_color_brewer(palette = "Dark2") +
  
  labs(
    title = "Conversion Rate by Language and Gender",
    x = "Language",
    y = "Average Conversion Rate",
    color = "Gender",
    caption = "Line chart showing how male/female audiences convert across campaign languages."
  ) +
  
  theme_minimal() +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Scenario 5: Conversion and Enagement by Customer Segment and Language

df <- Marketing_Campaign_Dataset_A1_2_imp

## 1. Summary by Language x Customer_Segment
lang_seg_summary <- df %>%
  group_by(Language, Customer_Segment) %>%
  summarise(
    mean_conv = mean(Conversion_Rate,  na.rm = TRUE),
    mean_eng  = mean(Engagement_Score, na.rm = TRUE),
    n         = n(),
    .groups   = "drop"
  )

## 2. Scale Engagement 
scale_factor <- max(lang_seg_summary$mean_eng, na.rm = TRUE) /
  max(lang_seg_summary$mean_conv, na.rm = TRUE)

lang_seg_summary <- lang_seg_summary %>%
  mutate(eng_scaled = mean_eng / scale_factor)

## 3. Bar + Line chart 
ggplot(lang_seg_summary,
       aes(x = Customer_Segment)) +
  
  geom_col(aes(y = mean_conv, fill = Customer_Segment),
           width = 0.6, alpha = 0.9) +
  
  geom_line(aes(y = eng_scaled, group = 1, color = "Average Engagement Score"),
            linewidth = 1.1) +
  
  geom_point(aes(y = eng_scaled, color = "Average Engagement Score"),
             size = 2.5) +
  
  scale_fill_brewer(palette = "Set2", name = "Customer Segment") +
  scale_color_manual(values = c("Average Engagement Score" = "#d95f02"),
                     name = "") +
  
  scale_y_continuous(
    name = "Average Conversion Rate",
    sec.axis = sec_axis(~ ., name = NULL, labels = NULL, breaks = NULL)
  ) +
  
  facet_wrap(~ Language) +
  
  labs(
    title = "Conversion and Engagement by Customer Segment and Language",
    x     = NULL,
    caption = "Figure 5: Conversion and Engagement rate by Customer Segment and Language"
  ) +
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    axis.text.x     = element_blank(),     
    axis.ticks.x    = element_blank(),    
    legend.position = "bottom"
  )

## Export file
library(openxlsx)

write.xlsx(Marketing_Campaign_Dataset_A1_2_imp,
           file = "Marketing_Campaign_Imputed.xlsx")
setwd("~/Downloads")
