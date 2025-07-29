
biogeochem_var <- c("ln_TC", "ln_TN", "ln_LF", "ln_FF", "ln_FRB","ln_SOC", "ln_Ts", "ln_SM")

###########
### Residue
##############
CC_biogeochem_Residue <- list()
Residue_class <- c(Y = "Y", N = "N")

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(Residue_class)) {
    group_name <- paste("CC", var1, var2, sep = "_")
    CC_biogeochem_Residue[[group_name]] <- run_boot_lmer(
      CC %>% filter(Residue == Residue_class[[var2]]),
      var1
    )
  }
}

# Bind and process results
CC_biogeochem_Residue_results <- 
  bind_rows(CC_biogeochem_Residue, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Residue = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  ) 

write.csv(CC_biogeochem_Residue_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/CC_biogeochem_Residue_result.csv", row.names = F)


CC_Residue_SOC <- aov(ln_SOC ~ Residue, data = CC)
summary(CC_Residue_SOC)

CC_Residue_Ts <- aov(ln_Ts ~ Residue, data = CC)
summary(CC_Residue_Ts)

CC_Residue_SM <- aov(ln_SM ~ Residue, data = CC)
summary(CC_Residue_SM)

CC_Residue_LF <- aov(ln_LF ~ Residue, data = CC)
summary(CC_Residue_LF)


###########
### Climate
##############
CC_biogeochem_Climate <- list()

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(CC_climate)) {
    group_name <- paste("CC", var1, var2, sep = "_")
    CC_biogeochem_Climate[[group_name]] <- run_boot_lmer(
      CC %>% filter(Climate_zone == CC_climate[[var2]]),
      var1
    )
  }
}

# Bind and process results
CC_biogeochem_Climate_results <- 
  bind_rows(CC_biogeochem_Climate, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Climate = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  ) 

write.csv(CC_biogeochem_Climate_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/CC_biogeochem_Climate_result.csv", row.names = F)


CC_Climate_TC <- aov(ln_TC ~ Climate_zone, data = CC)
summary(CC_Climate_TC)

CC_Climate_TN <- aov(ln_TN ~ Climate_zone, data = CC)
summary(CC_Climate_TN)
TukeyHSD(CC_Climate_TN)

CC_Climate_LF <- aov(ln_LF ~ Climate_zone, data = CC)
summary(CC_Climate_LF)

CC_Climate_FF <- aov(ln_FF ~ Climate_zone, data = CC)
summary(CC_Climate_FF)
TukeyHSD(CC_Climate_FF)

CC_Climate_FRB <- aov(ln_FRB ~ Climate_zone, data = CC)
summary(CC_Climate_FRB)
TukeyHSD(CC_Climate_FRB)

CC_Climate_SOC <- aov(ln_SOC ~ Climate_zone, data = CC)
summary(CC_Climate_SOC)
TukeyHSD(CC_Climate_SOC)

CC_Climate_Ts <- aov(ln_Ts ~ Climate_zone, data = CC)
summary(CC_Climate_Ts)

CC_Climate_SM <- aov(ln_SM ~ Climate_zone, data = CC)
summary(CC_Climate_SM)
TukeyHSD(CC_Climate_SM)



###########
### Forest
##############

CC_biogeochem_Forest <- list()

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(Forest)) {
    group_name <- paste("CC", var1, var2, sep = "_")
    CC_biogeochem_Forest[[group_name]] <- run_boot_lmer(
      CC %>% filter(Forest_type == Forest[[var2]]),
      var1
    )
  }
}

# Bind and process results
CC_biogeochem_Forest_results <- 
  bind_rows(CC_biogeochem_Forest, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Forest = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  ) 

write.csv(CC_biogeochem_Forest_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/CC_biogeochem_Forest_result.csv", row.names = F)


CC_Forest_TC <- aov(ln_TC ~ Forest_type, data = CC)
summary(CC_Forest_TC)

CC_Forest_TN <- aov(ln_TN ~ Forest_type, data = CC)
summary(CC_Forest_TN)

CC_Forest_LF <- aov(ln_LF ~ Forest_type, data = CC)
summary(CC_Forest_LF)

CC_Forest_SOC <- aov(ln_SOC ~ Forest_type, data = CC)
summary(CC_Forest_SOC)

CC_Forest_Ts <- aov(ln_Ts ~ Forest_type, data = CC)
summary(CC_Forest_Ts)

CC_Forest_SM <- aov(ln_SM ~ Forest_type, data = CC)
summary(CC_Forest_SM)



##############
# Severity
#############
CC_biogeochem_Severity <- list() 

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(CC_Severity)) {
    group_name <- paste("CC", var1, var2, sep = "_")
    CC_biogeochem_Severity[[group_name]] <- run_boot_lmer(
      CC %>% filter(Severity_level == CC_Severity[[var2]]),
      var1
    )
  }
}

# Bind and process results
CC_biogeochem_Severity_results <- 
  bind_rows(CC_biogeochem_Severity, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(CC_biogeochem_Severity_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/CC_biogeochem_severity_result.csv", row.names = F)

CC_Severity_SOC <- aov(ln_SOC ~ Severity_level, data = CC)
summary(CC_Severity_SOC)

CC_Severity_Ts <- aov(ln_Ts ~ Severity_level, data = CC)
summary(CC_Severity_Ts)
TukeyHSD(CC_Severity_Ts)

CC_Severity_SM <- aov(ln_SM ~ Severity_level, data = CC)
summary(CC_Severity_SM)


##############
# timing
#############
CC_biogeochem_Time <- list() 

CC_time <- CC %>% 
  mutate(Time_since_cat = 
           ifelse(Time_since <= 2, "early", ifelse((Time_since > 2 & Time_since <= 5), "medium", "late")))
time_since_list <- c(early = "early", medium = "medium", late = "late")

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(time_since_list)) {
    group_name <- paste("CC", var1, var2, sep = "_")
    CC_biogeochem_Time[[group_name]] <- run_boot_lmer(
      CC_time %>% filter(Time_since_cat == time_since_list[[var2]]),
      var1
    )
  }
}

# Bind and process results
CC_biogeochem_time_results <- 
  bind_rows(CC_biogeochem_Time, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(CC_biogeochem_time_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/CC_biogeochem_time_result.csv", row.names = F)

CC_time_TC <- aov(ln_TC ~ Time_since_cat, data = CC_time)
summary(CC_time_TC)

CC_time_TN <- aov(ln_TN ~ Time_since_cat, data = CC_time)
summary(CC_time_TN)

CC_time_LF <- aov(ln_LF ~ Time_since_cat, data = CC_time)
summary(CC_time_LF)
TukeyHSD(CC_time_LF)

CC_time_FF <- aov(ln_FF ~ Time_since_cat, data = CC_time)
summary(CC_time_FF)

CC_time_FRB <- aov(ln_FRB ~ Time_since_cat, data = CC_time)
summary(CC_time_FRB)

CC_time_SOC <- aov(ln_SOC ~ Time_since_cat, data = CC_time)
summary(CC_time_SOC)

CC_time_Ts <- aov(ln_Ts ~ Time_since_cat, data = CC_time)
summary(CC_time_Ts)
TukeyHSD(CC_time_Ts)

CC_time_SM <- aov(ln_SM ~ Time_since_cat, data = CC_time)
summary(CC_time_SM)

################
### THINNING ###
################
# Climate

TH_biogeochem_Climate <- list()

# Define severity levels
TH_climate <- c(Temperate = "Temperate", Mediterranean = "Mediterranean", Subtropical = "Subtropical", Toropical = "Tropical")

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(TH_climate)) {
    group_name <- paste("TH", var1, var2, sep = "_")
    TH_biogeochem_Climate[[group_name]] <- run_boot_lmer(
      TH %>% filter(Climate_zone == TH_climate[[var2]]),
      var1
    )
  }
}

# Bind and process results
TH_biogeochem_Climate_results <- 
  bind_rows(TH_biogeochem_Climate, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Climate = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  ) 

write.csv(TH_biogeochem_Climate_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/TH_biogeochem_Climate_result.csv", row.names = F)

TH_Climate_TN <- aov(ln_TN ~ Climate_zone, data = TH)
summary(TH_Climate_TN)

TH_Climate_LF <- aov(ln_LF ~ Climate_zone, data = TH)
summary(TH_Climate_LF)

TH_Climate_FF <- aov(ln_FF ~ Climate_zone, data = TH)
summary(TH_Climate_FF)

TH_Climate_FRB <- aov(ln_FRB ~ Climate_zone, data = TH)
summary(TH_Climate_FRB)
TukeyHSD(TH_Climate_FRB)

TH_Climate_SOC <- aov(ln_SOC ~ Climate_zone, data = TH)
summary(TH_Climate_SOC)
TukeyHSD(TH_Climate_SOC)

TH_Climate_Ts <- aov(ln_Ts ~ Climate_zone, data = TH)
summary(TH_Climate_Ts)
TukeyHSD(TH_Climate_Ts)

TH_Climate_SM <- aov(ln_SM ~ Climate_zone, data = TH)
summary(TH_Climate_SM)


##########
# Severity
#########
TH_biogeochem_Severity <- list() 

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(TH_Severity)) {
    group_name <- paste("TH", var1, var2, sep = "_")
    TH_biogeochem_Severity[[group_name]] <- run_boot_lmer(
      TH %>% filter(Severity_level == TH_Severity[[var2]]),
      var1
    )
  }
}

# Bind and process results
TH_biogeochem_Severity_results <- 
  bind_rows(TH_biogeochem_Severity, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(TH_biogeochem_Severity_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/TH_biogeochem_severity_result.csv", row.names = F)

# ANOVA
TH_Severity_TN <- aov(ln_TN ~ Severity_level, data = TH)
summary(TH_Severity_TN)

TH_Severity_LF <- aov(ln_LF ~ Severity_level, data = TH)
summary(TH_Severity_LF)

TH_Severity_FF <- aov(ln_FF ~ Severity_level, data = TH)
summary(TH_Severity_FF)

TH_Severity_FRB <- aov(ln_FRB ~ Severity_level, data = TH)
summary(TH_Severity_FRB)
TukeyHSD(TH_Severity_FRB)

TH_Severity_SOC <- aov(ln_SOC ~ Severity_level, data = TH)
summary(TH_Severity_SOC)
TukeyHSD(TH_Severity_SOC)

TH_Severity_Ts <- aov(ln_Ts ~ Severity_level, data = TH)
summary(TH_Severity_Ts)

TH_Severity_SM <- aov(ln_SM ~ Severity_level, data = TH)
summary(TH_Severity_SM)
TukeyHSD(TH_Severity_SM)

#########
# Forest type ##
########

TH_biogeochem_Forest <- list()

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(Forest)) {
    group_name <- paste("TH", var1, var2, sep = "_")
    TH_biogeochem_Forest[[group_name]] <- run_boot_lmer(
      TH %>% filter(Forest_type == Forest[[var2]]),
      var1
    )
  }
}

# Bind and process results
TH_biogeochem_Forest_results <- 
  bind_rows(TH_biogeochem_Forest, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(TH_biogeochem_Forest_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/TH_biogeochem_Forest_result.csv", row.names = F)

TH_Forest_LF <- aov(ln_LF ~ Forest_type, data = TH)
summary(TH_Forest_LF)

TH_Forest_FF <- aov(ln_FF ~ Forest_type, data = TH)
summary(TH_Forest_FF)

TH_Forest_FRB <- aov(ln_FRB ~ Forest_type, data = TH)
summary(TH_Forest_FRB)

TH_Forest_SOC <- aov(ln_SOC ~ Forest_type, data = TH)
summary(TH_Forest_SOC)

TH_Forest_Ts <- aov(ln_Ts ~ Forest_type, data = TH)
summary(TH_Forest_Ts)

###########
### Residue
##############
TH_biogeochem_Residue <- list()

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(Residue_class)) {
    group_name <- paste("TH", var1, var2, sep = "_")
    TH_biogeochem_Residue[[group_name]] <- run_boot_lmer(
      TH %>% filter(Residue == Residue_class[[var2]]),
      var1
    )
  }
}

# Bind and process results
TH_biogeochem_Residue_results <- 
  bind_rows(TH_biogeochem_Residue, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(TH_biogeochem_Residue_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/TH_biogeochem_Residue_result.csv", row.names = F)


TH_Residue_FF <- aov(ln_FF ~ Residue, data = TH)
summary(TH_Residue_FF)

TH_Residue_FRB <- aov(ln_FRB ~ Residue, data = TH)
summary(TH_Residue_FRB)

TH_Residue_Ts <- aov(ln_Ts ~ Residue, data = TH)
summary(TH_Residue_Ts)

TH_Residue_SM <- aov(ln_SM ~ Residue, data = TH)
summary(TH_Residue_SM)

##############
# timing
#############
TH_biogeochem_Time <- list() 

TH_time <- TH %>% 
  mutate(Time_since_cat = 
           ifelse(Time_since <= 2, "early", ifelse((Time_since > 2 & Time_since <= 5), "medium", "late")))
time_since_list <- c(early = "early", medium = "medium", late = "late")

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(time_since_list)) {
    group_name <- paste("TH", var1, var2, sep = "_")
    TH_biogeochem_Time[[group_name]] <- run_boot_lmer(
      TH_time %>% filter(Time_since_cat == time_since_list[[var2]]),
      var1
    )
  }
}

# Bind and process results
TH_biogeochem_time_results <- 
  bind_rows(TH_biogeochem_Time, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(TH_biogeochem_time_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/TH_biogeochem_time_result.csv", row.names = F)

TH_time_TN <- aov(ln_TN ~ Time_since_cat, data = TH_time)
summary(TH_time_TN)

TH_time_LF <- aov(ln_LF ~ Time_since_cat, data = TH_time)
summary(TH_time_LF)

TH_time_FF <- aov(ln_FF ~ Time_since_cat, data = TH_time)
summary(TH_time_FF)

TH_time_FRB <- aov(ln_FRB ~ Time_since_cat, data = TH_time)
summary(TH_time_FRB)
TukeyHSD(TH_time_FRB)

TH_time_SOC <- aov(ln_SOC ~ Time_since_cat, data = TH_time)
summary(TH_time_SOC)

TH_time_Ts <- aov(ln_Ts ~ Time_since_cat, data = TH_time)
summary(TH_time_Ts)
TukeyHSD(TH_time_Ts)

TH_time_SM <- aov(ln_SM ~ Time_since_cat, data = TH_time)
summary(TH_time_SM)









##########
## PB
############

# timing

PB_biogeochem_Time <- list() 

PB_time <- PB %>% 
  mutate(Time_since_cat = 
           ifelse(Time_since <= 2, "early", ifelse((Time_since > 2 & Time_since <= 5), "medium", "late")))

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(time_since_list)) {
    group_name <- paste("PB", var1, var2, sep = "_")
    PB_biogeochem_Time[[group_name]] <- run_boot_lmer(
      PB_time %>% filter(Time_since_cat == time_since_list[[var2]]),
      var1
    )
  }
}

# Bind and process results
PB_biogeochem_time_results <- 
  bind_rows(PB_biogeochem_Time, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(PB_biogeochem_time_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/PB_biogeochem_time_result.csv", row.names = F)

PB_time_SM <- aov(ln_SM ~ Time_since_cat, data = PB_time)
summary(PB_time_SM)

###########
### Repeat
##############
PB_biogeochem_Repeat <- list()
Repeat_class <- c(Y = "Y", N = "N")

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(Repeat_class)) {
    group_name <- paste("PB", var1, var2, sep = "_")
    PB_biogeochem_Repeat[[group_name]] <- run_boot_lmer(
      PB %>% filter(Repeat == Repeat_class[[var2]]),
      var1
    )
  }
}

# Bind and process results
PB_biogeochem_Repeat_results <- 
  bind_rows(PB_biogeochem_Repeat, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Severity = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(PB_biogeochem_Repeat_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/PB_biogeochem_Repeat_result.csv", row.names = F)

PB_repeat_TC <- aov(ln_TC ~ Repeat, data = PB)
summary(PB_repeat_TC)

PB_repeat_LF <- aov(ln_LF ~ Repeat, data = PB)
summary(PB_repeat_LF)

PB_repeat_FF <- aov(ln_FF ~ Repeat, data = PB)
summary(PB_repeat_FF)

PB_repeat_FRB <- aov(ln_FRB ~ Repeat, data = PB)
summary(PB_repeat_FRB)

PB_repeat_SOC <- aov(ln_SOC ~ Repeat, data = PB)
summary(PB_repeat_SOC)

PB_repeat_Ts <- aov(ln_Ts ~ Repeat, data = PB)
summary(PB_repeat_Ts)

PB_repeat_SM <- aov(ln_SM ~ Repeat, data = PB)
summary(PB_repeat_SM)


########
# Forest type ##
########

PB_biogeochem_Forest <- list()

# Run boot_lmer for each component and severity level
for (var1 in biogeochem_var) {
  for (var2 in names(Forest)) {
    group_name <- paste("PB", var1, var2, sep = "_")
    PB_biogeochem_Forest[[group_name]] <- run_boot_lmer(
      PB %>% filter(Forest_type == Forest[[var2]]),
      var1
    )
  }
}

# Bind and process results
PB_biogeochem_Forest_results <- 
  bind_rows(PB_biogeochem_Forest, .id = "Group") %>% 
  mutate(
    var = word(Group, 3, sep = "_"),
    Forest = word(Group, -1, sep = "_")
  ) %>% 
  mutate(
    across(Intercept:Intercept_Upper, 
           ~ (exp(1)^(.) - 1) * 100,
           .names = "{.col}_pct_change")
  )

write.csv(PB_biogeochem_Forest_results, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/PB_biogeochem_Forest_result.csv", row.names = F)

PB_Forest_TC <- aov(ln_TC ~ Forest_type, data = PB)
summary(PB_Forest_TC)

PB_Forest_TN <- aov(ln_TN ~ Forest_type, data = PB)
summary(PB_Forest_TN)

PB_Forest_LF <- aov(ln_LF ~ Forest_type, data = PB)
summary(PB_Forest_LF)

PB_Forest_FF <- aov(ln_FF ~ Forest_type, data = PB)
summary(PB_Forest_FF)

PB_Forest_SOC <- aov(ln_SOC ~ Forest_type, data = PB)
summary(PB_Forest_SOC)

PB_Forest_Ts <- aov(ln_Ts ~ Forest_type, data = PB)
summary(PB_Forest_Ts)

PB_Forest_SM <- aov(ln_SM ~ Forest_type, data = PB)
summary(PB_Forest_SM)



# Set the path to the folder containing the CSV files
folder_path <- "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_ind/"  # Change this to your actual folder path

# List only CSV files with "biogeochem" in their filenames
csv_files <- list.files(
  path = folder_path,
  pattern = ".*biogeochem.*\\.csv$",
  full.names = TRUE
)

# Read and merge all matched CSV files into one dataframe
biogeochem_df <- csv_files %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  bind_rows() %>% 
  rename(significant = "X")

#write.csv(biogeochem_df, "G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_biogeochem_summary.csv", row.names = F)
biogeochem_df_v2 <- read.csv("G:/My Drive/Research/Projects/SR_meta_analysis/Data/cat_biogeochem_summary.csv")


plot_cat_biogeochem_summary <- 
  biogeochem_df_v2 %>%
  filter(n_study > 2) %>%
  filter(!is.na(Intercept)) %>% 
  mutate(
    color_group = case_when(
      Intercept_Lower_pct_change > 0 & Intercept_Upper_pct_change > 0 ~ "royalblue",
      Intercept_Lower_pct_change < 0 & Intercept_Upper_pct_change < 0 ~ "red2",
      TRUE ~ "black")
  )

cat_biogeochem_graph <- function(df, title, xmin, xmax){
  df$var2 <- factor(df$var2, levels = rev(unique(df$var2)))  # preserve order in df
  ggplot(df) +
    geom_point(aes(
      x = Intercept_pct_change,
      y = var2,
      color = color_group
    ), size = 5) +
    geom_errorbarh(aes(
      y = var2,
      xmin = Intercept_Lower_pct_change,
      xmax = Intercept_Upper_pct_change,
      color = color_group), 
      height = 0.2, linewidth = 1.5) +
    geom_vline(xintercept = 0, color = "black", size = 1, alpha = 0.4) +
    geom_text(aes(
      x = Intercept_Upper_pct_change + 25,
      y = var2,
      label = paste0(n_obs, " (", n_study, ")")
    ),
    vjust = 0.5, size = 5) +
    geom_text(aes(
      x = Intercept_Upper_pct_change + 7,
      y = var2,
      label = significant
    ),
    vjust = 0.5, size = 5) +
    scale_color_identity(guide = "none") + 
    xlab("") +
    ggtitle(title) +
    theme_classic2() +
    xlim(xmin, xmax) +
    theme(
      axis.title.x = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 18),
      strip.text = element_text(size = 18),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.1),
      plot.title = element_text(size = 19, face = "bold")
    )
}

# Clearcut
CC_TC <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "TC") %>%
  filter(Var %in% c("Climate", "Forest", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Clearcut", -20, 175) 

CC_TN <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "TN") %>%
  filter(Var %in% c("Climate", "Forest", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Clearcut", -30, 220) 

CC_LF <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "LF") %>%
  filter(Var %in% c("Forest", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Clearcut", -100, 100) 


CC_FRB <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "FRB") %>%
  filter(Var %in% c("Recovery")) %>% 
  cat_biogeochem_graph(title = "Clearcut", -100, 100) 

CC_SOC <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "SOC") %>%
  cat_biogeochem_graph(title = "Clearcut", -60, 90)

CC_Ts <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "Ts") %>%
  cat_biogeochem_graph(title = "Clearcut", -10, 110)

CC_SM <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Clearcut", var1 == "SM") %>%
  cat_biogeochem_graph(title = "Clearcut", -50, 100)

# thinning

TH_TN <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "TN") %>%
  filter(Var %in% c("Climate", "Severity", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Thinning", -80, 110) 

TH_LF <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "LF") %>%
  filter(Var %in% c("Severity", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Thinning", -80, 60) 

TH_FF <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "FF") %>%
  filter(Var %in% c("Climate", "Forest", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Thinning", -80, 60) 

TH_FRB <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "FRB") %>%
  filter(Var %in% c("Climate", "Severity", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Thinning", -60, 60) 

TH_SOC <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "SOC") %>%
  filter(Var %in% c("Severity", "Recovery")) %>% 
  cat_biogeochem_graph(title = "Thinning", -25, 75)

TH_Ts <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "Ts") %>%
  cat_biogeochem_graph(title = "Thinning", -40, 80)

TH_SM <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Thinning", var1 == "SM") %>%
  cat_biogeochem_graph(title = "Thinning", -25, 50)

# PB
PB_TN <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Prescribed burn", var1 == "TN") %>%
  filter(Var %in% c("Forest", "Repeat")) %>% 
  cat_biogeochem_graph(title = "Prescribed burn", -10, 50) 

PB_FF <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Prescribed burn", var1 == "FF") %>%
  filter(Var %in% c("Repeat")) %>% 
  cat_biogeochem_graph(title = "Prescribed burn", -100, 10) 

PB_Ts <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Prescribed burn", var1 == "Ts") %>%
  filter(Var %in% c("Forest", "Repeat")) %>% 
  cat_biogeochem_graph(title = "Prescribed burn", 0, 50) 

PB_SM <- 
  plot_cat_biogeochem_summary %>%
  filter(Management == "Prescribed burn", var1 == "SM") %>%
  cat_biogeochem_graph(title = "Prescribed burn", -20, 50) 



CC_TC
CC_TN + TH_TN + PB_TN
CC_SOC
CC_LF + TH_LF
TH_FF + PB_FF
CC_FRB + TH_FRB
CC_Ts + TH_Ts + PB_Ts
CC_SM + TH_SM + PB_SM
