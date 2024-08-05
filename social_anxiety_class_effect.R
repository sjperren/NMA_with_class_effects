# Social Anxiety
library(devtools)
setwd("C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Summar of Fun/multinma")

library(multinma)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(ggtext)
library(gridExtra)
library(stringr)
library(gt)
library(xtable)
library(igraph)
library(ggraph)

options(mc.cores = parallel::detectCores())

#================= Network Setup ==================================

# Read in data
sa <- read_tsv("C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Summar of Fun/Tasks from Supervisors/social_anxiety.txt")
sa_trt <- read_tsv("C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Summar of Fun/Tasks from Supervisors/social_anxiety_treatments.txt")
sa_class <- read_tsv("C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Summar of Fun/Tasks from Supervisors/social_anxiety_classes.txt")

# Tidy data
social_anxiety <- sa %>%
  mutate(Var1 = V) %>%
  pivot_longer(cols = matches("[0-9]+$"),
               names_to = c(".value", "arm"),
               names_pattern = "^([a-zA-Z]+)([0-9]+)$") %>%
  filter(!is.na(t)) %>%
  transmute(studyn, studyc, trtn = t, y = y, se = sqrt(Var)) %>%
  # Add in treatment names
  left_join(sa_trt, by = "trtn") %>%
  # Add in class details
  left_join(sa_class, by = "classn")


# Create network dataframe
sa_net <- set_agd_contrast(social_anxiety,
                           studyc, trtc,
                           y = y, se = se,
                           trt_class = classc,
                           trt_ref = "Waitlist")

#================= Model simulations ==================================

# UME model vs trt effects
sa_UME_FE <- nma(sa_net,
                 trt_effects = "fixed",
                 consistency = "ume",
                 prior_trt = normal(0, 100),
                 prior_het = half_normal(5))

sa_UME_RE <- nma(sa_net,
                 trt_effects = "random",
                 consistency = "ume",
                 prior_trt = normal(0, 100),
                 prior_het = half_normal(5))

sa_fit_FE <- nma(sa_net,
                 trt_effects = "fixed",
                 prior_trt = normal(0, 100),
                 prior_het = half_normal(5),
)

sa_fit_RE <- nma(sa_net,
                 trt_effects = "random",
                 prior_trt = normal(0, 100),
                 prior_het = half_normal(5),
)

sa_fit_EXclass_RE <- nma(sa_net,
                 trt_effects = "random",
                 prior_trt = normal(0, 100),
                 prior_het = half_normal(5),
                 class_effects = "exchangeable",
                 #class_sd = "common",
                 prior_class_sd = normal(0.33,0.1),
                 class_sd = list(`Exercise and SH no support` = c("Exercise promotion", "Self-help no support"),
                                 `SSRIs and NSSA` = c("SSRI/SNRI", "NSSA")
                                 # `Psychodynamic & Other psychological therapies` = c("Psychodynamic psychotherapy", "Other psychological therapies")
                                 )
)

sa_fit_COclass_RE <- nma(sa_net,
                       trt_effects = "random",
                       prior_trt = normal(0, 100),
                       prior_het = half_normal(5),
                       class_effects = "common")
                       #class_sd = "common",
                       #prior_class_sd = normal(0.33,0.1))
                       #class_sd = list(`Exercise and SH no support` = c("Exercise promotion", "Self-help no support"),
                                       #`SSRIs and NSSA` = c("SSRI/SNRI", "NSSA"),
                                       # `Psychodynamic & Other psychological therapies` = c("Psychodynamic psychotherapy", "Other psychological therapies")))

sa_fit_EXclass_FE <- nma(sa_net,
                         trt_effects = "fixed",
                         prior_trt = normal(0, 100),
                         prior_het = half_normal(5),
                         class_effects = "exchangeable",
                         #class_sd = "common",
                         prior_class_sd = normal(0.33,0.1),
                         class_sd = list(`Exercise and SH no support` = c("Exercise promotion", "Self-help no support"),
                                         `SSRIs and NSSA` = c("SSRI/SNRI", "NSSA")
                                         # `Psychodynamic & Other psychological therapies` = c("Psychodynamic psychotherapy", "Other psychological therapies")
                         )
)

sa_fit_COclass_FE <- nma(sa_net,
                         trt_effects = "fixed",
                         prior_trt = normal(0, 100),
                         prior_het = half_normal(5),
                         class_effects = "common"
                         #class_sd = "common",
                         #prior_class_sd = normal(0.33,0.1),
                         #class_sd = list(`Exercise and SH no support` = c("Exercise promotion", "Self-help no support"),
                         # `SSRIs and NSSA` = c("SSRI/SNRI", "NSSA")
                         # `Psychodynamic & Other psychological therapies` = c("Psychodynamic psychotherapy", "Other psychological therapies")
)

#================= DIC table creation ==================================

### DIC RESULTS
(sa_dic_ume_FE <- dic(sa_UME_FE))
(sa_dic_ume_RE <- dic(sa_UME_RE))
(sa_dic_FE <- dic(sa_fit_FE))
(sa_dic_RE <- dic(sa_fit_RE))
(sa_dic_COclass_FE <- dic(sa_fit_COclass_FE))
(sa_dic_COclass_RE <- dic(sa_fit_COclass_RE))
(sa_dic_EXclass_FE <- dic(sa_fit_EXclass_FE))
(sa_dic_EXclass_RE <- dic(sa_fit_EXclass_RE))

## TAU RESULTS
summary(sa_UME_RE, pars = "tau")
summary(sa_fit_RE, pars = "tau")
summary(sa_fit_COclass_RE, pars = "tau")
summary(sa_fit_EXclass_RE, pars = "tau")

# CLASS_SD
summary(sa_fit_EXclass_FE, pars = "class_sd")
summary(sa_fit_EXclass_RE, pars = "class_sd")

# Data for the table
models <- c("UME", "UME", "NMA", "NMA", "Common Class Effects", "Common Class Effects", "Exchangeable Class Effects", "Exchangeable Class Effects")
treatment_effect_model <- c("Fixed", "Random", "Fixed", "Random", "Fixed", "Random", "Fixed", "Random")
residual_deviance <- c(285.1, 160.7, 288.2, 162, 377, 157.8, 284.6, 162.7)
pD <- c(60.1, 108.7, 40, 94.5, 15.9, 93, 34.1, 88.4)
DIC <- c(345.2, 269.4, 328.2, 256.5, 392.9, 250.8, 318.7, 251.1)
tau_values <- c("NA", "0.22 (0.16 - 0.29)", "NA", "0.21 (0.15 - 0.27)", "NA", "0.25 (0.20 - 0.31)", "NA", "0.20 (0.14 - 0.26)")
tau_std_errors <- c("NA", 0.03, "NA", 0.03, "NA", 0.03, "NA", 0.03)
tau_combined <- paste0(tau_values, " (", tau_std_errors, ")")

# Creating a data frame
model_data <- data.frame(Model = models,
                         Treatment.effect = treatment_effect_model,
                         Residual.deviance = residual_deviance,
                         pD = pD,
                         DIC = DIC,
                         tau = tau_values)

# Rename columns to include spaces
colnames(model_data) <- c("Model", "Treatment effect", "Residual deviance", "pD", "DIC", "tau")

# Rest of your code for creating and printing the gt table remains the same
library(gt)

# Creating a graphical table with gt and centering the text in all columns
gt_table <- gt(model_data) %>%
  cols_width(
    vars(pD, DIC) ~ px(100),
    vars(`Treatment effect`, tau) ~ px(150)
  ) %>%
  cols_align(align = "center", columns = everything()) # Center align all columns

# Print the table
print(gt_table)
print(xtable(model_data, type='latex'))

#Exchangeable CLASS_SD table
class <- c("Anticonvulsants", "Benzodiazepines", "CBT group", "CBT individual", "Combined", "Exercise and SH no support", "Exposure", "MAOI", "SSRIs and NSSA", "Other psychological therapies", "Self-help with support")
random <- c("0.31 (0.12 - 0.50)", "0.32 (0.14 - 0.52)", "0.30 (0.11 - 0.36)", "0.33 (0.16 - 0.50)", "0.35 (0.08 - 0.52)", "0.31 (0.11 - 0.50)", "0.31 (0.12 - 0.51)", "0.35 (0.19 - 0.54)", "0.17 (0.03 - 0.37)", "0.30 (0.10 - 0.49)", "0.30 (0.11 - 0.50)")
fixed <- c("0.31 (0.11 - 0.50)", "0.32 (0.14 - 0.51)", "0.30 (0.13 - 0.49)", "0.33 (0.17 - 0.50)", "0.37 (0.23 - 0.53)", "0.30 (0.12 - 0.50)", "0.30 (0.10 - 0.51)", "0.35 (0.19 - 0.53)", "0.13 (0.03 - 0.32)", "0.29 (0.11 - 0.49)", "0.30 (0.10 - 0.50)")
n <- c(3, 2, 3, 4, 5, 3, 2, 2, 8, 3, 2)

# Creating a data frame
model_data <- data.frame(`Exchangeable Class` = class,
                         `Random` = random,
                         `Fixed` = fixed,
                         n = n)

# Rename columns to include spaces
colnames(model_data) <- c("Exchangeable Class", "Random", "Fixed", "n")

# Creating a graphical table with gt


gt_table <- gt(model_data) %>%
  cols_width(
    columns = vars(`Exchangeable Class`) ~ px(250),
    columns = vars(n) ~ px(65),
    columns = vars(Random, Fixed) ~ px(165)
  ) %>%
  cols_align(align = "center", columns = vars(Random, Fixed, n))  # Center align specific columns

# Print the table
print(gt_table)

model_data <- data.frame(`Exchangeable Class` = class,
                         `Random` = random,
                         n = n)

# Rename columns to include spaces
colnames(model_data) <- c("Exchangeable Class", "Class sd(95% CI)", "n")

gt_table <- gt(model_data) %>%
  cols_width(
    columns = vars(`Exchangeable Class`) ~ px(250),
    columns = vars(n) ~ px(65),
    columns = vars(`Class sd(95% CI)`) ~ px(165)
  ) %>%
  cols_align(align = "center", columns = vars(`Class sd(95% CI)`, n))  # Center align specific columns

# Print the table
print(gt_table)

setwd("C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables")

# DEV DEV Plot
PlotA <- plot(sa_dic_ume_RE, sa_dic_COclass_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - UME RE Model") +
  ylab("Residual deviance - Common RE Model")

ggsave(filename = "PlotA.pdf", plot = PlotA, path = "C:/Users/sjper/OneDrive/Desktop")


view(sa_dic_RE$pointwise)
view(sa_dic_ume_RE$pointwise)
# Studies with the most differing ResDev is "ALDEN2011" and "EMMELKAMP2006"
# ALDEN2011 compares trt 1 to 30 (Waitlist vs CBT group)
# A lot of others trails are using CBT group so results from ALDEN2011 could differ the most from the rest of trt 30 trails.
# ALDEN2011 trt_effect -1.88 second largest trt_effect out of all trts. Biggest effect compared to other trt 30 trials.
# EMMELKAMP2006 compares trt 1, 29, 35 (Waitlist vs Psychodynamic psychotherapy vs CBT individual)
# Again there are plenty of other studies using both trt 29 and 35, so results from EMMELKAMP2006 could differ the most compared to the rest
# EMMELKAMP2006 when compared to ref has the largest "negative" difference 0.159 (comparison performed worse than reference)

# DEV DEV Plot
plotume <- plot(sa_dic_RE, sa_dic_ume_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - No Class model") +
  ylab("Residual deviance - UME model") +
  theme(text = element_text(size = 18))
plotume
ggsave("dev-dev_umeVSNMA.pdf", plot = plotume, width = 20, height = 20, units = "cm", path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables", scale = 1)

resdev_RE <- sa_dic_RE$pointwise$agd_contrast$resdev
resdev_ume_RE <- sa_dic_ume_RE$pointwise$agd_contrast$resdev

dev_dev_data <- data.frame(resdev_RE, resdev_ume_RE)
dev_dev_data$markerType <- 1
rownames(dev_dev_data)[grepl("ALDEN2011", rownames(dev_dev_data))] -> special_cases1
rownames(dev_dev_data)[grepl("EMMELKAMP2006", rownames(dev_dev_data))] -> special_cases2
dev_dev_data$markerType[rownames(dev_dev_data) %in% special_cases1] <- "ALDEN2011"
dev_dev_data$markerType[rownames(dev_dev_data) %in% special_cases2] <- "EMMELKAMP2006"

# Adjust these values as needed based on your data's range
xmin <- min(dev_dev_data$resdev_RE, dev_dev_data$resdev_ume_RE) - 0.1
xmax <- max(dev_dev_data$resdev_RE, dev_dev_data$resdev_ume_RE) + 0.1

plotUMEvsRE<-ggplot(data = dev_dev_data, aes(x = resdev_RE, y = resdev_ume_RE)) +
  geom_point(aes(shape = factor(markerType)), size = 4) +  # Use shape based on markerType
  scale_shape_manual(values = c("1" = 20, "ALDEN2011" = 1, "EMMELKAMP2006" = 4),
                     breaks = c("ALDEN2011", "EMMELKAMP2006")) +
  geom_abline(intercept = 0, slope = 1) +  # x=y line
  scale_x_continuous(name = "Residual Deviation - No Class model", limits = c(xmin, xmax)) +
  scale_y_continuous(name = "Residual Deviation - UME model", limits = c(xmin, xmax)) +
  labs(shape = "Study") +
  coord_fixed(ratio = 1) +  # Ensure the plot is a perfect square
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), # Add solid line around the graph
        legend.position = c(0.65, 0.05), # Position legend at bottom left
        legend.justification = c(0, 0), # Anchor point for legend position
        legend.background = element_rect(fill = "white", colour = "black", size = 1), # Optional: remove background
        legend.text = element_text(size = 12), # Increase legend text size
        axis.title = element_text(size = 14), # Increase axis title size
        axis.text = element_text(size = 12), # Increase axis text size
        plot.title = element_text(size = 16), # Increase plot title size
        legend.box.background = element_blank(), # Optional: remove box background
        legend.box.margin = margin(0,0,0,0) # Optional: adjust spacing around the legend
  ) +
  ggtitle("Comparison of Residual Deviations")

plotUMEvsRE

ggsave("dev-dev_UMEvsRE.pdf", plot = plotUMEvsRE, width = 20, height = 20, units = "cm", path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables", scale = 1)

# DEV DEV Plot EX vs no class
plotEX <- plot(sa_dic_EXclass_RE, sa_dic_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Exchangeable Class model") +
  ylab("Residual deviance - No Class model") +
  theme(text = element_text(size = 18))
plotEX
ggsave("dev-dev_EXvsNMA.pdf", plot = plotEX, width = 20, height = 20, units = "cm", path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables", scale = 1)

resdev_RE_class <- sa_dic_RE$pointwise$agd_contrast$resdev
class_mapping_dic <- sa_net$agd_contrast
# Remove rows where the .trt column contains "Waitlist"
class_mapping_dic_cleaned <- class_mapping_dic[!is.na(class_mapping_dic$.y), ]

class_mapping_dic_cleaned_unique <- class_mapping_dic_cleaned %>%
  distinct(.study, classn, .keep_all = TRUE)

# DEV DEV Plot
plotEXCO <- plot(sa_dic_COclass_RE, sa_dic_EXclass_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Common Class model") +
  ylab("Residual deviance - Exchangeable Class model")+
  theme(text = element_text(size = 18))
plotEXCO
ggsave("dev-dev_EXvsCO.pdf", plot = plotEXCO, width = 20, height = 20, units = "cm", path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables", scale = 1)

# DEV DEV Plot
plotCO <- plot(sa_dic_COclass_RE, sa_dic_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Common Class model") +
  ylab("")

grid.arrange(plotEX, plotCO, ncol = 2)


# UME vs CLASS Model
plotEX1 <- plot(sa_dic_COclass_RE, sa_dic_ume_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Common Class model") +
  ylab("Residual deviance - UME model")

plotCO1 <- plot(sa_dic_EXclass_RE, sa_dic_ume_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Exchangable Class model") +
  ylab("")

grid.arrange(plotEX1, plotCO1, ncol = 2)



#=========== TREATMENT EFFECT PLOTS (Exchangeable Class)=====================
sa_fit_EXclass_RE_sum <- summary(sa_fit_EXclass_RE)
sa_fit_EXclass_RE_sum <- as.data.frame(sa_fit_EXclass_RE_sum$summary)
sa_fit_EXclass_RE_sum <- mutate(sa_fit_EXclass_RE_sum, Model = "Exchangeable Class")
sa_fit_EXclass_RE_sum_trt <- filter(sa_fit_EXclass_RE_sum, grepl('^d\\[.*\\]$', parameter)|
                                                           grepl('^class_mean\\[.*\\]$', parameter))

# Add 'type' column based on 'parameter'
sa_fit_EXclass_RE_sum_trt <- sa_fit_EXclass_RE_sum_trt %>%
  mutate(type = case_when(
    grepl('^class_mean\\[.*\\]$', parameter) ~ "Class",
    grepl('^d\\[.*\\]$', parameter) ~ "Treatment"
  ))

# Clean 'parameter' column
sa_fit_EXclass_RE_sum_trt <- sa_fit_EXclass_RE_sum_trt %>%
  mutate(parameter = gsub("d\\[|class_mean\\[|\\]", "", parameter))


# Create a mapping from treatments to classes
treatment_to_class <- setNames(sa_net$classes, sa_net$treatments)
# Add a 'class' column using the mapping, and replace NA values with 'parameter' values
sa_fit_EXclass_RE_sum_trt_a <- sa_fit_EXclass_RE_sum_trt %>%
  rowwise() %>%
  mutate(class = if_else(is.na(treatment_to_class[parameter]), parameter, treatment_to_class[parameter]))

# Reset the data frame to standard evaluation
sa_fit_EXclass_RE_sum_trt_a <- ungroup(sa_fit_EXclass_RE_sum_trt_a)

# Ordering rows by class (alphabetically) and prioritizing parameters matching their class
sa_fit_EXclass_RE_sum_trt_a_ordered <- sa_fit_EXclass_RE_sum_trt_a %>%
  mutate(is_class_parameter_match = class == parameter) %>%
  arrange(class, is_class_parameter_match, parameter) %>%
  select(-is_class_parameter_match)

# Ensure 'parameter' is a character string
sa_fit_EXclass_RE_sum_trt_a_ordered$parameter <- as.character(sa_fit_EXclass_RE_sum_trt_a_ordered$parameter)

# Modify 'parameter' to "Class Mean" if 'type' is 'Class', before applying the bold formatting
sa_fit_EXclass_RE_sum_trt_a_ordered <- sa_fit_EXclass_RE_sum_trt_a_ordered %>%
  mutate(parameter = if_else(type == "Class", "Class Mean", parameter))

sa_fit_EXclass_RE_sum_trt_a_ordered <- sa_fit_EXclass_RE_sum_trt_a_ordered %>%
  mutate(is_bold = ifelse(str_detect(parameter, "Class Mean"), TRUE, NA))

# Now apply bold formatting to 'parameter' if 'type' is 'Class'
sa_fit_EXclass_RE_sum_trt_a_ordered$parameter <- ifelse(
  sa_fit_EXclass_RE_sum_trt_a_ordered$type == "Class",
  paste0("<b>", sa_fit_EXclass_RE_sum_trt_a_ordered$parameter, "</b>"),
  sa_fit_EXclass_RE_sum_trt_a_ordered$parameter
)

sa_fit_EXclass_RE_sum_trt_a_ordered$type <- as.character(sa_fit_EXclass_RE_sum_trt_a_ordered$type)

sa_fit_EXclass_RE_sum_trt_a_ordered %>%
  ggplot(aes(x = parameter, y = mean, shape = type, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(size = ifelse(type == "Class", 1, 0.5))) +
  scale_size_identity() +  # Treat the size aesthetic as given, without scaling
  geom_pointrange(position = position_dodge(width = .5), size = 0.5) +
  coord_flip() +
  xlab("Treatment") + ylab("Relative treatment effect vs reference treatment") +
  facet_grid(rows = vars(class), scales = "free_y", space = "free") +
  theme_bw() +
  scale_shape_manual(values = c("Treatment" = 19, "Class" = 4),
                     labels = c("**Class**", "Treatment")) +  # Use markdown for bold
  theme(strip.text.y = element_text(angle = 0, hjust = 1),
        axis.text.y = element_markdown(),
        legend.text = element_markdown()) +
  guides(size = FALSE)  # Use ggtext for legend text



#====EXCHANGEABLE CLASS VS NMA=============================
sa_fit_RE_sum <- summary(sa_fit_RE)
sa_fit_RE_sum <- as.data.frame(sa_fit_RE_sum$summary)
sa_fit_RE_sum <- mutate(sa_fit_RE_sum, Model = "No Class")
sa_fit_RE_sum_trt <- filter(sa_fit_RE_sum, grepl('^d\\[.*\\]$', parameter)|
                              grepl('^class_mean\\[.*\\]$', parameter))

# Add 'type' column based on 'parameter'
sa_fit_RE_sum_trt <- sa_fit_RE_sum_trt %>%
  mutate(type = case_when(
    grepl('^class_mean\\[.*\\]$', parameter) ~ "Class",
    grepl('^d\\[.*\\]$', parameter) ~ "Treatment"
  ))

# Clean 'parameter' column
sa_fit_RE_sum_trt <- sa_fit_RE_sum_trt %>%
  mutate(parameter = gsub("d\\[|class_mean\\[|\\]", "", parameter))

# Add a 'class' column using the mapping, and replace NA values with 'parameter' values
sa_fit_RE_sum_trt <- sa_fit_RE_sum_trt %>%
  rowwise() %>%
  mutate(class = if_else(is.na(treatment_to_class[parameter]), parameter, treatment_to_class[parameter]))

sa_fit_RE_sum_trt <- sa_fit_RE_sum_trt %>%
                     mutate(is_bold = NA)

sa_fit_RE_EX_NMA_combined <- rbind(sa_fit_EXclass_RE_sum_trt_a_ordered, sa_fit_RE_sum_trt)

sa_fit_RE_EX_NMA_combined <- sa_fit_RE_EX_NMA_combined %>%
  mutate(class_wrapped = str_wrap(class, width = 15))


forestplot_NMAvsEX<-sa_fit_RE_EX_NMA_combined %>%
  ggplot(aes(x = parameter, y = mean, shape = Model, linetype = Model, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point(position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(size = (ifelse(type == "Class", 1, NA))), width = 0) +
  scale_size_identity() +
  coord_flip() +
  xlab("Treatment") + ylab("Relative treatment effect vs reference treatment") +
  facet_grid(rows = vars(class), scales = "free_y", space = "free") +
  theme_bw() +
  scale_shape_manual(values = c("Exchangeable Class" = 19, "No Class" = 4)) +
  scale_linetype_manual(values = c("Exchangeable Class" = "solid", "No Class" = "dashed")) +
  theme(strip.text.y = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))


ggsave("forestplot_NMAvsEX.pdf",
       plot = forestplot_NMAvsEX,
       width = 25, height = 30, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)


#=========== TREATMENT EFFECT PLOT COMMON & NMA

sa_fit_COclass_RE_sum <- summary(sa_fit_COclass_RE)
sa_fit_COclass_RE_sum <- as.data.frame(sa_fit_COclass_RE_sum$summary)
sa_fit_COclass_RE_sum <- mutate(sa_fit_COclass_RE_sum, Model = "Common Class")
sa_fit_COclass_RE_sum_trt <- filter(sa_fit_COclass_RE_sum, grepl('^d\\[.*\\]$', parameter)|
                                      grepl('^class_mean\\[.*\\]$', parameter))

# Add 'type' column based on 'parameter'
sa_fit_COclass_RE_sum_trt <- sa_fit_COclass_RE_sum_trt %>%
  mutate(type = case_when(
    grepl('^d\\[.*\\]$', parameter) ~ "Class"
  ))

# Clean 'parameter' column
sa_fit_COclass_RE_sum_trt <- sa_fit_COclass_RE_sum_trt %>%
  mutate(parameter = gsub("d\\[|class_mean\\[|\\]", "", parameter))

# Add a 'class' column using the mapping, and replace NA values with 'parameter' values
sa_fit_COclass_RE_sum_trt_a <- sa_fit_COclass_RE_sum_trt %>%
  rowwise() %>%
  mutate(class = if_else(is.na(treatment_to_class[parameter]), parameter, treatment_to_class[parameter]))

# Reset the data frame to standard evaluation
sa_fit_COclass_RE_sum_trt_a <- ungroup(sa_fit_COclass_RE_sum_trt_a)

# Ensure 'parameter' is a character string
sa_fit_COclass_RE_sum_trt_a$parameter <- as.character(sa_fit_COclass_RE_sum_trt_a$parameter)

# Modify 'parameter' to "Class Mean" if 'type' is 'Class', before applying the bold formatting
sa_fit_COclass_RE_sum_trt_a <- sa_fit_COclass_RE_sum_trt_a %>%
  mutate(parameter = if_else(type == "Class", "Class Mean", parameter))

# Now apply bold formatting to 'parameter' if 'type' is 'Class'
sa_fit_COclass_RE_sum_trt_a$parameter <- ifelse(
  sa_fit_COclass_RE_sum_trt_a$type == "Class",
  paste0("<b>", sa_fit_COclass_RE_sum_trt_a$parameter, "</b>"),
  sa_fit_COclass_RE_sum_trt_a$parameter
)

sa_fit_COclass_RE_sum_trt_a$type <- as.character(sa_fit_COclass_RE_sum_trt_a$type)

sa_fit_COclass_RE_sum_trt_a <- sa_fit_COclass_RE_sum_trt_a %>%
                                mutate(is_bold = NA)

sa_fit_RE_CO_NMA_combined <- rbind(sa_fit_COclass_RE_sum_trt_a, sa_fit_RE_sum_trt)

# Ordering rows by class (alphabetically) and prioritizing parameters matching their class
sa_fit_RE_CO_NMA_combined_ordered <- sa_fit_RE_CO_NMA_combined %>%
  mutate(is_class_parameter_match = class == parameter) %>%
  arrange(class, is_class_parameter_match, parameter) %>%
  select(-is_class_parameter_match)

forestplot_NMAvsCO <- sa_fit_RE_CO_NMA_combined_ordered %>%
  ggplot(aes(x = parameter, y = mean, shape = Model, linetype = Model, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point(position = position_dodge(width = 0.5), size = 1) +
  geom_hline(yintercept = 0) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  xlab("Treatment") + ylab("Relative treatment effect vs reference treatment") +
  facet_grid(rows = vars(class), scales = "free_y", space = "free") +
  theme_bw() +
  scale_shape_manual(values = c("Common Class" = 19, "No Class" = 4)) +
  scale_linetype_manual(values = c("Common Class" = "solid", "No Class" = "dashed")) +
  theme(strip.text.y = element_text(angle = 0, hjust = 1),
        axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown())

ggsave("forestplot_NMAvsCO.pdf",
       plot = forestplot_NMAvsCO,
       width = 25, height = 30, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)


#=========== TREATMENT EFFECT PLOTS (Common Class) =====================
# Summarize your data and convert to data frame
sa_fit_COclass_RE_sum <- summary(sa_fit_COclass_RE)
sa_fit_COclass_RE_sum <- as.data.frame(sa_fit_COclass_RE_sum$summary)

# Filter and clean 'parameter' column
sa_fit_COclass_RE_sum_trt <- sa_fit_COclass_RE_sum %>%
  filter(grepl('^d\\[.*\\]$', parameter)) %>%
  mutate(parameter = gsub("^d\\[|\\]$", "", parameter))

# Set 'parameter' as a factor with levels in the order they appear, then reverse the levels
sa_fit_COclass_RE_sum_trt$parameter <- factor(sa_fit_COclass_RE_sum_trt$parameter,
                                              levels = rev(unique(sa_fit_COclass_RE_sum_trt$parameter)))

# Plotting
ggplot(sa_fit_COclass_RE_sum_trt, aes(x = parameter, y = mean, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_hline(yintercept = 0) +
  geom_pointrange(position = position_dodge(width = .5), size = 0.5) +
  coord_flip() +
  xlab("Class") +
  ylab("Relative Class Effect vs Reference Treatment for Common Classes with random treatment effects") +
  theme_bw()


#================= EX and CO classes results ==================================

sa_fit_EXclass_RE_sum_CLASS <- filter(sa_fit_EXclass_RE_sum, grepl('^class_mean\\[.*\\]$', parameter) | parameter %in% c('d[Psychological placebo]', 'd[Psychodynamic psychotherapy]', 'd[Pill placebo]', 'd[Mirtazapine]', 'd[Exercise promotion]')) %>%
  mutate(parameter = gsub("^class_mean\\[|\\]$", "", parameter),
         parameter = gsub("^d\\[|\\]$", "", parameter),
         parameter = ifelse(parameter == "Mirtazapine", "NSSA", parameter))


sa_fit_COclass_RE_sum_trt <- sa_fit_COclass_RE_sum_trt %>%
                                mutate(Model = "Common Class")

sa_fit_CLASS_RE_combined <- rbind(sa_fit_EXclass_RE_sum_CLASS, sa_fit_COclass_RE_sum_trt)
ls(sa_fit_EXclass_RE_sum_CLASS)
ls(sa_fit_COclass_RE_sum_trt)

sa_fit_CLASS_RE_combined_sorted <- sa_fit_CLASS_RE_combined %>%
  mutate(parameter = factor(parameter, levels = rev(sort(unique(parameter)))))

# Assuming 'Model' is the variable for which the legend order needs to be corrected
sa_fit_CLASS_RE_combined_sorted$Model <- factor(sa_fit_CLASS_RE_combined_sorted$Model,
                                                levels = c("Exchangeable Class", "Common Class"))


forestplot_EXvsCO <-sa_fit_CLASS_RE_combined_sorted %>%
  ggplot(aes(x=parameter, y=mean, ymin=`2.5%`, ymax=`97.5%`, shape = Model, linetype = Model), ref_line = 0) +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_hline(yintercept=0) +
  geom_pointrange(position = position_dodge(width = .5), size = 0.5) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Treatment") + ylab("Relative Class Effect vs Reference Treatment") +
  scale_shape_manual(values = c("Exchangeable Class" = 19, "Common Class" = 4)) +
  scale_linetype_manual(values = c("Exchangeable Class" = "solid", "Common Class" = "dashed")) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0, hjust = 1, size = 15),
        axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

ggsave("forestplot_EXvsCO.pdf",
       plot = forestplot_EXvsCO,
       width = 25, height = 20, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)

#================= Full Summary table of Results ==================================
result_data <- read.csv("C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables/Summary_results.csv")
result_data <- result_data %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "")))

names(result_data)[names(result_data) == "X"] <- "Treatment"

summary_results <- gt(result_data) %>%
  tab_header(
    title = "Summary of treatment effects compared with waitlist"
  ) %>%
  cols_label(
    Trails = "Trials",
    Paticipants = "Participants",
    NMA.Model = "NMA Model",
    Common.Class = "Common Class",
    Exchangeable.Class = "Exchangeable Class"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Treatment,
      rows = c(2, 6, 26, 51)
    )
  ) %>%
  # Center text in all columns except the first
  tab_style(
    style = list(
      cell_text(align = "center")

    ),
    locations = cells_body(columns = -1)  # Excludes the first column
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels(columns = everything())
  ) %>%
  cols_width(
    1 ~ px(200)  # Set the width of the first column
    # Other columns will use the default width
  ) %>%
  tab_style(
    style = list(
      cell_text(indent = 3)
    ), location = cells_body(
      columns = Treatment,
      rows = c(3, 4, 5))
  )

summary_results

summary_results <- gt(result_data) %>%
  tab_header(
    title = "Summary of treatment effects compared with waitlist"
  ) %>%
  cols_label(
    Trails = "Trials",
    Paticipants = "Participants",
    `NMA.Model` = "NMA Model",
    `Common.Class` = "Common Class",
    `Exchangeable.Class` = "Exchangeable Class"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c("Treatment"),  # Updated to use `c()` instead of `vars()`
      rows = c(2, 6, 26, 51)
    )
  ) %>%
  # Center text in all columns except the first
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = -1)  # Excludes the first column
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels(columns = everything())
  ) %>%
  cols_width(
    1 ~ px(200)  # Set the width of the first column
  ) %>%
  tab_stub_indent(
    rows = c(5),  # Specify the rows you wish to indent
    indent = 2    # Specify the level of indentation as a numeric value
  )

summary_results


library(webshot)
webshot::install_phantomjs()
webshot::webshot("file:///C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year%201/Class%20effects/Plots%20and%20tables/file65986dfe6b1a.html", "summary_results.png")
# Path to your HTML file
html_file_path <- "file:///C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year%201/Class%20effects/Plots%20and%20tables/file65986dfe6b1a.html"

# Destination PNG file path
png_file_path <- "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables/summary_table_test.png"

# Use webshot to capture the HTML as a PNG image
webshot::webshot(url = html_file_path, file = png_file_path, zoom = 2)



gtsave(summary_results, "summary_results.pdf")

#==========================Treatment Rank Results==================================

### Rank RESULTS for class model
(sa_ranks_RE <- posterior_ranks(sa_fit_RE, lower_better = TRUE))
(sa_ranks_EXclass_RE <- posterior_ranks(sa_fit_EXclass_RE, lower_better = TRUE))

# Example extraction (modify based on your actual object structure):
data_RE <- sa_ranks_RE$summary %>%
  mutate(Model = "No Class")

data_class_RE <- sa_ranks_EXclass_RE$summary %>%
  mutate(Model = "Exchangeable Class")
# Combine the data
combined_data <- rbind(data_RE, data_class_RE)

# Create a mapping from treatments to classes
treatment_to_class <- setNames(sa_net$classes, sa_net$treatments)
# Add a 'class' column using the mapping, and replace NA values with 'parameter' values
combined_data <- combined_data %>%
  rowwise() %>%
  mutate(class = if_else(is.na(treatment_to_class[.trt]), .trt, treatment_to_class[.trt]))

# Convert 'class' to a character to reset any previous factor levels
combined_data$class <- as.character(combined_data$class)

# Now, redefine 'class' as a factor with sorted levels
combined_data$class <- factor(combined_data$class, levels = sort(unique(combined_data$class)))

combined_data <- combined_data %>%
  mutate(class_wrapped = str_wrap(class, width = 15))

RankCI_NMAvsEX <- combined_data %>%
ggplot(aes(x=.trt, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, shape = Model)) +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_pointrange(aes(linetype = Model), position = position_dodge(width = .5), size = 0.5) +
  facet_grid(rows = vars(class_wrapped), scales = "free_y", space = "free") +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Treatment") + ylab("Posterior Ranks") +
  scale_shape_manual(values = c("Exchangeable Class" = 19, "No Class" = 4)) +
  theme_bw() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 1, size = 9),
    axis.text.y = ggtext::element_markdown(),
    legend.text = ggtext::element_markdown(),
    text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10))

RankCI_NMAvsEX
ggsave("RankCI_NMAvsEX.pdf",
       plot = RankCI_NMAvsEX,
       width = 25, height = 30, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)


### Rank RESULTS for class model
(sa_rankprobs_RE <- posterior_rank_probs(sa_fit_RE, lower_better = TRUE))
(sa_rankprobs_class_RE <- posterior_rank_probs(sa_fit_EXclass_RE, lower_better = TRUE))

data_RE <- sa_rankprobs_RE$summary %>%
  mutate(Model = "No Class")

data_class_RE <- sa_rankprobs_class_RE$summary %>%
  mutate(Model = "Exchangable Class")
# Combine the data
combined_data <- rbind(data_RE, data_class_RE)

# Reshape the data to long format
long_data <- pivot_longer(combined_data,
                          cols = starts_with("p_rank"),
                          names_to = "Rank",
                          values_to = "Probability",
                          names_prefix = "p_rank\\[")

# Convert Rank to a numeric value
long_data <- long_data %>%
  mutate(Rank = as.numeric(gsub("\\]", "", Rank)))


# Create the plot
ggplot(long_data, aes(x = Rank, y = Probability, color = Model)) +
  geom_line() +  # Use geom_line for line plots
  facet_wrap(~ .trt, scales = "free_x") +
  scale_color_manual(values = c("blue", "red")) +  # Adjust colors as needed
  theme_minimal() +
  labs(title = "Rank Probability Distribution for Each Treatment by Model", x = "Rank", y = "Probability")


RankDensity_EXvsNMA <- ggplot(long_data, aes(x = Rank, y = Probability, linetype = Model)) +
  geom_line() +  # Continue using geom_line for line plots
  facet_wrap(~ .trt, scales = "free_x") +
  scale_linetype_manual(values = c("Exchangable Class" = "solid", "No Class" = "dashed")) +  # Specify linetypes for each Model
  theme_minimal() +
  labs(title = "Rank Probability Distribution for Each Treatment by Model",
       x = "Rank",
       y = "Probability") +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

ggsave("RankDensity_EXvsNMA.pdf",
       plot = RankDensity_EXvsNMA,
       width = 40, height = 25, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)


#====Class ranks====
EXclass_mean <- (as.matrix(sa_fit_EXclass_RE))
COclass_mean <- (as.matrix(sa_fit_COclass_RE, pars = "d"))


# Specifying the specific columns to retain
specific_columns_to_keep <- c(
  "d[Exercise promotion]",
  "d[Mirtazapine]",
  "d[Pill placebo]",
  "d[Psychodynamic psychotherapy]",
  "d[Psychological placebo]"
)

# Get all column names from EXclass_mean
all_col_names <- colnames(EXclass_mean)

# Dynamically include columns that start with "class_mean"
class_mean_cols <- grep("^class_mean", all_col_names, value = TRUE)

# Combine "class_mean" columns with specific "d[" columns
columns_to_keep <- c(class_mean_cols, specific_columns_to_keep)

# Filter the matrix to keep only the specified columns
EXclass_mean <- EXclass_mean[, colnames(EXclass_mean) %in% columns_to_keep]

EXclass_mean <- cbind(`d[Reference]` = 0, EXclass_mean)
COclass_mean <- cbind(`d[Reference]` = 0, COclass_mean)

# Take ranks at each iteration
EXranks <- t(apply(EXclass_mean, 1, rank))
COranks <- t(apply(COclass_mean, 1, rank))

# Posterior mean ranks for each class
colMeans(EXranks)
colMeans(COranks)

# Posterior median and 95% CrI
apply(EXranks, 2, quantile, probs = c(0.025, 0.5, 0.975))
apply(COranks, 2, quantile, probs = c(0.025, 0.5, 0.975))

# Assuming 'results' is your matrix from the 'apply' function
EXresults <- t(apply(EXranks, 2, quantile, probs = c(0.025, 0.5, 0.975)))
COresults <- t(apply(COranks, 2, quantile, probs = c(0.025, 0.5, 0.975)))


# Convert to data frame and make it long format
EXresults_df <- as.data.frame(EXresults)
EXresults_df$class <- rownames(EXresults_df)

COresults_df <- as.data.frame(COresults)
COresults_df$class <- rownames(COresults_df)

# Remove 'class[]' from the row names
EXresults_df$class <- gsub("class_mean\\[|\\]", "", EXresults_df$class)
EXresults_df$class <- gsub("d\\[|\\]", "", EXresults_df$class)
COresults_df$class <- gsub("d\\[|\\]", "", COresults_df$class)

# Convert 'class' to a factor and order it alphabetically
EXresults_df$class <- factor(EXresults_df$class, levels = sort(unique(EXresults_df$class), decreasing = TRUE))
COresults_df$class <- factor(COresults_df$class, levels = sort(unique(COresults_df$class), decreasing = TRUE))

EXresults_df_sorted <- arrange(EXresults_df, class)
COresults_df_sorted <- arrange(COresults_df, class)
EXresults_df_sorted <- EXresults_df_sorted %>%
                        mutate(Model = "Exchangeable")
COresults_df_sorted <- COresults_df_sorted %>%
                        mutate(Model = "Common")

results_df_combined <- rbind(EXresults_df_sorted, COresults_df_sorted)


results_df_combined <- results_df_combined %>%
  mutate(class = if_else(class == "NSSA", "Mirtazapine", class))

# Now, redefine 'class' as a factor with sorted levels
results_df_combined$class <- with(results_df_combined, factor(class, levels = rev(sort(unique(class)))))

RankCI_COvsEX<-results_df_combined %>%
  ggplot(aes(x=class, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, shape = Model)) +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_pointrange(aes(linetype = Model), position = position_dodge(width = .5), size = 0.5) +
  facet_grid(scales = "free_y", space = "free") +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Treatment") + ylab("Posterior Ranks") +
  scale_shape_manual(values = c("Exchangeable" = 4, "Common" = 19)) +
  theme_bw() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 1),
    text = element_text(size = 18),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.text.y = ggtext::element_markdown(),
    legend.text = ggtext::element_markdown()
  )

RankCI_COvsEX

ggsave("RankCI_COvsEX.pdf",
       plot = RankCI_COvsEX,
       width = 30, height = 25, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)


EXranks_df <- as.data.frame(EXranks)
COranks_df <- as.data.frame(COranks)

# RANK PROBS for class
rank_probs_EX <- apply(EXranks_df, 2, function(x) table(factor(x, levels = 1:ncol(EXranks_df))) / nrow(EXranks_df))
rank_probs_CO <- apply(COranks_df, 2, function(x) table(factor(x, levels = 1:ncol(COranks_df))) / nrow(COranks_df))

# Convert to data frame
rank_probs_df_EX <- as.data.frame(rank_probs_EX)
rank_probs_df_CO <- as.data.frame(rank_probs_CO)

# Convert the data frame to a long format
rank_probs_long_EX <- rank_probs_df_EX %>%
  mutate(Rank = row_number()) %>%
  pivot_longer(
    cols = -Rank,
    names_to = "Class",
    values_to = "Probability"
  )

rank_probs_long_CO <- rank_probs_df_CO %>%
  mutate(Rank = row_number()) %>%
  pivot_longer(
    cols = -Rank,
    names_to = "Class",
    values_to = "Probability"
  )

# Adjusting the Class names
rank_probs_long_EX <- rank_probs_long_EX %>%
  mutate(Class = gsub("class_mean\\[", "", Class), # Remove 'class_mean['
         Class = gsub("d\\[", "", Class),
         Class = gsub("\\]", "", Class),           # Remove closing ']'
         Class = ifelse(Class == "Reference", "Waitlist", Class),
         Model = "Exchangeable") # Rename 'Reference' to 'Waitlist'

rank_probs_long_CO <- rank_probs_long_CO %>%
  mutate(Class = gsub("class_mean\\[", "", Class), # Remove 'class_mean['
         Class = gsub("d\\[", "", Class),
         Class = gsub("\\]", "", Class),           # Remove closing ']'
         Class = ifelse(Class == "Reference", "Waitlist", Class),
         Model = "Common") # Rename 'Reference' to 'Waitlist'


rank_probs_long_combined <- rbind(rank_probs_long_CO, rank_probs_long_EX)

# Assuming 'Model' is the column containing "Mirtazapine"
rank_probs_long_combined <- rank_probs_long_combined %>%
  mutate(Class = ifelse(Class == "Mirtazapine", "NSSA", Class))



RankDensity_EXvsCO<-ggplot(rank_probs_long_combined, aes(x = Rank, y = Probability, group = interaction(Class, Model), linetype = Model)) +
  geom_line() +
  facet_wrap(~ Class) + # Assuming you might want each class to have its own y scale for clarity
  theme_minimal() +
  labs(title = "Rank Probability Distribution by Class",
       x = "Rank",
       y = "Probability") +
  scale_x_continuous(breaks = 1:max(rank_probs_long_combined$Rank)) + # Ensure you're referencing the correct dataframe
  scale_linetype_discrete(name = "Model") + # Optional: Customize the legend title for linetypes
  theme(
  strip.text.y = element_text(angle = 0, hjust = 1),
  text = element_text(size = 18),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 11),
  axis.text.y = ggtext::element_markdown(),
  legend.text = ggtext::element_markdown()
)

RankDensity_EXvsCO

ggsave("RankDensity_EXvsCO.pdf",
       plot = RankDensity_EXvsCO,
       width = 45, height = 25, units = "cm",
       path = "C:/Users/sjper/OneDrive/Desktop/PhD-Compass_Scheme/Year 1/Class effects/Plots and tables",
       scale = 1)
