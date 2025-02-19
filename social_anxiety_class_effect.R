# Social Anxiety
library(devtools)
setwd("~/Desktop/Github/multinma")
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
sa <- read_tsv("data-raw/social_anxiety/social_anxiety.txt")
sa_trt <- read_tsv("data-raw/social_anxiety/social_anxiety_treatments.txt")
sa_class <- read_tsv("data-raw/social_anxiety/social_anxiety_classes.txt")

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

undebug(multinma::nma)
#================= Model simulations ==================================
set.seed(951)
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
                 QR = TRUE
)

sa_fit_EXclass_RE <- nma(sa_net,
                         trt_effects = "random",
                         prior_trt = normal(0, 100),
                         prior_het = half_normal(5),
                         class_effects = "exchangeable",
                         prior_class_sd = normal(0.33,0.1),
                         class_sd = list(`Exercise and SH no support` = c("Exercise promotion", "Self-help no support"),
                                         `SSRIs and NSSA` = c("SSRI/SNRI", "NSSA"),
                                         `Psychodynamic & Other psychological therapies` = c("Psychodynamic psychotherapy", "Other psychological therapies")
                         )
)

sa_fit_COclass_RE <- nma(sa_net,
                         trt_effects = "random",
                         prior_trt = normal(0, 100),
                         prior_het = half_normal(5),
                         class_effects = "common")

sa_fit_EXclass_FE <- nma(sa_net,
                         trt_effects = "fixed",
                         prior_trt = normal(0, 100),
                         prior_het = half_normal(1),
                         class_effects = "exchangeable",
                         prior_class_sd = normal(0.33,0.1),
                         class_sd = list(`Exercise and SH no support` = c("Exercise promotion", "Self-help no support"),
                                         `SSRIs and NSSA` = c("SSRI/SNRI", "NSSA"),
                                         `Psychodynamic & Other psychological therapies` = c("Psychodynamic psychotherapy", "Other psychological therapies")
                         )
)

sa_fit_COclass_FE <- nma(sa_net,
                         trt_effects = "fixed",
                         prior_trt = normal(0, 100),
                         prior_het = half_normal(5),
                         class_effects = "common"
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

summary(sa_fit_RE, pars = "d")
summary(sa_fit_EXclass_RE, pars = "class_mean")
summary(sa_fit_EXclass_RE, pars = "d")
summary(sa_fit_COclass_RE, pars = "d")

# DEV DEV Plot
PlotA <- plot(sa_dic_ume_RE, sa_dic_COclass_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - UME RE Model") +
  ylab("Residual deviance - Common RE Model")

PlotA

#========Nodesplitting==========

Investigate <- sa_fit_RE$network$agd_contrast
(sa_dic_RE$pointwise)

get_nodesplits <- get_nodesplits(sa_net, include_consistency = FALSE)
has_direct(sa_net, "Waitlist", "Psychodynamic psychotherapy")
has_indirect(sa_net, "Waitlist", "Psychodynamic psychotherapy")

therapy_data <- data.frame(
  Group = c("CBT individual", "Waitlist", "Waitlist"),
  Therapy_Type = c("Psychodynamic psychotherapy", "Psychodynamic psychotherapy", "CBT individual")
)

therapy_data_1 <- data.frame(
  Group = c("Waitlist"),
  Therapy_Type = c("CBT group")
)

sa_fit_RE_nodesplit <- nma(sa_net,
                           consistency = "nodesplit",
                           nodesplit = therapy_data,
                           trt_effects = "random",
                           prior_trt = normal(0, 100),
                           prior_het = half_normal(5),
)

sa_fit_RE_nodesplit_1 <- nma(sa_net,
                             consistency = "nodesplit",
                             nodesplit = therapy_data_1,
                             trt_effects = "random",
                             prior_trt = normal(0, 100),
                             prior_het = half_normal(5),
)

summary(sa_fit_RE_nodesplit)
summary(sa_fit_RE_nodesplit_1)

plot(sa_dic_RE)
sa_dic_RE$pointwise$agd_contrast
# DEV DEV Plot
plotume <- plot(sa_dic_RE, sa_dic_ume_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - No Class model") +
  ylab("Residual deviance - UME model") +
  theme(text = element_text(size = 18))

plotume

resdev_RE <- sa_dic_RE$pointwise$agd_contrast$resdev
resdev_ume_RE <- sa_dic_ume_RE$pointwise$agd_contrast$resdev
resdev_RE_EX <- sa_dic_EXclass_RE$pointwise$agd_contrast$resdev
resdev_RE_CO <- sa_dic_COclass_RE$pointwise$agd_contrast$resdev
df <- sa_dic_RE$pointwise$agd_contrast$n_contrast
sa_dic_EXclass_RE$pointwise

dev_dev_UMEvsNMA <- data.frame(resdev_RE, resdev_ume_RE, df)
dev_dev_NMAvsEX <- data.frame(resdev_RE_EX, resdev_RE, df)
dev_dev_EXvsCO <- data.frame(resdev_RE_CO, resdev_RE_EX, df)
dev_dev_UMEvsNMA$dev_diff <- dev_dev_UMEvsNMA$resdev_RE - dev_dev_UMEvsNMA$resdev_ume_RE
dev_dev_NMAvsEX$dev_diff <- dev_dev_NMAvsEX$resdev_RE_EX - dev_dev_NMAvsEX$resdev_RE
dev_dev_EXvsCO$dev_diff <- dev_dev_EXvsCO$resdev_RE_CO - dev_dev_EXvsCO$resdev_RE_EX

sa_net$agd_contrast$.trtclass

#============dev-dev plots==========

dev_dev_UMEvsNMA$markerType <- 1
rownames(dev_dev_UMEvsNMA)[grepl("ALDEN2011", rownames(dev_dev_UMEvsNMA))] -> special_cases1
rownames(dev_dev_UMEvsNMA)[grepl("EMMELKAMP2006", rownames(dev_dev_UMEvsNMA))] -> special_cases2
dev_dev_UMEvsNMA$markerType[rownames(dev_dev_UMEvsNMA) %in% special_cases1] <- "ALDEN2011"
dev_dev_UMEvsNMA$markerType[rownames(dev_dev_UMEvsNMA) %in% special_cases2] <- "EMMELKAMP2006"

# Adjust these values as needed based on your data's range
xmin <- min(dev_dev_UMEvsNMA$resdev_RE, dev_dev_UMEvsNMA$resdev_ume_RE) - 0.1
xmax <- max(dev_dev_UMEvsNMA$resdev_RE, dev_dev_UMEvsNMA$resdev_ume_RE) + 0.1

plotUMEvsRE<-ggplot(data = dev_dev_UMEvsNMA, aes(x = resdev_RE, y = resdev_ume_RE)) +
  geom_point(aes(shape = factor(markerType), color = factor(df)), size = 4) +  # Use shape based on markerType
  scale_shape_manual(values = c("1" = 20, "ALDEN2011" = 20, "EMMELKAMP2006" = 20),
                     breaks = c("ALDEN2011", "EMMELKAMP2006")) +
  scale_color_manual(values = c("#87CEEB", "#1E90FF", "#4169E1", "#000080")) +
  geom_abline(intercept = 0, slope = 1) +  # x=y line
  scale_x_continuous(name = "Residual Deviation - No Class model", limits = c(xmin, xmax)) +
  scale_y_continuous(name = "Residual Deviation - UME model", limits = c(xmin, xmax)) +
  labs(shape = "Study", color = "Degrees of Freedom") +
  coord_fixed(ratio = 1) +  # Ensure the plot is a perfect square
  theme_minimal() +
  ggrepel::geom_text_repel(aes(label = markerType),
                           data = ~filter(., resdev_RE > 6),
                           nudge_y = -0.3, # Adjusts position downward
                           direction = "y", # Keeps labels aligned vertically
                           min.segment.length = 0,
                           segment.color = NA) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), # Add solid line around the graph
        legend.position = "right", # Position legend at bottom left
        axis.title = element_text(size = 14), # Increase axis title size
        axis.text = element_text(size = 12), # Increase axis text size
        plot.title = element_text(size = 16), # Increase plot title size
  ) +
  ggtitle("Comparison of Residual Deviations")

plotUMEvsRE

plotREvsEX<-ggplot(data = dev_dev_NMAvsEX, aes(x = resdev_RE_EX, y = resdev_RE)) +
  geom_point(aes(color = factor(df)), size = 4) +  # Use shape based on markerType
  scale_color_manual(values = c("#87CEEB", "#1E90FF", "#4169E1", "#000080")) +
  geom_abline(intercept = 0, slope = 1) +  # x=y line
  scale_x_continuous(name = "Residual Deviation - Exchangeable class model", limits = c(xmin, 7.5)) +
  scale_y_continuous(name = "Residual Deviation - No class model", limits = c(xmin, 7.5)) +
  labs(shape = "Study", color = "Degrees of Freedom") +
  coord_fixed(ratio = 1) +  # Ensure the plot is a perfect square
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), # Add solid line around the graph
        legend.position = "right", # Position legend at bottom left
        axis.title = element_text(size = 14), # Increase axis title size
        axis.text = element_text(size = 12), # Increase axis text size
        plot.title = element_text(size = 16), # Increase plot title size
  ) +
  ggtitle("Comparison of Residual Deviations")

plotREvsEX

plotEXvsCO<-ggplot(data = dev_dev_EXvsCO, aes(x = resdev_RE_CO, y = resdev_RE_EX)) +
  geom_point(aes(color = factor(df)), size = 4) +  # Use shape based on markerType
  scale_color_manual(values = c("#87CEEB", "#1E90FF", "#4169E1", "#000080")) +
  geom_abline(intercept = 0, slope = 1) +  # x=y line
  scale_x_continuous(name = "Residual Deviation - Common class model", limits = c(xmin, 7.5)) +
  scale_y_continuous(name = "Residual Deviation - Exchageable class model", limits = c(xmin, 7.5)) +
  labs(shape = "Study", color = "Degrees of Freedom") +
  coord_fixed(ratio = 1) +  # Ensure the plot is a perfect square
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), # Add solid line around the graph
        legend.position = "right", # Position legend at bottom left
        axis.title = element_text(size = 14), # Increase axis title size
        axis.text = element_text(size = 12), # Increase axis text size
        plot.title = element_text(size = 16), # Increase plot title size
  ) +
  ggtitle("Comparison of Residual Deviations")

plotEXvsCO

# DEV DEV Plot EX vs no class
plotEX <- plot(sa_dic_EXclass_RE, sa_dic_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Exchangeable Class model") +
  ylab("Residual deviance - No Class model") +
  theme(text = element_text(size = 18))
plotEX

resdev_RE_EX <- sa_dic_EXclass_RE$pointwise$agd_contrast$resdev
resdev_RE <- sa_dic_RE$pointwise$agd_contrast$resdev
resdev_RE
dev_dev_UMEvsNMA
class_mapping_dic <- sa_net$agd_contrast
# Remove rows where the .trt column contains "Waitlist"
class_mapping_dic_cleaned <- class_mapping_dic[!is.na(class_mapping_dic$.y), ]

class_mapping_dic_cleaned_unique <- class_mapping_dic_cleaned %>%
  distinct(.study, classn, .keep_all = TRUE)

# DEV DEV Plot
plotEXCO <- plot(sa_dic_COclass_RE, sa_dic_EXclass_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Common Class model") +
  ylab("Residual deviance - Exchangeable Class model") +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed") +   # Line for x = y + 1
  geom_abline(slope = 1, intercept = -1, linetype = "dashed") +  # Line for x + 1 = y
  theme(text = element_text(size = 18))

plotEXCO

# DEV DEV Plot
plotCO <- plot(sa_dic_COclass_RE, sa_dic_RE, show_uncertainty = FALSE) +
  xlab("Residual deviance - Common Class model") +
  ylab("")

grid.arrange(plotEX, plotCO, ncol = 2)

plot(sa_net, weight_edges = TRUE, show_trt_class = TRUE) +
  ggplot2::theme(legend.position = "bottom", legend.box = "vertical")

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

forestplot_NMAvsEX

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
  mutate(parameter = if_else(type == "Class", "Common class mean", parameter))

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

forestplot_NMAvsCO

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

sa_fit_EXclass_RE_sum_CLASS <- filter(sa_fit_EXclass_RE_sum, grepl('^class_mean\\[.*\\]$', parameter)) %>%
  mutate(parameter = gsub("^class_mean\\[|\\]$", "", parameter),
         parameter = gsub("^d\\[|\\]$", "", parameter),
         parameter = ifelse(parameter == "Mirtazapine", "NSSA", parameter))

sa_fit_COclass_RE_sum_trt <- sa_fit_COclass_RE_sum_trt %>%
  mutate(Model = "Common Class")

sa_fit_COclass_RE_sum_trt

sa_fit_CLASS_RE_combined <- rbind(sa_fit_EXclass_RE_sum_CLASS, sa_fit_COclass_RE_sum_trt)
ls(sa_fit_EXclass_RE_sum_CLASS)
ls(sa_fit_COclass_RE_sum_trt)

sa_fit_CLASS_RE_combined_sorted <- sa_fit_CLASS_RE_combined %>%
  mutate(parameter = factor(parameter, levels = rev(sort(unique(parameter)))))

# Assuming 'Model' is the variable for which the legend order needs to be corrected
sa_fit_CLASS_RE_combined_sorted$Model <- factor(sa_fit_CLASS_RE_combined_sorted$Model,
                                                levels = c("Exchangeable Class", "Common Class"))
sa_fit_CLASS_RE_combined_sorted <- sa_fit_CLASS_RE_combined_sorted %>%
  filter(!grepl("Pill placebo|Psychological placebo", parameter))

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

forestplot_EXvsCO

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
  labs(x = "Rank",
       y = "Probability") +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

RankDensity_EXvsNMA

#====Class ranks====
EXclass_mean <- (as.matrix(sa_fit_EXclass_RE))
COclass_mean <- (as.matrix(sa_fit_COclass_RE, pars = "d"))


# Specifying the specific columns to retain
specific_columns_to_keep <- c(
  "d[Pill placebo]","d[Psychological placebo]"
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

levels(results_df_combined$class)[levels(results_df_combined$class) == "Reference"] <- "Waitlist"


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

#----
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
  labs(x = "Rank",
       y = "Probability") +
  scale_x_continuous(breaks = 1:max(rank_probs_long_combined$Rank)) + # Ensure you're referencing the correct dataframe
  scale_linetype_discrete(name = "Model") + # Optional: Customize the legend title for linetypes
  theme(
    strip.text.y = element_text(angle = 0, hjust = 1),
    text = element_text(size = 18),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    axis.text.y = ggtext::element_markdown(),
    legend.text = ggtext::element_markdown(),
    legend.position = c(0.95, 0.03),
    legend.justification = c("right", "bottom")
  )

RankDensity_EXvsCO

#===== Leverage plot =============

# Extract data for plotting
resdev <- sa_dic_EXclass_RE[["pointwise"]][["agd_contrast"]][["resdev"]]
resdev_sqrt <- sqrt(resdev)
leverage <- sa_dic_EXclass_RE[["pointwise"]][["agd_contrast"]][["leverage"]]
df <- sa_dic_EXclass_RE[["pointwise"]][["agd_contrast"]][["n_contrast"]]

# Create a data frame for ggplot2, including the existing labels
plot_data <- data.frame(
  resdev = resdev,
  Leverage = leverage,
  df = df,
  Label = names(resdev)  # Use the names of resdev as the labels
)

# Divide resdev and leverage by df
plot_data$resdev_per_df <- plot_data$resdev / plot_data$df
plot_data$leverage_per_df <- plot_data$Leverage / plot_data$df
plot_data$resdevper_df_sqrt <- sqrt(plot_data$resdev_per_df)


# Subset for specific points to label
labels <- plot_data[plot_data$Label %in% c("resdev[ALDEN2011]"), ]
# Modify the Label column to remove "resdev[" and "]"
labels$Label <- gsub("resdev\\[|\\]", "", labels$Label)


# Calculate the sums
sum_resdev <- sum(resdev, na.rm = TRUE)
sum_leverage <- sum(leverage, na.rm = TRUE)

# Create a data frame for DIC lines
dic_values <- c(1, 2, 3)  # DIC values to plot
dic_lines <- data.frame(
  SqrtResidualDeviance = numeric(),
  Leverage = numeric(),
  DIC = factor()
)

# Generate curves for each DIC value
for (dic in dic_values) {
  sqrt_resdev_seq <- seq(0, sqrt(dic), length.out = 100)  # Generate sqrt resdev values
  leverage_seq <- dic - sqrt_resdev_seq^2                # Corresponding leverage values
  dic_lines <- rbind(dic_lines, data.frame(
    SqrtResidualDeviance = sqrt_resdev_seq,
    Leverage = leverage_seq,
    DIC = factor(dic)
  ))
}

# Create the annotation text
annotation_text <- paste0(
  "Sum of Residual Deviance: ", round(sum_resdev, 2), "\n",
  "Sum of Leverage: ", round(sum_leverage, 2)
)

# Create the plot
Leverage_plot <- ggplot() +
  # Plot the main data points
  geom_point(data = plot_data, aes(x = resdevper_df_sqrt, y = leverage_per_df), color = "blue") +
  # Plot the DIC curves
  geom_line(data = dic_lines, aes(x = SqrtResidualDeviance, y = Leverage, color = DIC), linetype = "dashed") +
  # Add the annotation text
  annotate(
    "text", x = max(resdev_sqrt) * 0.95, y = max(leverage) * 0.8,
    label = annotation_text, hjust = 0.9, vjust = -0.5, size = 4, color = "black"
  ) +
  # Add labels for specific points
  geom_text(data = labels, aes(x = resdevper_df_sqrt, y = leverage_per_df, label = Label),
            hjust = 0.6, vjust = -2, size = 4, color = "red") +
  # Customize labels and appearance
  labs(
    x = "Square Root of Residual Deviance",
    y = "Leverage",
    color = "DIC"
  ) +
  theme_minimal(base_size = 15)

Leverage_plot <- Leverage_plot +
  theme(text = element_text(size = 10))

Leverage_plot


