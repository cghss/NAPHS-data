#############################################
## Background ###############################
#############################################

## This script contains code to generate a simple barplot graphic
## assume working directory is set to NAPHS-data/global-summary 
## (this is also the source file location)

#############################################
## Setup ####################################
#############################################

## NOTE: if you don't already have these libraries installed, you can do so by running install.packages()
## example: install.packages("ggplot2")

## Load libraries
library(tidyverse) ## for data management
library(scales) ## for commas on axes of plots

#############################################
## Read in data #############################
#############################################

## NAPHS country data - one row per WHO member state
## Data as of 
countries <- read.delim("countries.tsv")

#############################################
## Summarize data: ##########################
## Printed summary ##########################
#############################################

## print summary info, globally
countries %>%
  summarize(total_member_states = sum(who_member_state == TRUE),
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE))

#############################################
## Global funnel: Manuscript barplot ########
#############################################
## prior interations of this barplot included below

countries %>%
  summarize(aggregation = "global",
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE)) %>%
  pivot_longer(!aggregation, names_to = "metric", values_to = "count") %>%
  mutate(label = factor(
    recode(metric, 
           completed_jee = "Completed JEE",
           completed_naphs = "Completed NAPHS",
           published_naphs  = "Published NAPHS",
           published_naphs_data = "Published costed line items",
           machine_readable_data = "Published machine readable data"),
    levels = rev(c("Completed JEE", "Completed NAPHS", "Published NAPHS", "Published costed line items", "Published machine readable data")))) %>%
  mutate(pct = count/max(count)) %>% 
  ggplot(aes(x = count, y = label, fill = label)) +
  geom_bar(stat = "identity", color = "black", fill = "#088BBE") +
  geom_text(aes(label = paste(count, 
                              ifelse(pct != 1,
                                     paste(" (", round(pct*100, 0), "%)", sep = ""),
                                     paste("")))),
            hjust = 0, nudge_x = 4, size = 3) +
  ylab("") +
  xlab("Number of Countries\n(IHR member States)") +
  xlim(0, 108) +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 7)) +
  ggtitle("Participation in Monitoring and Evaluation\nFramework Process")

#############################################
## Global funnel: barplot (v1) ##############
#############################################

countries %>%
  summarize(aggregation = "global",
            total_member_states = sum(who_member_state == TRUE),
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE)) %>%
  pivot_longer(!aggregation, names_to = "metric", values_to = "count") %>%
  mutate(label = factor(
    recode(metric, 
           total_member_states = "All Countries (IHR Member States)",
           completed_jee = "Completed JEE",
           completed_naphs = "Completed NAPHS",
           published_naphs  = "Published NAPHS",
           published_naphs_data = "Published costed line items",
           machine_readable_data = "Published machine readable data"),
    levels = rev(c("All Countries (IHR Member States)", "Completed JEE", "Completed NAPHS", "Published NAPHS", "Published costed line items", "Published machine readable data")))) %>%
  mutate(pct = count/max(count)) %>% 
  ggplot(aes(x = count, y = label, fill = label)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste(round(pct*100, 0), "%", sep = "")),
            hjust = 0, nudge_x = 4, size = 3) +
  ylab("") +
  xlab("Number of Countries\n(IHR member States)") +
  scale_x_continuous(expand = c(0, 45)) +
  scale_fill_manual(values = rev(c("#172869", "#088BBE", "#1BB6AF", "#F8CD9C", "#F6A1A5", "#EA7580")),
                    guide = guide_legend(reverse = TRUE)) + 
  labs(caption = "", fill = "Process") +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 7), legend.position = "none") +
  ggtitle("Participation in Monitoring and Evaluation\nFramework Process")

#############################################
## Global funnel: barplot (v2) ##############
#############################################

countries %>%
  summarize(aggregation = "global",
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE)) %>%
  pivot_longer(!aggregation, names_to = "metric", values_to = "count") %>%
  mutate(label = factor(
    recode(metric, 
           completed_jee = "Completed JEE",
           completed_naphs = "Completed NAPHS",
           published_naphs  = "Published NAPHS",
           published_naphs_data = "Published costed line items",
           machine_readable_data = "Published machine readable data"),
    levels = rev(c("Completed JEE", "Completed NAPHS", "Published NAPHS", "Published costed line items", "Published machine readable data")))) %>%
  mutate(pct = count/max(count)) %>% 
  ggplot(aes(x = count, y = label, fill = label)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste(round(pct*100, 0), "%", sep = "")),
            hjust = 0, nudge_x = 4, size = 3) +
  ylab("") +
  xlab("Number of Countries\n(IHR member States)") +
  scale_x_continuous(expand = c(0, 45)) +
  scale_fill_manual(values = rev(c("#172869", "#088BBE", "#1BB6AF", "#F8CD9C", "#F6A1A5")),
                    guide = guide_legend(reverse = TRUE)) + 
  labs(caption = "", fill = "Process") +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 7), legend.position = "none") +
  ggtitle("Participation in Monitoring and Evaluation\nFramework Process")

#############################################
## Global funnel: barplot (v3) ##############
#############################################

countries %>%
  summarize(aggregation = "global",
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE)) %>%
  pivot_longer(!aggregation, names_to = "metric", values_to = "count") %>%
  mutate(label = factor(
    recode(metric, 
           completed_jee = "Completed JEE",
           completed_naphs = "Completed NAPHS",
           published_naphs  = "Published NAPHS",
           published_naphs_data = "Published costed line items",
           machine_readable_data = "Published machine readable data"),
    levels = rev(c("Completed JEE", "Completed NAPHS", "Published NAPHS", "Published costed line items", "Published machine readable data")))) %>%
  mutate(pct = count/max(count)) %>% 
  ggplot(aes(x = count, y = label, fill = label)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste(count, " (", round(pct*100, 0), "%)", sep = "")),
            hjust = 0, nudge_x = 4, size = 3) +
  ylab("") +
  xlab("Number of Countries\n(IHR member States)") +
  scale_fill_manual(values = rev(c("#172869", "#088BBE", "#1BB6AF", "#F8CD9C", "#F6A1A5")),
                    guide = guide_legend(reverse = TRUE)) + 
  labs(caption = "", fill = "Process") +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 7), legend.position = "none") +
  ggtitle("Participation in Monitoring and Evaluation\nFramework Process")

#############################################
## Funnel by region: barplot (v1) ###########
#############################################

countries %>%
  group_by(who_region) %>%
  summarize(total_member_states = sum(who_member_state == TRUE),
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE)) %>%
  pivot_longer(!who_region, names_to = "metric", values_to = "count") %>%
  mutate(label = factor(
    recode(metric, 
           total_member_states = "All Countries in Region",
           completed_jee = "Completed JEE",
           completed_naphs = "Completed NAPHS",
           published_naphs  = "Published NAPHS",
           published_naphs_data = "Published costed line items",
           machine_readable_data = "Published machine readable data"),
    levels = rev(c("All Countries in Region", "Completed JEE", "Completed NAPHS", "Published NAPHS", "Published costed line items", "Published machine readable data")))) %>%
  mutate(pct = count/max(count)) %>%
  ggplot(aes(x = pct, y = who_region, fill = label)) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Percent of Countries in Region") +
  ylab("") +
  scale_fill_manual(values = rev(c("#172869", "#088BBE", "#1BB6AF", "#F8CD9C", "#F6A1A5", "#EA7580")),
                    guide = guide_legend(reverse = TRUE)) +
  labs(caption = "", fill = "Process") +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 7), legend.position = "right") +
  ggtitle("Participation in Monitoring and Evaluation\nFramework Process")

#############################################
## Funnel by region: barplot (v2) ###########
#############################################

countries %>%
  group_by(who_region) %>%
  summarize(total_member_states = sum(who_member_state == TRUE),
            completed_jee = sum(completed_jee == TRUE),
            completed_naphs = sum(completed_naphs == TRUE),
            published_naphs = sum(published_naphs == TRUE, na.rm = TRUE ),
            published_naphs_data = sum(naphs_includes_line_item_costs == TRUE, na.rm = TRUE),
            machine_readable_data = sum(naphs_data_machine_readable == TRUE, na.rm = TRUE)) %>%
  pivot_longer(!who_region, names_to = "metric", values_to = "count") %>%
  mutate(label = factor(
    recode(metric, 
           total_member_states = "All Countries in Region",
           completed_jee = "Completed JEE",
           completed_naphs = "Completed NAPHS",
           published_naphs  = "Published NAPHS",
           published_naphs_data = "Published costed line items",
           machine_readable_data = "Published machine readable data"),
    levels = rev(c("All Countries in Region", "Completed JEE", "Completed NAPHS", "Published NAPHS", "Published costed line items", "Published machine readable data")))) %>%
  mutate(pct = count/max(count)) %>%
  ggplot(aes(x = pct, y = label, fill = label, group = who_region)) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Percent of Countries in Region") +
  ylab("") +
  scale_fill_manual(values = rev(c("#172869", "#088BBE", "#1BB6AF", "#F8CD9C", "#F6A1A5", "#EA7580")),
                    guide = guide_legend(reverse = TRUE)) +
  labs(caption = "", fill = "Process") +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 7), legend.position = "none") +
  ggtitle("Participation in Monitoring and Evaluation\nFramework Process") +
  facet_wrap(~who_region)
