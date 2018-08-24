library(ggplot2)
library(ggradar)
library(scales)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(grid)
library(gridExtra)
library(ggthemes)

setwd("PATH\\Survey Results")

datain <- read_excel("...Survey Results.xls", sheet = 1)

group <- c("Overall", "FF", "EF", "Prefer Not to Answer")
co <- c("purple", "springgreen", "red", "dodgerblue")

for (i in seq_along(group)) {
 j <- co[i]

#Overall dot plot
overall_val <- filter(datain, Level == group[i] & Category == "Value") %>%
               select(-Category,-Total, -Level, -Mean)

tidy <- gather(overall_val, Question, count)
               
colnames(tidy) <- c("Question", "rate", "count")

order <- c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree")

tidy_expand <- tidy[rep(seq_len(nrow(tidy)), tidy$count), 1:2]
tidy_expand <- arrange(tidy_expand, Question, rate) %>%
               mutate(rate = ifelse(rate == "_5", "Strongly agree", rate),
                      rate = ifelse(rate == "_4", "Agree", rate),
                      rate = ifelse(rate == "_3", "Neither agree/disagree", rate),
                      rate = ifelse(rate == "_2", "Disagree", rate),
                      rate = ifelse(rate == "_1", "Strongly disagree", rate))
tidy_expand$rate <- factor(tidy_expand$rate, levels = order)

footnote <- "Each dot represents a response; black line is the average"

overall_1 <- ggplot(tidy_expand, aes(x = Question, y = rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center", method = "histodot", fill = j, color = 
                j, alpha = .6, dotsize = .75) +
  stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean", aes(group = Question), geom = "crossbar", 
               size = .4, width = .5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
  scale_y_discrete("rate") +
  labs(title = paste("Distribution for Q1: Group =", group[i])) +
  theme_hc() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5, vjust = -6, size = 30, face = "bold"),
        axis.text = element_text(size = 17, face = "bold", color = "black"))

overall_1 <- arrangeGrob(overall_1, bottom = textGrob(footnote, x = 0, hjust = -.1, vjust = .05, gp = gpar(fontsize = 14)))
grid.draw(overall_1)

ggsave(overall_1, file = paste("Q1", group[i], ".png", sep = ""), scale = 1, width = 19, height = 15)

#Question 2
overall_ma <- filter(datain, Level == group[i] & Category == "Meeting Aspects") %>%
              select(-Category,-Total, -Level, -Mean)

tidy <- gather(overall_ma, Question, count)

colnames(tidy) <- c("Question", "rate", "count")
order <- c("Very unsatisfied", "Unsatisfied", "Neither satisfied/unsatisfied", "Satisfied", "Very satisfied")

tidy_expand <- tidy[rep(seq_len(nrow(tidy)), tidy$count), 1:2]
tidy_expand <- arrange(tidy_expand, Question, rate) %>%
  mutate(rate = ifelse(rate == "_5", "Very satisfied", rate),
         rate = ifelse(rate == "_4", "Satisfied", rate),
         rate = ifelse(rate == "_3", "Neither satisfied/unsatisfied", rate),
         rate = ifelse(rate == "_2", "Satisfied", rate),
         rate = ifelse(rate == "_1", "Very satisfied", rate))
tidy_expand$rate <- factor(tidy_expand$rate, levels = order)

overall_2 <- ggplot(tidy_expand, aes(x = Question, y = rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center", method = "histodot", fill = j, color = 
                 j, alpha = .6, dotsize = .75) +
  stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean", aes(group = Question), geom = "crossbar", 
               size = .4, width = .5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
  scale_y_discrete("rate") +
  labs(title = paste("Distribution for Q2: Group =", group[i])) +
  theme_hc() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5, vjust = -6, size = 30, face = "bold"),
        axis.text = element_text(size = 17, face = "bold", color = "black"))

overall_1 <- arrangeGrob(overall_2, bottom = textGrob(footnote, x = 0, hjust = -.1, vjust = .05, gp = gpar(fontsize = 14)))
grid.draw(overall_1)

ggsave(overall_2, file = paste("Q2", group[i], ".png", sep = ""), scale = 1, width = 19, height = 15)

#Question 3
overall_speak <- filter(datain, Level == group[i] & Category == "Speakers") %>%
                 select(-Category,-Total, -Level, -Mean)

tidy <- gather(overall_speak, Question, count)

colnames(tidy) <- c("Question", "rate", "count")
order <- c("Very unsatisfied", "Unsatisfied", "Neither satisfied/unsatisfied", "Satisfied", "Very satisfied")

tidy_expand <- tidy[rep(seq_len(nrow(tidy)), tidy$count), 1:2]
tidy_expand <- arrange(tidy_expand, Question, rate) %>%
  mutate(rate = ifelse(rate == "_5", "Very satisfied", rate),
         rate = ifelse(rate == "_4", "Satisfied", rate),
         rate = ifelse(rate == "_3", "Neither satisfied/unsatisfied", rate),
         rate = ifelse(rate == "_2", "Satisfied", rate),
         rate = ifelse(rate == "_1", "Very satisfied", rate))
tidy_expand$rate <- factor(tidy_expand$rate, levels = order)

overall_3 <- ggplot(tidy_expand, aes(x = Question, y = rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center", method = "histodot", fill = j, color = 
                 j, alpha = .6, dotsize = .75) +
  stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean", aes(group = Question), geom = "crossbar", 
               size = .4, width = .5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
  scale_y_discrete("rate") +
  labs(title = paste("Distribution for Q3: Group =", group[i])) +
  theme_hc() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5, vjust = -6, size = 30, face = "bold"),
        axis.text = element_text(size = 17, face = "bold", color = "black"))

overall_1 <- arrangeGrob(overall_3, bottom = textGrob(footnote, x = 0, hjust = -.1, vjust = .05, gp = gpar(fontsize = 14)))
grid.draw(overall_1)

ggsave(overall_3, file = paste("Q3", group[i], ".png", sep = ""), scale = 1, width = 19, height = 15)
}

#Radar Plots
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

order <- c("Overall", "FF", "EF", "Prefer Not to Answer")
cats <- c("Value", "Meeting Aspects", "Speakers")

radar_value <- datain %>%
               mutate(Level = factor(Level, levels = order)) %>%
               select(Question, Category, Level, Mean) %>%
               arrange(desc(Category), Question, Level) %>%
               rename(Group = Level)

for(i in seq_along(cats)) {
  ques <- c(1, 2, 3)

radar <- filter(radar_value, Category == cats[i])

footnote <- "Responses for each: Overall, n = 17; FF, n = 10; EF, n = 2, PNA, n = 5"

a <- ggplot(radar, aes(x = Question, y = Mean, label = Mean)) + 
  geom_polygon(aes(group = Group, color = Group), fill = NA, size = 1) +
  geom_line(aes(group = Group, color = Group), size = 1) +
  scale_color_manual(values = c("purple", "springgreen", "red", "dodgerblue")) +
  scale_fill_manual(values = c("purple", "springgreen", "red", "dodgerblue")) +
  coord_radar() +
  geom_text(check_overlap = TRUE) +
  theme_minimal() +
  labs(title = paste("Averages for Question", ques[i], "by Group")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = .5, vjust = -6, size = 30, face = "bold"))
print(a)
}
