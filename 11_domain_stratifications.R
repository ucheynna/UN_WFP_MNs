#Folate
tanzpred <- read.csv("folatemerged_output.csv")


data1 <- merge(tanzpred, tanz4geoloc, by="y4_hhid")

folate1 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(Prediction == 1),
    flpredicted4 = (count_1 / total) *100
  ) %>%
  select(domain, flpredicted4)

folate2 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(foladq == 1),
    flestimated4 = (count_1 / total) *100
  ) %>%
  select(domain, flestimated4)

folatedf <- merge(folate1, folate2, by="domain")


tanzpred <- read.csv("zincmerged_output.csv")


data1 <- merge(tanzpred, tanz4geoloc, by="y4_hhid")

zinc1 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(Prediction == 1),
    znpredicted4 = (count_1 / total) *100
  ) %>%
  select(domain, znpredicted4)

zinc2 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(znadq == 1),
    znestimated4 = (count_1 / total) *100
  ) %>%
  select(domain, znestimated4)

zincdf <- merge(zinc1, zinc2, by="domain")

tanz2 <- read.csv("tza.csv") %>%
  mutate(znadq = ifelse(zn_mg < 10.2, 1, 0),
         foladq = ifelse(folate_mcg  < 250, 1, 0)) %>%
  select(y5_hhid, znadq, foladq)

#Folate
tanzpred <- read.csv("folatewave5pred.csv")
tanzpred <- merge(tanz2, tanzpred, by="y5_hhid")
tanzpred <- na.omit(tanzpred)
data1 <- merge(tanzpred, tanz5geoloc, by="y5_hhid")

folate1 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(Prediction == 1),
    flpredicted5 = (count_1 / total) *100
  ) %>%
  select(domain, flpredicted5)

folate2 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(foladq == 1),
    flestimated5 = (count_1 / total) *100
  ) %>%
  select(domain, flestimated5)

folatedf2 <- merge(folate1, folate2, by="domain")


tanzpred <- read.csv("zincwave5pred.csv")
tanzpred <- merge(tanz2, tanzpred, by="y5_hhid")
tanzpred <- na.omit(tanzpred)
data1 <- merge(tanzpred, tanz5geoloc, by="y5_hhid")


zinc1 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(Prediction == 1),
    znpredicted5 = (count_1 / total) *100
  ) %>%
  select(domain, znpredicted5)

zinc2 <- data1 %>%
  group_by(domain) %>%
  summarize(
    total = n(),
    count_1 = sum(znadq == 1),
    znestimated5 = (count_1 / total) *100
  ) %>%
  select(domain, znestimated5)

zincdf2 <- merge(zinc1, zinc2, by="domain")


bin1 <- merge(zincdf, zincdf2, by="domain")
bin2 <- merge(folatedf, folatedf2, by="domain")
merged <- merge(bin2, bin1, by="domain")


merged$domain <- factor(merged$domain, 
                        levels = c(1, 2, 3, 4), 
                        labels = c("Dar es Salaam", "Other urban mainland", "Other rural mainland", "Zanzibar"))
merged_df <- merged


# merged_df <- readRDS("fulldata.Rds")
long_df <- merged_df %>%
  select(domain, flestimated4, flestimated5) %>%
  pivot_longer(cols = -domain, 
               names_to = "Variable", 
               values_to = "Value")

# Create the line plot with customized settings
ggplot(long_df, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +            
  geom_point(size = 3) +          
  scale_color_manual(values = c("flestimated4" = "blue", "flestimated5" = "red")) + 
  labs(title = "Line Plot of Zn Predictions and Estimates",
       x = "Location (Domain)",
       y = "Value",
       color = "Variables") +      
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))  

long_df <- merged_df %>%
  select(domain, flpredicted4, flpredicted5) %>%
  pivot_longer(cols = -domain, 
               names_to = "Variable", 
               values_to = "Value")

# Create the line plot with customized settings
ggplot(long_df, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +           
  geom_point(size = 3) +           
  scale_color_manual(values = c("flpredicted4" = "blue", "flpredicted5" = "red")) + 
  labs(title = "Line Plot of Zn Predictions and Estimates",
       x = "Location (Domain)",
       y = "Value",
       color = "Variables") +     
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))  



long_df <- merged_df %>%
  select(domain, znestimated4, znestimated5) %>%
  pivot_longer(cols = -domain, 
               names_to = "Variable", 
               values_to = "Value")

# Create the line plot with customized settings
ggplot(long_df, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +            
  geom_point(size = 3) +           
  scale_color_manual(values = c("znestimated4" = "blue", "znestimated5" = "red")) + 
  labs(title = "Line Plot of Zn Predictions and Estimates",
       x = "Location (Domain)",
       y = "Value",
       color = "Variables") +      
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))  

long_df <- merged_df %>%
  select(domain, znpredicted4, znpredicted5) %>%
  pivot_longer(cols = -domain, 
               names_to = "Variable", 
               values_to = "Value")

# Create the line plot with customized settings
ggplot(long_df, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +            
  geom_point(size = 3) +          
  scale_color_manual(values = c("znpredicted4" = "blue", "znpredicted5" = "red")) + 
  labs(title = "Line Plot of Zn Predictions and Estimates",
       x = "Location (Domain)",
       y = "Value",
       color = "Variables") +      
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))  


dumbbell_df4 <- merged_df %>%
  select(domain, znpredicted4, znpredicted5) %>%
  pivot_longer(cols = c(znpredicted4, znpredicted5), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = factor(Type, levels = c("znpredicted4", "znpredicted5"),
                       labels = c("Predicted4", "Predicted5"))) %>%
  arrange(domain)

# Create the Wave 4 dumbbell plot
plot_wave4 <- ggplot(dumbbell_df4, aes(y = factor(domain, levels = unique(dumbbell_df4$domain)))) +
  geom_segment(data = merged_df, 
               aes(x = znpredicted4, xend = znpredicted5, yend = domain, color = "Difference"), 
               size = 1) +
  geom_point(data = dumbbell_df4 %>% filter(Type == "Predicted4"), 
             aes(x = Value, color = Type), size = 3) +
  geom_point(data = dumbbell_df4 %>% filter(Type == "Predicted5"), 
             aes(x = Value, color = Type), size = 3) +
  labs(title = "Dumbbell Plot of Predicted Values Wave 4 vs 5",
       x = "Value",
       y = "Domain",
       color = "Legend") +
  scale_color_manual(values = c("Predicted4" = "blue", 
                                "Predicted5" = "red",
                                "Difference" = "grey")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),         
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_text(angle = 0),
        legend.position = "right")

dumbbell_df5 <- merged_df %>%
  select(domain, znestimated4, znestimated5) %>%
  pivot_longer(cols = c(znestimated4, znestimated5), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = factor(Type, levels = c("znestimated4", "znestimated5"),
                       labels = c("Estimated4", "Estimated5"))) %>%
  arrange(domain)

# Create the Wave 5 dumbbell plot
plot_wave5 <- ggplot(dumbbell_df5, aes(y = factor(domain, levels = unique(dumbbell_df5$domain)))) +
  geom_segment(data = merged_df, 
               aes(x = znestimated4, xend = znestimated5, yend = domain, color = "Difference"), 
               size = 1) +
  geom_point(data = dumbbell_df5 %>% filter(Type == "Estimated4"), 
             aes(x = Value, color = Type), size = 3) +
  geom_point(data = dumbbell_df5 %>% filter(Type == "Estimated5"), 
             aes(x = Value, color = Type), size = 3) +
  labs(title = "Dumbbell Plot of Estimated Values Wave 4 vs 5",
       x = "Value",
       y = "Domain",
       color = "Legend") +
  scale_color_manual(values = c("Estimated4" = "blue", 
                                "Estimated5" = "red",
                                "Difference" = "grey")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),         
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_text(angle = 0),
        legend.position = "right")

grid.arrange(plot_wave4, plot_wave5, ncol = 2)


####
# Prepare data for Wave 4 plot
dumbbell_df4 <- merged_df %>%
  select(domain, znpredicted4, znpredicted5) %>%
  pivot_longer(cols = c(znpredicted4, znpredicted5), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = factor(Type, levels = c("znpredicted4", "znpredicted5"),
                       labels = c("Predicted(Wave4)", "Predicted(Wave5)")))

# Create the Wave 4 dumbbell plot
plot_wave4 <- ggplot(dumbbell_df4, aes(y = domain)) +
  geom_segment(data = merged_df, 
               aes(x = znpredicted4, xend = znpredicted5, yend = domain, color = "Difference"), 
               size = 1) +
  geom_point(data = dumbbell_df4 %>% filter(Type == "Predicted(Wave4)"), 
             aes(x = Value, color = Type), size = 3) +
  geom_point(data = dumbbell_df4 %>% filter(Type == "Predicted(Wave5)"), 
             aes(x = Value, color = Type), size = 3) +
  labs(x = "Proportion(%)",
       y = "Strata",
       color = "Scenario") +
  scale_color_manual(values = c("Predicted(Wave4)" = "blue", 
                                "Predicted(Wave5)" = "red")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),         
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_text(angle = 0),
        legend.position = "right")

# Prepare data for Wave 5 plot
dumbbell_df5 <- merged_df %>%
  select(domain, znestimated4, znestimated5) %>%
  pivot_longer(cols = c(znestimated4, znestimated5), 
               names_to = "Type", 
               values_to = "Value") %>%
  mutate(Type = factor(Type, levels = c("znestimated4", "znestimated5"),
                       labels = c("Estimated(Wave4)", "Estimated(Wave5)")))

# Create the Wave 5 dumbbell plot
plot_wave5 <- ggplot(dumbbell_df5, aes(y = domain)) +
  geom_segment(data = merged_df, 
               aes(x = znestimated4, xend = znestimated5, yend = domain, color = "Difference"), 
               size = 1) +
  geom_point(data = dumbbell_df5 %>% filter(Type == "Estimated(Wave4)"), 
             aes(x = Value, color = Type), size = 3) +
  geom_point(data = dumbbell_df5 %>% filter(Type == "Estimated(Wave5)"), 
             aes(x = Value, color = Type), size = 3) +
  labs(x = "Proportion(%)",
       y = "Strata",
       color = "Scenario") +
  scale_color_manual(values = c("Estimated(Wave4)" = "blue", 
                                "Estimated(Wave5)" = "red")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),         
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_text(angle = 0),
        legend.position = "right")

# Arrange plots side by side
grid.arrange(plot_wave4, plot_wave5, ncol = 2)

