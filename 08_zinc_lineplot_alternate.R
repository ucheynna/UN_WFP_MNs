#saveRDS(merged_df, file = "fulldata.Rds")
# Prepare data for the first line plot (znestimated4 and znestimated5)
long_df1 <- merged_df %>%
  select(domain, znestimated4, znpredicted4) %>%
  pivot_longer(cols = -domain, 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           "znestimated4" = "Estimated",
                           "znpredicted4" = "Predicted"))

# Create the first line plot
plot1 <- ggplot(long_df1, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +  
  scale_color_manual(values = c("Estimated" = "blue", "Predicted" = "red")) +  
  labs(title = "Wave 4",
       x = "Strata",
       y = "Proportion(%)",
       color = "Wave") +  
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.background = element_rect(color = "black", size = 1))  

# Prepare data for the second line plot (znpredicted4 and znpredicted5)
long_df2 <- merged_df %>%
  select(domain, znestimated5, znpredicted5) %>%
  pivot_longer(cols = -domain, 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           "znestimated5" = "Estimated",
                           "znpredicted5" = "Predicted"))

# Create the second line plot
plot2 <- ggplot(long_df2, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  scale_color_manual(values = c("Estimated" = "blue", "Predicted" = "red")) + 
  labs(title = "Wave 5",
       x = "Strata",
       y = "Proportion(%)",
       color = "Wave") +  
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.background = element_rect(color = "black", size = 1))  

# Arrange plots side by side with borders
plot_grid(plot1, plot2, ncol = 2, rel_widths = c(1, 1))


##zinc scaled
# Find the common y-axis limits
y_limits <- range(c(long_df1$Value, long_df2$Value), na.rm = TRUE)

# Create the first line plot with common y-axis limits
plot1 <- ggplot(long_df1, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  scale_color_manual(values = c("Estimated" = "blue", "Predicted" = "red")) +  
  labs(title = "Wave 4",
       x = "Strata",
       y = "Proportion(%)",
       color = "Wave") +  
  ylim(y_limits) +  
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5),  
        axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.background = element_rect(color = "black", size = 1))  

# Create the second line plot with common y-axis limits
plot2 <- ggplot(long_df2, aes(x = domain, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  scale_color_manual(values = c("Estimated" = "blue", "Predicted" = "red")) + 
  labs(title = "Wave 5",
       x = "Strata",
       y = "Proportion(%)",
       color = "Wave") +  
  ylim(y_limits) +  
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5),  
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.background = element_rect(color = "black", size = 1))  

# Arrange plots side by side with the same y-axis scale
plot_grid(plot1, plot2, ncol = 2, rel_widths = c(1, 1))
