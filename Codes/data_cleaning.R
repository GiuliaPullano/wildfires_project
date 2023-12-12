library(ggplot2)
library(dplyr)
date_aqi$date<-as.Date(date_aqi$date)
date_aqi$fips_code<-as.factor(date_aqi$fips_code)
date_aqi$state<-as.factor(date_aqi$state)

date_aqi<-read.csv('./wildfires_project/Data/data_AQI_2020.csv')




indoor_activity<- readxl::read_excel('./wildfires_project/Data/indoor_activity_2018_2021_not_smoothed.csv')

indoor_activity<- as.data.frame(indoor_activity)
indoor_activity$date<-as.Date(indoor_activity$date)
indoor_activity$group_name<-as.factor(indoor_activity$`group name`)
indoor_activity$county_fips<-as.factor(indoor_activity$county_fips)


indoor_activity<-indoor_activity%>%
  filter(date > '2020-06-01' & date < '2020-11-02')






# Filter the data for the group names starting with "hit"
# Filter the data for the group names starting with "hit"
indoor_activity <- indoor_activity[grep("^hit_", indoor_activity$group_name), ]

# Get unique county_fips values
county_fips_list <- unique(hit_data$county_fips)

# Create separate plots for each county
plot_list <- list()


for (county_fips in county_fips_list) {
  # Filter the data for the current county
  county_data <- hit_data[hit_data$county_fips == county_fips, ]
  
  # Create the plot for the current county
  plot <- ggplot(county_data, aes(x = date, y = r, color = group_name)) +
    geom_line() +
    labs(x = "Date", y = "r") +
    ggtitle(paste("County:", county_fips)) +
    
    theme_minimal()+
  # Add the time window
  geom_vline(xintercept = as.numeric(as.Date('2020-09-06')), color = 'red') +
    geom_vline(xintercept = as.numeric(as.Date('2020-09-15')), color = 'red') +
    geom_rect(xmin = as.numeric(as.Date('2020-09-06')), xmax = as.numeric(as.Date('2020-09-15')), ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.1)
  
  # Add the plot to the plot list
  plot_list[[county_fips]] <- plot
}

# Arrange the plots in a grid layout with three columns
grid_plot <- do.call(grid.arrange, c(plot_list, ncol = 3))


# Display the grid of plots
print(grid_plot)








ref_data <- indoor_activity[indoor_activity$group_name %in% c("ref_ca", "ref_or", "ref_wa"), ]

# Calculate the median values and confidence intervals by date and group
ref_summary <- ref_data %>%
  group_by( group_name, date) %>%
  summarise(
    median_r = median(r),
    ci_lower = quantile(r, 0.25),
    ci_upper = quantile(r, 0.75)
  )




ref_summary$group_name<-as.factor(ref_summary$group_name)
  


# Filter the data for the group names starting with "hit_or"
hit_or_data <- indoor_activity[grep("^hit_or", indoor_activity$group_name), ]

# Get unique county_fips in hit_or_data
county_fips_list <- unique(hit_or_data$county_fips)



county_df <- data.frame(
  FIPS_Code = c("6053", "6015", "41051", "41005", "41047", "41039", "41067", "53033", "53011", "53077", "53067", "53063"),
  County_Name = c("Monterey County", "Del Norte County", "Multnomah County", "Clackamas County", "Marion County", "Lane County", "Washington County", "King County", "Clark County", "Yakima County", "Thurston County", "Spokane County")
)

# Remove " County" from county names
county_df$County_Name <- gsub(" County", "", county_df$County_Name)


# Merge the county names with the merged_df dataframe
merged_df <- merge(merged_df, county_df, by.x = "county_fips", by.y = "FIPS_Code", all.x = TRUE)
merged_df$County_Name<-as.factor(merged_df$County_Name)


###



# Rename the column in the dataframe

###

# Create the plot
library(forcats)
state_names <- c("or" = "Oregon", "wa" = "Washington", "ca" = "California")
# Replace the abbreviated state names with full names
merged_df$state <- state_names[merged_df$state]
state_order <- c("Oregon", "Washington", "California")
merged_df$state <- factor(merged_df$state, levels = state_order)
# Reorder County_Name by state
county_order <- c("Multnomah", "Clackamas", "Marion", "Lane", "Washington", "King", "Clark", "Yakima", "Thurston", "Spokane", "Monterey", "Del Norte")

# Create a factor variable with the desired order
merged_df$County_Name <- factor(merged_df$County_Name, levels = county_order)

merged_df$County_Name1 <- recode(merged_df$County_Name,
                                "Multnomah(OR)"= "Multnomah, OR",
                                 "Clackamas(OR)"= 'Clackamas, OR (Portland)',
                                "Marion(OR)"="Marion, OR" ,
                                "Lane(OR)"="Lane, OR",
                                "Washington(OR)"= "Washington, OR",
                                "King(WA)"= "King, WA (Seatle)",
                                 "Clark(WA)"=  "Clark, WA",
                                  "Yakima(WA)"="Yakima, WA",
                                "Thurston(WA)"= "Thurston, WA" ,
                                 "Spokane(WA)"= "Spokane, WA",
                                 "Monterey(CA)"="Monterey, CA",
                                "Del Norte(CA)" = "Del Norte, CA")

merged_df$County_Name<-merged_df$County_Name1 

##UPDATE THE REFEreNCE
merged_df <- merged_df %>%
  mutate(
    median_r = ifelse(county_fips == '6015', ref_norte$median_r[match(date, ref_norte$date)], median_r),
    ci_lower = ifelse(county_fips == '6015', ref_norte$ci_lower[match(date, ref_norte$date)], ci_lower),
    ci_upper = ifelse(county_fips == '6015', ref_norte$ci_upper[match(date, ref_norte$date)], ci_upper)
  )




merged_df_copy<-merged_df

