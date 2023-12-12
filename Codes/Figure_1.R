library(ggplot2)
library(ggsci)
library(dplyr)



reference_group_name<- read.csv('./wildfires_project/Data/non_affected_top25pop_only2020.csv')
indoor_allcounty<- read.csv('./wildfires_project/Data/indoor_activity_2018_2021_not_smoothed.csv')
reference_group <- merge(reference_group_name,indoor_allcounty, by = "county")




reference_group<- as.data.frame(reference_group)
reference_group$date<-as.Date(reference_group$date)
reference_group$county<-as.factor(reference_group$county)
colnames(reference_group)[colnames(reference_group) == "county"] <- "county_fips"



reference_group <-reference_group %>%
  group_by(date) %>%
  summarize(median = median(r), 
            ci_lower = quantile(r, 0.05), 
            ci_upper = quantile(r, 0.95))






new_ref_combine <- merge(merged_df_copy, reference_group, by= c('date'), all.x = TRUE)
new_ref_combine <- subset(new_ref_combine, County_Name != "Del Norte, CA")
new_ref_combine$county_fips<-as.character(new_ref_combine$county_fips)
new_ref_combine$county_fips[new_ref_combine$county_fips == "6053"] <- c("6063")
new_ref_combine$county_fips<-as.factor(new_ref_combine$county_fips)



new_ref_combine <- merge(new_ref_combine, date_aqi, by.x = c("county_fips", "date"), by.y = c("fips_code", "date"), all.x = TRUE)
new_ref_combine$County_Name <- recode(new_ref_combine$County_Name, "Monterey, CA" = "Plumas, CA")



new_ref_combine<-subset(new_ref_combine, date >= as.Date("2020-06-30") )



levels(new_ref_combine$County_Name)[levels(new_ref_combine$County_Name)=="King, WA (Seatle)"] <- "King, WA (Seattle)"




new_ref_combine<- new_ref_combine %>%
  mutate(County_Name = sub("(.*),\\s*(\\w{2})", "\\1 County, \\2", County_Name))



#change the sequence

new_ref_combine$County_Name <- factor(new_ref_combine$County_Name, levels = c(
  "Multnomah, OR", 
  "Washington, OR", 
  "Clackamas, OR (Portland)", 
  "Lane, OR", 
  "Marion, OR", 
  "King, WA (Seattle)", 
  "Spokane, WA", 
  "Yakima, WA", 
  "Clark, WA", 
  "Thurston, WA",
  "Plumas, CA", 
  "Del Norte, CA"  # Include other levels if they exist
))

###2023.12.4 use it
ggplot(new_ref_combine[new_ref_combine$state.x!='California',], aes(x = date, group = County_Name)) +
  geom_line(aes(y = r), color ='black', linetype = "solid", size = 1.5) +
  geom_line(aes(y = median), color = "#FFA500", linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_lower.y , ymax = ci_upper.y), fill = "#FFA500", alpha = 0.4) +  # Updated color and removed border
  geom_hline(yintercept = 1, color = "#808080") +
  geom_point(data = new_ref_combine[combined_df$r == 1, ], aes(y = r), color = "#808080") +
  geom_line(data = new_ref_combine[new_ref_combine$state.x!='California',], aes(y = AQI.index/1000+0.6), color = "#AD00A1", size = 1.5,) + 
  scale_y_continuous(sec.axis = sec_axis(~.*1000-500, name = "Air Quality Index (AQI)")) +
  facet_wrap(~ County_Name, ncol = 2, strip.position = "top", dir = "v") +
  labs(y = "Indoor activity seasonality", x='') +
  theme_bw() +
  theme(legend.position = 'NONE',
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        strip.background = element_blank(),
        strip.placement='outside',
        plot.title = element_text(size = 16, face = "bold"), 
        axis.text.x = element_text(size = 8, angle=15, hjust=1 , face = "bold"),
        axis.text.y = element_text(size = 10 , face = "bold"),  
        strip.text.x = element_text(size = 9,  face = "bold"), 
        strip.text.y = element_text(size = 8),
        axis.title=element_text(size=13),
        panel.spacing.x= unit(1, "lines"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.text.y.right = element_text(color = "#AD00A1"),
        axis.title.y.right = element_text(color = "#AD00A1")
        
) + scale_x_date(date_labels="%b %Y")





ggsave("./wildfires_project/Plots/my_plot.tiff", test, width = 7.2, height = 10, device='tiff', dpi=700)













\