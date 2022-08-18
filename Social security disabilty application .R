# Social security Disability case study 

# Load in the libraries that we'll need
pacman::p_load(tidyverse, lubridate, stringr)

### loading the dataset 
social_security <- read_csv("ssadisability.csv")

glimpse(social_security)
view(social_security)
attach(social_security)
### this data seems so wide so we can make it longer so we get a single data in each row 
social_security_longer <- pivot_longer(social_security, !Fiscal_Year, names_to = "month", 
                                       values_to = "applications")
view(social_security_longer)

### Formatting the dates, we look at the unique values of the months 
unique(social_security_longer$month)

### this seems like there is a problem here as the internet application and the 
### normal application are all combined together, we need to separate them 
social_security_longer <- social_security_longer %>% 
  separate(month, c("month", "application_method"), sep = "_")
view(social_security_longer)

### we now have to look at the unique variables in the month 
unique(social_security_longer$month)

### you realised that "May" "June" "July"  and  "August"  are written in full and this
### might cause a problem so we have to extract only the first three letter of all the 
### month 
social_security_longer <- social_security_longer %>% 
  mutate(month =  substr(month, start = 1, stop = 3))
view(social_security_longer)

### Lets also have a look at the unique values of the fiscal years 
unique(social_security_longer$Fiscal_Year)

### this looks likes the years are not in the standard formats so we will have to replace 
### all the FYs with 20
social_security_longer <-social_security_longer %>% 
  mutate(Fiscal_Year = str_replace(Fiscal_Year, pattern = "FY", replacement = "20"))
view(social_security_longer)

### Because the month and year are in a separate forms, we can combine them to be in
### in a single from using the paste function

social_security_longer$date <- dmy(paste('01', social_security_longer$month,
                                    social_security_longer$Fiscal_Year))
view(social_security_longer)
### this gives you a date to work with 

### Working with fiscal year and calender year, we need to convert the fiscal year to a calender year 

social_security_longer <- social_security_longer %>% 
  mutate(Fiscal_Year=as.numeric(Fiscal_Year)) %>%
  mutate(Fiscal_Year=ifelse(month(date)>=10, Fiscal_Year-1, Fiscal_Year)) %>%
  mutate(date=dmy(paste("01", month, Fiscal_Year)))

view(social_security_longer)

### Changing the name well 
social_security_longer <- social_security_longer %>%
  rename(Fiscal_Year = Fiscal_Year, Month = month, Application_methods = application_method,
         Applications = applications, Date = date) 


view(social_security_longer)
summary(social_security_longer)
attach(social_security_longer)
### We realized that the Fiscal year and the months are no more important so we get rid of them 
### also the Application method show that it is a character variable so we change it to factors
social_security_longer <- social_security_longer %>%
  select(-Fiscal_Year, -Month) %>%
  mutate(Application_methods = as.factor(Application_methods))
view(social_security_longer)
summary(social_security_longer)    

### everything looks perfect now

social_security <- pivot_wider(social_security_longer, names_from = Application_methods, 
                               values_from = Applications) %>% 
  select(Date, Internet, Total)
attach(social_security)
view(social_security)

### Looking at the percentage of people who applied online 
social_security <- social_security %>%
mutate(Online_Percentage = Internet / Total * 100)

view(social_security)

### Creating a scatter plot to see if internet application has grown over time
ggplot(data = social_security, mapping = aes(x = Date, y = Online_Percentage)) + 
  geom_point()

