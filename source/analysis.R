library(tidyverse)
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# The functions might be useful for A4


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function(HW) {
  return ("Hello world")
}
test_query1(HW)
# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}
test_query2()
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

Incarceration_ratios <- incarceration_trends %>% select(year, state, county_name, region, white_jail_pop, white_pop_15to64, black_pop_15to64, black_jail_pop, total_jail_pop, total_prison_pop) 
Incarceration_ratios <- na.omit(Incarceration_ratios)
Prison_Totals <- data.frame(Incarceration_ratios)
Prison_Totals <- Prison_Totals %>% 
rename(Prisoners = total_prison_pop) %>% 
  select(year, Prisoners)



the_states <- data.frame(Incarceration_ratios)
the_states <- filter(the_states, state %in% c("WA", "PA", "IL", "MS"))
the_states
df4 <- select(the_states, year, state, total_prison_pop) 
  group_by(the_states, year) 
  summarise(the_states, total_prison_pop)




wa_comp <- incarceration_trends
wa_comp <- select(wa_comp, year, county_name, state, black_jail_pop, white_jail_pop, aapi_jail_pop, latinx_jail_pop) 
wa_comp <- wa_comp %>% 
na.omit() %>% 
filter(state == "WA") %>%
filter(year == 2018) %>%
  group_by(county_name) %>% 
mutate(black_jail_pop = max(black_jail_pop),
       white_jail_pop = max(white_jail_pop),
       aapi_jail_pop = max(aapi_jail_pop),
       latinx_jail_pop = max(latinx_jail_pop))
  wa_county_max <- wa_comp %>% 
  select(county_name, white_jail_pop, black_jail_pop, aapi_jail_pop, latinx_jail_pop)  
wa_county_max <- gather(wa_county_max, key = "races", value = "CountyMax",  -county_name)




county_comp <- incarceration_trends
county_comp <- county_comp %>% 
  select(state, county_name, total_pop_15to64, black_pop_15to64, white_pop_15to64, black_jail_pop, white_jail_pop, total_jail_pop) %>% 
  filter(state == "AL") %>% 
  na.omit() %>% 
  mutate(black_15to64_ratio = black_pop_15to64/total_pop_15to64,
         white_15to64_ratio = white_pop_15to64/total_pop_15to64, 
         black_jail_ratio = black_jail_pop/total_jail_pop,
         white_jail_ratio = white_jail_pop/total_jail_pop) 



AL_Comp <- county_comp %>%   
  select(county_name, black_jail_ratio, black_15to64_ratio, white_jail_ratio, white_15to64_ratio) %>% 
  group_by(county_name) %>% 
  mutate(black_jail_ratio = mean(black_jail_ratio),
         black_15to64_ratio = mean(black_15to64_ratio), 
         white_15to64_ratio = mean(white_15to64_ratio), 
         white_jail_ratio = mean(white_jail_ratio)) %>% 
  mutate(black_equality = black_jail_ratio/black_15to64_ratio, 
         white_equality = white_jail_ratio/white_15to64_ratio)%>%
  select(black_equality, county_name)%>%
  distinct() %>% 
  mutate(county_name = gsub("County", "", county_name)) 
 


state_shape <- map_data("county") %>% 
  filter(region == "alabama") 
state_shape1 <- full_join(state_shape, AL_Comp, by = character())


  




  ## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function(f3) {
df3 <- Prison_Totals %>% 
  group_by(year) %>%
  summarise(Prisoners) %>% 
  arrange(year) %>% 
return(df3)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(df3)  {
chart3 <- ggplot(Prison_Totals) +
  geom_col(mapping = aes(x = year, y = sum(Prisoners))) +
  labs(
    title = "Growth of the U.S. Prison Population", 
    x = "Year", 
    y = "Number of Prisoners",
    color = "Red" 
  )
   return(chart3)   
} 

chart3 <- plot_jail_pop_for_us(df3)
chart3


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


states_in_region <- function(f4) {
  df4 <- select(the_states, year, state, total_prison_pop) 
  group_by(the_states, year) 
  summarise(the_states, total_prison_pop)
  return(df4)
}

plot_jail_pop_by_states <- function(df4) {
    chart4 <- ggplot(the_states)+
      aes(x=year, y=total_prison_pop, color=state, fill=state) +
    geom_col(position = "dodge", width = .5) +
    ggtitle("State Prison Populations") +
      labs(
        title = "Prison Populations from 1990 to Present", 
        x = "Year", 
        y = "Number of Incarcerated",
      )
return(chart4)
}
chart4 <- plot_jail_pop_by_states(df4)
chart4
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_race <- function(f5){


    
    df5 <- wa_comp %>% 
    select(county_name, white_jail_pop, black_jail_pop, aapi_jail_pop, latinx_jail_pop)  
df5 <- gather(df5, key = "races", value = "CountyMax",  -county_name)
  
  
  return(df5)
}
 

plot_reg_comparison <- function(wa_county_max){
 chart5 <- ggplot(wa_county_max) +
  geom_point(mapping = aes(x = county_name, y = CountyMax, color = races, fill = races)) +
   aes(position = "dodge", width = .5)+
   coord_flip()+
   labs(
     title = "2018 Jail Maxes by Race in Washington", 
     y = "Number Jailed", 
     x = "County",
   )
return(chart5)

}

plot5 <- plot_reg_comparison(wa_county_max)
plot5

  ## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
Alabama_Map <- function(df6){
  county_comp <- incarceration_trends
  county_comp <- county_comp %>% 
    select(state, county_name, total_pop_15to64, black_pop_15to64, white_pop_15to64, black_jail_pop, white_jail_pop, total_jail_pop) %>% 
    filter(state == "AL") %>% 
    na.omit() %>% 
    mutate(black_15to64_ratio = black_pop_15to64/total_pop_15to64,
           white_15to64_ratio = white_pop_15to64/total_pop_15to64, 
           black_jail_ratio = black_jail_pop/total_jail_pop,
           white_jail_ratio = white_jail_pop/total_jail_pop) 
  
  
  
  AL_Comp <- county_comp %>%   
    select(county_name, black_jail_ratio, black_15to64_ratio, white_jail_ratio, white_15to64_ratio) %>% 
    group_by(county_name) %>% 
    
    mutate(black_jail_ratio = mean(black_jail_ratio),
           black_15to64_ratio = mean(black_15to64_ratio), 
           white_15to64_ratio = mean(white_15to64_ratio), 
           white_jail_ratio = mean(white_jail_ratio)) %>% 
    mutate(black_equality = black_jail_ratio/black_15to64_ratio, 
           white_equality = white_jail_ratio/white_15to64_ratio)%>%
    select(black_equality, county_name)%>%
    #gather(key = "races", value = "ratio", -county_name) %>% 
    #distinct() %>% 
    mutate(county_name = gsub("County", "", county_name)) %>% 
  AL_Comp$county_name <- tolower(AL_Comp$county_name) %>% 
  unite(col='Black Equality', c('black_equality', 'county_name'), sep=' ')
  
  
  state_shape <- map_data("county") %>% 
    filter(region == "alabama") %>% 
    state_shape1 <- left_join(state_shape, AL_Comp, by = "black_equality" )
  
  
  
  
return(state_shape)
}

Alabama_chart <- function(AL_chart){
  chart6 <- ggplot(state_shape1) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_equality),
    color = "black", 
    size = .1        
  ) +
  coord_map()+
    labs(
      title = "Black Equality in Alabama Counties"
    )+
    scale_fill_gradient(low = "white", high = "red")
return(chart6)
  }

plot6 <- Alabama_chart(AL_chart)
plot6
## Load data frame ---- 
