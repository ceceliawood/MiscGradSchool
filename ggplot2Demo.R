# Plotting in R (ggplot2) Demo
# Cece Wood
# 18 Feb 2026


# our first step is to load in any libraries we'll need
# this lets us use pre-made functions

pacman::p_load(dplyr, tidyr, readxl, readr, ggplot2, ggpubr)

# the p_load function installs and calls all listed packages


# we'll be using the mtcars data - this is example data that's included with RStudio
# use " <- " to assign the data to a variable
df1 <- mtcars

# let's look at the data frame
view(df1)

# let's look at the structure of the data frame
str(df1)

# we want the names of the make and model to be a column for plotting
# let's make a new variable and use a function to add a new column called 'MakeModel'

df2 <- df1 %>% 
  rownames_to_column(var = 'MakeModel')
view(df2)
str(df2)


# time to plot! let's make a plot with horsepower (hp) on the x-axis and mpg on the y-axis
help(plot)
plot(df2$hp, df2$mpg)

# let's change the color and the shape
plot(df2$hp, df2$mpg, col = 'blue', pch = 16)

# what if we want to see a few plots at once?
par(mfrow = c(2,2))
plot(df2$hp, df2$mpg, col = 'blue', pch = 16)
plot(df2$wt, df2$mpg, col = 'red', pch = 15)
plot(df2$gear, df2$mpg, col = 'darkgreen', pch = 17)
plot(df2$cyl, df2$mpg, col = 'darkorange', pch = 18)

# return back to 1 plot view
par(mfrow = c(1,1))


# these kinds of plots are great for quick checks on your data! But they're not as useful
# for complex data and plots...
# enter ggplot!

# the syntax for ggplot is different from base R plotting
ggplot(df2) +
  geom_point(aes(hp, mpg))

# you can make quick changes to how your plot looks by adding a theme
ggplot(df2) + theme_classic() +
  geom_point(aes(hp, mpg))

ggplot(df2) + theme_bw() +
  geom_point(aes(hp, mpg))

ggplot(df2) + theme_dark() +
  geom_point(aes(hp, mpg))

# we can also start to add dimension to the plot
ggplot(df2) + theme_bw() +
  geom_point(aes(hp, mpg, color = wt))

ggplot(df2) + theme_bw() +
  geom_point(aes(hp, mpg, shape = as.factor(cyl)))

ggplot(df2) + theme_bw() +
  geom_point(aes(hp, mpg, size = as.factor(gear)), shape = 1, color = 'red')

# we can also add lines of best fit, threshold lines, etc
ggplot(df2) + theme_bw() +
  geom_hline(yintercept = 20) +
  geom_smooth(aes(hp, mpg), method = 'lm', se = FALSE, linetype = 2) +
  geom_point(aes(hp, mpg, size = as.factor(gear)), shape = 1, color = 'red')


# as you can see, ggplot has a lot more options and tends to look more polished
# but before we get into the final touches of making a complete figure, let's go
# over the basics of data cleaning and formatting

metals_raw <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5")
view(metals_raw)
str(metals_raw)
summary(metals_raw)

# this data set is really large! And it contains a lot of information that we might not
# care that much about. For instance, perhaps we're only interested in metals data from 
# 2023. Or perhaps we're only interested in plotting the major cations. We can filter 
# the data to only see what we want

metals_2023 <- metals_raw %>% 
  filter(between(DateTime, as.Date('2023-01-01'), as.Date('2023-12-31')))
summary(metals_2023)

metals_majorCations <- metals_raw %>% 
  select(Reservoir, DateTime, Site, Depth_m, TNa_mgL, SNa_mgL, TMg_mgL, SMg_mgL,
         TK_mgL, SK_mgL, TCa_mgL, SCa_mgL) %>% 
  na.omit() %>% 
  mutate(Year = year(DateTime),
         Date = date(DateTime))

# we can plot individual metals easily when the data is in a wide format
metals_majorCations %>%
  filter(Site == 50,
         Reservoir != 'CCR') %>% 
  ggplot() + scale_x_date(date_breaks = '3 months', date_labels = '%b') +
  facet_grid(cols = vars(Year), rows = vars(Reservoir), scales = 'free', space = "free_x") +
  geom_point(aes(Date, TCa_mgL))

# but it's hard to plot all the metals at once! It's a lot of code and there's
# no legend on the plot
metals_majorCations %>%
  filter(Site == 50,
         Reservoir != 'CCR',
         Depth_m == 9) %>% 
  ggplot() + scale_x_date(date_breaks = '3 months', date_labels = '%b') +
  facet_grid(cols = vars(Year), rows = vars(Reservoir), scales = 'free', space = "free_x") +
  geom_point(aes(Date, TCa_mgL), color = 'red') +
  geom_point(aes(Date, TMg_mgL), color = 'blue') +
  geom_point(aes(Date, TK_mgL), color = 'darkgreen') +
  geom_point(aes(Date, TNa_mgL), color = 'darkorange')

# to make things easier, we will change the data to a long format
majorCats_long <- metals_majorCations %>% 
  filter(Site == 50,
         Reservoir != 'CCR') %>% 
  pivot_longer(cols = TNa_mgL:SCa_mgL, names_to = 'Element',
               values_to = 'Concentration_mgL')
view(majorCats_long)

majorCats_long %>%
  filter(Depth_m == 9) %>% 
  ggplot() + scale_x_date(date_breaks = '3 months', date_labels = '%b') +
  facet_grid(cols = vars(Year), rows = vars(Reservoir), scales = 'free', space = "free_x") +
  geom_point(aes(Date, Concentration_mgL, color = Element))

# what if we only want to plot the totals?
majorCats_long %>% 
  mutate(Filter = if_else(grepl('T', Element), 'Total', 'Soluble')) %>% 
  filter(Filter == 'Total',
         Depth_m == 9) %>%
  ggplot() + scale_x_date(date_breaks = '3 months', date_labels = '%b') +
  facet_grid(cols = vars(Year), rows = vars(Reservoir), scales = 'free', space = "free_x") +
  geom_point(aes(Date, Concentration_mgL, color = Element))

# let's refine the plot a little more  
  
majorCats_long %>% 
  mutate(Filter = if_else(grepl('T', Element), 'Total', 'Soluble'),
         Element = factor(Element,
                          levels = c('TCa_mgL', 'TK_mgL', 'TMg_mgL', 'TNa_mgL'),
                          labels = c('Calcium', 'Potassium', 'Magnesium', 'Sodium'))) %>% 
  filter(Filter == 'Total',
         Depth_m == 9) %>%
  ggplot() + theme_bw() +
  scale_x_date(date_breaks = '3 months', date_labels = '%b') +
  facet_grid(cols = vars(Year), rows = vars(Reservoir), scales = 'free', space = "free_x") +
  geom_point(aes(Date, Concentration_mgL, color = Element)) +
  ylab('Concentration (mg/L)') + ggtitle('Major Cations in FCR and BVR') +
  theme(legend.title = element_blank())

ggsave(filename = 'TotalConc_2020_2024.png', height = 4, width = 8, units = 'in',
       dpi = 300)

# resources!
# https://r-graphics.org/
# https://ggplot2-book.org/
# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.455.9