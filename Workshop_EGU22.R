# Call the libraries that we will use further.
# Handle geospatial data
library(raster) # reading, writing, manipulating, analyzing of spatial data
library(sf) # work with simple features
# Plot data
library(ggplot2) # creating graphics
library(ggspatial) # framework for interacting with spatial data using ggplot2 
library(cowplot) # add-on to ggplot, provides set of themes, functions to align plots
library(tmap) # create thematic maps
library(tmaptools) # facilitates 'tmap'
library(viridis) # provide a series of color maps
# Data mutation
library(stringr) #  string manipulations
library(dplyr) # grammar of data manipulation
library(stats) #  statistical calculations and random number generation
library(tidyr) # contains tools for changing the shape and hierarchy of dataset


# Set working directory
setwd("//teamwork.org.aalto.fi/T213/T21301/T20507/Remotesensing/PEATSPEC/Conferences/EGU22_Workshop_OPTRAM_IB")


# Sentinel-2 data covering peatland Mannikjarve in Estonia were downloaded from 
# Google Earth Engine for the vegetation period in 2018 (May - September). 
# In Google Earth Engine, we already performed cloud masking, STR and NDVI 
# calculations. Overall, we downloaded 36 images from that period. 
# The studied period was abnormally dry, with heatwaves that highly affected 
# the moisture conditions in Estonian peatlands.
# Now we will plot NDVI and STR values for one date - 03/03/2018.

# Open downloaded Sentinel-2 NDVI data
NDVI_raster <- raster("S2 image/20180303T093029_20180303T093028_T35VMF_NDVI.tif")
NDVI_raster # see the properties of NDVI RasterLayer

# Look at the variable name
names(NDVI_raster)

# Change the long name "X20180303T093029_20180303T093028_T35VMF_NDVI" to 
# more understandable "NDVI"
names(NDVI_raster)<- "NDVI"
names(NDVI_raster)

# Open downloaded Sentinel-2 STR data
STR_raster <- raster("S2 image/20180303T093029_20180303T093028_T35VMF_STR.tif")
STR_raster

# Change the long name to "STR"
names(STR_raster)
names(STR_raster)<- "STR"

# Open the shapefile with peatland boundaries
peatland_st <- st_read("shp/EE_MAN_4326.shp") 
peatland_st # the Bounding box parameters are shown in degrees

# Check whether coordinate projections of raster data (STR and NDVI) is the same 
# as for the vector data
st_crs(peatland_st) == st_crs(STR_raster) # FALSE means that their projections are not similar
peatland_st <- st_transform (peatland_st, st_crs(STR_raster))# transform reference system of peatland_st
st_crs(peatland_st) == st_crs(STR_raster) # TRUE

# We will test several packages for plotting.
# First plot will be created with base R function "plot"
plot (STR_raster) # plot STR raster
plot(peatland_st, border="black", add=TRUE, color = NaN, lwd = 2)# add polygon

# Second plot is created with tmap package
tm_shape(STR_raster)+
  tm_raster(style = "order", # classification method for data binning
            palette = "-BuPu",
            n = 3,# number of classes
            legend.reverse = "TRUE") +
  tm_layout(legend.position = c(0.8, .2),  
            scale=1)+
  tm_scale_bar(position = c(0.1, .01),
               breaks = c(0, 0.2),
               text.size = 1)+
  tm_shape (peatland_st)+
  tm_borders(col = "black", lwd = 2)

# Third plot create with ggplot
# ggplot requires data transformation from raster file to the data frame.
STR_spdf <- as(STR_raster, "SpatialPixelsDataFrame") # define spatial grid by offset, cell size and dimensions
STR_df <- as.data.frame(STR_spdf)

str(STR_raster) # the initial structure of raster layer
str(STR_df) # the structure of data frame
head(STR_df, 10) # check the top 10 rows in the new data frame

# Plot transformed STR_df data
fig_STR <- ggplot()+
  # plot raster file and peatland boundary
  geom_raster(data=STR_df, aes(x=x, y=y, fill=STR))+
  geom_sf(data = peatland_st, 
          fill = NA, # no inner fill
          colour = "black", # boundary colour
          size=2)+
  # set the colour palette for raster file
    scale_fill_gradientn(colours  = c("#005b8a", "#e5feff", "#fdb586", 
                                      "#f23333", "#cf001b", "#ac0002", 
                                      "#8b0000", "#6e031e",  "#6b0000"),
                       # brakes in the gradient legend
                       breaks = c(4,6,8, 10, 15), 
                       # set the legend location and other parameters
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              # set the title above the legend
                                              # and in the middle
                                              title.position = 'top',
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))+
  #set theme parameters
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17))+
  # add scale and north arrow if needed
  annotation_scale(location = "bl", 
                   style = "ticks",
                   text_cex = 1.2) +
  annotation_north_arrow(width = unit(0.5, "cm"), 
                         location = "tl")+
  # set the coordinate referances of the map 
  coord_sf(datum=st_crs(32635))+ 
  # set the breaks of the map net
  scale_x_continuous(breaks = seq(round(min(STR_df$x), digits = -3),
                                  round(max(STR_df$x), digits = -3),
                                  500))+
  scale_y_continuous(breaks = seq(round(min(STR_df$y), digits = -3),
                                  round(max(STR_df$y), digits = -3),
                                  500))
fig_STR

# Transform and plot NDVI data
NDVI_spdf <- as(NDVI_raster, "SpatialPixelsDataFrame")
NDVI_df <- as.data.frame(NDVI_spdf)
str(NDVI_df)

fig_NDVI <- ggplot()+  
  # plot raster file and peatland boundary
  geom_raster(data=NDVI_df, aes(x=x, y=y, fill=NDVI))+
  geom_sf(data = peatland_st, 
          fill = NA, 
          colour = "black", 
          size=2)+
  # set the colour palette for raster file
  scale_fill_gradientn(colours  = c("#FCFDE1", "#467C46", "#040B0B"), 
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              title.position = 'top',
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))+
  #set theme parameters
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position="bottom",
        legend.key.width=unit(1,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text=element_text(size=15),
        legend.title = element_text(size=17))+
  # add scale and north arrow if needed
  annotation_scale(location = "bl", 
                   style = "ticks",
                   text_cex = 1.2)  +
  annotation_north_arrow(width = unit(0.5, "cm"), 
                         location = "tl")+
  # set the coordinate referances of the map 
  coord_sf(datum=st_crs(32635))+ 
  # set the breaks of the map net
  scale_x_continuous(breaks = seq(round(min(NDVI_df$x), digits = -3),
                                  round(max(NDVI_df$x), digits = -3),
                                  500))+
  scale_y_continuous(breaks = seq(round(min(NDVI_df$y), digits = -3),
                                  round(max(NDVI_df$y), digits = -3),
                                  500))
fig_NDVI

# Plot both STR and NDVI rasters
plot_grid(fig_STR, fig_NDVI, ncol = 2, nrow = 1)

# Sentinel-2 data were downloaded from Google Earth Engine as .tif files and 
# later converted to table format. This is time-consuming, so we have already 
# prepared converted data for you. Nevertheless, the code to convert Sentinel-2
# data is given below (no need to run it).


# Code to convert STR raster data to csv. The same algorithm should be applied 
# for NDVI data

# files <- list.files(path="S2 image/", full.names=TRUE, pattern="*_STR.tif")
# lapply(files , function(x) {
#   t <- raster
#   filename <- names(t)
#   d<- cbind(coordinates(t), v=values(t))
#   df<-as.data.frame(d)
#   name <-names(t)[1] # load file
#   name <-str_sub(name, 2,9)
#   date <- as.Date(name , format="%Y%m%d")
#   df$date<-date 
#   names(df)[names(df) == "v"] <- "STR"
#   write.csv(df, file=paste0("your path",filename,".csv"))
# })

# The converted csv files can be found in the folder "S2 data in csv"
STR_filename <- list.files("S2 data in csv/", pattern="*_STR.csv", full.names=TRUE, recursive=FALSE)
STR_filename # 36 csv files were created for 36 Sentinel-2 tif files

# Read each file and join them all together
STR_tables <- Map(cbind, lapply(STR_filename, data.table::fread, sep=","))
STR_table <- do.call(rbind, lapply(STR_tables, subset))
head (STR_table, 10) # see the top 10 rows of the table
unique (STR_table$Date) # see the dates with data

# Repeat the same algorithm for csv file with NDVI data
NDVI_filename <- list.files("S2 data in csv/", pattern="*_NDVI.csv", full.names=TRUE, recursive=FALSE)
NDVI_filename 
NDVI_tables <- Map(cbind, lapply(NDVI_filename, data.table::fread, sep=","))
NDVI_table <- do.call(rbind, lapply(NDVI_tables, subset))
head (NDVI_table, 10) # see the top 10 rows of the table

# Join the tables STR_table and NDVI_table by x, y, and Date, 
# since every pixel (with unique x- and y-coordinates) for each Date 
# has both STR and NDVI values. 
S2_table <- full_join(STR_table, NDVI_table, by=c("x", "y", "Date"))
str(S2_table)

# We can remove some columns that we don't need ("V1.x" and "V1.y")
S2_table <- S2_table %>% dplyr:: select(-"V1.x", -"V1.y")

# Convert Date column to the date format
S2_table$Date <- as.Date(S2_table$Date, format = "%Y-%m-%d")

# Now we have the table we can further use for OPTRAM calculation. 
# Every pixel has NDVI and STR values for each date in this table.
summary(S2_table)

# # Let's see the time series of NDVI and STR values for one pixel. 
# For this, we need to know the x and y parameters of the pixel we want to plot. 
# You can choose the random pixel from S2_table. Another option is to plot 
# time series of the pixel with the maximum number of observation days.
# So let's calculate how many observations are present in S2_table for 
# each pixel (unique x and y)
n_table <- S2_table %>% group_by(x, y)%>% # group by unique x and y values
                        summarise(n_obs =n ())

summary(n_table) # min is 18 observations and max is 27
max(n_table$n_obs)

# From n_table select pixels that have the maximum number of observations
n_table <- n_table %>% filter(n_obs== max(n_table$n_obs))
n_table # there are 21 pixels with 27 observations
n_max <- n_table[1,] # select the first pixel among these 21 pixels

# Plot NDVI and STR time series of the selected pixel
fig_TS_S2 <- ggplot(subset(S2_table, x %in% n_max$x  & y %in% n_max$y ))+
  # add STR 
  geom_point(aes( x= Date, y=STR), 
             color= "#FF8D29", # show STR with orange colour
             size=3.5, 
             alpha=0.9)+ # alpha set the transparency (0 - transparent, 1 - opaque) 
  geom_line(aes( x= Date, y=STR), 
            color= "#FF8D29", 
            size=2, 
            alpha=0.1)+
  # add NDVI
  geom_point(aes( x= Date, y=NDVI*10), # scale NDVI to show it on one plot with STR
             color= "#8B9A46", # show NDVI with green colour
             size=3.5, 
             alpha=0.9)+
  geom_line(aes( x= Date, y=NDVI*10), 
            color= "#8B9A46", 
            size=2, 
            alpha=0.1)+
  # Set the axis
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m/%Y")+
  scale_y_continuous(name = "STR", 
                     # show NDVI on the secondary axis
                     sec.axis = sec_axis(~./10, name= "NDVI"))+
  # theme parameters
  theme_bw()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=10),
        # right axis
        axis.line.y.right = element_line(color = "#8B9A46"), 
        axis.ticks.y.right = element_line(color = "#8B9A46"),
        axis.text.y.right = element_text(color = "#8B9A46"), 
        axis.title.y.right = element_text(color = "#8B9A46"),
        # left axis
        axis.line.y.left = element_line(color = "#FF8D29"), 
        axis.ticks.y.left = element_line(color = "#FF8D29"),
        axis.text.y.left = element_text(color = "#FF8D29"), 
        axis.title.y.left = element_text(color = "#FF8D29")
        )
fig_TS_S2  

# OPTRAM is calculated based on NDVI-STR space. Let's see how this space 
# looks like for our data.
fig_NDVIvsSTR <- ggplot(S2_table, aes(x=NDVI, y=STR))+
  geom_point(alpha = 1/80)+
  theme_bw()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=10))
fig_NDVIvsSTR

# Based on the observed NDVI-STR cloud, we can set the parameters 
# for wet and dry edges
minimal_ndvi <- 0.4 # min NDVI value 
maximal_ndvi <- 0.9 # max NDVI value 
sub_number <- 10 # number of subintervals in each interval
step <- 0.001 #  step for intervals
max_i <- (maximal_ndvi - minimal_ndvi) / step
print(max_i)
total_int_number <- max_i / sub_number # number of intervals


# Wet edge
# Within the NDVI-STR space we derive the max STR value for each NDVI subinterval
# This max STR value is arranged with the median NDVI value of each NDVI subinterval
median_ndvi_intervals_we <- list()
max_str_intervals_we <- list()

for (i in 1:max_i) {
  # find the max and max NDVI for each subinterval
  current_low <- minimal_ndvi + step*(i-1)*(maximal_ndvi - minimal_ndvi)
  current_high <- minimal_ndvi + step*i*(maximal_ndvi - minimal_ndvi)
  
  # filter data that belong to the subinterval
  current_df <- S2_table[(S2_table$NDVI < current_high) & 
                           (S2_table$NDVI >= current_low),]
  
  # derive the max STR within the subinterval 
  max_str <- max(current_df$STR)
  max_str_intervals_we[[length(max_str_intervals_we)+1]] <- max_str
  
  # derive the median NDVI value of the subinterval
  current_median_ndvi <- median(current_df$NDVI)
  median_ndvi_intervals_we[[length(median_ndvi_intervals_we)+1]] <- current_median_ndvi
}

# max STR values within each NDVI subinterval
print(unlist(max_str_intervals_we))
subinterval_data_we <- data.frame(STR=unlist(max_str_intervals_we), 
                                  NDVI=unlist(median_ndvi_intervals_we))

# Each NDVI interval has subintervals within which we derived max STR values.
# Now, within each interval we calculate the median and std of mxx STR values.
# Within each interval, we filter out max STR values that are bigger than 
# median max STR + std max STR. The remained max STR values are averaged 
# (median) and associated with the median NDVI value within each interval.
filtered_max_str_we <- list()
filtered_median_ndvi_we <- list()

for (i in 1:total_int_number) {
  current_data_chunk <- subinterval_data_we[round((i-1)*sub_number, 0):round(i*sub_number, 0),]
  
  # Within each interval find the max STR values that is lower than 
  # median max STR + std max STR
  str_threshold <- median(current_data_chunk$STR) + sd(current_data_chunk$STR)
  
  # Filter based on this condition
  filtered_data <- current_data_chunk[current_data_chunk$STR < str_threshold,]
  
  # Average remained max STR values for each interval and calculate 
  # median NDVI within each interval
  filtered_max_str_we[[length(filtered_max_str_we)+1]] <- median(filtered_data$STR)
  filtered_median_ndvi_we[[length(filtered_median_ndvi_we)+1]] <- median(filtered_data$NDVI)
}

# NDVI and STR values that are used for further dry edge estimation
print(unlist(filtered_max_str_we))

# Linear model with all the max STR and NDVI values
interval_data <- data.frame(STR=unlist(filtered_max_str_we), 
                            NDVI=unlist(filtered_median_ndvi_we))
relation_wetedge <- lm(STR~NDVI, data=interval_data)
coef(relation_wetedge) # coefficients of the linear model

intercept_we <- coef(relation_wetedge)["(Intercept)"]
intercept_we
slope_we <- coef(relation_wetedge)["NDVI"]
slope_we

# Dry edge
# A similar algorithm is applied for dry edge estimation. 
# For the dry edge, we need to derive the min STR value for each NDVI interval.
# This min STR value is arranged with the median NDVI value of each NDVI subinterval
median_ndvi_intervals_de <- list()
min_str_intervals_de <- list()

for (i in 1:max_i) {
  current_low <- minimal_ndvi + step*(i-1)*(maximal_ndvi - minimal_ndvi)
  current_high <- minimal_ndvi + step*i*(maximal_ndvi - minimal_ndvi)
  
  # filter data that belong to the subinterval
  current_df <- S2_table[(S2_table$NDVI < current_high) & (S2_table$NDVI >= current_low),]
  
  # derive the min STR within the subinterval 
  min_str <- min(current_df$STR)
  min_str_intervals_de[[length(min_str_intervals_de)+1]] <- min_str
  
  # derive the median NDVI value of the  subinterval
  current_median_ndvi <- median(current_df$NDVI)
  median_ndvi_intervals_de[[length(median_ndvi_intervals_de)+1]] <- current_median_ndvi
}

# min STR values within each NDVI subinterval
print(unlist(min_str_intervals_de))
subinterval_data_de <- data.frame(STR=unlist(min_str_intervals_de), 
                                  NDVI=unlist(median_ndvi_intervals_de))

# Each NDVI interval has subintervals within which we derived min STR values.
# Now, within each interval we calculate the median and std of min STR values.
# Within each interval, we filter out min STR values that are bigger than median 
# min STR - std min STR. The remained min STR values are averaged (median) and 
# associated with the median NDVI value within each interval.
filtered_min_str_de <- list()
filtered_median_ndvi_de <- list()

for (i in 1:total_int_number) {
  current_data_chunk <- subinterval_data_de[round((i-1)*sub_number, 0):round(i*sub_number, 0),]
  
  # Within each interval find the max STR values that is lower than median min STR + std min STR
  str_threshold <- median(current_data_chunk$STR) - sd(current_data_chunk$STR)
  
  # Filter based on this condition
  filtered_data <- current_data_chunk[current_data_chunk$STR > str_threshold,]
  
  # Average remained min STR values for each interval and calculate median NDVI within each interval
  filtered_min_str_de[[length(filtered_min_str_de)+1]] <- median(filtered_data$STR)
  filtered_median_ndvi_de[[length(filtered_median_ndvi_de)+1]] <- median(filtered_data$NDVI)
}

# NDVI and STR values that are used for further dry edge estimation
print(unlist(filtered_min_str_de))

# PARAMETERS FOR DRY EDGE
# Linear model with all the min STR and NDVI values
interval_data <- data.frame(STR=unlist(filtered_min_str_de), NDVI=unlist(filtered_median_ndvi_de))
relation_dryedge <- lm(STR~NDVI, data=interval_data)
coef(relation_dryedge)

intercept_de <- coef(relation_dryedge)["(Intercept)"]
intercept_de
slope_de <- coef(relation_dryedge)["NDVI"]
slope_de

# Plot dry and wet edges 
fig_NDVIvsSTR_edges<- fig_NDVIvsSTR +
  # Wet edge
  geom_abline(intercept = intercept_we, 
              slope = slope_we, 
              color = "#2E94B9",
              size = 2)+
  # Dry edge
  geom_abline(intercept = intercept_de, 
              slope = slope_de, 
              color = "#FD5959",
              size = 2)
fig_NDVIvsSTR_edges

# OPTRAM calculation
S2_table$OPTRAM <- (S2_table$STR - (intercept_de + S2_table$NDVI * slope_de)) /
                   ((intercept_we + slope_we * S2_table$NDVI)-(intercept_de + slope_de * S2_table$NDVI ))
summary(S2_table)

S2_table$OPTRAM [S2_table$OPTRAM >1] <- NaN   
S2_table$OPTRAM [S2_table$OPTRAM <0] <- NaN 

ggplot(data = S2_table, aes(x = NDVI, y = STR, color = OPTRAM))+
  geom_point(alpha = 1/30)+
  # Wet edge
  geom_abline(intercept = intercept_we, 
              slope = slope_we, 
              color = "#2E94B9",
              size = 2)+
  # Dry edge
  geom_abline(intercept = intercept_de, 
              slope = slope_de, 
              color = "#FD5959",
              size = 2)+
  # Set gradient color
  scale_color_gradient(low="#FD5959",
                       high="#2E94B9")+
  # Set theme
  theme_bw()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15))
  
# Read WTD data
WTD_table <- read.csv ("WTD data/EE_MAN.csv")
str(WTD_table)
summary(WTD_table)

WTD_table$Date <- as.Date(WTD_table$Date, format = "%d/%m/%Y")

str(WTD_table)

fig_TS_WTD <- ggplot(WTD_table, aes(x = Date, y = WTD))+
  geom_ribbon(aes(ymin = min(WTD,na.rm = TRUE)-1 , ymax= WTD),
              fill="#7FB5FF", alpha = 0.7)+
  theme_bw()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y",
               limits = c(as.Date("1/05/2018", format = "%d/%m/%Y"),
                      as.Date("1/09/2018", format = "%d/%m/%Y")))
fig_TS_WTD

plot_grid(fig_TS_S2, fig_TS_WTD, ncol = 1, nrow = 2, align = "v")


# Join WTD and S2 data
data_table <- full_join(WTD_table, S2_table, by = "Date")
str(data_table)

# Before we calculate correlation betweeb OPTRAM and WTD, we need to filter out 
# pixels with several values, since cor.test will result in error
data_table_n <- data_table %>%
  group_by(x,y) %>%
  mutate(n_obs = n()) %>%
  ungroup()

summary(data_table_n)
data_table_n <- data_table_n %>% filter (n_obs > 10)

data_table_cor<-data_table_n%>%
  dplyr::select(x,y, WTD, OPTRAM)%>%
  group_by(x,y) %>%
  dplyr::summarize(R_pvalue=cor.test(WTD, OPTRAM, use="pairwise.complete.obs")$p.value,
                   R=cor.test(WTD, OPTRAM, use="pairwise.complete.obs")$estimate)
summary(data_table_cor)

ggplot()+  
  geom_raster(data=data_table_cor, aes(x=x, y=y, fill=R))+
  scale_fill_viridis(
    option = "inferno",
    direction = 1,
    name = "R",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    ))+
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position="bottom",
        legend.key.width=unit(1,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text=element_text(size=15),
        legend.title = element_text(size=17))+
  
  annotation_scale(location = "bl", 
                   style = "ticks",
                   text_cex = 1.2)  +
  annotation_north_arrow(width = unit(0.5, "cm"), location = "tl")+ 
  coord_equal()


data_table_bestcor <- data_table_cor %>% filter (R_pvalue < 0.05)
data_table_bestcor <- data_table_bestcor[which.max(data_table_bestcor$R),]

# Plot WTD time series with OPTRAM time series
fig_TS_WTD+
  geom_point(data = subset(data_table, x %in% data_table_bestcor$x  & y %in% data_table_bestcor$y),
             aes(x = Date, y = OPTRAM*85-65),
             color = "#FF165D",
             size = 2)+
  geom_segment(data = subset(data_table, x %in% data_table_bestcor$x  & y %in% data_table_bestcor$y),
                aes(x = Date, 
                    xend=Date, 
                    y=min(WTD,na.rm = TRUE)-1, 
                    yend=OPTRAM*85-65),
                alpha=0.1,
                color = "#FF165D")+
  scale_y_continuous(name = "STR", 
                     sec.axis = sec_axis((~./85 + 65/85), name= "OPTRAM"))+
  theme(axis.line.y.right = element_line(color = "#FF165D"), 
        axis.ticks.y.right = element_line(color = "#FF165D"),
        axis.text.y.right = element_text(color = "#FF165D"), 
        axis.title.y.right = element_text(color = "#FF165D"),
        
        axis.line.y.left = element_line(color = "#7FB5FF"), 
        axis.ticks.y.left = element_line(color = "#7FB5FF"),
        axis.text.y.left = element_text(color = "#7FB5FF"), 
        axis.title.y.left = element_text(color = "#7FB5FF")
  )

# We will create a set of maps of OPTRAM 
ggplot()+
  geom_raster(data=S2_table, aes(x=x, y=y, fill=OPTRAM))+ 
  coord_equal()+
  scale_fill_gradientn(colours  = c("#f94144", "#faf3dd", "#2a6f97", "#014f86", 
                                    "#01497c",  "#012a4a"), 
                       na.value = "#F0F0F0",
                       limits = c(0,1),
                       breaks = c(0, 0.5, 1),
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))+
  theme_void()+
  theme(axis.title = element_blank(),
        legend.position="bottom",
        legend.key.width=unit(1,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text=element_text(size=15),
        legend.title = element_text(size=17))+
  # Add location of the "best pixel"
  geom_point(data = subset(S2_table, x %in% data_table_bestcor$x  & 
                             y %in% data_table_bestcor$y),
             aes(x = x, y = y),
             shape = 5,
             color = "#F900BF",
             size = 2,
             stroke = 2)+
  facet_wrap(Date ~ ., ncol = 5)

