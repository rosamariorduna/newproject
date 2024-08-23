#Convert Spec to Color and stats
# After you install the packages, load them using the library function
library(tidyverse)
library(pavo)
library(jpeg)

rm(list = ls())
# Read in data (make sure you are in the correct working directory)
df = read.csv("~/Downloads/pr 2 fishes.csv", skip = 4)

# Clean up the column names
# Extracting numbers from column names with X and number
new_names <- gsub("X([0-9]+)", "\\1", names(df))  
names(df) <- new_names

# Get only spectra and first column
spec_dat = df[,c(1,which(colnames(df)==350):which(colnames(df) == 1002))]

# Remove NA
spec_dat = na.omit(spec_dat)

# Clean up ID's (Removes anything after "_")
# (note, strsplit is a function from the tidyverse package)
ids = sapply(strsplit(spec_dat$name, "_"), function(x) x[1])

# Remove ID's and transpose matrix for pavo format 
spec_pavo = t(spec_dat[,-c(1)])

# Rename Columns
colnames(spec_pavo) = ids

# if its under 380, then its zero (for inside only, since there is no UV)
spec_pavo[1:8,1:ncol(spec_pavo)] <- 0
#spec_pavo$wl # To check wavelength

# Add wavelength column for pavo
wl = as.numeric(rownames(spec_pavo))
spec_pavo = data.frame(wl, spec_pavo)
colnames(spec_pavo) = c("wl", ids)
#spec_pavo = spec_pavo[,2:ncol(spec_pavo)]

# Convert to an rspec objest
spec_format = as.rspec(spec_pavo, lim = c(350, 1000), exceed.range = T)
spec_format_for_color = as.rspec(spec_pavo, lim = c(300, 700), exceed.range = T)

# Fix negative values
spec_format = procspec(spec_format, fixneg = "zero")
spec_format[,2:ncol(spec_format)] <- spec_format[,2:ncol(spec_format)]*100

# Fix negative values for color
spec_format_for_color = procspec(spec_format_for_color, fixneg = "zero")
spec_format_for_color[,2:ncol(spec_format_for_color)] <- spec_format_for_color[,2:ncol(spec_format_for_color)]*100

# Plot all spec
plot(spec_format, col = spec2rgb(spec_format_for_color), 
     type = "overlay", xlim = c(350, 1000))

# Extract out summary statistics used in pavo
summary_stats = summary(spec_format)

# Plotting multiple images

# Extract out specific columns
pull_spec_numb = function(spec_format, spec_format_for_color, col_num){
  rgb_color <- spec2rgb(spec_format_for_color)
  extracted_spec <- spec_format[,c(1,col_num)]
  if(col_num == 1){
    stop("Need column to be greater than 1")
  }
  temp_list <- vector(mode = "list", length = 2)
  temp_list[[1]] <- extracted_spec 
  temp_list[[2]] <- rgb_color[col_num]
  return(temp_list)
}

# Code to pull out a column
temp = pull_spec_numb(spec_format = spec_format,
               spec_format_for_color = spec_format_for_color,
               col_num = 2)

# Read in picture
img<-readJPEG("~/Dropbox/perclass_R_tutorials/pictures/ai_fish.jpeg")

# Extract out column 2
col2 = pull_spec_numb(spec_format = spec_format,
               spec_format_for_color = spec_format_for_color,
               col_num = 2)
# Extract out column 3
col3 = pull_spec_numb(spec_format = spec_format,
                      spec_format_for_color = spec_format_for_color,
                      col_num = 3)


# Plot figures side by side
par(mfrow = c(1,2))
plot(col2[[1]], col = col2[[2]], lwd = 4, ylim = c(0, 150), xlim = c(400, 720))
rasterImage(img, xleft = 410,
            xright = 600,
            ybottom = 100,
            ytop = 150)
text(x = 450, y = 10, 
     labels = colnames(spec_format)[2], cex = 0.9, adj =0)

plot(col3[[1]], col = col3[[2]], lwd = 4, ylim = c(0, 150), xlim = c(400, 720))
rasterImage(img, xleft = 410,
            xright = 600,
            ybottom = 100,
            ytop = 150)
text(x = 450, y = 10, 
     labels = colnames(spec_format)[3], cex = 0.9, adj = 0)

# Boxplot of color stats
# Need to adjust the wlmin and wlmax to whichever is appropriate for you system
# DO NOT USE DEFAULT
summary_stats = summary(spec_format, wlmin = 500, wlmax = 700)
df <- summary_stats
df[,colnames(df)]
df <- df[,colnames(df)[!colnames(df) %in% c("B1", "S5")]]
df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "Treatment", values_to = "Value")

# Create the boxplot
ggplot(df_long, aes(x = Treatment, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Color Statistics", x = "Color Statistics", y = "Value") #+ scale_y_log10() 

