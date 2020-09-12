#Matthew Carr (3733741)
#11 September 2020
#Quantitative exam

#Loading packages

library(tidyverse)
library(ggplot2)
library(ggfortify)
library(vegan)
library(devtools)
library(viridis)
library(viridisLite)
library(scales)
library(ggsn)
library(ggpubr)
library(maps)

####pre questions setup -----------------------------------------------------



#Loading the species and environmental data

env <- read_csv("EnvData.csv")

spe <- read_csv("FishSpeciesData.csv")

cord <- read_csv("Coordinates_Data.csv")

#Looking at the species data

glimpse(spe)
head(spe)
tail(spe)
summary(spe)

#Looking at the species data

glimpse(env)
head(env)
tail(env)
summary(env)

####Question 1 --------------------------------------------------------------

####Question A#########

#Specifing the demensions of species data
dim(spe)
#Species data has dim value of 89 31, hwich means there are 89 observations (rows) and 31 variables (columns)

#Specifing the demensions of environmental data 

dim(env)
#Environmnetal data has dim value of 89 3, hwich means there are 89 observations (rows) and 3 variables (columns)

####Question B#####


#Spcies richness

#creating new df to change into presence and absence
spe2 <- spe[,-1]#remove the id column
spe2[spe2 > 0] <- 1#converting the values to present or absence data

#number of species in each ite (species richness/alph diversity)
spe_rich <- cbind(species_richness = rowSums(spe2))


#doing the shannon weaver indices
shann <- diversity(spe, index = "shannon", MARGIN = 1, base = exp(1))
shannon <- as.data.frame(shann)#making the value into data

#doing the simpsons index
simp <- diversity(spe, index = "simpson", MARGIN = 1, base = exp(1))
simpson <- as.data.frame(simp)

#Creating df with all the indices on it 
spe3 <- spe[,1]
#cbinding the incides onto this df
spe3<- cbind(spe3, spe_rich, shannon = shannon, simpson)

# Load world map

load("data/africa_map.RData")

#Loading the world map

ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

###i)
#Creating the map of the plots 

baret1 <- ggplot() +
  borders(fill = "brown4", colour = "black") +
  coord_cartesian(xlim = c(10, 40), ylim = c(65, 80), expand = 0)+
  annotate("text", label = "Svalbard", #annotate is adding the word svalbald onto the map
           x = 19.5, y = 76, 
           size = 3.5, 
           angle = 0, 
           colour = "black") +
  annotate("text", label = "Barents\nsea", 
           x = 35, y = 75, 
           size = 3.5, 
           angle = 0, 
           colour = "black")+
  annotate("text", label = "Norway", 
           x = 15, y = 70, 
           size = 3.5, 
           angle = 0, 
           colour = "black")+
  annotate("text", label = "Russia", 
           x = 35, y = 69.8, 
           size = 3.5, 
           angle = 0, 
           colour = "black")

baret1


#i) Species richness (alha diversity)

sp_rich_plot <- baret1 + geom_point(data = cord, aes(x = Longitude, y = Latitude, size = spe3$species_richness))+# Force lon/lat extent
  labs(size = "Species richness")
sp_rich_plot  

#ii) Shannon-Weaver index

Shannon_plot <- baret1 + geom_point(data = cord, aes(x = Longitude, y = Latitude, size = spe3$shann))+# Force lon/lat extent
  labs(size = "Shannon-Weaver")
Shannon_plot  

#iii) Simpsons index

Simpsons_plot <- baret1 + geom_point(data = cord, aes(x = Longitude, y = Latitude, size = spe3$simp))+# Force lon/lat extent
  labs(size = "Simpsons")
Simpsons_plot 

ggarrange(Simpsons_plot, Shannon_plot, sp_rich_plot, nrow = 2, ncol = 2, common.legend = FALSE )

####Question C#############

#i) Temperature map scales
temp_plot <- baret1 + geom_point(data = cord, aes(x = Longitude, y = Latitude, size = env$Temperature))+# Force lon/lat extent
  labs(size = "Temperature (C)")
temp_plot 

#ii) Depth map scales

depth_plot <- baret1 + geom_point(data = cord, aes(x = Longitude, y = Latitude, size = env$Depth))+# Force lon/lat extent
  labs(size = "Depth (m)")
depth_plot


ggarrange(temp_plot, depth_plot, nrow = 2, ncol = 2, common.legend = FALSE )
#### Question D####

#Findings 

#The diversity indices on b all show the the same patten. They are more diverse (larger values/circles) at the north and south end of the sites, while the least diversity of species occur in the middle
#sites. This coincides with the different environmnental gradients that occur in c. Temperature is the highest in the southern end of the sites, where they are close to the coast line and colder the at
#the northen end, where it is away from the coast. The depth  shows a reverse of the temperature gradient, with it ebing deeper the further away from the coast you move.These environmnetal gradients greatly
#influence the species diversity, because the highest amount of diversity (from all 3 indices) shows to follw the environmental graient. The mojority of the diverse sites ate located in the northen endof the 
#sites where the deepest water occurs, this can indicate that the species diversity in theses sites increase along the depth gradient. The species on the southern end riight next to the coast also shows a 
#high diversity, where the temperature gradient is the highest, showing that these sites species diversty is reliant on higher temperature. The temperature gradient  decreases as the depth gradients increases 
#and vice versa. The least amount of diversity happens in thw middle of all the sites, where there are the lowest environmnetal gradients to influence diversity.

####Question E#####

#Descriptive statistics


d_stat_depth <- env %>% # selecting df to summarize
  summarise(mean = round(mean(Depth, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Depth, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Depth, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Depth), # sum of the values of that col
            min = min(Depth), # minimum value of the col
            qrt1 = quantile(Depth, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Depth), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Depth, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Depth),# the maximum value of the col x
            n = n()) # number of observations

d_stat_temp <- env %>% # selecting df to summarize
  summarise(mean = round(mean(Temperature, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Temperature, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Temperature, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Temperature), # sum of the values of that col
            min = min(Temperature), # minimum value of the col
            qrt1 = quantile(Temperature, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Temperature), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Temperature, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Temperature),# the maximum value of the col x
            n = n()) # number of observations

#i)Creating table for the descriptive statistics
Desp_stat <- rbind(d_stat_temp,d_stat_depth)

rownames(Desp_stat) = c("Temperature", "Depth")

Desp_stat

#ii)Creating figures for descriptive statistics

par(mfrow = c(2,1))

# Box plot for Depth
desp_box <- boxplot(env$Depth, ylab = "Depth (m)", horizontal = TRUE)

# Boxplot for temperature
temp_desp_box <- boxplot(env$Temperature, ylab = "Temperature (C)", horizontal = TRUE)

#Findings 
#The depth box plot shows us that it is skews to the left, meaning that the median (320.00) is closer to the first Quatile (285).The max value is 486 and min is 167. Its sum is 29293, with a mean of 329.13
# and standard deviation of 66.07.
#The Temperature box plot is more normal distrubution, the median (1.85) leave equal sides to the two quantiles. The max value is 4.45 and min is 0.15 Its sum is 167.95, with a mean of 1.89
# and standard deviation of 1.04. 

####Question F#####

#i)
#Creating the env vectors
env_cord <- cbind(cord[,-1], env[,-1])

####RAW DATA CA####
(spe.ca <- cca(spe[, -1]))#CA together will erxplain the added up variations off the variations

summary(spe.ca)

#Plotting the Correspondence Analysis (CA), to show which sepcies are influenced by what site

par(mfrow = c(1, 2))

plot(spe.ca, scaling = 2, choices = c(1,2), main = "CA - scaling 1")#is the pca plot that is being created
plot(spe.ca, choices = c(1,2), main = "CA - scaling 2")


#evplot <- function(ev)

#Fit and Plot Smooth Surfaces of Variables on Ordination

require('viridis')

palette(viridis(8))

par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))

#Plot Bo_sa species
with(spe, tmp <- ordisurf(spe.ca ~ Bo_sa, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Bo_sa"))

abline(h = 0, v = 0, lty = 3)

#Plot Tr_spp species
with(spe, tmp <- ordisurf(spe.ca ~ Tr_spp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tr_spp"))

abline(h = 0, v = 0, lty = 3)

with(spe, tmp <- ordisurf(spe.ca ~ No_rk, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "No_rk"))

abline(h = 0, v = 0, lty = 3)

#plot species Tr_es
with(spe, tmp <- ordisurf(spe.ca ~ Tr_es, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tr_es"))

abline(h = 0, v = 0, lty = 3)

# A posterior projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active

(spe.ca.env <- envfit(spe.ca, env_cord, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env)

# Plot significant variables with a different colour

plot(spe.ca.env, p.max = 0.05, col = "red")


####PRESENCE AND ABSENCE####

(spe.ca <- cca(spe2))#CA together will erxplain the added up variations off the variations

summary(spe.ca)

#Plotting the Correspondence Analysis (CA), to show which sepcies are influenced by what site

par(mfrow = c(1, 2))

plot(spe.ca, scaling = 2, choices = c(1,2), main = "CA - scaling 1")#is the pca plot that is being created
plot(spe.ca, choices = c(1,2), main = "CA - scaling 2")


#evplot <- function(ev)

#Fit and Plot Smooth Surfaces of Variables on Ordination

require('viridis')

palette(viridis(8))

par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))

#Plot satr species
with(spe2, tmp <- ordisurf(spe.ca ~ Bo_sa, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Bo_sa"))

abline(h = 0, v = 0, lty = 3)

#Plot scer species
with(spe2, tmp <- ordisurf(spe.ca ~ Tr_spp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tr_spp"))

abline(h = 0, v = 0, lty = 3)

with(spe2, tmp <- ordisurf(spe.ca ~ No_rk, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "No_rk"))

abline(h = 0, v = 0, lty = 3)

#plot species Cogo
with(spe2, tmp <- ordisurf(spe.ca ~ Tr_es, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tr_es"))

abline(h = 0, v = 0, lty = 3)

# A posterior projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active

(spe.ca.env <- envfit(spe.ca, env_cord, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env)

# Plot significant variables with a different colour

plot(spe.ca.env, p.max = 0.05, col = "red")

####LOG TRANFORMED DATA####

#creating log transformed data
log <- decostand(spe[,-1], method = "log")

(spe.ca <- cca(log))#CA together will erxplain the added up variations off the variations

summary(spe.ca)

#Plotting the Correspondence Analysis (CA), to show which sepcies are influenced by what site

par(mfrow = c(1, 2))

plot(spe.ca, scaling = 2, choices = c(1,2), main = "CA - scaling 1")#is the pca plot that is being created
plot(spe.ca, choices = c(1,2), main = "CA - scaling 2")


#evplot <- function(ev)

#Fit and Plot Smooth Surfaces of Variables on Ordination

require('viridis')

palette(viridis(8))

par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))

#Plot satr species
with(log, tmp <- ordisurf(spe.ca ~ Bo_sa, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Bo_sa"))

abline(h = 0, v = 0, lty = 3)

#Plot scer species
with(log, tmp <- ordisurf(spe.ca ~ Tr_spp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tr_spp"))

abline(h = 0, v = 0, lty = 3)

with(log, tmp <- ordisurf(spe.ca ~ No_rk, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "No_rk"))

abline(h = 0, v = 0, lty = 3)

#plot species Cogo
with(log, tmp <- ordisurf(spe.ca ~ Tr_es, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tr_es"))

abline(h = 0, v = 0, lty = 3)

# A posterior projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active

(spe.ca.env <- envfit(spe.ca, env_cord, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env)

# Plot significant variables with a different colour

plot(spe.ca.env, p.max = 0.05, col = "red")


####ii)####

#spe    CA1       CA2
#Bo_sa  -2.67408 -0.02479
#Tr_spp -1.83734  0.07040
#No_rk   0.50376 -1.64629
#Tr_es   0.59654  1.50230 

#The Correspondence Analysis (CA)(function cca) allows us to see the total inertia and eigen values of the species dataset. The total inirtia is the combined eigenvalues added up, the total inertia
#is the total amount of variation explained. Then each CA you use if you want to have  the proportion of the variation explained (e.i. CA1/ total inertia). The total inertia of the given data 
#has a value of 2.781, with CA1 (value of 0.7765) explaing the largest proportion of variation(0.2792 of the variation is explained in CA1). The CA2 has an eigenvalue of 0.5410,which explains 
#0.1946 of the variations. All together it explains 0.4738 (47%) of the total variation in the data. 

#By looking at the eigen vectors we are able to see what affect each species has on the community structure. The species Tr_es has the largest affect on the CA1 with a high species
#score of 0.59654 in CA1 and species No_rk has the second highest specie value of 0.50376. This shows us that species Tr_es and No_rk have the hisghest positive valeus, meaning thy have high influence
#Species Bo_sa has the lowest species score of -2.67408 in CA1 and species Tr_spp -1.83734  has the second lowest, these calues show that thaye have the largest negative values, meaning that the are quite
#infleuncial on the data.
#On the bilot, species No_rk and Tr_es with the highest positive values in CA1 will be most abundant on the postive values of the grapgh and so they will be found on the right hand side of the biplots. The two 
#species with the highest negative values in CA1 are situated in left hand side, where they wiil be most abundant.

####iii)#######

##i) RAw DATA

#                        CA1    CA2    CA3   
#Eigenvalue            0.7765 0.5410 0.4850 
#Proportion Explained  0.2792 0.1946 0.1744 
#Cumulative Proportion 0.2792 0.4738 0.6482

#The summary of the spe.ca data will give you these values, this shows that CA1, 2 and 3 each explains 0.7765, 0.5410 and 0.4850 respectively of the toatl inertia of 2.781. They portion of varian that they exlain
#is 0.2792 0.1946 0.1744 0.1055 respectively. Cumulatively they explain 0.6482 (65%) of the variations. This tells us that they explain more than half of the variation, making them good at capturing the variation
#that exist across space in the community composition.

##ii) PRESENT ABSENT DATA

#                        CA1    CA2    CA3   
#Eigenvalue            0.3220 0.1631 0.12187 
#Proportion Explained  0.2083 0.1055 0.07883 
#Cumulative Proportion 0.2083 0.3138 0.39266

#The summary of the spe.ca data will give you these values, this shows that CA1, 2 and 3 each explains 0.3220, 0.1631, 0.12187 respectively of the toatl inertia of 1.546. The portion of varian that they exlain
#is 0.2083, 0.1055 and 0.07883 respectively. Cumulatively they explain 0.39266 (39%) of the variations. This tells us that they explain less than half of the variation, making them bad at capturing the variation
#that exist across space in the community composition.

##iii) LOG TRANSFORMED DATA

#                        CA1    CA2    CA3   
#Eigenvalue            0.3892 0.1916 0.11464 
#Proportion Explained  0.2861 0.1409 0.08428 
#Cumulative Proportion 0.2861 0.4270 0.51124

#The summary of the spe.ca data will give you these values, this shows that CA1, 2 and 3 each explains 0.3892, 0.1916 and 0.11464 respectively of the toatl inertia of 1.36. The portion of varian that they exlain
#is 0.2861, 0.1409 and 0.08428 respectively. Cumulatively they explain 0.51124 (51%) of the variations. This tells us that they explain just more than half of the variation, making them relatively good at capturing the 
#variation that exist across space in the community composition, just not to strongly.

####iv)####

#i) RAW DATA

#Species on these biplot can be better explained by taking 4 influencial species (Bo_sa, Tr_spp, No_rk, Tr_es) and  show their abundance on te Correspondence Analysis (CA). This plot shows how
#the species occur in the data and where they are more abundant. The Bo_sa specie sthat has the lowest species values in CA1  IS most abundnat in the left hand side of the plot. The largest circle 
#appering here, the species are present ion other spots of the plot, but they are not nearly so abundant as the there as in the left hand side where the largest abundance occurs on one spot (largest circle)
#The depth and temp gradients does influence the presence of the speccies, that occur along the lines (gradients seen in Tr_es species plot), but they are nor abundant there, showing that this species is not greatly
#influenced by the these gradiets that are shown.
#Species Tr_spp is similar than Bo_sa as it has a very high negative species score and shwos similar pattern on the lot. It is alos most abundant on the far left hand side, but is also present (no abundant)
#on the right. It is a bit more abundant thans sp. Bo_sa in the lower negative values, with a few relatively big circles there. They are however most abundant in the far left similar to Bo_sa. This means they are
#also not really infleunce with the env. gradients shown here.
#Species No_rk has a high species score which means that they are most abundnat on the right hand side of the plot, this is where the largest abundace circle occurs. This species is greatly influenced by the 
#env. gradient where the species are seen to occupy spaces mainly along the gradients. This species is greatly influence by the depth, they are most abundant at large depths, with the largest abundance (circle) 
#occuring near the end of the depth gradient. It lays almost down a linear pathe with the gradient and does not even occur in shallower water (opposite end of the depth line).
#Species Tr_es has the highset species score and so it occurs on the right hand side of the plot. It is mostly indluenced by env. gradient, whith most occurences being near the temp and depth lines.
#This sp. is greatly influenced by the temp. gradient, where it is most abundant t the end of the temp gradient (high temperatures in the environment). It does not occur in areas where the temp is low (opposte
#end of the temp gradient).

#ii) Present absent data


#Because the prsent and absence data shows if this species is present at a particular spot, the dat wil look different to raw dat that takes other factors into consideration like how many times they are found there
#SPecies Bo_sa is most abundnat on the right hand side of the plot. This shows in abesent and prsence datat negative species score will appear on the right hand side.
#Species Tr_spp is similar to the Bo_sa sp.,because they both have negative species value. It is also most abundnat on the right hand side and does not seem to be infleunced greatly by env, gradients.
#Species No_rk that has a high postive sp. score is situated in the left hand side of the plot. It is greatly influenced by the env, gradient of epth, they occure most abundant along this gradient line.
#Species Tr_es is also occurs in the left handside, due to its positive value. The sp is greatlly inlfuenced by both env gradient, and is present all along these two (temp and depths) lines.

#iii) Log transformed data

#SPecies Bo_sa is most abundnat in the right handside and the abundance increases as it moves to the bootom right corner, down a almost linear line. Once again thety are not really influenced by any gradient, 
# it is not situated near it. 
#Species Tr_spp shwos a similar pattern to BO_sa it is alsoabundnat in the right handside and the abundance increases as it moves to the bottom right corner. It is also not seemingly influenced by any en. gradient.
#Species No_rk mostly occur in the bigging top corner of left handside and is greatly influenced by the depth gradient, at deeper depths the species is most abundnat. 
#The species Tr_es is allocated on the left and is mostly reliant on the temp. gradient, this species is most abundant at higher temperatures. 


# Question 2 --------------------------------------------------------------

#Loading teh species and env data of Barro Colorado Island Tree Counts

data("BCI") 

data("BCI.env")

#presence and absence data

PA_bci <- BCI

PA_bci[PA_bci > 0] <- 1

BCI.env$Age.cat <- as.numeric(factor(BCI.env$Age.cat))
BCI.env$Geology <- as.numeric(factor(BCI.env$Geology ))
BCI.env$Habitat <- as.numeric(factor(BCI.env$Habitat))
BCI.env$Stream <- as.numeric(factor(BCI.env$Stream))


####CREATING PCA####

# Creating the clearplot function -----------------------------------------

'cleanplot.pca' <-
  function(res.pca, ax1=1, ax2=2, scaling=2, plot.sites=TRUE,
           plot.spe=TRUE, label.sites=TRUE, label.spe=TRUE, cex.char1=0.7,
           pos.sites=2, pos.spe=4, mult.spe=1, select.spe=NULL,
           mar.percent=0.1, optimum=TRUE, move.origin=c(0,0), silent=TRUE)
    #
    # A function to draw a triplot (scaling 1 or scaling 2) from an object
    # of class "rda" containing RDA result from vegan's rda() function.
    # This is version dev1
    #
    # ARGUMENTS
    #
    # ##### General parameters
    # res.pca          An rda{vegan} object.
    # ax1, ax2         Canonical axes to be drawn as abscissa and ordinate. Defaults: 1 and 2.
    # site.sc          Can be set to "lc" (linear constraints or model scores, default)
#                  or "wa" (weighted averages, default in vegan).
# scaling          Scaling type: only 1 or 2 are supported. Default: 2.
#
# ##### Items to be plotted
# plot.sites       If TRUE, the sites will be plotted as small circles.
# plot.spe         If TRUE, the species (or other response variables) will be plotted.
# label.sites      If TRUE, labels are added to the site symbols.
# label.spe        If TRUE, labels are added to the species arrows.
# label.env        If TRUE, labels are added to the environmental variable arrows.
# label.centr      If TRUE, labels are added to the centroids of factor levels.
# cex.char1        Character size (for sites and response variables).
#
# ##### Label positions
# ## Positions: 1=below the point, 2=left, 3=above, 4=right. Default: 4.
# ## Note - Argument pos=NULL centres the label on the position of the object (site point,
# ## species or environmental variable arrow, centroid) when the object is not drawn.
# pos.sites        Position of site labels. 1 to 4, as above. Default: 2.
# pos.spe          Position of species labels. 1 to 4, as above. Default: 4.
#
# ##### Multipliers, selection of species to be plotted
# mult.spe         Multiplier for length of the species arrows. Default: 1.
# select.spe       Vector containing a selection of the species numbers to be drawn in
#                  the biplot, e.g. c(1,2,5,8,12). Draw all species if select.spe=NULL
#                  (default value). The species that are well represented in the RDA plot
#                  can be identified using goodness(RDA.output.object,display="species")
#
# ##### Position of the plot in frame, margins
# mar.percent      Factor to expand plot size to accomodate all items and labels. Positive
#                  values increase the margins around the plot, negative values reduce
#                  them.
# optimum          If TRUE, the longest species and environmental arrows are stretched to
#                  a length equal to the distance to the origin of the site farthest from
#                  the origin in the plot of (ax1,ax2). This is an optimal combined
#                  representation of the three elements. The lengths of the species and
#                  environmental arrows can be further modified using the arguments
#                  mult.spe and mult.arrow.
# move.origin      Move plot origin right-left and up-down. Default: move.origin=c(0,0).
#                  Ex. move.origin=c(-1,0.5) moves origin by 1 unit left and 0.5 unit up.
#
# ##### Varia
# silent           If FALSE, intermediate computation steps are printed. Default: TRUE.
#
# # Example 1 - Table 11.3 of Legendre & Legendre (2012, p. 644), first 6 species only
#
# Y.mat = matrix(c(1,0,0,11,11,9,9,7,7,5,0,0,1,4,5,6,7,8,9,10,0,0,0,0,17,0,13,0,10,0,0,
# 0,0,0,7,0,10,0,13,0,0,0,0,8,0,6,0,4,0,2,0,0,0,1,0,2,0,3,0,4),10,6)
# Depth = 1:10
# Sub. = as.factor(c(rep(1,3),4,2,4,2,4,2,4))
# env = cbind(data.frame(Depth),data.frame(Sub.))
#
# rda.out = rda(Y.mat~ .,env)
#
# # Scaling=1
# par(mfrow=c(1,2))
# triplot.rda(rda.out, scaling=1, mar.percent=0)
# triplot.rda(rda.out, scaling=1, move.origin=c(5,-5), mar.percent=-0.1)
#
# # Scaling=2
# par(mfrow=c(1,2))
# triplot.rda(rda.out, scaling=2, mar.percent=0.15, silent=FALSE)
# triplot.rda(rda.out, scaling=2, move.origin=c(0.4,-0.25), mar.percent=0.05,silent=FALSE)
#
# # Example 2 - Dune data
#
# library(vegan)
# data(dune)
# data(dune.env)
#
# rda.dune = rda(dune ~ .,dune.env)
#
# tmp = goodness(rda.dune)
# ( sp.sel = which(tmp[,2] >= 0.4) )
#
# Scaling=2
# par(mfrow=c(1,2))
# triplot.rda(rda.dune, mar.percent=0)
# triplot.rda(rda.dune, select.spe=sp.sel, move.origin=c(-0.3,0), mar.percent=0.1)
#
#
#
# License: GPL-2
# Authors: Francois Gillet, Daniel Borcard & Pierre Legendre, 2016
  {
    ### Internal functions
    #
    'stretch' <-
      function(sites, mat, ax1, ax2, n, silent=silent) {
        # Compute stretching factor for the species arrows
        # First, compute the longest distance to centroid for the sites
        tmp1 <- rbind(c(0,0), sites[,c(ax1,ax2)])
        D <- dist(tmp1)
        target <- max(D[1:n])
        # Then, compute the longest distance to centroid for the species arrows
        if(class(mat)=="matrix") {
          p <- nrow(mat)   # Number of species to be drawn
          tmp2 <- rbind(c(0,0), mat[,c(ax1,ax2)])
          D <- dist(tmp2)
          longest <- max(D[1:p])
        } else { tmp2 <- rbind(c(0,0), mat[c(ax1,ax2)])
        longest <- dist(tmp2)
        # print(tmp2)
        }  # If a single row left in 'mat'
        #
        if(!silent) cat("target =",target," longest =",longest," fact =",target/longest,"\n")
        fact <- target/longest
      }
    #
    'larger.plot' <-
      function(sit.sc, spe.sc, percent, move.origin, ax1, ax2) {
        # Internal function to expand plot limits (adapted from code by Pierre Legendre)
        mat <- rbind(sit.sc, spe.sc)
        range.mat <- apply(mat, 2, range)
        rownames(range.mat) <- c("Min","Max")
        z <- apply(range.mat, 2, function(x) x[2]-x[1])
        range.mat[1,] <- range.mat[1,]-z*percent
        range.mat[2,] <- range.mat[2,]+z*percent
        if(move.origin[1] != 0) range.mat[,ax1] <- range.mat[,ax1] - move.origin[1]
        if(move.origin[2] != 0) range.mat[,ax2] <- range.mat[,ax2] - move.origin[2]
        range.mat
      }
    ### End internal functions
    
    epsilon <- sqrt(.Machine$double.eps)
    if(!class(res.pca)[1]=="rda") stop("The input file is not a vegan output object of class 'rda'", call.=FALSE)
    if(scaling!=1 & scaling!=2) stop("Function only available for scaling = 1 or 2", call.=FALSE)
    
    k <- length(res.pca$CA$eig)         # n. of PCA eigenvalues
    n.sp <- length(res.pca$colsum)      # n. of species
    ahead <- 0.05   # Length of arrow heads
    aangle <- 30    # Angle of arrow heads
    # 'vec' will contain the selection of species to be drawn
    if(is.null(select.spe)){ vec <- 1:n.sp } else { vec <- select.spe }
    
    # Scaling 1: the species scores have norms of 1
    # Scaling 1: the site scores are scaled to variances = can.eigenvalues
    # Scaling 2: the species scores have norms of sqrt(can.eigenvalues)
    # Scaling 2: the site scores are scaled to variances of 1
    # --------------------------------------------------------------------
    
    ### This version reconstructs and uses the original RDA output of L&L 2012, Section 11.1.3
    
    Tot.var = res.pca$tot.chi        # Total variance in response data Y
    eig.val = res.pca$CA$eig         # Eigenvalues of Y-hat
    Lambda = diag(eig.val)           # Diagonal matrix of eigenvalues
    eig.val.rel = eig.val/Tot.var    # Relative eigenvalues of Y-hat
    Diag = diag(sqrt(eig.val.rel))   # Diagonal matrix of sqrt(relative eigenvalues)
    U.sc1 = res.pca$CA$v             # Species scores, scaling=1
    U.sc2 = U.sc1 %*% Lambda^(0.5)   # Species scores, scaling=2
    n = nrow(res.pca$CA$u)           # Number of observations
    Z.sc2 = res.pca$CA$u*sqrt(n-1)   # Site scores, scaling=2
    Z.sc1 = Z.sc2 %*% Lambda^(0.5)   # Site scores, scaling=1
    #
    # Print the equilibrium circle in scaling 2 if the variables have been standardized
    longueur <- function(vec) sqrt(sum(vec^2))
    long.var.sc2 <- apply(U.sc2,1,longueur)
    # print(long.var.sc2)
    if(any(abs(long.var.sc2-1) > epsilon)) circle2 <- FALSE  else  circle2 <- TRUE
    cat("circle2 =",circle2,"\n")
    #
    if(is.null(select.spe)){ vec <- 1:n.sp } else { vec <- select.spe }
    #
    if(scaling==1) {
      sit.sc <- Z.sc1
      spe.sc <- U.sc1[vec,]
    } else {          # For scaling=2
      sit.sc <- Z.sc2
      spe.sc <- U.sc2[vec,]
    }
    # if(is.null(rownames(sit.sc))) rownames(sit.sc) <- paste("Site",1:n,sep="")
    if(is.null(rownames(sit.sc))) rownames(sit.sc) <- 1:n
    if(is.null(rownames(spe.sc))) rownames(spe.sc) <- paste("Sp",1:n.sp,sep="")
    #
    fact.spe <- 1
    if(optimum) {
      fact.spe <- stretch(sit.sc[,1:k], spe.sc[,1:k], ax1, ax2, n, silent=silent)
    }
    if(!silent) cat("fac.spe =",fact.spe,"\n\n")
    spe.sc <- spe.sc*fact.spe*mult.spe
    #
    lim <- larger.plot(sit.sc[,1:k], spe.sc[,1:k], percent=mar.percent, move.origin=move.origin, ax1=ax1, ax2=ax2)
    if(!silent) print(lim)
    
    ### Drawing the biplot begins ###
    ###
    # Draw the main plot
    mat <- rbind(sit.sc[,1:k], spe.sc[,1:k])
    plot(mat[,c(ax1,ax2)], type="n", main=paste("PCA biplot - Scaling", scaling), xlim=c(lim[1,ax1], lim[2,ax1]), ylim=c(lim[1,ax2], lim[2,ax2]),
         xlab=paste("PCA ",ax1), ylab=paste("PCA ",ax2), asp=1)
    abline(h=0, v=0, col="grey60")
    
    # Draw the site scores
    if(plot.sites) {
      points(sit.sc[,ax1], sit.sc[,ax2], pch=20)
      if(label.sites)
        text(sit.sc[,ax1], sit.sc[,ax2], labels = rownames(sit.sc), col="black", pos=pos.sites, cex=cex.char1)
    } else {
      if(label.sites)
        text(sit.sc[,ax1], sit.sc[,ax2], labels = rownames(sit.sc), col="black", pos=NULL, cex=cex.char1)
    }
    
    # Draw the species scores
    if(plot.spe) {
      arrows(0, 0, spe.sc[,ax1], spe.sc[,ax2], length=ahead, angle=aangle, col="red")
      if(label.spe)
        text(spe.sc[,ax1], spe.sc[,ax2], labels = rownames(spe.sc), col="red", pos=pos.spe, cex=cex.char1)
    } else {
      if(label.spe)
        text(spe.sc[,ax1], spe.sc[,ax2], labels = rownames(spe.sc), col="red", pos=NULL, cex=cex.char1)
    }
    
    # If scaling=1 draw circle of equilibrium contribution
    #  if(scaling==1 | (scaling==2 & circle2)){
    if(scaling==1){
      pcacircle(res.pca, mult.spe=mult.spe, fact.spe=fact.spe, silent=silent)
    }
  }

"pcacircle" <- function (pca, mult.spe, fact.spe, silent=silent)
{
  # Draws a circle of equilibrium contribution on a PCA plot
  # generated from a vegan analysis.
  
  eigenv <- pca$CA$eig
  p <- length(eigenv)
  n <- nrow(pca$CA$u)
  tot <- sum(eigenv)
  radius <- (2/p)^0.5 * mult.spe * fact.spe
  symbols(0, 0, circles=radius, inches=FALSE, add=TRUE, fg="blue")
  if(!silent) cat("\nSpecies arrows and the radius of the equilibrium circle are stretched by a factor of", mult.spe*fact.spe)
  if(!silent) cat("\nThe radius of the equilibrium circle is thus", (2/p)^0.5, "*", mult.spe, "*", fact.spe, "=", radius,"\n")
}

####PCA2######
#Creating a PCA of the environmental data to help anylise gradients
ggbiplot(BCI.env)

#Laoding interpout as redundancy analysis (downloaded from ikmanva )
#rda is applying pronciple corrdinae anylasis.
env_pca <- rda(BCI.env, scale = TRUE) #brings all the variasbles down, sodthat the highest values donot have the most influence

#to see pc 1 and 2
summary(env_pca)#PC1 and 2 data is most important

#creating PCA
par(mfrow = c(1, 2))#setting the paramaters (if paramaters are too large shorten it)
biplot(env_pca, scaling = 2, choices = c(1,2), main = "PCA - scaling 1")#is the pca plot that is being created
biplot(env_pca, choices = c(1,2), main = "PCA - scaling 2")
cleanplot.pca(env_pca, scaling = 1, mar.percent = 0.08)# Using the function created to circle around the gradients
cleanplot.pca(env_pca, scaling = 2, mar.percent = 0.04)

#Using this to plot the contours of the linear trend surface
#par(mfrow = c(1, 1))
#palette(viridis(8))
#biplot(env_pca, type = c("text", "points"))
#tmp <- ordisurf(data = env_pca ~ bod, add= TRUE, col = "turquoise", knots =1)#adds the lines
#tmp <- ordisurf(env_pca ~ alt, add= TRUE, col = "salmon", knots =1)

####Creating CA#####

(spe.ca1 <- cca(PA_bci))#CA together will erxplain the added up variations off the variations

summary(spe.ca1)

#Plotting the Correspondence Analysis (CA), to show which sepcies are influenced by what site

par(mfrow = c(1, 2))

plot(spe.ca1, scaling = 2, choices = c(1,2), main = "CA - scaling 1")#is the pca plot that is being created
plot(spe.ca1, choices = c(1,2), main = "CA - scaling 2")


(spe.ca.env1 <- envfit(spe.ca1, BCI.env, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env1)

# Plot significant variables with a different colour

plot(spe.ca.env1, p.max = 0.05, col = "red")
#evplot <- function(ev)

#Fit and Plot Smooth Surfaces of Variables on Ordination

require('viridis')

palette(viridis(8))

par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))

#Plot Marila.laxiflora species (0.877674 -0.057013)
with(PA_bci, tmp <- ordisurf(spe.ca1 ~ Marila.laxiflora, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Marila.laxiflora"))

abline(h = 0, v = 0, lty = 3)

#Plot Vachellia.melanoceras  species (0.781645  1.012246)
with(PA_bci, tmp <- ordisurf(spe.ca1 ~ Vachellia.melanoceras , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Vachellia.melanoceras"))

abline(h = 0, v = 0, lty = 3)

#Chimarrhis.parviflora (-4.017029  1.024256) 
with(PA_bci, tmp <- ordisurf(spe.ca1 ~ Chimarrhis.parviflora, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Chimarrhis.parviflora"))

abline(h = 0, v = 0, lty = 3)

#plot species Alibertia.edulis (-4.017029  1.024256 )
with(PA_bci, tmp <- ordisurf(spe.ca1 ~ Alibertia.edulis, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Alibertia.edulis"))

abline(h = 0, v = 0, lty = 3)

# A posterior projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active

(spe.ca.env1 <- envfit(spe.ca1, BCI.env, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env1)

# Plot significant variables with a different colour

plot(spe.ca.env1, p.max = 0.05, col = "red")


####A) ######

#descriptive stats
d_stat_UTM.EW <- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(UTM.EW, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(UTM.EW, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(UTM.EW, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(UTM.EW), # sum of the values of that col
            min = min(UTM.EW), # minimum value of the col
            qrt1 = quantile(UTM.EW, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(UTM.EW), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(UTM.EW, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(UTM.EW),# the maximum value of the col x
            n = n()) # number of observations

d_stat_UTM.NS<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(UTM.NS, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(UTM.NS, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(UTM.NS, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(UTM.NS), # sum of the values of that col
            min = min(UTM.NS), # minimum value of the col
            qrt1 = quantile(UTM.NS, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(UTM.NS), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(UTM.NS, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(UTM.NS),# the maximum value of the col x
            n = n()) # number of observations

d_stat_Precipitation<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(Precipitation, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Precipitation, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Precipitation, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Precipitation), # sum of the values of that col
            min = min(Precipitation), # minimum value of the col
            qrt1 = quantile(Precipitation, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Precipitation), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Precipitation, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Precipitation),# the maximum value of the col x
            n = n()) # number of observations


d_stat_Elevation<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(Elevation, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Elevation, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Elevation, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Elevation), # sum of the values of that col
            min = min(Elevation), # minimum value of the col
            qrt1 = quantile(Elevation, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Elevation), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Elevation, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Elevation),# the maximum value of the col x
            n = n()) # number of observations

d_stat_Age.cat<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(Age.cat, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Age.cat, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Age.cat, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Age.cat), # sum of the values of that col
            min = min(Age.cat), # minimum value of the col
            qrt1 = quantile(Age.cat, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Age.cat), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Age.cat, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Age.cat),# the maximum value of the col x
            n = n()) # number of observations

d_stat_Geology<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(Geology, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Geology, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Geology, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Geology), # sum of the values of that col
            min = min(Geology), # minimum value of the col
            qrt1 = quantile(Geology, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Geology), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Geology, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Geology),# the maximum value of the col x
            n = n()) # number of observations

d_stat_Habitat<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(Habitat, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Habitat, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Habitat, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Habitat), # sum of the values of that col
            min = min(Habitat), # minimum value of the col
            qrt1 = quantile(Habitat, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Habitat), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Habitat, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Habitat),# the maximum value of the col x
            n = n()) # number of observations

d_stat_Stream<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(Stream, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(Stream, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(Stream, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(Stream), # sum of the values of that col
            min = min(Stream), # minimum value of the col
            qrt1 = quantile(Stream, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(Stream), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(Stream, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(Stream),# the maximum value of the col x
            n = n()) # number of observations

d_stat_EnvHet<- BCI.env %>% # selecting df to summarize
  summarise(mean = round(mean(EnvHet, na.rm = TRUE), 2), #getting the mean of col x and rounding it off to 2 decimal positions
            median = median(EnvHet, na.rm = TRUE), # getting the median and na.rm = TRUE is used to exclude NA from equations
            sd = round(sd(EnvHet, na.rm = TRUE), 2), #getting standard deviation, rounding it off to 2 dec. and excluding NA
            sum = sum(EnvHet), # sum of the values of that col
            min = min(EnvHet), # minimum value of the col
            qrt1 = quantile(EnvHet, p = 0.25),# getting the quatrant value of the first quadrant(1/4, 0.25) of the the col
            median = median(EnvHet), # gettinhg the median of that col (quadrant 2 value)
            qrt3 = quantile(EnvHet, p = 0.75),# getting the quatrant value of the third quadrant(3/4, 0.75) of the the col
            max = max(EnvHet),# the maximum value of the col x
            n = n()) # number of observations

#Collecting all the data into one table

DS_table <- rbind(d_stat_UTM.EW,d_stat_UTM.NS, d_stat_Precipitation,d_stat_Elevation, d_stat_Age.cat, d_stat_Geology, d_stat_Habitat, d_stat_Stream, d_stat_EnvHet)

#naming rows
rownames(DS_table) = c("UTM.EW", "UTM.NS","Precipitation", "Elevation", "Age.cat", "Geology", "Habitat", "Stream", "EnvHet")

#Descriptive figure

#par(mfrow = c(2,1))

# Box plots
UTM.EW_box <- boxplot(BCI.env$UTM.EW, ylab = "UTM.EW", horizontal = TRUE)


UTM.NS_desp_box <- boxplot(BCI.env$UTM.NS, ylab = "UTM.NS", horizontal = TRUE)

#Precipitation_desp_box <- boxplot(BCI.env$Precipitation, ylab = "Precipitation", horizontal = TRUE)

#Elevation_desp_box <- boxplot(BCI.env$Elevation, ylab = "Elevation", horizontal = TRUE)

#Age.cat_desp_box <- boxplot(BCI.env$Age.cat, ylab = "Age.cat", horizontal = TRUE)

#Geology_desp_box <- boxplot(BCI.env$Geology, ylab = "Geology", horizontal = TRUE)

Habitat_desp_box <- boxplot(BCI.env$Habitat, ylab = "Habitat", horizontal = TRUE)

#Stream_desp_box <- boxplot(BCI.env$Stream, ylab = "Stream", horizontal = TRUE)

EnvHet_desp_box <- boxplot(BCI.env$EnvHet, ylab = "EnvHet", horizontal = TRUE)

####Question B######

#i)number of species in each ite (species richness/alph diversity)

alpha <- cbind(species_richness = rowSums(PA_bci))
alpha <- as.data.frame(alpha)
#assigning x value
alpha <- tibble::rowid_to_column(alpha, "ID")

#doing the shannon weaver indices
shann1 <- diversity(PA_bci, index = "shannon", MARGIN = 1, base = exp(1))
shannon1 <- as.data.frame(shann1)#making the value into data
shannon1 <- tibble::rowid_to_column(shannon1, "ID")

#doing the simpsons index
simp1 <- diversity(PA_bci, index = "simpson", MARGIN = 1, base = exp(1))
simpson1 <- as.data.frame(simp1)
simpson1 <- tibble::rowid_to_column(simpson1, "ID")

#Plot indices

par(mfrow = c(1, 3))

alpha_plot2 <- plot(alpha, type = "line", ylab = "species_richness", xlab = "ID", )

shan_plot2 <- plot(shannon1, type = "line", ylab = "shann1", xlab = "ID", )

simp_plot2 <- plot(simpson1, type = "line", ylab = "simp1", xlab = "ID", )



#Patterns

# From the plots we can see that the patterns in Species richness (alpha), Shannon-weaver and simpsons indices have a very similar pattern, almost
#Identical to each other. They fluctuate between sites and have no linear trend, thelowest value occur at site 31 and highest occur in site 25, relatively
#cvlose to each other.


####Question C#########

#                   Inertia Proportion
#Total               6          1
#Unconstrained       6          1

#Importance of components:
 #                      PC1    PC2    PC3    PC4    PC5     PC6
#Eigenvalue            1.5471 1.2030 1.0071 0.9810 0.7615 0.50025
#Proportion Explained  0.2579 0.2005 0.1679 0.1635 0.1269 0.08337
#Cumulative Proportion 0.2579 0.4583 0.6262 0.7897 0.9166 1.00000

#env scores
#Highest
#                PC1      PC2      PC3      PC4       PC5        PC6
#Age.cat        8.392e-01  2.267e-01  3.545e-01  1.116e+00  8.498e-01  9.110e-02
#Stream         8.378e-01  9.264e-01 -4.264e-01 -8.179e-01  3.080e-01  5.933e-01
#lowest
#Elevation     -2.334e-16  2.251e-17 -1.884e-16 -3.718e-16 -3.276e-16  8.296e-17
#Geology        0.000e+00  3.216e-18 -1.884e-16  1.859e-16  5.118e-18 -7.778e-19

#Site scores
#Highest
#    PC1      PC2      PC3      PC4       PC5        PC6
#50  1.14428 -0.28172  0.17889 -1.25481  0.571450  1.062e+00
#1   1.03477  1.13761 -1.03927 -0.29783 -0.324372 -9.121e-02
#Lowest
#38 -0.78461 -0.18971  0.02240  0.13359  0.748824  5.425e-01
#34 -0.76871 -0.05600  0.41980 -0.01424  0.741471  3.969e-01


#The summary of the PCA env data will give you the PC  Eigenvalue  from PC1-6.They have the Eigenvalue of  1.5471, 1.2030, 1.0071, 0.9810, 0.7615 and 0.50025 respectively of the total inertia value of 6. The portion of variables that they explain
#is given as 0.2579, 0.2005, 0.1679, 0.1635, 0.1269 and 0.08337 respectively. Cumulatively they explain 1.00 (100%) of the variations. Pc 1 and PC2 does not explain the majority of the data, Cumulatively only explains 0.4583 (46%) of the variations.
#Only After PC3 is included will the they explain the majority of the variations (above 50 %), with a value of 0.6262 (62%).

#The environmental scores that was the highest was the Age.cat (8.392e-01) and Stream (8.378e-01) and the lowest are elevation (-2.334e-16) and geology (0.000e+00). These environmental factors have the most extreme values on the opposite ends (positive 
#and negative) and so will have the most significant influence on the data.Geology will have no influence on the data, due to its value of 0.

#The sites that are the most significant values in PC1 are sites 50 (1.14428) and 1(1.03477) with the highest positive values (the first and last site). The lowest values on the data is from sites 38 (-0.78461) and 34 (-0.76871). These opposite extremes will have the greatest
#influence on the data. 

####Question D#####

#             Inertia Proportion
#Total           1.463          1
#Unconstrained   1.463          1

#                       CA1     CA2     CA3     CA4     CA5     CA6   
#Eigenvalue            0.08573 0.07513 0.06770 0.06047 0.05591 0.05359 
#Proportion Explained  0.05862 0.05137 0.04629 0.04135 0.03823 0.03664 
#Cumulative Proportion 0.05862 0.10999 0.15628 0.19762 0.23585 0.27249 

#Species scores
#Highest
#                        CA1       CA2        CA3        CA4        CA5        CA6
#Ficus.trigonata        0.910294  0.696359 -2.352e-01  1.2065306  0.1450310 -0.0935792
#Vachellia.melanoceras  0.781645  1.012246  1.503e-01  0.5662438  0.2594313 -1.0630445
#Highest negative
#Alibertia.edulis      -4.017029  1.024256  1.663e-01  0.9795730 -1.5069604 -0.0785815
#Chimarrhis.parviflora -4.017029  1.024256  1.663e-01  0.9795730 -1.5069604 -0.0785815

#Site scores (weighted averages of species scores)
#         CA1      CA2      CA3       CA4      CA5       CA6
#Highest
#44  1.272698  0.47830 -0.01895  2.166669  0.27757 -0.758878
#43  1.165979  1.01137  0.12488  1.449431  0.44576 -0.646203
#Lowest
#13 -4.017029  1.02426  0.16634  0.979573 -1.50696 -0.078581
#18 -3.188640  1.11724  0.66103 -0.450969  1.69738  0.964040


#Findings
#The summary of the CA spe data will give you the CA  Eigenvalue  from CA1-50.The Eigenvalue of  will add up to the total inertia value of 1.463 . The Proportion Explained of the variables of the first  two CA are 0.05862 and 0.0513. Together the explain
#0.10999 (11%) of the variation in this data, this is a very weak value and so this is a bad at capturing the variation that exist across space in the community composition.

#The species scores  was the highest at sp Ficus.trigonata (0.910294)  and sp Vachellia.melanoceras (0.781645). The lowest species score is from species Alibertia.edulis (-4.017029) and sp Chimarrhis.parviflora (-4.017029). All these species exhibit the outer 
#extremes of the species values and so will have the greatest influence on the data.

#The sites that are the most significant values in CA1 are sites 50 (1.035560) and 42 (0.964960) with the highest positive values. The lowest values on the data is from sites 13 (-4.017029) and 18 (-3.188640). These opposite extremes will have the greatest
#influence on the data of the sites. 

####Question E#####

par(mfrow = c(2, 2))

#CA1 sp higest influencial values
with(BCI, tmp <- ordisurf(spe.ca1 ~ Alibertia.edulis, bubble = 3, # setting ca wrt a single species specifically                           
                          family = gaussian, knots = 2, col = 6,                           
                          display = "sites", main = "Alibertia.edulis")) 

abline(h = 0, v = 0, lty = 3)

#Chimarrhis.parviflora

with(BCI, tmp <- ordisurf(spe.ca1 ~ Chimarrhis.parviflora, bubble = 3, # setting ca wrt a single species specifically                           
                          family = gaussian, knots = 2, col = 6,                           
                          display = "sites", main = "Chimarrhis.parviflora")) 

abline(h = 0, v = 0, lty = 3)

#Largest values in CA2

#Abarema.macradenia
with(BCI, tmp <- ordisurf(spe.ca1 ~ Abarema.macradenia, bubble = 3, # setting ca wrt a single species specifically                           
                          family = gaussian, knots = 2, col = 6,                           
                          display = "sites", main = "Abarema.macradenia")) 

abline(h = 0, v = 0, lty = 3)


#Ficus.colubrinae                  

with(BCI, tmp <- ordisurf(spe.ca1 ~ Ficus.colubrinae, bubble = 3, # setting ca wrt a single species specifically                           
                          family = gaussian, knots = 2, col = 6,                           
                          display = "sites", main = "Ficus.colubrinae")) 

abline(h = 0, v = 0, lty = 3)

# A posterior projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active

(spe.ca.env1 <- envfit(spe.ca1, BCI.env, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env1)

# Plot significant variables with a different colour

plot(spe.ca.env1, p.max = 0.05, col = "red")

#CA  analysis is the to use when you want to look at the species data, PCA is more important when looking at the variations in the environmental data. The CA1 analysis of the species will be more important to look at than the CA2. The CA1 will be a better 
#representation of the variations, becuase CA1 explains a larger Proportion off the data than CA2, CA1 explains 0.05862 (6%) of the variation, where CA2 only explains  0.05137 (5%) of the variation. WE can see CA1 has the higher and more significant values.

#From the CA1 plots has higher values where the species occur in abundance than the CA2 plot. This can indicate a higher and significant species score  than the CA2.

####Question F#########

#Non-Metric Multidimensional Scaling (NMDS)

spe.nmds <- metaMDS(PA_bci, distance = "jaccard", choices = c(1,2))
spe.nmds 

spe.nmds$stress

#dv new

plot(spe.nmds, type = "t", main = paste("NMDS/Percentage difference - Stress =", round(spe.nmds$stress, 3)))

#sharpness & goodness plot of fit

par(mfrow = c(1,2))

stressplot(spe.nmds, main = "shepard plot")

gof <- goodness(spe.nmds)

plot(spe.nmds, type = "t", main = "Goodness of fit")

points(spe.nmds, display = "sites", cex = gof*300)


(spe.ca.env1 <- envfit(spe.ca1, BCI.env, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env1)

# Plot significant variables with a different colour

plot(spe.ca.env1, p.max = 0.05, col = "red")


#NMDS 1 nad 3

spe.nmds1 <- metaMDS(PA_bci, distance = "jaccard", choices = c(1,3))
spe.nmds1 

spe.nmds1$stress

#dv new

plot(spe.nmds1, type = "t", main = paste("NMDS/Percentage difference - Stress =", round(spe.nmds$stress, 3)))

#sharpness & goodness plot of fit

par(mfrow = c(1,2))

stressplot(spe.nmds1, main = "shepard plot")

gof <- goodness(spe.nmds1)

plot(spe.nmds1, type = "t", main = "Goodness of fit")

points(spe.nmds1, display = "sites", cex = gof*300)


(spe.ca.env1 <- envfit(spe.ca1, BCI.env, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env1)

# Plot significant variables with a different colour

plot(spe.ca.env1, p.max = 0.05, col = "red")


#NMDS Findings

#A NMDS represent as well as possible the ordering relationships among objects in a small and specified number of axes. Nmds can produce ordinations of objects from any distance matrix. After the function of metaMDS is applied(using the the jaccard method, 
#because the data is binary), we obtain the nmds data. Then by looking at the Stress value of the nmds data we get the value of 0.2722068 This stress value is higher than 0.2, so it tell us that the data is not reliable (stress is the scatter of observed 
#dissimilarities against expected monotone regression). 

#From the shepard/stress plot shows the scatter of values from the red line. The plot shows that the points are relatively scattered, with the majority of the points being located relativey random (scattered) and away from the red line.This indicates that 
#the observed ordination distances and dissimilarity is relatively large and that there are relatively high amounts of stress.

#The goodness of fit plot represents the results from the  goodness() function that looks at the cumulative proportion of inertia accounted by species up to chosen axes. The values of the goodness matrix indicates how well each site is fitted into the data.
#The plot shows the sites with a bubble around them, poorly fitted sites have larger bubbles around them, because they have larger values. This is evident in these sites because they are all very large indcating that they are stressed and this can be because
#the data is unreliable (due toits value being larger than 0.2).The large circles is indicating that the data is poorly fitted.


####Question G#######
?BCI

#i) 
#The anylasis that were done in previous papers like Pyke et.al. (2001), also used NMDS in their study. They focused more on env. data such as dry sites to wet site (Precepitation) which BCI is. Our anylasis will be ables to add to the 
#env data of these analysis by showing the species reponses more clearly of the wet site such as BCI. Other papers like Harms et.al. in 2001, did not include the a NMDS plot.By adding our own NMDS plot to their data we will able to 
#include more environmental dat to the study that mainly focused on species variables like stem density. The NMDS will also be able to explain the stresses that occur within these datasets

#ii) 
#In our anylasis we never looked at the molecular phylogeny such as in the paper of Zanne et.al. in 2014. We also did not look at genetic analysis of DNA barcoding that was done by Kress et.al. in 2009.


# Question 3 --------------------------------------------------------------

#RDA BCI anylasis

spe.hel <-as.tibble(decostand(BCI, "hellinger"))

spe_rda <- rda(spe.hel ~ ., BCI.env)


sum(spe_rda$CA$eig)

sum(spe_rda$CCA$eig)


#Proportion of variation explained

spe_rda$CCA$eig[1] /sum(spe_rda$CCA$eig)+ sum(spe_rda$CA$eig)

summary(spe_rda)#scalinf 2 default

#Canonical coefficients from rda object

coef(spe_rda)

#unadjusted R^2
(R2 <- RsquareAdj(spe_rda)$r.squared)

#Adjusted R^2
(R2adj <- RsquareAdj(spe_rda)$adj.r.squared)

rda_plot <-plot(spe_rda)

#Explanations

#RDA is a method to extract and summarise the variation in a set of response variables that can be explained by a set of explanatory variables. RDA is a direct gradient analysis which summarises linear relationships between components of response variables 
#that are redundant“ wich (i.e. „explained“ by) a set of explanatory variables. 

#              Inertia Proportion
#Total         0.25010     1.0000
#Constrained   0.05936     0.2373
#Unconstrained 0.19074     0.7627


#                       RDA1    RDA2     RDA3     RDA4     RDA5     RDA6
#Eigenvalue            0.02741 0.01119 0.008305 0.005908 0.004009 0.002538
#Proportion Explained  0.46181 0.18844 0.139913 0.099536 0.067539 0.042763
#Cumulative Proportion 0.46181 0.65025 0.790162 0.889698 0.957237 1.000000

#The total inertia of the Constrained RDA data is 0.05936 The eigenvalue ofthe RDA1 fill 0.02741 of the total inertia, this is the largest eigen value that will contribute.
#The Propotion explained by the RDA1 and RDA2 are 0.46181 and 0.18844, respectively. Cumulatively they explain (65%), meaing that they explain more than the half
#the variation of the dataset. It will go to RDA6 before the 1 value is filled, showingthere is 6 RDA eigenvalues.

#Eigenvalues, and their contribution to the variance 
#Importance of components:
#                       RDA1    RDA2     RDA3     RDA4     RDA5     RDA6     PC1     PC2     PC3
#Eigenvalue            0.02741 0.01119 0.008305 0.005908 0.004009 0.002538 0.03681 0.01727 0.01237
#Proportion Explained  0.10961 0.04472 0.033207 0.023624 0.016030 0.010149 0.14717 0.06905 0.04945
#Cumulative Proportion 0.10961 0.15433 0.187539 0.211162 0.227192 0.237342 0.38451 0.45356 0.50301

#The total inertia of the RDA data is 0.25010. The eigenvalue of the RDA1 fills 0.02741 of the total inertia, this is the largest eigen value that will contribute. This will 
#contribute less to the Cumulative variation explains, due to the increase in the number of eigenvalues (PC1-43).  Proportionly it only explains 0.10961 (11%), which is much 
#less significant than the constrained (46%) and RDA2 only explains 0.04472 (4%). Comulatively they explain only 0.15433 (15%) of the variance.


#Species, rows (sites) and environmental data is also included in the RDA values  which gives Species scores, Site scores (weighted sums of species scores)/Site constraints 
#(linear combinations of constraining variables)  and Biplot scores for constraining variables(env), respectively.

#These values are plotted, showing the reposes of species and sites to the environment and each other.