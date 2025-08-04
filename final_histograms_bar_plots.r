library(tidyverse)
library(ggplot2)
library(shadowtext)
library(scales) 

#--------------# Loading trait data
td <- read.csv("species-level_trait.csv")

# Abbreviation of trait name
names(td)[c(5,8,11:14,17,20:30)] <- c("Shell_length","Shell_width","Shell_depth","R1","R2","Shell_shape",
                     "Aperture_length","Aperture_width","R3","R4","Aperture_position","Aperture_invagination",
                     "Aperture_rim","Collar","Shell_cover","Partition","Spine","Feeding_type"
                     )

td$Shell_shape <- factor(td$Shell_shape, levels = c("sphere", "hemisphere", "cylinder", "patelliform", "rectangular cuboid", "ovoid", "pyriform", "spiral"))
td$Aperture_position <- factor(td$Aperture_position, levels = c("straight terminal", "sub terminal", "central ventral", "shifted ventral", "amphistomic"))
td$Aperture_invagination <- factor(td$Aperture_invagination, levels = c("absent", "slightly", "strongly"))
td$Aperture_rim <- factor(td$Aperture_rim, levels = c("straight", "curved", "lobbed", "denticular"))
td$Shell_cover <- factor(td$Shell_cover, levels = c("organic", "xenosomes", "idiosomes", "cleptostomes"))
td$Feeding_type <- factor(td$Feeding_type, levels = c("mixotrophy", "bacterivory", "predatory"))
td$Collar <- factor(td$Collar, levels = c("presence", "absence"))
td$Spine <- factor(td$Spine, levels = c("presence", "absence"))
td$Partition <- factor(td$Partition, levels = c("presence", "absence"))

# Drawing format
th <- theme(plot.title=element_text(size=20, color="black", family  = "sans", face= "bold",vjust=0.5,hjust=0.5),
            axis.line=element_line(size=.5, colour="black"),
            axis.ticks=element_line(color="black"),
            axis.text= element_text(size = 12, color="black", family  = "sans", face= "bold", vjust=0.5, hjust=0.5),
            axis.title = element_text(size=20, color="black", family  = "sans",face= "bold", vjust=0.5, hjust=0.5),
            legend.position="none",
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(colour = 'black', size = 20,  family  = "sans",face = 'bold'),
            legend.title=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank()  ) 


# Figure 9. Number of species in each genus for trait data table.
spe.sm <- td |> group_by(Genus) |> summarise(n = n())

spe.sm %>%
  ggplot(aes(x = reorder(Genus, -n), y = n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=0.7, width=0.8) +
  coord_flip() +
  labs(x = "", y= "Number of species") +
  th 


# Figure 10. Histogram of distribution for the numerical traits
ggplot(data = td, aes(x = Shell_length)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Average shell length", y= "Count") +
  th



ggplot(data = td, aes(x = Shell_width)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Average shell width", y= "Count") +
  th



ggplot(data = td, aes(x = Shell_depth)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Average shell depth", y= "Count") +
  th



ggplot(data = td, aes(x = Aperture_length)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Average aperture length", y= "Count") +
  th



ggplot(data = td, aes(x = Aperture_width)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "Average aperture width", y= "Count") +
  th



ggplot(data = td, aes(x = R1)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R1", y= "Count") +
  th



ggplot(data = td, aes(x = R2)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R2", y= "Count") +
  th



ggplot(data = td, aes(x = R3)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R3", y= "Count") +
  th



ggplot(data = td, aes(x = R4)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(x = "R4", y= "Count") +
  th



# Creating the summary of the frequency for all categorical and binary traits to plot Figure 11.

# Creating the summary of the frequency for each type of shell shape.
td.shape <- td |> 
  count(Shell_shape, sort = TRUE) |>  
  rename(var = Shell_shape) |>       
  mutate(trait = "Shell shape")

# Creating the summary of the frequency for each type of aperture position.
td.pos <- td |> 
  count(Aperture_position, sort = TRUE) |>
  rename(var = Aperture_position) |>
  mutate(trait = "Position of the aperture")

# Creating the summary of the frequency for each type of degree of invagination of aperture.
td.inv <- td |> 
  count(Aperture_invagination, sort = TRUE) |>
  rename(var = Aperture_invagination) |>
  mutate(trait = "Degree of invagination of aperture")

# Creating the summary of the frequency for each type of aperture rim.
td.rim <- td |> 
  count(Aperture_rim, sort = TRUE) |>
  rename(var = Aperture_rim) |>
  mutate(trait = "Aperture rim")

# Creating the summary of the frequency for presence/absence of collar.
td.collar <- td |> 
  count(Collar, sort = TRUE) |>
  rename(var = Collar) |>
  mutate(trait = "Collar")

# Creating the summary of the frequency for each type of shell covering.
td.cover <- td |> 
  count(Shell_cover, sort = TRUE) |>
  rename(var = Shell_cover) |>
  mutate(trait = "Shell covering")

# Presence/absence of internal partitions
td.Partition <- td |> 
  count(Partition, sort = TRUE) |>
  rename(var = Partition) |>
  mutate(trait = "Internal partitions")

# Creating the summary of the frequency for presence/absence of spines or horns.
td.Spine <- td |> 
  count(Spine, sort = TRUE) |>
  rename(var = Spine) |>
  mutate(trait = "Spines or horns")

# Creating the summary of the frequency for each feeding type.
td.feed <- td |> 
  count(Feeding_type, sort = TRUE) |>
  rename(var = Feeding_type) |>
  mutate(trait = "Feeding type")

counts <- rbind(td.shape,  td.cover, td.pos, td.inv, td.rim, td.feed, td.collar, td.Partition, td.Spine)
counts$trait <- factor(counts$trait, levels = c("Shell shape","Shell covering","Position of the aperture",
                                                "Degree of invagination of aperture","Aperture rim","Feeding type",
                                                "Collar","Internal partitions","Spines or horns"))


# Figure 11. Barplot of distribution for the categorical and binary traits.
counts |> ggplot(aes(x = trait, y = n, fill = var)) + 
  geom_col(color = 1, position = position_dodge2(width = 15, preserve = "single"), show.legend =  F) +
  scale_fill_manual(values = c('#B3DE69','#B3DE69','#B3DE69','#B3DE69','#B3DE69','#B3DE69','#B3DE69','#B3DE69',
                               '#f768a1','#f768a1','#f768a1','#f768a1',        
                               '#E69F00','#E69F00','#E69F00','#E69F00','#E69F00',
                               '#80B1D3','#80B1D3','#80B1D3',
                               '#bcbddc','#bcbddc','#bcbddc','#bcbddc',
                               '#66c2a5','#66c2a5','#66c2a5',
                               '#fc9272','#fc9272','#fc9272','#fc9272','#fc9272','#fc9272')) +
  labs(x = "", y = "Frequency") +
  ylim(0, 400) +
  geom_text(aes(label = var, angle = 90), position = position_dodge2(width = 0.9, preserve = "single"),
                  vjust = 0, hjust = -0.05, colour = "black") +
  scale_x_discrete(labels = label_wrap(10)) +
  th 




