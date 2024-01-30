library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)
library(grid)
library(tidyverse)

# Assuming you have two data frames: migration_df and voting_df
# The migration_df contains columns 'Year' and 'Zuzüge - bereinigt'
# The voting_df contains columns 'Year', 'SPD', 'CDU', 'Grüne', 'FDP', 'Linke', 'PDS', 'AfD'

migration_df <- read.csv("migration_data.csv")
voting_df <- read.csv("voting_data.csv")

# Convert the 'Year' column to a Date type if it's not already
migration_df$Year <- as.Date(as.character(migration_df$Year), format = "%Y")
voting_df$Year <- as.Date(as.character(voting_df$Year), format = "%Y")

# Umwandeln des Voting-Datenframes in ein long format für ggplot
voting_long <- melt(voting_df, id.vars = "Year", variable.name = "Party", value.name = "Percentage")

# Transformation der Migrationsdaten (nur als Beispiel, Sie müssen dies an Ihre Daten anpassen)
max_migration <- max(migration_df$`Zuzüge`, na.rm = TRUE)
max_percentage <- 100  # Maximaler Prozentsatz für Wahl
scaling_factor <- max_migration / max_percentage

# Define the color mapping for the parties
color_mapping <- c("Migration" = "grey", "SPD" = "red", "CDU" = "black", "Grüne" = "green",
                   "FDP" = "yellow", "Linke" = "darkred", "PDS" = "purple", "AfD" = "blue")

migration_df$TransformedMigration <- migration_df$`Zuzüge` / scaling_factor

# Replace NA values with 0 in migration_df
migration_df[is.na(migration_df)] <- 0

# Replace NA values with 0 in voting_long
voting_long[is.na(voting_long)] <- 0

# Plot the data
ggplot() +
  geom_line(data = migration_df, aes(x = Year, y = TransformedMigration, color = "Migration"), linewidth = 1) +
  geom_line(data = voting_long, aes(x = Year, y = Percentage, color = Party), linewidth = 1) +
  scale_x_date(breaks = voting_df$Year, labels = format(voting_df$Year, "%Y")) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), name = "Stimmenanteil in Prozent",
                     labels = label_comma(),) +
  scale_color_manual(values = c("Migration" = "grey", "SPD" = "red", "CDU" = "black", "Grüne" = "green",
                                "FDP" = "yellow", "Linke" = "darkred", "PDS" = "purple", "AfD" = "blue")) +
  labs(x = "Jahr",
       title = "Wahlverhalten und Migration ab 2000",
       color = "Legende") +
  theme_minimal()

# Plot only the migration data
migration_plot <- ggplot() +
  geom_line(data = migration_df, aes(x = Year, y = `Zuzüge`, color = "Migration"), linewidth = 1) +
  scale_x_date(breaks = migration_df$Year, labels = format(migration_df$Year, "%Y")) +
  scale_y_continuous(name = "Zuzüge in absoluten Werten",
                     labels = label_comma()) +
  scale_color_manual(values = c("Migration" = "grey")) +
  labs(x = "Jahr",
       title = "Migration ab 2000",
       color = "Legende") +
  theme_minimal()

# Display the plot
ggsave("migration_plot.png", migration_plot, width = 13, height = 20)

# Get the unique parties
parties <- unique(voting_long$Party)

# Create an empty list to store the plots
plots <- list()

# Subset the data to include only the years from 2000 onwards
migration_df <- subset(migration_df, Year >= as.Date("2002-01-01"))
voting_long <- subset(voting_long, Year >= as.Date("2002-01-01"))

# Loop over each party
for (i in seq_along(parties)) {
  # Subset the data for the current party
  party_data <- subset(voting_long, Party == parties[i])

  # Create the plot for the current party
  p <- ggplot() +
    geom_line(data = party_data, aes(x = Year, y = Percentage, color = Party), linewidth = 1) +
    geom_line(data = migration_df, aes(x = Year, y = TransformedMigration, color = "Migration"), linewidth = 1) +
    scale_x_date(breaks = voting_df$Year, labels = format(voting_df$Year, "%Y")) +
    ylim(0, 100) +  # Adjust the y-axis limits here
    scale_y_continuous(
      name = "Stimmenanteil in Prozent",
      labels = label_comma(),
      sec.axis = sec_axis(~ . * scaling_factor, name="Zuzüge in absoluten Werten", labels = label_comma())
    ) +
    labs(x = "Jahr", y = "Wahlprozentsatz",   color = "Legende",
    title = paste("Stimmenanteil der", parties[i], "ab 2002")) +
    scale_color_manual(values = c(color_mapping, "Migration" = "grey")) +
    theme_minimal()

  # Add the plot to the list
  plots[[i]] <- p
}

# Combine all the plots into one grid
combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 1))

ggsave("combined_plot.png", combined_plot, width = 13, height = 20)

# Replace NA values with 0 in voting_df
voting_df <- replace_na(voting_df, list(SPD = 0, CDU = 0, Grüne = 0, FDP = 0, Linke = 0, PDS = 0, AfD = 0))

# Calculate the difference to the last period
voting_df <- voting_df %>%
  mutate(SPD_diff = SPD - lag(SPD, default = 0),
         CDU_diff = CDU - lag(CDU, default = 0),
         Grüne_diff = Grüne - lag(Grüne, default = 0),
         FDP_diff = FDP - lag(FDP, default = 0),
         Linke_diff = Linke - lag(Linke, default = 0),
         PDS_diff = PDS - lag(PDS, default = 0),
         AfD_diff = AfD - lag(AfD, default = 0))

# Reshape the data to a long format
voting_diff_long <- voting_df %>%
  pivot_longer(cols = ends_with("_diff"), names_to = "Party", values_to = "Difference")

# Remove the '_diff' from the party names
voting_diff_long$Party <- sub("_diff", "", voting_diff_long$Party)

# Subset the data to include only the years from 2002 to 2021
voting_diff_long <- subset(voting_diff_long, Year >= as.Date("2002-01-01") & Year <= as.Date("2021-12-31"))

# Plot the difference data
ggplot(voting_diff_long, aes(x = Year, y = Difference, color = Party)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = color_mapping) +
  scale_x_date(breaks = voting_df$Year, labels = format(voting_df$Year, "%Y")) +
  scale_y_continuous(breaks = seq(-100, 100, by = 5)) +
  labs(x = "Jahr", y = "Difference in Voting Percentage", color = "Legende",
       title = "Yearly Difference in Voting Percentage for Each Party") +
  theme_minimal()

# Calculate the percentage difference in migration data
migration_df <- migration_df %>%
  mutate(Migration_diff = ((`Zuzüge` - lag(`Zuzüge`, default = `Zuzüge`[1])) / lag(`Zuzüge`, default = `Zuzüge`[1])) * 100)

# Reshape the migration data to a long format
migration_diff_long <- migration_df %>%
  pivot_longer(cols = Migration_diff, names_to = "Party", values_to = "Difference")

# Remove the '_diff' from the party names
migration_diff_long$Party <- sub("_diff", "", migration_diff_long$Party)

# Select only the necessary columns
voting_diff_long <- voting_diff_long[, c("Year", "Party", "Difference")]
migration_diff_long <- migration_diff_long[, c("Year", "Party", "Difference")]

# Combine the voting and migration difference data
combined_diff_long <- rbind(voting_diff_long, migration_diff_long)

# Create an empty list to store the plots
diff_plots <- list()

# Loop over each party
for (i in seq_along(parties)) {
  # Subset the data for the current party
  party_diff_data <- subset(combined_diff_long, Party == parties[i])

  migration_diff_data <- subset(migration_diff_long, Party == "Migration")

  # Create the bar chart for the current party
  p_diff <- ggplot(party_diff_data, aes(x = Year, y = Difference, fill = Party)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_bar(data = migration_diff_data, aes(x = Year, y = Difference, fill = Party), stat = "identity", position = "dodge") +
    scale_fill_manual(values = color_mapping) +
    scale_x_date(breaks = voting_df$Year, labels = format(voting_df$Year, "%Y")) +
    scale_y_continuous(breaks = seq(-100, 100, by = 5)) +
    labs(x = "Jahr", y = "Veränderung in Prozent", fill = "Legende",
         title = paste("Veränderung des Wahlergebnis der", parties[i])) +
    theme_minimal()

  # Add the plot to the list
  diff_plots[[i]] <- p_diff
}

# Combine all the bar charts into one grid
combined_diff_plot <- do.call(gridExtra::grid.arrange, c(diff_plots, ncol = 1))

ggsave("combined_diff_plot.png", combined_diff_plot, width = 13, height = 20)