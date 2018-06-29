install.packages("ggplot2")
library("ggplot2")
install.packages("readxl")
library("readxl")
install.packages("stringr")
library("stringr")
install.packages("dplyr")
library("dplyr")
install.packages("plyr")
library("plyr")
install.packages("ggmap")
library("ggmap")
install.packages("reshape2")
library("reshape2")

#Read in all the tabls from the file 
for (i in seq(1, 11)){
  df.CoC <- paste("df.CoC_", i, sep = "")
  assign(df.CoC, read_excel("2007-2017-PIT-Counts-by-CoC.xlsx", sheet = i))
  temp_df.CoC <- get(df.CoC)
  temp_col <- colnames(temp_df.CoC)
  #Extract the year from the column name
  #Extract numbers of exactly '4' length
  year <- str_extract(temp_col, "\\d{4}+")
  #Set a column 'Year' to the extracted year
  #Remove years from the column names
  temp_col_no_year <- sub(",.*", "", temp_col)
  
  colnames(temp_df.CoC) <- temp_col_no_year
  temp_df.CoC$Year <- year[3]
  assign(df.CoC, temp_df.CoC)
}

#Create the 2007-2017 dataframe by 'bind_row()' 
df.CoC <- lapply(df.CoC_1, as.character)
for (i in seq(2, 11)){
  df <- paste("df.CoC_", i, sep = "")
  test2 <- lapply(get(df), as.character)
  df.CoC <- bind_rows(df.CoC, test2)
}
df.CoC_Cleaned <- df.CoC[!is.na(df.CoC$`CoC Number`) & !is.na(df.CoC$`CoC Name`) & df.CoC$`Total Homeless` != '.', ]
df.CoC_Cleaned <- data.frame(df.CoC_Cleaned)

homeless_per_year <- ddply(df.CoC_Cleaned, "Year", summarise, total_homeless = sum(as.numeric(Total.Homeless)))
ggplot(homeless_per_year, aes(Year, total_homeless, group = 1)) +
  geom_line()

df.OAS <- subset(df.CoC_Cleaned, df.CoC_Cleaned$`CoC.Number` == 'FL-507' | 
                   df.CoC_Cleaned$`CoC.Number` == 'WA-500' | 
                   df.CoC_Cleaned$`CoC.Number` == 'GA-500' |
                   df.CoC_Cleaned$`CoC.Number` == 'OH-502')
df.OAS_total_homeless <- ddply(df.OAS, "Year", summarise, total_homeless_OAS =
                                 sum(as.numeric(Total.Homeless)), unsheltered = sum(as.numeric(Unsheltered.Homeless)))

#Add city, state column for geocode lat/lon lookup
df.OASC_2017 <- subset(df.OAS, df.OAS$Year == 2017)
df.OASC_2017$City <- c("Orlando, FL", "Atlanta, GA", "Cleveland, OH", "Seattle, WA")
#for each city in the df, query and save the longitude and latitude location using geocode
for (i in seq(1, 4)){
  temp <- geocode(df.OASC_2017$City[i])
  df.OASC_2017$lon[i] <- temp$lon
  df.OASC_2017$lat[i] <- temp$lat
}
US_map <- qmap("United States", zoom = 4) 
US_map +
  geom_point(data = df.OASC_2017, aes(x= lon, y = lat, size = as.numeric(Total.Homeless)), color = "red") +
  geom_label(data = df.OASC_2017, aes(x= lon + 7, y = lat + 1, label = City), size = 3, color = "red", fill = "white") +
  geom_label(data = df.OASC_2017, aes(x= lon + 4.75, y = lat - 1, label = Total.Homeless), size = 3, color = "red", fill = "white") 

ggplot() +
  geom_line(data = df.OAS_total_homeless, aes(x = as.numeric(Year), y = total_homeless_OAS)) +
  geom_text(label = df.OAS_total_homeless$total_homeless_OAS) +
  scale_x_continuous(breaks = seq(2007, 2017)) +
  ggtitle("Total Homeless - ORL, ATL, SEA, CLE") +
  xlab("Year") +
  ylab("Homeless Population")

ggplot(data = df.OAS, aes(x = as.numeric(Year), y = as.numeric(Total.Homeless))) +
  geom_bar(aes(fill = CoC.Number), stat = "identity") +
  ggtitle("Total Homeless per city - ORL, ATL, SEA, CLE") +
  xlab("Year") +
  ylab("Homeless Population")

ggplot(df.OAS, aes(as.numeric(Year), as.numeric(Total.Homeless), color = CoC.Number)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2007, 2017)) +
  ggtitle("Total Homeless per city - ORL, ATL, SEA, CLE") +
  xlab("Year") +
  ylab("Homeless Population")

df_OASC_melted <- melt(df.OASC_2017, id.var = c("CoC.Number", "CoC.Name", "City"))

shelter_vs_not <- c("Sheltered.Homeless", "Unsheltered.Homeless")

df_OASC_sheltered_vs_not <- subset(df_OASC_melted, 
                                   df_OASC_melted$variable %in% shelter_vs_not)

ggplot(data = df_OASC_sheltered_vs_not, aes(City, as.numeric(value), group = variable)) +
  geom_col(aes(fill = variable)) +
  geom_text(aes(label = value, y = as.numeric(value) + 10), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

Youth <- c("Homeless.Unaccompanied.Youth..Under.25.", "Sheltered.Homeless.Unaccompanied.Youth..Under.25.", "Unsheltered.Homeless.Unaccompanied.Youth..Under.25.")

df_OASC_Youth <- subset(df_OASC_melted, 
                        df_OASC_melted$variable %in% Youth)

levels(df_OASC_Youth$variable)[levels(df_OASC_Youth$variable) == "Homeless.Unaccompanied.Youth..Under.25."] <- "Homeless.Youth"
levels(df_OASC_Youth$variable)[levels(df_OASC_Youth$variable) == "Sheltered.Homeless.Unaccompanied.Youth..Under.25."] <- "Sheltered.Youth"
levels(df_OASC_Youth$variable)[levels(df_OASC_Youth$variable) == "Unsheltered.Homeless.Unaccompanied.Youth..Under.25."] <- "Unsheltered.Youth"

ggplot(data = df_OASC_Youth, aes(City, as.numeric(value), group = variable)) +
  geom_col(aes(fill = variable)) +
  geom_text_repel(aes(label = value, y = as.numeric(value)), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "top")

