getwd()
#enters a single number into x x=1
x y<-1
y

#c for combine
v <- c(1,2,3,4,5)
v

#using sequences 1:9
1.5:10

#repetition rep(5,3)

#check the data type class(1)
class("a")

#R as calculator #addition rep(5,3)

#subtraction
124 - 26.82

#multiplication c(5,6,7,8) * 10

#division 40/8

#data structure in R #atomic
5
"Learn Basic R" TRUE

#vector
c(5, 6, 7, 8)
c("a", "b", "c", "d") c(TRUE, FALSE, TRUE, FALSE)

#create vector data

v1 = c(5,6,7,8)
v2 = c("a","b","c","d")
v3 = c(TRUE,FALSE,FALSE,TRUE)

#create data frame
df = data.frame(v1,v2,v3) print(df)
#create data frame using COVID-19 data #create vector data
regency <- c("Yogyakarta", "Sleman", "Bantul", "Kulon Progo", "Gunung Kidul")
cases <- c(2769, 4457, 5233, 1347, 2119)
percentage <- c(0.1739, 0.2769, 0.3286, 0.0846, 0.1331)

#create percentage vector
percent <- scales::percent(percentage, accuracy = 0.01) df = data.frame(regency,cases,percent)
print(df)

#create data frame
c19diy = data.frame(regency, cases, percent)

#print data frame print(c19diy) View(c19diy)

#create matrix mx_value = c(1:12)
mx = matrix(mx_value, 3, 4) print(mx)
View(mx)

#create list data
l1 = list(1,"SIG", FALSE, v1, df, mx) print(l1)
View(l1)

#export data
write.csv(c19diy, "Covid 19 Cases in DIY 2022.csv")

#import from external data
gcc19 = read.csv('WHO-COVID-19-global-data.csv') head(gcc19)
tail(gcc19) unique(gcc19$WHO_region)

#manage the data frame

library(magrittr) library(dplyr) library(tidyverse) library(ggplot2)

#select COVID19 cases in China from 2022
gbc19 = gcc19 %>% filter(Country == "China", Date_reported > "01/02/2022") View(gbc19)

#filter the table
gbc19 = select(gbc19, c("Date_reported", "Country", "New_cases", "Cumulative_cases", "New_deaths", "Cumulative_deaths")) View(gbc19)

colnames(gbc19) = c("date", "country", "new_cases","cum_cases", "new_deaths", "cum_deaths")
View(gbc19)


#data visualization for descriptive statistics #bar chart
barplot(cases, names.arg = regency) barplot(cases, names.arg = regency, col = "blue")

#bar chart 2 library(ggplot2)
barplot_c19diy = ggplot(data = c19diy, aes(x = regency, y = cases)) + geom_bar(stat = "identity", fill = "brown") + (labs(title = "Covid-19 Cases in DIY 2022", x = "regency", y = "cases")) + theme(plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label = stat(cases)), stat = "identity", vjust = -0.5)
print(barplot_c19diy)

#pie chart
piec19 = data.frame(regency, cases, percentage) piec19 = piec19 %>%
  arrange(desc(regency)) %>%
  mutate(prop = cases / sum(piec19$cases) * 100) %>% mutate(ypos = cumsum(prop)- 0.5*prop)

ggplot(piec19, aes(x ="", y = prop, fill = regency)) + geom_bar(stat = "identity", width = 1, color = "white") + coord_polar("y", start = 0) + theme_void() + theme(legend.position = "right") + geom_text(aes(y = ypos, label = cases), color = "white", size = 3) + scale_fill_brewer(palette = "Set2")
ggplot(piec19, aes(x ="", y = prop, fill = regency)) + geom_bar(stat = "identity", width = 1, color = "white") + coord_polar("y", start = 0) + theme_void() + geom_text(aes(y = ypos, label = percentage), color = "white", size = 2.5) + labs(x = NULL, y = NULL, fill = NULL, title = "Covid 19 Cases in
 
DIY Jan 2022", size = 4) + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, color = "#666666")) + scale_fill_manual(values = c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419"))

#create pareto chart
pc19 = data.frame(regency,cases)
pc19 = arrange(pc19, desc(cases)) %>% mutate(
  cumsum = cumsum(cases),
  freq = round(cases / sum(cases), 3), cum_freq = cumsum(freq))
print(pc19)

#pareto chart version 1 pcv1 = barplot(pc19$cases,
width = 1, space = 0.2, border = NA, axes = F, ylim = c(0, 1.05 * max(pc19$cumsum, na.rm = T)), ylab = "Cummulative Counts" , cex.names = 0.7, xlab = "Regency" ,names.arg = pc19$regency, main = "Pareto Chart (version 1)")
lines(pcv1, pc19$cumsum, type = "b", cex = 0.7, pch = 19, col="cyan4") box(col = "grey62")
axis(side = 2, at = c(0, pc19$cumsum), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, pc19$cumsum), labels = paste(c(0, round(pc19$cum_freq
                                                                 * 100)) ,"%",sep=""),
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)

#pareto chart version 2 pcv2 = barplot(pc19$cases,
width = 1, space = 0.2, border = NA, axes = F, ylim = c(0, 1.05 * max(pc19$cases, na.rm = T)), ylab = "Counts" , cex.names = 0.7,
xlab = "Regency" , names.arg = pc19$regency,
main = "Pareto Chart (version 2)")
axis(side = 2, at = seq(0, max(pc19$cases, na.rm = T), by= max(pc19$cases, na.rm = T) / 5),
     las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8) box(col = "grey62")
px <- pc19$cum_freq * max(pc19$cases, na.rm = T)
lines(pcv2, px, type = "b", cex = 0.7, pch = 19, col = "cyan4")

axis(side = 4, at = seq(0, max(px), by = max(px) / 5), labels = paste0(seq(0, 100, by = 20), "%"),
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)

#create pareto using qcc library install.packages("qcc") library(qcc)
pareto.chart(pc19$cases, main="Covid-19 Cases in DIY Jan 2022")

#create frequency table library(summarytools)
freqwhoreg= freq(gcc19$WHO_region, plain.ascii = F, report.nas = F) View(freqwhoreg)
