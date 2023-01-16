library(tidyverse)

here::i_am('who_data_obesity.r', uuid = 'ef6803d3-a0b9-4ccb-930d-6d8cb1f55db4')
library(here)

x <- data.table::fread("who_data_obesity.csv") %>% as_tibble()
eur <- filter(x, 
    (`WHO region` == "Europe") | (Country == "United States of America"),
	!is.na(Numeric))
uc <- unique(eur$Country)
vals <- setNames(rep('gray', length(uc)), uc)
toplot <- filter(eur, Sex != "Both sexes")
p1 <- filter(toplot, Country != "United States of America") %>%
    ggplot(aes(x = Year, y = Numeric)) +
	geom_line(aes(group = Country, color = Country)) + 
	scale_color_manual(values = vals, guide = FALSE) +
	theme_bw(base_size = 28) +
	ylab("%Overweight (BMI >= 25)") +
	ggtitle("WHO data") +
	facet_wrap(~ Sex) + xlab("") +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p3 <- p1 + geom_line(data = filter(toplot, Country == "United States of America"), 
                     color = 'red', size = 1)

# Whole world, separate sexes
world <- filter(x, !is.na(Numeric), Country != 'Sudan (former)')
# Sudan has jagged measurements so exclude ...
uc <- unique(world$Country)
vals <- setNames(rep('gray', length(uc)), uc)
toplot <- filter(world, Sex != "Both sexes")
p1 <- filter(toplot, Country != "United States of America") %>%
    ggplot(aes(x = Year, y = Numeric)) +
    geom_line(aes(group = Country, color = Country), alpha = 0.5) + 
    scale_color_manual(values = vals, guide = FALSE) +
    theme_bw(base_size = 28) +
    ylab("%Overweight (BMI >= 25)") +
    ggtitle("WHO data") +
    facet_wrap(~ Sex) + xlab("") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p3 <- p1 + geom_line(data = filter(toplot, Country == "United States of America"), 
                     color = 'red', size = 1)

w <- 10
h <- 6
ggsave("who_data_obesity_world.png", plot = p1, width = w, height = h)
ggsave("who_data_obesity_world_US.png", plot = p3, width = w, height = h)




# Additional interesting plots!
toplot <- filter(x, Sex == "Both sexes", !is.na(Numeric))
uc <- unique(toplot$Country)
vals <- setNames(rep('gray', length(uc)), uc)
p1 <- ggplot(toplot, aes(x = Year, y = Numeric)) +
	geom_line(aes(group = Country, color = Country)) + 
	scale_color_manual(values = vals, guide = FALSE) +
	theme_bw(base_size = 16) +
	ylab("%Overweight (BMI >= 25)") +
	ggtitle("WHO data") +
	facet_wrap(~ `WHO region`) + xlab("")
p1 + geom_line(data = filter(toplot, Country == "France"), color = 'blue', size = 1) +
	geom_line(data = filter(toplot, Country == "United States of America"), color = 'red', size = 1) +
	geom_line(data = filter(toplot, Country == "China"), color = 'green', size = 1) +
	geom_line(data = filter(toplot, Country == "India"), color = 'orange', size = 1)
