library(tidyverse)
library(gapminder)
#2 continuous variables - scatterplot examples
ggplot(gapminder, aes(x=gdpPercap, y = lifeExp)) + geom_point() #scatterplot
ggplot(gapminder, aes(x=gdpPercap, y = lifeExp)) + geom_point(color = "blueviolet")
ggplot(gapminder, aes(x=gdpPercap, y = lifeExp, color = continent)) + geom_point() #color changes by continent
ggplot(gapminder, aes(x=gdpPercap, y = lifeExp, color = continent, size = pop)) + geom_point() #size changes based on pop

#1 continuous variables, 1 discrete - histogram
ggplot(gapminder, aes(x = lifeExp)) + geom_histogram() #histogram
ggplot(gapminder, aes(x = lifeExp)) + geom_density(color = "orange", fill = "turquoise") #density plot
ggplot(gapminder, aes(x = lifeExp, fill = continent)) + geom_density(alpha = .2) #alpha for transparency

#single variable across groups: boxplot.

ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_boxplot() #boxplot
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_violin() #violinplot
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_violin() + geom_jitter(alpha = .1) #can add more layers too!
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_jitter(alpha = .1) + geom_violin() #second one goes on top

#change across time
ggplot(gapminder, aes(x = year,  y = lifeExp)) + geom_point()
ggplot(gapminder, aes(x = year, y = lifeExp, color = continent)) + geom_line(aes(group= country))
ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_point() + geom_smooth()
#lin model works here

#bargraph for discrete set of x vals
ggplot(gapminder, aes(x=gdpPercap, y = lifeExp)) + geom_point() + geom_smooth()
gapminder%>%filter(year ==2007) %>% ggplot(aes(x = continent)) + geom_bar()

#use summarize, groupby to use multiple years
gapminder %>%filter(year==2007 | year == 1952) %>% group_by(continent, year) %>% summarize(avgle = mean(lifeExp)) %>% ggplot(aes(x=continent, y = avgle)) + geom_col(aes(fill = year))
#Need to fix 2 probs- yearcolor is a gradient and colors stacked on top of each other
gapminder %>%filter(year==2007 | year == 1952) %>% group_by(continent, year) %>% summarize(avgle = mean(lifeExp)) %>% ggplot(aes(x=continent, y = avgle)) + geom_col(aes(fill = factor(year)), position = "dodge")
#aes iis a function that defines a mapping from data to geom aesthetics
#generally not supposed to use $ inside of an aes()

#example of labeling, and using different data subset for different parts
to_label <- gapminder %>% filter(year ==2007 & pop > 100000000) 
gapminder %>% filter(year ==2007) %>% ggplot(aes(gdpPercap, lifeExp)) + geom_point(aes(size = pop, color = continent)) + geom_text(aes(label = country), data = to_label)

#faceting - divide plot in subgroups facet_wrap, facet_grid
ggplot(gapminder, aes(x = lifeExp, fill = continent)) + geom_density() + facet_wrap(~continent)

#scales map data to aesthetic values. (use for color!)
# Use saved (GGPLOT) + scale_color_manual(values = c("1" = "#F2CED8", "2" = "#88B8B8"))
#USe scale_color_gradientn(colors = c("blue", "red", "green")) for continuous var.

#use reorder to change order of factors to change plot order.
#use +labs(x = "Xname", y = "Yname", title = "Plot Title") to label plot
#For equations, use expression()
#ggplot themes ( p + theme_grey(), bw(), linedraw(), light(), dark(), minimal()
