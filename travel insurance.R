# Load Library ------------------------------------------------------------

library(readr)
library(dplyr)
library(extrafont)
library(ggplot2)
library(paletteer)
library(moments)
library(reshape2)
library(gghighlight)



# Load Data ---------------------------------------------------------------

travel_insurance <- read_csv(url("https://raw.githubusercontent.com/mhdzahier/Data-Visualization-Travel-Insurance/master/travel%20insurance.csv"))


# Data Summary ------------------------------------------------------------

travel_insurance %>% summary()


# Filter Age --------------------------------------------------------------

travel_insurance<-travel_insurance%>%
  filter(Age !=118,
         Age>=0)


# Change data structure ---------------------------------------------------


travel_insurance$Claim<-as.factor(travel_insurance$Claim)
travel_insurance$Agency<-as.factor(travel_insurance$Agency)
travel_insurance$`Agency Type`<-as.factor(travel_insurance$`Agency Type`)



# Create theme ------------------------------------------------------------

zahier_theme <- function(){
  list(
    theme_minimal(),
    theme(text = element_text(colour = "white", family = "Air Americana"),
          plot.title = element_text(family = "Air Americana", size = 20,face = "bold",
                                    margin = margin(b=8)),
          plot.subtitle  = element_text(size = 14,family = "Air Americana"),
          plot.background = element_rect(fill = "#363232"),
          panel.background = element_rect(fill = "#363232",
                                          colour = "#363232"),
          panel.grid = element_line(colour = "gray40"),
          panel.grid.major = element_blank(),
          axis.text = element_text(size=15, colour = "white"),
          axis.title = element_text(size=15))
  )
}



# Plot graph 1 ------------------------------------------------------------

travel_insurance%>%
  group_by(Agency)%>%
  summarise(total=n())%>%
  arrange(desc(total))%>%
  top_n(5)%>%
  ggplot(aes(x=reorder(Agency,-total),y=total,fill=Agency),alpha=0.3)+
  geom_bar(stat="identity", width = 0.5, fill ="#2E86C1" )+
  geom_text(aes(label=total), vjust=-0.5, colour="white",size=5, family= "Air Americana")+
  gghighlight(Agency == "EPX")+
  labs(title="Travel Insurance Agencies in Singapore",
       subtitle="Top 5 Singapore Travel Insurance Agencies in 2017",
       x="Agency",
       y="Total",
       caption="@mhdzahier")+
  zahier_theme()



ggsave(filename = "graph1.png", device = "png",
       width = 14,
       height = 9 )


# Plot graph 2 ------------------------------------------------------------

travel_insurance %>% 
  ggplot(aes(x=Age)) + 
  geom_histogram(aes(y = ..density..), fill="white",binwidth = 1.45)+
  geom_density(alpha = .6, fill="#2E86C1")+
  labs(title= "Age of Travel Insurers",
       caption="@mhdzahier")+
  zahier_theme()

ggsave(filename = "graph2.png", device = "png",
       width = 14,
       height = 9 )



# Plot graph 3 ------------------------------------------------------------

travel_insurance%>%
  filter(Duration<=1000)%>%
  ggplot(aes(x=Duration,y=Age,colour=`Agency Type`,fill=`Agency Type`))+
  geom_point(position = position_jitter(w = 1, h = 1))+
  geom_smooth(aes(group=`Agency Type`),method = "lm",color="white",size=1)+
  labs(title="Duration vs Age of Travel Insurers",
       caption="@mhdzahier")+
  scale_fill_manual(values = c("#2E86C1","#F7DC6F"))+
  scale_colour_manual(values = c("#2E86C1","#F7DC6F"))+
  zahier_theme()
  
ggsave(filename = "graph3.png", device = "png",
       width = 14,
       height = 9 )



# Plot graph 4 ------------------------------------------------------------

travel_insurance%>%
  ggplot(aes(x=Claim,y=Age))+
  geom_boxplot(aes(fill=Claim),colour="white",alpha=0.5, outlier.color = "white",outlier.alpha = 0.1)+
  labs(title="Travel Insurers: How old are they?",
       subtitle= "Separate by the Claim Status in 2017",
       caption="@mhdzahier")+
  scale_fill_manual(values = c("#2E86C1","#F7DC6F"))+
  zahier_theme()+
  theme(legend.position = c(0.5, 0.15),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#363232"),
        legend.box.background = element_rect(fill = "#363232"),
        legend.key = element_rect(fill = "#363232", colour= "#363232"),
        legend.title = element_text(size = 15))


ggsave(filename = "graph4.png", device = "png",
       width = 14,
       height = 9 )

  
