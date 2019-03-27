library(shiny)
library(shinydashboard)


header<-dashboardHeader(title="Menu")

sidebar<-dashboardSidebar(sidebarMenu(
  menuItem("Dane",
           menuSubItem("Surowe dane", tabName = "zbior_danych")),
  menuItem("Wykresy", 
           menuSubItem("Wykresy frekwencji", tabName = "frekwencja"),
           menuSubItem("Wykresy zależności", tabName = "zaleznosc")
           ),
  menuItem("Info o aplikacji", tabName = "appinfo")
))

body<-dashboardBody(tabItems(
  
  tabItem("appinfo", verbatimTextOutput("info_text")),
  
  tabItem("zbior_danych",
          infoBoxOutput("infob1"),
          infoBoxOutput("infob2"),
          infoBoxOutput("infob3"),
          tabBox(tabPanel("Surowe dane",tableOutput("surowe_dane")),
                 tabPanel("Wartości odstające", box(plotOutput("wykres_odst"))),
                 tabPanel("Wartości brakujące", tableOutput("braki")), width = NULL)),
  
  tabItem("frekwencja",
          fluidRow(
            box(plotOutput("bar_pietro")),
            #box(plotOutput("bar_dzielnica")),
            box(plotOutput("hist_cena")),
            box(plotOutput("hist_powierzchnia")),
            box(plotOutput("bar_rok.budowy")),
            box(plotOutput("bar_rynek")),
            box(plotOutput("bar_stan")),
            box(plotOutput("bar_wlasnosc")),
            box(plotOutput("bar_zabudowa"))
          )),
  
  tabItem("zaleznosc",
          fluidRow(
            infoBoxOutput("info1"),
            infoBoxOutput("info2"),
            infoBoxOutput("info3"),
            infoBoxOutput("info4"),
            infoBoxOutput("info5")
          ),
          tabBox(title = "Cena vs",
                 side = "right",
                 tabPanel("Piętro", box(plotOutput("Cena_Pietro"))),
                 tabPanel("Dzielnica", (tableOutput("Cena_Dzielnica"))),
                 tabPanel("Rok budowy", (tableOutput("Cena_Rok.budowy"))),
                 tabPanel("Rynek", box(plotOutput("Cena_Rynek"))),
                 tabPanel("Stan wykończenia", box(plotOutput("Cena_Stan_wykonczenia"))),
                 tabPanel("Własność", box(plotOutput("Cena_Wlasnosc"))),
                 tabPanel("Zabudowa", box(plotOutput("Cena_Zabudowa"))),
                 tabPanel("Powierzchnia", box(plotOutput("Cena_Powierzchnia"))), width = NULL)
  )
          
))

ui<- dashboardPage(
  header,
  sidebar,
  body
)

server<-function(input,output,session){
  dane <- read.csv("https://raw.githubusercontent.com/blaszczakm/Web_Scrapping/master/mieszkania_otodom.csv",sep=",")
  output$surowe_dane <- renderTable({head(dane,10)})
  
  output$infob1<-renderInfoBox(infoBox("Ilość ogłoszeń",nrow(dane), color = "red"))
  
  output$info_text<-renderText(paste(
    "Autor: Marek Błaszczak",
    "biblioteki: shiny, shinydashboard, dplyr, ggplot2",
    "Źródło ogłoszeń: otodom.pl",
    sep = "\n"
  ))
  
  #observe aby nie zmienialo od razu ramki danych, aby nie reagowalo od razu
  observe({
    str(dane)
    dane$Cena <- as.double(gsub(",",".",dane$Cena)) #aby moc obliczyc statystyki
    dane$Powierzchnia <- as.double(gsub(",",".",dane$Powierzchnia))
    
    #lokalizacja wartosci odstajacych
    summary(dane$Cena)
    Q1<-quantile(dane$Cena,0.25)
    Q2<-quantile(dane$Cena,0.5)
    Q3<-quantile(dane$Cena,0.75)
    QR=Q3-Q1
    dolna_gr<-Q1-(1.5*QR)
    gorna_gr<-Q2+(1.5*QR)
    odst_1<-which(dane$Cena<dolna_gr)
    odst_2<-which(dane$Cena>gorna_gr)
    length(odst_1) #brak
    length(odst_2) #169
    
    output$infob2<-renderInfoBox(infoBox("Ceny odstające",length(odst_2), color = "yellow"))
    
    #wykres danych odstajacych
    #install.packages("ggplot2")
    
    library(ggplot2)
    odstajace<-ggplot(dane,aes(x=1:length(Cena),y=Cena))+
      geom_point()+
      labs(title = "Wartości odstające",x="Nr ogłoszenia",y="Cena [zł]")+
      geom_hline(aes(yintercept = dolna_gr),color="red",linetype="dashed")+
      geom_hline(aes(yintercept=gorna_gr),color="red",linetype="dashed")
    output$wykres_odst<-renderPlot({odstajace})
    
    #brakujace dane
    #diagnostyka odnosnie brakow danych
    sapply(dane,function(x) sum(is.na(x))) #sa wartosci NA
    sapply(dane,function(x) length(which(x==" ")))
    sapply(dane, function(x) length(which(x==',')))
    sapply(dane, function(x) length(which(x=='?')))
    sapply(dane, function(x) length(which(x==''))) # sa wartosci''
    #usuniecie brakujacych danych
    #'' w Dzielnica, Pietro, Stan_wykonczenia, Wlasnosc, Zabudowa
    dim(dane)
    dane$Dzielnica[which(dane$Dzielnica=="")]<-NA 
    dane$Pietro[which(dane$Pietro=="")]<-NA #zamiana "" na NA 
    dane$Stan_wykonczenia[which(dane$Stan_wykonczenia=="")]<-NA
    dane$Wlasnosc[which(dane$Wlasnosc=="")]<-NA
    dane$Zabudowa[which(dane$Zabudowa=="")]<-NA
    #wartosci odstajace
    a<-sapply(dane,function(x) sum(is.na(x)))
    s<-data.frame("Nazwa kolumny"=colnames(dane),"Ilosc brakow"=a)
    output$braki <- renderTable({s})
    
    #info o brakach
    dane_z_brakami<-dane
    output$infob3<-renderInfoBox(infoBox("Brakujące informacje",sum(is.na(dane_z_brakami)), 
                                         color = "purple"))
    
    #usuwamy odstajace aby nie zaburzylo statystyk
    dane<-dane[-odst_2,]
    
    #usuwamy brakujace
    dane<-na.omit(dane)
    
    output$info1 <- renderInfoBox(infoBox("Liczba pełnych ogłoszeń",paste(length(dane$Cena)),color="yellow"))
    output$info2 <- renderInfoBox(infoBox("Najtańsze mieszkanie",paste(min(dane$Cena),"zł"), color="red"))
    output$info3 <- renderInfoBox(infoBox("Najdroższe mieszkanie",paste(max(dane$Cena),"zł"), color="green"))
    
    output$info4 <- renderInfoBox(infoBox("Średnia cena w Warszawie",paste(round(mean(dane$Cena),2),"zł"),
                                          color = "purple"))
    output$info5 <- renderInfoBox(infoBox("Mediana cen w Warszawie",paste(round(median(dane$Cena),2),"zł"),
                                          color = "blue"))
    
    #faktoryzacja
    #dane dyskretne:
    dane$Rok.budowy<-as.factor(dane$Rok.budowy)
    
    
    bar_pietro<-ggplot(dane)+
      geom_bar(aes(x=Pietro, fill=Pietro))+
      labs(title = "Piętra", x="Nr piętra", y="ilość ogłoszeń")+
      theme(legend.position = "none")
    output$bar_pietro<-renderPlot({bar_pietro})
    
    # bar_dzielnica<-ggplot(dane,aes(x=as.factor(as.numeric(Dzielnica)),
    #                                fill=Dzielnica))+
    #   labs(title = "Dzielnica", x="Nr dzielnicy", y="ilość")+
    #   geom_bar()
    # output$bar_dzielnica<-renderPlot({bar_dzielnica})
    
    hist_cena<-ggplot(dane)+
      geom_density(aes(x=Cena),fill="lightblue")+
      geom_vline(xintercept = mean(dane$Cena))+
      geom_vline(xintercept = median(dane$Cena), color="red")+
      labs(title = "Cena", x="wartość", y="liczba ogłoszeń w %")
    output$hist_cena<-renderPlot({hist_cena})
    
    hist_powierzchnia<-ggplot(dane)+
      geom_density(aes(x=Powierzchnia), fill="lightblue")+
      geom_vline(xintercept = mean(dane$Powierzchnia))+
      geom_vline(xintercept = median(dane$Powierzchnia), color="red")+
      labs(title = "Metraż", x="powierzchnia [m^2]", y="ilość")
    output$hist_powierzchnia<-renderPlot({hist_powierzchnia})
    
    bar_rok<-ggplot(dane,aes(x=Rok.budowy, fill=Rok.budowy))+
      geom_bar()+
      labs(title = "Rok budowy", x="",y="ilość")+
      theme(text = element_text(size = 10))+
      coord_flip()
    output$bar_rok.budowy<-renderPlot({bar_rok})
    
    bar_rynek<-ggplot(dane,aes(x=Rynek, fill=Rynek))+
      geom_bar()+
      labs(title = "Rynek", x="Rynek", y="ilość")+
      theme(legend.position = "none")
    output$bar_rynek<-renderPlot({bar_rynek})
    
    bar_wlasnosc<-ggplot(dane,aes(x=Wlasnosc, fill=Wlasnosc))+
      geom_bar()+
      labs(title = "Rodzaj własności", x="własność", y="ilość")+
      theme(legend.position = "none")
    output$bar_wlasnosc<-renderPlot({bar_wlasnosc})
    
    bar_zabudowa<-ggplot(dane,aes(x=Zabudowa,fill=Zabudowa))+
      geom_bar()+
      labs(title = "Rodzaj zabudowy", x="zabudowa", y="ilość")+
      theme(legend.position = "none")
    output$bar_zabudowa<-renderPlot({bar_zabudowa})
    
    bar_stan<-ggplot(dane,aes(x=Stan_wykonczenia,fill=Stan_wykonczenia))+
      geom_bar()+
      labs(title = "Wykończenie", x="Stan", y="ilość")+
      theme(legend.position = "none")
    output$bar_stan<-renderPlot({bar_stan})
    
    #wykresy zależnosci
    Cena_Pietro <- ggplot(dane)+
      geom_boxplot(aes(x=Pietro, y=Cena, group=Pietro),
                   color="black", fill="lightblue")+
      labs(title = "Zależność ceny od piętra")
    output$Cena_Pietro<-renderPlot({Cena_Pietro})
    
    Cena_Rynek <- ggplot(dane)+
      geom_boxplot(aes(x=Rynek, y=Cena, group=Rynek),
                   color="black", fill="lightblue")+
      labs(title = "Zależność ceny od rynku")
    output$Cena_Rynek<-renderPlot({Cena_Rynek})
    
    Cena_Stan_wykonczenia <- ggplot(dane)+
      geom_boxplot(aes(x=Stan_wykonczenia, y=Cena, group=Stan_wykonczenia),
                   color="black", fill="lightblue")+
      labs(title = "Zależność ceny od stanu wykończenia")
    output$Cena_Stan_wykonczenia<-renderPlot({Cena_Stan_wykonczenia})
    
    Cena_Wlasnosc <- ggplot(dane)+
      geom_boxplot(aes(x=Wlasnosc, y=Cena, group=Wlasnosc),
                   color="black", fill="lightblue")+
      labs(title = "Zależność ceny od rodzaju własności")
    output$Cena_Wlasnosc<-renderPlot({Cena_Wlasnosc})
    
    Cena_Zabudowa <- ggplot(dane)+
      geom_boxplot(aes(x=Zabudowa, y=Cena, group=Zabudowa),
                   color="black", fill="lightblue")+
      labs(title = "Zależność ceny od zabudowy")
    output$Cena_Zabudowa<-renderPlot({Cena_Zabudowa})
    
    Cena_Powierzchnia <- ggplot(dane)+
      geom_smooth(aes(x=Cena, y=Powierzchnia))+
      labs(title = "Zależność ceny od powierzchni", x="Cena", y="Powierzchnia")
    output$Cena_Powierzchnia<-renderPlot({Cena_Powierzchnia})
    
    library(dplyr)
    Cena_Rok.budowy <- dane %>%
      group_by(Rok.budowy) %>%
      summarise(Średnia = mean(Cena),
                 Mediana = median(Cena),
                 Min= min(Cena),
                 Max = max(Cena),
                 Ilość_ogłoszeń=n())
    output$Cena_Rok.budowy <- renderTable({Cena_Rok.budowy})
    ungroup(dane)
    
    Cena_Dzielnica <- dane %>%
      group_by(Dzielnica) %>%
      summarise(Średnia = mean(Cena),
                 Mediana = median(Cena),
                 Min= min(Cena),
                 Max = max(Cena),
                 Ilość_ogłoszeń=n())
    output$Cena_Dzielnica <- renderTable({Cena_Dzielnica})
    ungroup(dane)
    
    
    
    
  })
    
    }

  

shinyApp(ui=ui,server=server)