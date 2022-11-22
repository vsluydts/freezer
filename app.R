suppressPackageStartupMessages(library(InteractiveComplexHeatmap))
suppressPackageStartupMessages(library(ComplexHeatmap))
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(readxl)

#install.packages(c('shiny', 'shinydashboard', 'DT', 'readxl','tidyverse','ComplexHeatmap','InteractiveComplexHeatmap'))
#install.packages()
rm(list = ls())

#install.packages("BiocManager")
#BiocManager::install("InteractiveComplexHeatmap")

path<-paste('C:\\Users\\vince\\OneDrive - Universiteit Antwerpen\\VISL_HD\\Projects\\DataMan\\Labo_Stockage\\DiepVries_git\\Vriezers_80.xlsx')
#path<-paste('./Vriezers_80.xlsx')
frigo.2<-read_excel(path, sheet = "F2")
frigo.3<-read_excel(path, sheet = "F3")
frigo.4<-read_excel(path, sheet = "F4")
frigo.5<-read_excel(path, sheet = "F5")


x.out<-as.factor(c(rep(rep(letters[1:4], 4),5), rep(rep(letters[5:8], 4),5), 
                   rep(rep(letters[9:12], 4),5), rep(rep(letters[13:16], 4),5), 
                   rep(rep(letters[17:20], 4),5)))

y.out<-as.factor(rep(rep(letters[1:20],each=4),5))



frigo.2$x<-x.out
frigo.2$y<-y.out
frigo.2$z<-frigo.2$pos_id

frigo.3$x<-x.out
frigo.3$y<-y.out
frigo.3$z<-frigo.3$pos_id

frigo.4$x<-x.out
frigo.4$y<-y.out
frigo.4$z<-frigo.4$pos_id

frigo.5$x<-x.out
frigo.5$y<-y.out
frigo.5$z<-frigo.5$pos_id

frigo.2$pos_id2<-paste(frigo.2$loc_3, frigo.2$position, sep="\n")
frigo.3$pos_id2<-paste(frigo.3$loc_3, frigo.3$position, sep="\n")
frigo.4$pos_id2<-paste(frigo.4$loc_3, frigo.4$position, sep="\n")
frigo.5$pos_id2<-paste(frigo.5$loc_3, frigo.4$position, sep="\n")


frigo.2$z<-frigo.2$pos_id2
frigo.3$z<-frigo.3$pos_id2
frigo.4$z<-frigo.4$pos_id2
frigo.5$z<-frigo.5$pos_id2

colors = structure(c("#ea0c1a", "#f1bf24", "#0e9c3b", "#FFFFFF","#24ebf1","#aae5de","#FFDEAD", "#964B00","#D3D3D3", "#C89D7C", "#4b68f0"), 
                   names = c("filled", "partially filled","no rack","no_space","empty", "loose items", "plastic bags", "cardboard box","plastic box", "brown enveloppe", "reserved"))


with(frigo.2, {
  mat2 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat2[cbind(x, y)] <- z
  mat2
})->mat2

mat2.a<-as.matrix(mat2)

with(frigo.3, {
  mat2 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat2[cbind(x, y)] <- z
  mat2
})->mat2

mat2.b<-as.matrix(mat2)

with(frigo.4, {
  mat2 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat2[cbind(x, y)] <- z
  mat2
})->mat2

mat2.c<-as.matrix(mat2)

with(frigo.5, {
  mat2 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat2[cbind(x, y)] <- z
  mat2
})->mat2

mat2.d<-as.matrix(mat2)


with(frigo.2, {
  mat1 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat1[cbind(x, y)] <- empty_spots2
  mat1
})->mat1


mat1.a<-as.matrix(mat1)

with(frigo.3, {
  mat1 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat1[cbind(x, y)] <- empty_spots2
  mat1
})->mat1


mat1.b<-as.matrix(mat1)

with(frigo.4, {
  mat1 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat1[cbind(x, y)] <- empty_spots2
  mat1
})->mat1

mat1.c<-as.matrix(mat1)

with(frigo.5, {
  mat1 <- matrix(nrow=nlevels(x), ncol=nlevels(y),
                 dimnames=list(levels(x), levels(y)))
  mat1[cbind(x, y)] <- empty_spots2
  mat1
})->mat1

mat1.d<-as.matrix(mat1)

ht.f2 =   Heatmap(mat1.a, name = "DIEPVRIES F2",col=colors, rect_gp = gpar(col = "white", lwd = 2),
                  row_split = sort(rep(c("S1", "S2", "S3", "S4", "S5"), 4)), column_split= rep(1:5, each=4),column_title = "-80 DIEPVRIES",
                  cluster_row_slices = FALSE,cluster_column_slices = FALSE, show_column_dend = FALSE, show_row_dend = FALSE, 
                  cell_fun = function(j, i, x, y, width, height, fill){grid.text(mat2.a[i, j], x, y, gp = gpar(fontsize = 9))},
                  width = unit(12, "cm"), show_column_names = F, show_row_names = F,row_order = order(rownames(mat1.a)), column_order=sort(colnames(mat1.a)))


ht1 = draw(ht.f2)

ht.f3 =   Heatmap(mat1.b, name = "DIEPVRIES F3",col=colors, rect_gp = gpar(col = "white", lwd = 2),
                  row_split = sort(rep(c("S1", "S2", "S3", "S4", "S5"), 4)), column_split= rep(1:5, each=4),column_title = "-80 DIEPVRIES",
                  cluster_row_slices = FALSE,cluster_column_slices = FALSE, show_column_dend = FALSE, show_row_dend = FALSE, 
                  cell_fun = function(j, i, x, y, width, height, fill){grid.text(mat2.b[i, j], x, y, gp = gpar(fontsize = 9))},
                  width = unit(12, "cm"), show_column_names = F, show_row_names = F,row_order = order(rownames(mat1.b)), column_order=sort(colnames(mat1.b)))
ht2 = draw(ht.f3)

ht.f4 =   Heatmap(mat1.c, name = "DIEPVRIES F4",col=colors, rect_gp = gpar(col = "white", lwd = 2),
                  row_split = sort(rep(c("S1", "S2", "S3", "S4", "S5"), 4)), column_split= rep(1:5, each=4),column_title = "-80 DIEPVRIES",
                  cluster_row_slices = FALSE,cluster_column_slices = FALSE, show_column_dend = FALSE, show_row_dend = FALSE, 
                  cell_fun = function(j, i, x, y, width, height, fill){grid.text(mat2.c[i, j], x, y, gp = gpar(fontsize = 9))},
                  width = unit(12, "cm"), show_column_names = F, show_row_names = F,row_order = order(rownames(mat1.c)), column_order=sort(colnames(mat1.c)))

ht3 = draw(ht.f4)

ht.f5 =   Heatmap(mat1.d, name = "DIEPVRIES F4",col=colors, rect_gp = gpar(col = "white", lwd = 2),
                  row_split = sort(rep(c("S1", "S2", "S3", "S4", "S5"), 4)), column_split= rep(1:5, each=4),column_title = "-80 DIEPVRIES",
                  cluster_row_slices = FALSE,cluster_column_slices = FALSE, show_column_dend = FALSE, show_row_dend = FALSE, 
                  cell_fun = function(j, i, x, y, width, height, fill){grid.text(mat2.d[i, j], x, y, gp = gpar(fontsize = 9))},
                  width = unit(12, "cm"), show_column_names = F, show_row_names = F,row_order = order(rownames(mat1.c)), column_order=sort(colnames(mat1.d)))

ht4 = draw(ht.f5)


count<-nrow(frigo.2[frigo.2$empty_spots2=="empty",])+nrow(frigo.3[frigo.3$empty_spots2=="empty",])+nrow(frigo.4[frigo.4$empty_spots2=="empty",])+nrow(frigo.5[frigo.5$empty_spots2=="empty",])

namen<-c("loc_ID" , "pos_id", "position", "cont_ID", "cont_ty", "samp_date"   , "samp_expire"  ,"samp_resp","samp_info", "comment", "remarks" )



# The three components are separately specified and put with different box().

body = dashboardBody(
  
  tabItems(
    #first tn item
    tabItem(tabName = "Freezer_Overview",
            fluidRow(
              infoBoxOutput("progressBox", width=4),
              infoBoxOutput("progressBox2", width=4)),
            fluidRow(
              imageOutput("my_image1"),
              imageOutput("my_image2"))
            
    ),
    
    # second tab content
    tabItem(tabName = "Freezer_F2", 
            
            fluidRow(
              
              InteractiveComplexHeatmapOutput("heatmap_2", layout = "1-(2|3)", width1 = 600, height1=700, width2=400)
              
              
            ),
            
            fluidRow(
              box(title="tabel_F2", width=10, solidHeader=TRUE, background = "green",
                  DT::dataTableOutput("table_F2"))
            )),
    
    
    
    # third tab content
    tabItem(tabName = "Freezer_F3", 
            
            
            fluidRow(
              
              InteractiveComplexHeatmapOutput("heatmap_3", layout = "1-(2|3)", width1 = 600, height1=700, width2=400)
              
              
            ),
            
            
            fluidRow(
              box(title="tabel_F3", width=10, solidHeader=TRUE, background = "green",
                  DT::dataTableOutput("table_F3"))
            )),
    
    # fourth tab content
    tabItem(tabName = "Freezer_F4", 
            
            
            fluidRow(
              
              InteractiveComplexHeatmapOutput("heatmap_4", layout = "1-(2|3)", width1 = 600, height1=700, width2=400)
              
              
            ),
            
            fluidRow(
              box(title="tabel_F4", width=10, solidHeader=TRUE, background = "green",
                  DT::dataTableOutput("table_F4"))
            )),
    
    # fifth tab content
    tabItem(tabName = "Freezer_F5", 
            
            
            fluidRow(
              
              InteractiveComplexHeatmapOutput("heatmap_5", layout = "1-(2|3)", width1 = 600, height1=700, width2=400)
              
              
            ),
            
            fluidRow(
              box(title="tabel_F5", width=10, solidHeader=TRUE, background = "green",
                  DT::dataTableOutput("table_F5"))
            ))
    
  ))




# UI of the interactive heatmap widget is generated by single_heatmap_ui().
ui = dashboardPage(
  dashboardHeader(title = "LABO"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Freezer Overview", tabName = "Freezer_Overview"),
      menuItem("Freezer F2", tabName = "Freezer_F2"),
      menuItem("Freezer F3", tabName = "Freezer_F3"),
      menuItem("Freezer F4", tabName = "Freezer_F4"),
      menuItem("Freezer F5", tabName = "Freezer_F5")
    )
  ),
  body)





server = function(input, output, session) {
  
  
  
  makeInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_2")
  makeInteractiveComplexHeatmap(input, output, session, ht2, "heatmap_3")
  makeInteractiveComplexHeatmap(input, output, session, ht3, "heatmap_4")
  makeInteractiveComplexHeatmap(input, output, session, ht4, "heatmap_5")
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Number of CRYOBOX spaces left", paste0(count), icon = icon("exclamation-triangle"),
      color = "green", fill=T, width=8
    )
  })
  
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Percent of all space filled", paste0(round(((1-count/1200)*100),2), "%"), icon = icon("exclamation-triangle"),
      color = "red", fill=T
    )
  })
  
  
  output$my_image1<-renderImage({list(src='C:\\Users\\vince\\OneDrive - Universiteit Antwerpen\\VISL_HD\\Projects\\DataMan\\Labo_Stockage\\DiepVries_git\\Diepvriezers_F2F3.png', height='300px', width='400px', alt="something went wrong")}, deleteFile = FALSE)
  output$my_image2<-renderImage({list(src='C:\\Users\\vince\\OneDrive - Universiteit Antwerpen\\VISL_HD\\Projects\\DataMan\\Labo_Stockage\\DiepVries_git\\Diepvriezers_F4F5.png', height='300px', width='400px', alt="something went wrong")}, deleteFile = FALSE)
  output$table_F2<-renderDataTable(frigo.2[,namen], extensions = list(Scroller = NULL), options = list(scrollX = TRUE))
  output$table_F3<-renderDataTable(frigo.3[,namen], extensions = list(Scroller = NULL), options = list(scrollX = TRUE))
  output$table_F4<-renderDataTable(frigo.4[,namen], extensions = list(Scroller = NULL), options = list(scrollX = TRUE))
  output$table_F5<-renderDataTable(frigo.5[,namen], extensions = list(Scroller = NULL), options = list(scrollX = TRUE))
}

shinyApp(ui, server)


#testje<- paste('./Diepvriezers_F2F3.png')

