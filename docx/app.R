library(jiebaR)
library(tm)
library(tmcn)
library(rJava)
library(arules)
library(arulesViz)
library(data.table)
library(qdapTools)
library(networkD3)
library(dplyr)
library(magrittr)
library(stringr)
library(shiny)
library(htmlwidgets)
library(ape)
library(stringdist)
library(shinydashboard)
library(igraph)
library(rstudioapi)


wd<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file.path(wd,"docxfilefolder"))

filelist <- list.files(path = ".", pattern="*.docx", full.names=TRUE)

tablelist <- sapply(filelist, read_docx)
tablelist<-lapply(tablelist,function(x) iconv(x, "UTF-8", "UTF-8"))
tablelist<-lapply(tablelist,function(x) toTrad(x))

setwd(wd)

stopt <- fread("stopword.txt", sep = "\t", dec = ",", colClasses = "character", 
               encoding = "UTF-8", header = FALSE)

seg = worker(stop_word = "stopword.txt",write="NOFILE")
stopt <- c(stopt$V1)
newword <- fread("newword.txt", sep = "\t", dec = ",", colClasses = "character", 
                 encoding = "UTF-8", header = FALSE)
n <- nrow(newword)
newword <- c(newword$V1)

for (i in 1:n) {
  new_user_word(seg, newword[i], "n")
}
politictext<-tablelist

politictext <- gsub("[0-9a-zA-Z]+?", "", politictext)
politictext <- gsub("\\.", "", politictext)
politictext <- gsub("\\s+", '', politictext)
politictext <- gsub(" ", "", politictext)
politictext <- as.list(politictext)
seg.1 <- apply_list(politictext, seg)

seg.1 <- filter_segment(seg.1, stopt)

trans <- as(seg.1, "transactions")
setwd(file.path(wd,"parameters"))
parameters<-fread("docxparameters.csv")
items <- apriori(trans, parameter = list(supp = parameters$supp, conf = parameters$conf, minlen = 2, 
                                         target = "frequent itemsets"), control = list(verbose = F))
setwd(wd)
png("networkgraph/Plot.png", width = 10, height = 8, units = "in", res = 300)
set.seed(333)
plot(items, method = "graph", control = list(type = "items", main = "", 
                                             cex = 2, nodeCol = c("purple")))
dev.off()

co.occurrence <- as(sort(items), "data.frame")
setwd(wd)
fwrite(co.occurrence, "co-occurrencetable/co-occurrencetable.csv")
seg.3 <- str_trim(seg.1)
seg.3 <- gsub("[0-9a-zA-Z]+?", "", seg.3)
seg.3 <- filter_segment(seg.3, stopt)
seg.3 <- seg[seg.3]
textunit <- data.table(table(seg.3))
gg <- subset(textunit, seg.3 != "")
freqparameter<-fread("freq.csv")


uniquemodels <- unique(as.character(gi$seg.3))
distancemodels <- stringdistmatrix(uniquemodels, uniquemodels, method = "osa")
rownames(distancemodels) <- uniquemodels
hc <- hclust(dist(distancemodels),"ave")
hc<-as.radialNetwork(hc)
radialNetwork(List = hc, fontSize = 10, opacity = 0.8)

ofreq<-fread("parameters/cooccurrence.csv")
itemv<-subset(co.occurrence,count>ofreq$var1)
itemv$support<-round(itemv$support,digit=5)

y<-strsplit(as.character(itemv$items),split=",",fixed=T)
y <- gsub("[^[:alnum:][:space:]']","", y)
y<-gsub("c","", y)
y<-data.table(str_split_fixed(y, " ", 2))

x<-data.frame(a=unlist(y[,1:2], use.names = FALSE))
z<-data.frame(a=unlist(y[,1:2], use.names = FALSE))
x<-setDT(x)
z<-setDT(z)
x$a<-as.numeric(as.factor(x$a))
x$a<-x$a-1

v<-summary(z$a)
v<-as.data.frame(v)
v$name<-rownames(v)
v$group<-rep(1, nrow(v))

n<-nrow(y)
N<-nrow(x)-nrow(y)
y$V3<-x[1:n,1]
y$V4<-x[n+1:N,1]
y$V5<-itemv$count
y$V6<-sqrt(itemv$count)
x$group<-rep(1, nrow(x))


edges<-as.data.frame(cbind(y$V1,y$V2,y$V6))

names(edges) <- c("source", "target","weight")
nodes<-as.data.frame(v$name)
names(nodes)<-c("name")
nodes$id<-(as.numeric(nodes$name)-1)

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
betweenness(g)
a<-eigen_centrality(g)
k<-as.data.frame(round(a[[1]], digit = 3))
names(k)[names(k) == "round(a[[1]], digit = 3)"] <- "eigen.centrality"
k$betweenness<-betweenness(g)
k$in.centrality<-round(closeness(g, mode="in"),digit=5)
k$out.centrality<-round(closeness(g, mode="out"),digit=5)

##group##
c<-quantile(v$v)
c<-as.data.frame(c)
c1<-c[5,1]
c2<-c[4,1]
c3<-c[3,1]
v$group<-v$v
v$group[v$group>=c1]<-12 
v$group[v$group>=c2 & v$group < c1]<-11 
v$group[v$group<c2]<-10




#### Server ####
server <- function(input, output) {
  
    output$force <- renderForceNetwork({
    forceNetwork(Links = y, Nodes = v,
                 Source = "V3", Target = "V4",
                 Value = "V6",Nodesize = "v",
                 NodeID="name",Group = "group",
                 fontSize = 20,
                 arrows=TRUE,bounded=TRUE,
                 opacity = input$opacity)
      
  })
    hcluster <- eventReactive(input$run, {
      
      gg <- gg[with(gg,order(-N)),]
      m<-input$termfreq
      gi <- gg[1:m,]
      uniquemodels <- unique(as.character(gi$seg.3))
      distancemodels <- stringdistmatrix(uniquemodels, uniquemodels, method = "osa")
      rownames(distancemodels) <- uniquemodels
      hc <- hclust(dist(distancemodels),"ave")
      hc<-as.radialNetwork(hc)
      radialNetwork(List = hc, fontSize = 10, opacity = input$opacity)
    })
    output$Hcluster = renderRadialNetwork({
      hcluster()
    })  
  output$contents = DT::renderDataTable({
    k
  })
  
  output$table1 = DT::renderDataTable({
    itemv
  })

}

#### UI ####

ui <-  dashboardPage(
  dashboardHeader(title = "共現詞組分析"),
  dashboardSidebar(sidebarMenu(
    menuItem("Network", tabName = "network", icon = icon("snowflake-o")),
    menuItem(sliderInput("opacity", "Opacity", 0.6, min = 0.1,
                         max = 1, step = .1)),
    menuItem( actionButton("run","reanalysis cluster")),
    menuItem( numericInput('termfreq','cluster-termfreq',10,min = 1,max = 999999))
  )),
  dashboardBody(
    
    tabItems(tabItem(tabName="network",(
      fluidRow(
        mainPanel(width = 12,
                  tabsetPanel(
                    tabPanel("co-occurrence network", forceNetworkOutput("force")),
                    tabPanel("centrality table", DT::dataTableOutput("contents")),
                    tabPanel("co-occurrence table", DT::dataTableOutput("table1")),
                    tabPanel("hcluster", radialNetworkOutput("Hcluster"))
                  ))
      )
    )
    )
    
    )))

#### Run ####
shinyApp(ui = ui, server = server)
