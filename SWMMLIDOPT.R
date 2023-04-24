# list of Packages used in SWMMLIDOPT
listOfPackages <- c("remotes","shiny","shinydashboard","mco","nsga2R","swmmr",
                    "lubridate","zoo","factorial2x2","ggplot2",
                    "shinybusy","ggpubr","rlang","DT","plotly",
                    "viridis","rio","dplyr","ggnewscale",
                    "shinyFiles","rlist","sf")

for (i in listOfPackages){
 
  if(! i %in% installed.packages()){
    if(i == "swmmr"){
      remotes::install_github("dleutnant/swmmr")
    }else{
      install.packages(i, dependencies = TRUE)
    }
    
  }
  require(i, character.only = TRUE)
}

##########Functions: #####################
# Search for SWMM exc file
swmm_exc = function(){
  pre <- list.files("C:/Program Files (x86)", "swmm5.exe|runswmm.exe", 
                    recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
  if(length(pre)==0){
  pre <- list.files("C:/Program Files", "swmm5.exe|runswmm.exe", 
                      recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
  }
  exc <- pre[ !grepl("epa", pre) ]
  return(exc)
}
exc = swmm_exc()

# plot the catchment
plot_catch = function(inp){
  sff <- inp_to_sf(inp)
  p = ggplot(sff[[1]]) +
    geom_sf(aes(fill = Name),show.legend = FALSE)+
    #geom_sf_label(aes(label = Name),show.legend = FALSE)+
    scale_fill_viridis_d(alpha = .4) +
    geom_sf(data= sff[[4]],color="blue", size=0.5)+
    geom_sf(data= sff[[2]],shape=21, fill="green", color="black", size=2)+
    geom_sf(data= sff[[3]],shape=21, fill="orange", color="black", size=2)+
    geom_sf(data= sff[[5]], shape = 10,size=5) + 
    theme_minimal()+
    theme(legend.position="none")
  p1 = ggplotly(p)
  return(p1)
}

# plot rainfall_runoff_flood figure
plot_rain_runoff_flood = function(inp){
  tmp_inp <- tempfile()
  write_inp(inp, tmp_inp)
  swmm_files <- run_swmm(tmp_inp,exec = exc, stdout = NULL)
  on.exit(file.remove(unlist(swmm_files)), add = TRUE)
  out1 <- read_out(
    file = swmm_files$out,
    iType =3,
    vIndex = 11
  )$system_variable$total_outflow_from_outfalls 
  out1 <- as.array(coredata(out1))
  
  p <- read_out(
    file = swmm_files$out,
    iType =3,
    vIndex = 1
  )$system_variable$total_rainfall
  p <- as.array(coredata(p))
  print(p)
  flood1 <- read_out(
    file = swmm_files$out,
    iType =3,
    vIndex = 10
  )$system_variable$total_external_flooding 
  flood1 <- as.array(coredata(flood1))
  plot(p, type="h", ylim=c(max(p)*2,0),
       axes=FALSE, xlab=NA, ylab= NA, col="lightblue",
       lwd=5, lend="square")
  #axis(side=4)
  mtext("", side = 4, line = 3)
  par(new=TRUE)
  plot(out1, type="l", lwd=2, ylim=c(0, max(out1)*2),col="blue",
       xlab = "", ylab = "")
  lines(flood1, col="red",lty=1,lwd=2)
  legend("topleft",legend = c("Precipitation","Outflow","Flooding"),lwd=c(7,3,3),
         col=c("lightblue","blue","red"),lty=1)
}

# Calculate the area of LID solution 
cal_area = function(x,inp,lid1){
  total_area = 0
  shape1 = inp$lid_usage[which(inp$lid_usage$`LID Process`==lid1),]
  x_lid = x[which(inp$lid_usage$`LID Process`==lid1)]
  for(i in 1:nrow(shape1)){
    sub1 = shape1$Subcatchment[i]
    area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
    total_area = total_area + (x_lid[i]*area1*shape1$Area[i])
  }
  
  return(total_area)
  
}


# Determine the peak runoff based on LID scenario x
peak_fun <- function(x,inp){
  n = nrow(inp$subcatchments)
  data2 = cbind.data.frame(inp$lid_usage,x)
  data_agg = aggregate(data2$x,by=list(data2$Subcatchment),FUN = "sum")
  g_array = data_agg$x - input$num4
  if(max(g_array)<=0){
    max1 = tryCatch({
      n = length(area1)
      area_array = array()
      k = 1
      kj = 1
      
      
      for(i in 1:nrow(inp$lid_usage)){
        
        sub1 = inp$lid_usage$Subcatchment[i]
        area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
        
        area_array[i] = x[i] * area1
        
      }
      
      num1 = area_array/inp$lid_usage$Area
      inp$lid_usage <- transform(
        inp$lid_usage,
        Number = num1
      )
      tmp_inp <- tempfile()
      write_inp(inp, tmp_inp)
      swmm_files <- run_swmm(tmp_inp,exec = exc, stdout = NULL)
      on.exit(file.remove(unlist(swmm_files)), add = TRUE)
      sim <- read_out(
        file = swmm_files$out,
        iType =3,
        vIndex = 11
      )$system_variable$total_outflow_from_outfalls
      sim <- as.array(coredata(sim))
      #sim <- (sim*1000*60)
      Urban = as.matrix(sim)
      max(Urban)}
      ,
      error = function(e){
        return(999)
      }
    )
  } else{
    return(999)
  }
  
}

# Determine the total_cost of LID scenario x
eval_f0 <- function(x,dat){
  total_cost = 0
  k =1
  u1 = 0
  n = nrow(inp$subcatchments)
  for(i in 1:nrow(inp$lid_usage)){
    j = which(dat[,1] == inp$lid_usage$`LID Process`[i])
    u1 = as.numeric(dat[j,3])
    sub1 = inp$lid_usage$Subcatchment[i]
    area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
    total_cost = total_cost + (u1*x[i]*area1)
    
  }
  return(total_cost)
}

# Plot overall_density of LID scenario x
plot_overall_density = function(inp,x,title1){
  sff <- inp_to_sf(inp)
  
  shape1 = sff[[1]]
  k = 1
  for(i in 1:length(shape1$Name)){
    if(is.na(shape1$Area.lid_usage[i])==FALSE){
      shape1$Number[i] = x[k] 
      k = k+1
    }
  }
  
  ar1 = aggregate(shape1$Number,by=list(shape1$Name),FUN = "sum")
  Shape2 = shape1[!duplicated(shape1[ , "Name"]),]
  
  colnames(ar1) = c("Name","x")
  
  
  Shape3 = full_join(Shape2, ar1, by = "Name")
  
  
  p1 = ggplot(Shape3, aes(fill = x)) +
    geom_sf(lwd = 0.2)+ 
    #scale_fill_gradient(low = "white",high = "darkgreen", na.value = "white") +
    scale_fill_viridis(alpha = 1,option= "D",direction=-1, na.value = "white") + 
    ggtitle(title1)+
    theme_void()+labs(fill = "LID density")
  
  return(p1)
}

# Plot the density of a specific LID measure (lid1) of a scenario x
plot_lid_density = function(inp,x,lid1){

  sff <- inp_to_sf(inp)
  
  shape1 = sff[[1]]
  k = 1
  for(i in 1:length(shape1$Name)){
    if(is.na(shape1$Area.lid_usage[i])==FALSE){
      shape1$Number[i] = x[k] 
      k = k+1
    }
  }
  
  shape2 = shape1[which(shape1$`LID Process`==lid1),]
  N0 = ggplot(shape1) +
    geom_sf(show.legend = NA, fill = "white", lwd = 0.2) +
    theme_void()
  
  
  N0 = N0+ 
    geom_sf(data = shape2, aes(fill = Number), lwd = 0.2)+
    scale_fill_viridis(alpha = 1,option= "D",direction=-1, na.value = "white") + 
    #scale_fill_gradient(low = "white",high = "darkgreen", na.value = "white") +
    theme_void()+labs(fill = paste0(lid1," density (Ratio)"))

  
  return(N0)
}

# Plot the difference of the LID density measure (lid1) between two scenarios x1 and x2
plot_lid_density_diff = function(inp,x1,x2,lid1){
  
  sff <- inp_to_sf(inp)
  
  shape1 = sff[[1]]
  k = 1
  for(i in 1:length(shape1$Name)){
    if(is.na(shape1$Area.lid_usage[i])==FALSE){
      shape1$Number[i] = x1[k]-x2[k]
      k = k+1
    }
  }
  
  shape2 = shape1[which(shape1$`LID Process`==lid1),]
  N0 = ggplot(shape1) +
    geom_sf(show.legend = NA, fill = "white", lwd = 0.2) +
    theme_void()
  
  
  N0 = N0+ 
    geom_sf(data = shape2, aes(fill = Number), lwd = 0.2)+
    scale_fill_viridis(alpha = 1,option= "D",direction=-1, na.value = "white") + 
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      midpoint = 0,
      na.value = "white"
    ) +
    theme_void()+labs(fill = paste0(" Difference (Ratio)"))
  
  
  return(N0)
}


##########UI: The user interface of the SWMMLIDopt ############################
ui <- dashboardPage(
  dashboardHeader(title = "SWMMLIDopt"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Current model", tabName = "Current", icon = icon("dashboard")),
      menuItem("LID creation tool", tabName = "lid", icon = icon("dashboard")),
      menuItem("Optimization",  tabName = "Optimization", icon = icon("dashboard")),
      menuItem("Optimization results",  tabName = "optim_results", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "cube-grid", margins = c(0, 10), color = "#FFF"),
    #add_busy_bar(color = "#112446"),
    tabItems(
      # First tab content
      tabItem(tabName = "Current",
              fluidPage(
                
                h1("Current catchment model"),
                fileInput("file1", "Choose inp File", accept = ".inp"),
                box(plotlyOutput("Current_catchment"),title = "Study catchment"),
                box(plotOutput("Current_hydro"),title = "Outflow and flooding"),
                box(dataTableOutput("catchment_stat"),title = "Catchment characteristics"),
                box(dataTableOutput("tags"),title = "Subcatchment tags")
              )
      ), 
      tabItem(tabName = "lid",
              fluidPage(
                
                h1("LID creation tool"),
                box(dataTableOutput("lid_create"),title = "LID creation"),
                downloadButton("lid_action", "Create LIDs and Export results")
              )
      ),
      tabItem(tabName = "Optimization",
              fluidPage(
                h1("Optimization of LIDs"),
                fileInput("file2", "Choose inp File (with LID objects)", accept = ".inp"),
                tabsetPanel(type = "tabs",
                            tabPanel("Optimization constrains",
                                  
                                     box(
                                       title = "Optimization constrains",
                                     numericInput("num4",h3("Maximum LID desnity (ratio)"),value = 0.8),
                                     box(dataTableOutput("Uplim"),title = "LID upper limits")
                                     )
          
                            ),
                            #tabPanel("Catchment", tableOutput("Cathment")),
                            tabPanel("Run optimization", 
                                   box(
                                       numericInput("num5",h3("Number of iterations"),value = 200),
                                       actionButton("act2","Run multi objectives optimization"),
                                                    textOutput("MultiOpt"),
                                                    downloadButton("downloadData2", "Export results"),
                                                    title = "Multi objectives optimization")
                                     
                          
              )
      ))),
      tabItem(tabName = "optim_results",
              fluidPage(
                h1("Optimization results"),
                
                tabsetPanel(type = "tabs",
                            tabPanel("Load optimization results (Multi objectives)", 
                                     fileInput("file3", "Choose .json result File", accept = ".json"),
                                     fileInput("file4", "Choose .inp File", accept = ".inp"),
                                     box(plotlyOutput("pareto_front"),title = "Optimization results"),
                                     numericInput("num6",h3("Pareto solution"),value = 1),
                                     box(plotOutput("Rainfall_Runoff_pareto"),title = "Optimization results"),
                                     box(dataTableOutput("optimal_pareto"),title = "Optimal scenario (table)"),
                                     box(plotlyOutput("optimal_catch_densty"),title = "Optimal scenario (map)"),
                                     box(downloadButton("downloadinp1", "Export inp file"),
                                         downloadButton("downloadtable1", "Export table results"),title = "Export LID scenario")),
                            tabPanel("Compare pareto solutions", 
                                     box(dataTableOutput("compare_pareto_input"),title = "Compare pareto solutions (input)"),
                                     box(plotlyOutput("pareto_plot1"),title = "Solution 1"),
                                     box(plotlyOutput("pareto_plot2"),title = "Solution 2"),
                                     box(plotlyOutput("pareto_plot3"),title = "Solution 3"),
                                     box(plotlyOutput("pareto_plot4"),title = "Solution 4"),
                                     box(plotlyOutput("pareto_plot5"),title = "Solution 5")),
                            tabPanel("Detailed pareto soultion comparsion", 
                                     box(dataTableOutput("compare_pareto_input1"),title = "Compare pareto solutions (input)"),
                                     box(dataTableOutput("compare_pareto_input2"),title = "Compare pareto solutions (input)"),
                                     box(plotlyOutput("pareto_SOL1"),title = "Solution 1"),
                                     box(plotlyOutput("pareto_SOL2"),title = "Solution 2"),
                                     box(plotlyOutput("pareto_com"),title = "Solution 1 - Solution 2"),
                                     box(plotlyOutput("barplot"),title = "Bar plot"))
                            
                )
 
              )
      )
    )
  )
)

##########server: ############################
server <- function(input, output){
  
  # 1. Swmm current state ###########################
  
  # 1.1. plot the subcatchment
  output$Current_catchment <- renderPlotly({
    inFile <- input$file1
    if(length(inFile)>0){
    inp <- read_inp(inFile$datapath)
      p1 = plot_catch(inp)
      p1
    }
    
  })   
  
  # 1.2. plot the current hydrograph 
  output$Current_hydro <- renderPlot({
    inFile <- input$file1
    if(length(inFile)>0){
    inp <- read_inp(inFile$datapath)
      p = plot_rain_runoff_flood(inp)
      p
    }
  }) 
  
  # 1.3. plot the catchment info.  
  output$catchment_stat <- renderDataTable({
    inFile <- input$file1
    if(length(inFile)>0){
    inp <- read_inp(inFile$datapath)
    
      data1 = as.data.frame(inp$subcatchments)
      newdata <- data1[c("Name", "Area", "Width")]
      newdata$Area = newdata$Area * 10000
      newdata2 = cbind.data.frame(newdata,inp$tags$text)
      colnames(newdata2) = c("Subcatchment name", "Area (m2)", "Width (m)","Land type")
      newdata2
    }
  }) 
  
  # 1.4. the current tags 
  choices1 = c('Not selected','Parking', 'Roof','Vegetation','Road','Walkway' )
  # 1.4.1 create a character vector of shiny inputs
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- numeric(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }
  
  # 1.4.2 obtain the values of inputs
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value <- input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }
  
  # 1.4.3 interactive table 
  it_df <- reactive({
    
    inFile <- input$file1
    if(length(inFile)>0){
    inp <- read_inp(inFile$datapath)
    
      data.frame(
        Tags =unique(inp$tags$text),
        class = shinyInput(selectInput, length(unique(inp$tags$text)),
                           'select_type', choices = choices1, width = "100px"),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # 1.4.4 final output tags table 
  output$tags <- DT::renderDataTable(
    it_df(), rownames = FALSE, escape = FALSE, options = list(
      autoWidth = TRUE, scrollX = TRUE, #scrollY = '400px',
      dom = 't', ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
  )
   
  # 2. LID creation tools ###########################
  # 2.1.1 interactive table 
  choices2 = c('Not selected','Parking', 'Roof','Vegetation','Road','Walkway' )
  lidType_df <- reactive({
    
    inFile <- input$file1
    if(length(inFile)>0){
    inp <- read_inp(inFile$datapath)
    
      data.frame(
        LID = c("BR","RG","GR","IT","PP","RB","VS"),
        LandUse1 = shinyInput(selectInput, 7,
                           'select_land1', choices = choices2, width = "100px"),
        LandUse2 = shinyInput(selectInput, 7,
                              'select_land2', choices = choices2, width = "100px"),
        LandUse3 = shinyInput(selectInput, 7,
                              'select_land3', choices = choices2, width = "100px"),
        LandUse4 = shinyInput(selectInput, 7,
                              'select_land4', choices = choices2, width = "100px"),
        LandUse5 = shinyInput(selectInput, 7,
                              'select_land5', choices = choices2, width = "100px"),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # 2.1.2 final output tags table 
  output$lid_create <- DT::renderDataTable(
    lidType_df(), rownames = FALSE, escape = FALSE, options = list(
      autoWidth = TRUE, scrollX = TRUE, #scrollY = '400px',
      dom = 't', ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
  )
  
  
  output$lid_action <- downloadHandler(
    
    filename = function() {
      paste("lidFile.inp", sep = "")
    },
    content = function(file) {
 
      
      inFile <- input$file1
      inp <- read_inp(inFile$datapath)
      if(length(inp)>0){
      
        
       
        #################################
 
        dat23 <- data.frame(
          tag1 = unique(inp$tags$text),
          tagS =  shinyValue('select_type', length(unique(inp$tags$text))))
        
        for (i in 1:length(dat23$tag1)) {
          inp$tags <- transform(
            inp$tags,
            text=ifelse(text == dat23$tag1[i] , dat23$tagS[i], text)
          )
        }
        

      ###################
      lid_tibble = tibble(
        Name = c(rep("BR",5),rep("RG",4),rep("GR",4),rep("IT",4),rep("PP",6),rep("RB",3),rep("VS",2)), 
        `Type/Layer` = c("BC","SURFACE","SOIL","STORAGE","DRAIN","RG",
                         "SURFACE","SOIL","STORAGE","GR","SURFACE","SOIL",
                         "DRAINMAT","IT","SURFACE","STORAGE","DRAIN","PP",
                         "SURFACE","PAVEMENT","SOIL","STORAGE","DRAIN","RB",      
                         "STORAGE","DRAIN","VS","SURFACE"), 
        Par1 = c(NA,10,30,400,0,NA,10,300,0,NA,10,150,3,NA,10,300,1,NA,10,100,30,300,0,NA,1000,0,NA,200),
        Par2 = c(NA,0.00,0.50,0.75,0.50,NA,0.00,0.50,0.75,NA,0.50,0.50,0.50,NA,0.00,0.75,0.50,NA,0.00,0.15,0.50,0.75,0.50,NA,0.75,0.50,NA,0.50),
        Par3 = c(NA,0.1,0.2,0.5,6.0,NA,0.1,0.2,0.5,NA,0.1,0.2,0.1,NA,0.1,0.5,6.0,NA,0.1,0.0,0.2,0.5,6.0,NA,0.5,6.0,NA,0.1),
        Par4 = c(NA,1.0,0.1,0.0,6.0,NA,1.0,0.1,0.0,NA,1.0,0.1,NA,NA,1.0,0.0,6.0,NA,1.0,100.0,0.1,0.0,6.0,NA,0.0,6.0,NA,1.0),
        Par5 = c(NA,5.0,0.5,NA,0.0,NA,5.0,0.5,NA,NA,5.0,0.5,NA,NA,5.0,NA,0.0,NA,5.0,0.0,0.5,NA,0.0,NA,NA,0.0,NA,5.0),
        Par6 = c(NA,NA,10,NA,0,NA,NA,10,NA,NA,NA,10,NA,NA,NA,NA,0,NA,NA,0,10,NA,0,NA,NA,0,NA,NA),
        Par7 = c(NA,NA,3.5,NA,NA,NA,NA,3.5,NA,NA,NA,3.5,NA,NA,NA,NA,NA,NA,NA,0.0,3.5,NA,NA,NA,NA,NA,NA,NA)
      )
      #################

      lid1 = c("BR","RG","GR","IT","PP","RB","VS")
      rpt1 = "*"
      drn1 = "*                0"
      
      lid_usage_tibble = tibble(
        Subcatchment = rep(inp$subcatchments$Name,each  = 7), 
        `LID Process` = rep(as.character(lid1),length(inp$subcatchments$Name)), 
        Number = rep(0,(length(inp$subcatchments$Name)*7)),
        Area = rep(1,(length(inp$subcatchments$Name)*7)),
        Width = rep(1,(length(inp$subcatchments$Name)*7)),
        InitSat = rep(0,(length(inp$subcatchments$Name)*7)),
        FromImp = rep(0,(length(inp$subcatchments$Name)*7)),
        ToPerv  = rep(0,(length(inp$subcatchments$Name)*7)),
        RptFile = rep("*",(length(inp$subcatchments$Name)*7)),
        DrainTo = rep("*                0",(length(inp$subcatchments$Name)*7))
      )
      #################
      inp$lid_controls = lid_tibble
      inp$lid_usage = lid_usage_tibble
      
      ################
      
      
      
      data1 = cbind.data.frame(inp$lid_usage,rep(inp$tags$text,each=7))
      colnames(data1)[11] = "LandUse"
      
   
      #################################
      dat33 <- data.frame(
        LID = c("BR","RG","GR","IT","PP","RB","VS"),
        LandUse1 = shinyValue('select_land1', 7),
        LandUse2 = shinyValue('select_land2', 7),
        LandUse3 = shinyValue('select_land3', 7),
        LandUse4 = shinyValue('select_land4', 7),
        LandUse5 = shinyValue('select_land5', 7))
  
      k = 1
      
      for(i in 1 : nrow(inp$lid_usage)){
        lid11 = data1$`LID Process`[i]
        j = which(dat33$LID==lid11)
        if((data1$`LID Process`[i] %in% dat33$LID[j]) && data1$LandUse[i] %in% dat33[j,(which(dat33[j,]!="Not selected"))][-1]){
          if(k ==1){
            data2 =  data1[i,]
            k = k+1
          }else{
            data2 = rbind.data.frame(data2, data1[i,])
            k = k+1
          }
        } 
      }
      
      
      
      inp$lid_usage = data2[,c(1:10)]
      write_inp(inp,file)
      
      }
    }
  )
  

  
  #3. Multi objective ####################### 
  
  res2 = eventReactive(input$act2, {
    
    inFile <- input$file2
    inp <- read_inp(inFile$datapath) 
    
    if(length(inp)>0){
      peak_fun <- function(x,inp){
        n = nrow(inp$subcatchments)
        
        data2 = cbind.data.frame(inp$lid_usage,x)
        data_agg = aggregate(data2$x,by=list(data2$Subcatchment),FUN = "sum")
        g_array = data_agg$x - input$num4
        
        if(max(g_array)<=0){
          
          max1 = tryCatch({
            #n = length(area1)
            area_array = array()
            k = 1
            kj = 1
            
            
            for(i in 1:nrow(inp$lid_usage)){
              
              sub1 = inp$lid_usage$Subcatchment[i]
              area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
              
              area_array[i] = x[i] * area1
              
            }
            print(max(area_array))
            num1 = area_array/inp$lid_usage$Area
            inp$lid_usage <- transform(
              inp$lid_usage,
              Number = num1
            )
            tmp_inp <- tempfile()
            write_inp(inp, tmp_inp)
            swmm_files <- run_swmm(tmp_inp,exec = exc, stdout = NULL)
            on.exit(file.remove(unlist(swmm_files)), add = TRUE)
            sim <- read_out(
              file = swmm_files$out,
              iType =3,
              vIndex = 11
            )$system_variable$total_outflow_from_outfalls
            sim <- as.array(coredata(sim))
            Urban = as.matrix(sim)
           
            max(Urban)}
            ,
            error = function(e){
              return(999)
            }
          )
        } else{
          return(999)
        }
        
      }
      dat <- data.frame(
        LID = unique(inp$lid_usage$`LID Process`),
        class = shinyValue('select_den', length(unique(inp$lid_usage$`LID Process`))),
        cost = shinyValue('select_cost', length(unique(inp$lid_usage$`LID Process`)))
      )
      
      
      
      inp$lid_usage$Area = rep(1,length(inp$lid_usage$Area))
      inp$lid_usage$Width = rep(1,length(inp$lid_usage$Area))
      
      functions <- function(x,inp){
        total_cost = 0
        k =1
        u1 = 0
        n = nrow(inp$subcatchments)
        for(i in 1:nrow(inp$lid_usage)){
          j = which(dat[,1] == inp$lid_usage$`LID Process`[i])
          u1 = as.numeric(dat[j,3])
          sub1 = inp$lid_usage$Subcatchment[i]
          area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
          total_cost = total_cost + (u1*x[i]*area1)
          
        }
        #print(total_cost)
        y = array()
        y[1] = round(total_cost/1000)
        
        #fun 2_peak
        
        y[2] = peak_fun(x,inp)
        
        #print(peak_fun(x,inp))
        ###########
        
        #y[3] = volume_fun(x1,inp)
        
        return(y)
      }
      constraint1 <- function(x1,inp) {
        n = nrow(inp$subcatchments)
        
        data2 = cbind.data.frame(inp$lid_usage,x1)
        
        data_agg = aggregate(data2$x1,by=list(data2$Subcatchment),FUN = "sum")
        g_array =  input$num4 - data_agg$x
        #print(g_array)
        return(as.array(g_array))
      }
      
      lb <- rep(0,nrow(inp$lid_usage))
      
      ub = array()
      

        for(i in 1:length(inp$lid_usage$Subcatchment)){
          
          j = which(dat$LID == inp$lid_usage$`LID Process`[i])
          ub[i] = as.numeric(dat[j,2])
  
        }
  
      
      x0 <- rep(0,nrow(inp$lid_usage))  
    
    
    
    n = nrow(inp$lid_usage)
    data2 = cbind.data.frame(inp$lid_usage,ub)
    data_agg = aggregate(data2$ub,by=list(data2$Subcatchment),FUN = "sum")
    #g_array =  input$num4 - data_agg$x
    print(dat)
    print(peak_fun(x= x0,inp=inp))
    x = ub
    total_cost = 0
    k =1
    u1 = 0

    n = nrow(inp$lid_usage)
    nsga2(functions, n, 2,
          popsize = 200,
          generations=input$num5,
          lower.bounds=lb, 
          upper.bounds=ub,
          constraints=constraint1,
          cdim=length(data_agg$x),
          inp = inp)
    }
  }) 
  
  output$MultiOpt <- renderText({ 
    
    
    inFile <- input$file2
    inp <- read_inp(inFile$datapath) 
    
    if(length(inp)>0){
      
      res = res2()
      if(length(res)>0){
        print("Optimization is finished. Please export the results")
      }
    }
  })
  
  output$downloadData2 <- downloadHandler(
    
    filename = function() {
      paste("MultiOpt.json", sep = "")
    },
    content = function(file) {
      
      res = res2()
      
      list1 = list(parameters = res[[1]],
                   objectives = res[[2]],
                   pareto = res[[3]])
      list.save(list1, file)
      
    }
  )
  
  #4. visualize the results #############################
  
 
  output$optimal_catch <- renderPlotly({
    
    
    
    inFile <- input$file5
    res5 <- list.load(inFile$datapath)
    if(length(res5)>0){
      
      inFile <- input$file6
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        
        x = res5$parameters
        sff <- inp_to_sf(inp)
        
        shape1 = sff[[1]]
        k = 1
        for(i in 1:length(shape1$Name)){
          if(is.na(shape1$Area.lid_usage[i])==FALSE){
            shape1$Number[i] = x[k] 
            k = k+1
          }
        }
        
        ar1 = aggregate(shape1$Number,by=list(shape1$Name),FUN = "sum")
        Shape2 = shape1[!duplicated(shape1[ , "Name"]),]
        
        colnames(ar1) = c("Name","x")
        
        
        Shape3 = full_join(Shape2, ar1, by = "Name")
        
        
        p1 = ggplot(Shape3, aes(fill = x)) +
          geom_sf()+ 
          scale_fill_gradient(
            low = "white",
            high = "darkgreen",
            na.value = "white"
          ) + ggtitle("Overall density")+
          theme_void()+labs(fill = "LID density (ratio)")
        
        ggplotly(p1)
        
        
      }
    }
   
  })

  output$pareto_front <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
        }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      

      x = c(1:ind1) 
      pareto1 = cbind.data.frame(x,res3$objectives[1:ind1,])
      colnames(pareto1) = c("Index","Cost (*1000)","Maximum outflow (l/s)")
    
      plot_ly(data = pareto1, x = ~`Cost (*1000)`, y = ~`Maximum outflow (l/s)`, text = ~Index,
                     marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',
                                   line = list(color = 'rgba(152, 0, 0, .8)',
                                               width = 2)))

    }
    
  })

  output$downloadinp1 <- downloadHandler(
    
    filename = function() {
      paste("optimal_LID.inp", sep = "")
    },
    content = function(file) {
      inFile <- input$file3
      res3 <- list.load(inFile$datapath)
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      if(length(inp)>0){
        
        
        y = as.numeric(input$num6)   
        
        params = as.data.frame(res3$parameters)
        
        x = t(params[y,])
        
        area1 = inp$subcatchments$Area * 10000
        n = length(area1)
        area_array = array()
        k = 1
        kj = 1
        
        
        for(i in 1:nrow(inp$lid_usage)){
          
          sub1 = inp$lid_usage$Subcatchment[i]
          area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
          
          area_array[i] = x[i] * area1
          
        }
        
        num1 = area_array/inp$lid_usage$Area
        inp$lid_usage <- transform(
          inp$lid_usage,
          Number = num1
        )

        write_inp(inp,file)
   
      }
    }
  )
  output$downloadtable1 <- downloadHandler(
    
    filename = function() {
      paste("table.csv", sep = "")
    },
    content = function(file) {
      inFile <- input$file3
      res3 <- list.load(inFile$datapath)
      if(length(res3)>0){
        if(length(which(res3$pareto=="FALSE")[1])>1){
          ind1 = which(res3$pareto=="FALSE")[1] -1 
        }else{
          ind1 = length(res3$pareto)
        }
        ind2 = order(res3$objectives[1:ind1,2])
        res3$objectives = res3$objectives[ind2,]
        res3$parameters =  res3$parameters[1:ind1,]
        res3$parameters = res3$parameters[ind2,]
        inFile <- input$file4
        inp <- read_inp(inFile$datapath) 
        
        if(length(inp)>0){
          
          
          y = as.numeric(input$num6)   
          
          params = as.data.frame(res3$parameters)
          
          x = t(params[y,])
          data1 = as.data.frame(inp$lid_usage)
          newdata <- data1[c("Subcatchment","LID Process", "Area", "Width")]
          
          n1 = nrow(inp$lid_usage)
          area_array = array()
          for(i in 1:n1){
            area_array[i] = (inp$subcatchments$Area[which(inp$lid_usage$Subcatchment[i]==inp$subcatchments$Name)])*10000
          }
          newdata2 <- cbind.data.frame(newdata,area_array,x)
          colnames(newdata2) = c("Subcatchment","LID type", "Unit area (m2)", "Unit width (m)","Subcatchment Area (m2)","LID density (ratio)")
          newdata2$`Total LID area (m2)` = newdata2$`LID density (ratio)`*newdata2$`Subcatchment Area (m2)`*newdata2$`Unit area (m2)`
          write.table(newdata2,file,sep = ",",col.names = T,row.names = F,quote = F)
        }
      }

    }
  )
  output$Rainfall_Runoff_pareto <- renderPlot({
    
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
     
        
        y = as.numeric(input$num6)   
        
        params = as.data.frame(res3$parameters)
        
        x = t(params[y,])
     
      #print(x)
       cost1 =  res3$objectives[y,1]
      rainfall_runoff_par <- function(x, inp,cost1){
        
        tmp_inp <- tempfile()
        write_inp(inp, tmp_inp)
        swmm_files <- run_swmm(tmp_inp,exec = exc, stdout = NULL)
        out1 <- read_out(
          file = swmm_files$out,
          iType =3,
          vIndex = 11
        )$system_variable$total_outflow_from_outfalls 
        Urban_NoLID <- as.array(coredata(out1))
        
        p <- read_out(
          file = swmm_files$out,
          iType =3,
          vIndex = 1
        )$system_variable$total_rainfall
        p <- as.array(coredata(p))
  
        
        
        area1 = inp$subcatchments$Area * 10000
        n = length(area1)
        area_array = array()
        k = 1
        kj = 1
        
        
        for(i in 1:nrow(inp$lid_usage)){
          
          sub1 = inp$lid_usage$Subcatchment[i]
          area1 = inp$subcatchments$Area[which(inp$subcatchments$Name ==sub1)] * 10000
          
          area_array[i] = x[i] * area1
          
        }
        
        num1 = area_array/inp$lid_usage$Area
        inp$lid_usage <- transform(
          inp$lid_usage,
          Number = num1
        )
        tmp_inp <- tempfile()
        write_inp(inp, tmp_inp)
        swmm_files <- run_swmm(tmp_inp,exec = exc, stdout = NULL)
        #on.exit(file.remove(unlist(swmm_files)), add = TRUE)
        sim <- read_out(
          file = swmm_files$out,
          iType =3,
          vIndex = 11
        )$system_variable$total_outflow_from_outfalls
        

        
        sim <- as.array(coredata(sim))
    
        Urban_LID = as.matrix(sim)
        main1 = paste0("Total cost = ", cost1,"k USD")
    
        plot(p, type="h", ylim=c(max(p)*2,0),
             axes=FALSE, xlab=NA, ylab= NA, col="lightblue",
             lwd=5, lend="square")
        #axis(side=4)
        mtext("", side = 4, line = 3)
        par(new=TRUE)
        plot(Urban_NoLID, type="l", lwd=2, ylim=c(0, max(out1)*2),col="red",
             xlab = "", ylab = "")
        lines(Urban_LID, col="blue",lty=1,lwd=2)
        legend("topleft",legend = c("Precipitation","With LID","No LID"),lwd=c(7,3,3),
               col=c("lightblue","blue","red"),lty=1)
        
        
        #on.exit(file.remove(unlist(swmm_files)), add = TRUE)
      }
      
      rainfall_runoff_par(x,inp,cost1)
      }
    }
  })
  
  output$optimal_pareto <- renderDataTable({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        
        y = as.numeric(input$num6)   
        
        params = as.data.frame(res3$parameters)
        
        x = t(params[y,])
        data1 = as.data.frame(inp$lid_usage)
        newdata <- data1[c("Subcatchment","LID Process", "Area", "Width")]
        
        n1 = nrow(inp$lid_usage)
        area_array = array()
        for(i in 1:n1){
          area_array[i] = (inp$subcatchments$Area[which(inp$lid_usage$Subcatchment[i]==inp$subcatchments$Name)])*10000
        }
        newdata2 <- cbind.data.frame(newdata,area_array,x)
        colnames(newdata2) = c("Subcatchment","LID type", "Unit area (m2)", "Unit width (m)","Subcatchment Area (m2)","LID density (ratio)")
        newdata2$`Total LID area (m2)` = newdata2$`LID density (ratio)`*newdata2$`Subcatchment Area (m2)`*newdata2$`Unit area (m2)`
        newdata2
      }
    }
  })
  
  output$optimal_catch_densty <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
      
      y = as.numeric(input$num6)  
     
      params = as.data.frame(res3$parameters)
      
      x = t(params[y,])
      sff <- inp_to_sf(inp)
      
      shape1 = sff[[1]]
      k = 1
      for(i in 1:length(shape1$Name)){
        if(is.na(shape1$Area.lid_usage[i])==FALSE){
          shape1$Number[i] = x[k] 
          k = k+1
        }
      }
      
      ar1 = aggregate(shape1$Number,by=list(shape1$Name),FUN = "sum")
      Shape2 = shape1[!duplicated(shape1[ , "Name"]),]
      
      colnames(ar1) = c("Name","x")
      
      
      Shape3 = full_join(Shape2, ar1, by = "Name")
      
      
      p1 = ggplot(Shape3, aes(fill = x)) +
        geom_sf()+ 
        scale_fill_gradient(low = "white",high = "darkgreen", na.value = "white") +
        #scale_fill_viridis(alpha = 1,option="A",direction=-1, na.value = "white") + 
        theme_void()+labs(fill = "LID density")
      ggplotly(p1)
    }
    }
  })
  
  
  
  # 1.4.3 interactive table 
  compare_df <- reactive({
    
    inFile <- input$file4
    inp <- read_inp(inFile$datapath)
    if(length(inp)>0){
      inFile <- input$file3
      res3 <- list.load(inFile$datapath)
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      nn1 = nrow(res3$parameters)
      
      data.frame(
        Solutions =paste0("Solution ",c(1:5)),
        Number = shinyInput(selectInput,5,
                           'select_pareto', choices = c(1:nn1), width = "100px"),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # 1.4.4 final output tags table 
  
  output$compare_pareto_input <- DT::renderDataTable(
    compare_df(), rownames = FALSE, escape = FALSE, options = list(
      autoWidth = TRUE, scrollX = TRUE, #scrollY = '400px',
      dom = 't', ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
  )
  
  output$pareto_plot1 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution",c(1:5)),
          Number = shinyValue('select_pareto', 5)
        )
        
        j = as.numeric(dat2$Number[1])
        x = res3$parameters[j,]
        title1= paste0("Soultion No ", j,", Total cost = ",
                         res3$objectives[j,1],"k, Max peak = ",
                         round(res3$objectives[j,2],3), " l/s")
       p = plot_overall_density(inp,x,title1)
       ggplotly(p)
        
       
      }
    }
  }) 
  
  
  output$pareto_plot2 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution",c(1:5)),
          Number = shinyValue('select_pareto', 5)
        )
        
        j = as.numeric(dat2$Number[2])
        x = res3$parameters[j,]
        title1= paste0("Soultion No ", j,", Total cost = ",
                       res3$objectives[j,1],"k, Max peak = ",
                       round(res3$objectives[j,2],3), " l/s")
        p = plot_overall_density(inp,x,title1)
        ggplotly(p)
        
        
      }
    }
  }) 
  
  output$pareto_plot3 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution",c(1:5)),
          Number = shinyValue('select_pareto', 5)
        )
        
        j = as.numeric(dat2$Number[3])
        x = res3$parameters[j,]
        title1= paste0("Soultion No ", j,", Total cost = ",
                       res3$objectives[j,1],"k, Max peak = ",
                       round(res3$objectives[j,2],3), " l/s")
        p = plot_overall_density(inp,x,title1)
        ggplotly(p)
        
        
      }
    }
  }) 
  
  
  output$pareto_plot4 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution",c(1:5)),
          Number = shinyValue('select_pareto', 5)
        )
        
        j = as.numeric(dat2$Number[4])
        x = res3$parameters[j,]
        title1= paste0("Soultion No ", j,", Total cost = ",
                       res3$objectives[j,1],"k, Max peak = ",
                       round(res3$objectives[j,2],3), " l/s")
        p = plot_overall_density(inp,x,title1)
        ggplotly(p)
        
        
      }
    }
  }) 
  
  output$pareto_plot5 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution",c(1:5)),
          Number = shinyValue('select_pareto', 5)
        )
        
        j = as.numeric(dat2$Number[5])
        x = res3$parameters[j,]
        title1= paste0("Soultion No ", j,", Total cost = ",
                       res3$objectives[j,1],"k, Max peak = ",
                       round(res3$objectives[j,2],3), " l/s")
        p = plot_overall_density(inp,x,title1)
        ggplotly(p)
        
        
      }
    }
  }) 
 
  
  ###### detailed
  # 1.4.3 interactive table 
  detailed1_df <- reactive({
    
    inFile <- input$file4
    inp <- read_inp(inFile$datapath)
    if(length(inp)>0){
      
      inFile <- input$file3
      res3 <- list.load(inFile$datapath)
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      nn1 = nrow(res3$parameters)
      
      
      data.frame(
        Solutions =paste0("Solution 1"),
        Number = shinyInput(selectInput,1,
                            'select_detaled1_sol', choices = c(NA,c(1:nn1)), width = "100px"),
        LID = shinyInput(selectInput,1,
                            'select_detaled1_lid', choices = unique(inp$lid_usage$`LID Process`), width = "100px"),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # 1.4.4 final output tags table 
  
  output$compare_pareto_input1 <- DT::renderDataTable(
    detailed1_df(), rownames = FALSE, escape = FALSE, options = list(
      autoWidth = TRUE, scrollX = TRUE, #scrollY = '400px',
      dom = 't', ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
  )
  
  
  ###### detailed
  # 1.4.3 interactive table 
  detailed2_df <- reactive({
    
    inFile <- input$file4
    inp <- read_inp(inFile$datapath)
    if(length(inp)>0){
      data.frame(
        Solutions =paste0("Solution 2"),
        Number = shinyInput(selectInput,1,
                            'select_detaled2_sol', choices = c(NA,c(1:100)), width = "100px"),
        LID = shinyInput(selectInput,1,
                         'select_detaled2_lid', choices = unique(inp$lid_usage$`LID Process`), width = "100px"),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # 1.4.4 final output tags table 
  
  output$compare_pareto_input2 <- DT::renderDataTable(
    detailed2_df(), rownames = FALSE, escape = FALSE, options = list(
      autoWidth = TRUE, scrollX = TRUE, #scrollY = '400px',
      dom = 't', ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
  )
  
  
  
  output$pareto_SOL1 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution 1"),
          Number = shinyValue('select_detaled1_sol', 1),
          LID = shinyValue('select_detaled1_lid', 1)
        )
        
        j = as.numeric(dat2$Number)
        x = res3$parameters[j,]
        lid1 =  dat2$LID
        p = plot_lid_density(inp,x,lid1)
        ggplotly(p)
        
      }
    }
  }) 
  
  output$pareto_SOL2 <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution 2"),
          Number = shinyValue('select_detaled2_sol', 1),
          LID = shinyValue('select_detaled2_lid', 1)
        )
        
        j = as.numeric(dat2$Number)
        x = res3$parameters[j,]
        lid1 =  dat2$LID
        p = plot_lid_density(inp,x,lid1)
        ggplotly(p)
        
      }
    }
  }) 

  output$pareto_com <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution 2"),
          Number1 = shinyValue('select_detaled1_sol', 1),
          Number2 = shinyValue('select_detaled2_sol', 1),
          LID = shinyValue('select_detaled2_lid', 1)
        )
        
        j1 = as.numeric(dat2$Number1)
        x1 = res3$parameters[j1,]
        
        j2 = as.numeric(dat2$Number2)
        x2 = res3$parameters[j2,]
        
        lid1 =  dat2$LID
        p = plot_lid_density_diff(inp,x1,x2,lid1)
        ggplotly(p)
        
      }
    }
  }) 
  
  output$barplot <- renderPlotly({
    inFile <- input$file3
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      if(length(which(res3$pareto=="FALSE")[1])>1){
        ind1 = which(res3$pareto=="FALSE")[1] -1 
      }else{
        ind1 = length(res3$pareto)
      }
      ind2 = order(res3$objectives[1:ind1,2])
      res3$objectives = res3$objectives[ind2,]
      res3$parameters =  res3$parameters[1:ind1,]
      res3$parameters = res3$parameters[ind2,]
      inFile <- input$file4
      inp <- read_inp(inFile$datapath) 
      
      if(length(inp)>0){
        
        dat2 <- data.frame(
          Solutions = paste0("Solution 2"),
          Number1 = shinyValue('select_detaled1_sol', 1),
          Number2 = shinyValue('select_detaled2_sol', 1),
          LID = shinyValue('select_detaled2_lid', 1)
        )
        
        j1 = as.numeric(dat2$Number1)
        x1 = res3$parameters[j1,]
        
        j2 = as.numeric(dat2$Number2)
        x2 = res3$parameters[j2,]
        
        lid1 =  dat2$LID
        print(cal_area(x1,inp,lid1))
        x <- c('Soultion 1', 'Soultion 2')
        y <- c(round(cal_area(x1,inp,lid1),2), 
               round(cal_area(x2,inp,lid1),2))
        
        data <- data.frame(x, y)
        
        fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
                       text = y, textposition = 'auto',
                       marker = list(color = 'rgb(158,202,225)',
                                     line = list(color = 'rgb(8,48,107)', width = 1.5)))
        fig <- fig %>% layout(title = lid1,
                              xaxis = list(title = ""),
                              yaxis = list(title = paste0(lid1, " area (m2)")))
        
        fig
        
      }
    }
  }) 
  
}

shinyApp(ui, server)


