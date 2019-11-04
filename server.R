#filename: server.R

# server.R for shiny_test

function(input, output, session){

  apikey <- ""
  
  
  #TRIP MAP
  output$Tripmap <- renderGoogle_map({
    dc <- ctbk %>% 
      filter(bikeid == input$bikeid & startdate == input$date) 
    
    dt <- dc[order(dc$bikeid,dc$starttime),]
    #print(dim(dt[0,]))
    
    lng <- nrow(dt)
    
    if(lng<=1){

      n<-1
      #print(lng)
      mydf <- data.frame(region = 1:1,
                       from_lat = dt$startstationlat[1],
                       from_long = dt$startstationlong[1],
                       end_lat = dt$endstationlat[1],
                       end_long = dt$endstationlong[1])
      
      polylines <- lapply(1:nrow(mydf), function(x){
        
        foo <- google_directions(origin = unlist(mydf[x, 2:3]),
                                 destination = unlist(mydf[x, 4:5]),
                                 key = apikey,
                                 mode = "bicycling",
                                 simplify = TRUE)
        
        ## no need to decode the line, just return the string as-is
        foo$routes$overview_polyline$points
      }
      )
      
      dk <- data.frame(polylines = unlist(polylines), stringsAsFactors = F)
      
      ## add some colour values for the markers
      mydf$colour_from <- "red"
      mydf$colour_to <- "blue"
      google_map(key = apikey) %>%
        add_markers(data = mydf, lat = "from_lat", lon = "from_long", colour = "colour_from") %>%
        add_markers(data = mydf, lat = "end_lat", lon = "end_long", colour = "colour_to") %>%
        add_polylines(data = dk, polyline = "polylines")
    } else if(lng>25) {
      
      n<-25
      #print(n)
      mydf <- data.frame(region = 1:(n-1),
                         from_lat = dt$startstationlat[1],
                         from_long = dt$startstationlong[1],
                         to_lat = dt$startstationlat[2:n],
                         to_long = dt$startstationlong[2:n],
                         end_lat = dt$startstationlat[n],
                         end_long = dt$startstationlong[n])
      
      polylines <- lapply(1:nrow(mydf), function(x){
        
      foo <- google_directions(origin = unlist(mydf[x, 2:3]),
                                 destination = unlist(mydf[x, 4:5]),
                                 key = apikey,
                                 mode = "bicycling",
                                 simplify = TRUE)
        
        ## no need to decode the line, just return the string as-is
        foo$routes$overview_polyline$points
      }
      )
      
      dk <- data.frame(polylines = unlist(polylines), stringsAsFactors = F)
      
      ## add some colour values for the markers
      mydf$colour_from <- "red"
      mydf$colour_to <- "blue"
      google_map(key = apikey) %>%
      add_markers(data = mydf, lat = "from_lat", lon = "from_long", colour = "colour_from") %>%
      add_markers(data = mydf, lat = "end_lat", lon = "end_long", colour = "colour_to") %>%
      add_polylines(data = dk, polyline = "polylines")
      
      } else{
        n=nrow(dt)
        #print(n)
      
        mydf <- data.frame(region = 1:(n-1),
                         from_lat = dt$startstationlat[1],
                         from_long = dt$startstationlong[1],
                         to_lat = dt$startstationlat[2:n],
                         to_long = dt$startstationlong[2:n],
                         end_lat = dt$startstationlat[n],
                         end_long = dt$startstationlong[n])
        
        
        polylines <- lapply(1:nrow(mydf), function(x){
          
          foo <- google_directions(origin = unlist(mydf[x, 2:3]),
                                   destination = unlist(mydf[x, 4:5]),
                                   key = apikey,
                                   mode = "bicycling",
                                   simplify = TRUE)
          
          ## no need to decode the line, just return the string as-is
          foo$routes$overview_polyline$points
        }
        )
        
        dk <- data.frame(polylines = unlist(polylines), stringsAsFactors = F)
        
        ## add some colour values for the markers
        mydf$colour_from <- "red"
        mydf$colour_to <- "blue"
        google_map(key = apikey) %>%
          add_markers(data = mydf, lat = "from_lat", lon = "from_long", colour = "colour_from") %>%
          add_markers(data = mydf, lat = "end_lat", lon = "end_long", colour = "colour_to") %>%
          add_polylines(data = dk, polyline = "polylines")
    }


  })
  
### HISTOGRAM
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    xx    <- ctbk %>%  filter(as.character(startdate) == input$date)
    x <- xx$starttime
    tt <- strptime(paste("2001-01-01", x), format="%Y-%m-%d %H:%M")
    f <- format(round(tt, units="hours"), format="%H:%M")
    Users <- as.numeric(sapply(strsplit(f,":"), `[`, 1))
    #class(ff)

    #bins <- seq(min(ff), max(ff), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(Users, col = 'darkgray', border = 'white')
  })
  
  #DENSITY MAP
  output$Denmap <- renderGoogle_map({
    
    # add 1 hour to imput time
    ps <- c(":00:00")
    d <- sapply(strsplit(input$chooseTime,":"), `[`, 1)
    d <- as.character(as.numeric(d)+1)
    edTime <- paste(d,ps, sep = "", collapse = NULL)
    
    #print(input$chooseTime)
    #print(edTime)
    
    
    filTm <- ctbk %>%  filter(as.character(startdate) == input$date) %>%
      filter(as.character(starttime) > input$chooseTime & as.character(starttime) < (edTime)) %>%
      group_by(startstaionid)
    
    # filTm <- ctbk %>%  filter(as.character(startdate) == input$date) %>% 
    #   filter(as.character(starttime) < input$chooseTime) %>%
    #   group_by(startstaionid) 
    
    # filTm <- ctbk %>%  filter(as.character(startdate) == input$date) %>% 
    #   filter(as.character(starttime) < edTime) %>%
    #   group_by(startstaionid) 
    
    #print(dim(filTm))
    
    unqStId <- (unique(filTm$startstaionid))
    # unqStLat <- (unique(filTm$startstationlat))
    # unqStLong <- (unique(filTm$startstationlong))
    
    mt <- match(unqStId,ctbk$startstaionid)
    lat <- ctbk$startstationlat[mt[]]
    long <- ctbk$startstationlong[mt[]]
    
    
    # print(length(unqStId))
    # print(length(unqStLat))
    # print(length(unqStLong))
    
    #unq <- cbind(unqStId[1:length(unqStId)],unqStLat[1:length(unqStId)],unqStLong[1:length(unqStId)])
    #unq <- cbind(unqStId[1:(length(unqStId)-10)],unqStLat[1:(length(unqStId)-10)],unqStLong[1:(length(unqStId)-10)])
    unq <- cbind(unqStId,lat,long)
    
    unq <- data.frame(unq)
    names(unq) = c("startstaionid","unqStLat","unqStLong")
    
    ct <- count(filTm, vars = "startstaionid")
    #ct$n <- ct$n
    
    #match(c(4,8),x)
    #match(unqStId[100],ctbk$startstaionid)
    
    
    jn <- inner_join(unq,ct, by = "startstaionid")
    
    #print(dim(jn))

    google_map(data = jn, key = apikey) %>%
      add_heatmap(lat = "unqStLat", lon = "unqStLong", option_radius = 0.0025,
                  weight = 'n',
                  option_gradient = c("plum1", "purple1", "peachpuff"))
    # google_map(data = jn, key = apikey) %>%
    #   add_heatmap(lat = "unqStLat", lon = "unqStLong", option_radius = 0.0025, weight='n')
     
  })
  
  
}
