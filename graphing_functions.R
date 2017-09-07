
#RANKING GRAPHS

#plots ranked bar chart of cities
rank_and_nb_group<-function(df, var, order="Descending", peers="Current",
                            plot_title="", y_title = "Percent", caption_text = "",
                            sigfig = 3, num_dec = 1, text = TRUE, h_line = FALSE,
                            thousands_comma = T){
  #SET GRAPH PARAMETERS
  
  #copy variable var to a new column for use with the '$' operator
  df$var <- df[[var]]
  
  #subset df to peer parameter
  if(peers=="Current"){
    df<-subset(df,current ==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,baseline ==1)
  }
  
  #sort df according to order parameter
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  
  #create numbered city labels for left side of graph
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$city)
  d.graph<-cbind(d.rank,names)
  
  #set bar color
  breaks <- classIntervals(d.graph$var,3,style="jenks")
  d.graph$color <- NA
  d.graph$color[d.graph$var <= breaks$brks[2]] <- "green"
  d.graph$color[d.graph$var > breaks$brks[2] & d.graph$var <= breaks$brks[3]] <- "yellow"
  d.graph$color[d.graph$var > breaks$brks[3]] <- "red"
  
  #round numbers in graph according to sigfif, num_dec, and thousands_comma parameters
  thousands_char <- if_else(thousands_comma == TRUE, ",", "")
  d.graph$round <- d.graph$var %>%
                    signif(digits = sigfig) %>%
                    round(digits = num_dec) %>%
                    format(big.mark = thousands_char)
  
  #Set text format, highlight and italicise Louisville text, highlight Louisville bar
  d.graph$textfont <- "Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"] <- "Museo Sans 300 Italic"
  d.graph$textcolor <- "black"
  d.graph$textcolor[d.graph$city == "Louisville"] <- "#00a9b7"
  d.graph$linecolor <- "white"
  d.graph$linecolor[d.graph$city == "Louisville"] <- "#00a9b7"

  #PLOT GRAPH
  
  #initial plot
  p <- ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                               y=var,fill=factor(color)))+guides(fill=FALSE)
  
  #add  bars
  p <- p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p <- p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p <- p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
  }
  
  #add features
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 18, hjust = 0.5),
                 axis.text.y=element_text(hjust=0, family = rev(d.graph$textfont),
                                          size=12, color = rev(d.graph$textcolor)),
                 axis.ticks=element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(),
                 plot.subtitle = element_text(hjust = 0.5))
  
  #add numeric labels to bars based on text parameter
  if (text == TRUE) {
    p <-
      p + geom_text(
        aes(label = round),
        hjust = 1.1,
        size = 5,
        family = "Museo Sans 300"
      )
  }
  
  #add vertical line to the left side of the bars based on the h_line parameter
  if (h_line == TRUE){
    p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 1)
  }
  p <- p+labs(title = plot_title, y= y_title,
              x = "", caption = caption_text)
  p
}

#The rank_and_nb_group function with a few adjustments for the net migration variable.
rank_and_nb_group_mig<-function(df, var, order="Descending", peers="Current",
                                plot_title="", y_title = "Percent", caption_text = "",
                                sigfig = 3, num_dec = 1, thousands_comma = T,
                                h_line = F){
  df$var <- df[[var]]
  if(peers=="Current"){
    df<-subset(df,current ==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,baseline ==1)
  }
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$city)
  d.graph<-cbind(d.rank,names)
  
  breaks <- classIntervals(d.graph$var,3,style="jenks")
  d.graph$color <- NA
  d.graph$color[d.graph$var<=breaks$brks[2]] <- "green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]] <- "yellow"
  d.graph$color[d.graph$var>breaks$brks[3]] <- "red"
  d.graph$round <- format(round(signif(d.graph$var, digits = sigfig), digits = num_dec), big.mark = if_else(thousands_comma == TRUE, ",", ""))
  d.graph$textfont <- "Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"] <- "Museo Sans 300 Italic"
  d.graph$linecolor <- "white"
  d.graph$linecolor[d.graph$city == "Louisville"] <- "#00a9b7"
  d.graph$textcolor <- "black"
  d.graph$textcolor[d.graph$city == "Louisville"] <- "#00a9b7"
  
  
  p <- ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                               y=var,fill=factor(color)))+guides(fill=FALSE)
  p <- p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p <- p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p <- p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
  }
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 18, hjust = 0.5),
                 axis.text.y=element_text(hjust=0, family = rev(d.graph$textfont),
                                          size=12, color = rev(d.graph$textcolor)),
                 axis.ticks=element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(),
                 plot.subtitle = element_text(hjust = 0.5))
  p <-
    p + geom_text(
      aes(label = round),
      hjust = ifelse(d.graph$var > 1000, 1.1, ifelse(
        d.graph$var > 0, .2, ifelse(d.graph$var > -1000, 1.0, -.1))),
      size = 5,
      family = "Museo Sans 300")
  if (h_line == TRUE){
    p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 1)
  }
  p <- p+labs(title = plot_title, y= y_title,
              x = "", caption = caption_text)
  p
}

#TRENDLINE GRAPHS

##rolling mean functions for 3-year and 5-year trendlines
rollmean3 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-1],x[i],x[i+1]))
    y[1] <- NA
  }
  y
}

rollmean5 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-2],x[i-1],x[i],x[i+1],x[i+2]))
    y[1] <- NA
    y[2] <- NA
  }
  y
}

#Basic trendline graph
graph_trendline<-function(df,var, plot_title="",y_title="Percent", peers = "Current", 
                          caption_text = "", subtitle_text = "",
                          rollmean = 1, xmin = 2005, xmax = 2015,
                          break_settings = ""){
  
  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]
  
  df = df %>% filter(year != 2016)
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  #calculate 25th and 75th percentiles
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var, year)
  
  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")
  
  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    break_settings = seq(xmin, xmax, 2)
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  
  #initial line plot
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  
  #add color and line types
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville",
      "75th Percentile",
      "Peer City Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    )
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Trendline graph adapted for MSAs
graph_trendline_msa<-function(df,var, plot_title="",y_title="Percent", 
                              peers = "Current", caption_text = "", 
                              subtitle_text = "", rollmean = 1, break_settings = "", 
                              xmin = 2005, xmax = 2015){
  df$var <- df[[var]]
  df = df %>% filter(year != 2016)
  if(peers=="Current"){
    df.wol <- filter(df,current == 1 & MSA!=31140)
  }
  if(peers=="Baseline"){
    df.wol <- filter(df,baseline == 1 & MSA!=31140)
  }
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>% 
    filter(MSA == 31140) %>% 
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  if(break_settings == ""){
    break_settings = seq(xmin, xmax, 2)
  }
  dat
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value)+min(data_long$value))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value) - border_space, max(data_long$value + border_space)))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville",
      "75th Percentile",
      "Peer City Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    )
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Trendline graph adapted for ky_ed data
graph_trendline_ky_ed<-function(df,var, plot_title="",y_title="Percent", 
                                caption_text = "", subtitle_text = "", rollmean = 1,
                                break_settings = "", xmin = 2005, xmax = 2015){
  df$var <- df[[var]]
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>% 
    filter(dist_name == "Jefferson County") %>% 
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }
  if(break_settings == ""){
    break_settings = seq(xmin, xmax, 2)
  }
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "JCPS",
      "75th Percentile",
      "KY School District Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "JCPS",
        "75th Percentile",
        "KY School District Mean",
        "25th Percentile"
      )
    )
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#Trendline graph adapted for more years of ky_ed data
ky_ed_data_long_trendline <- function(data_long, var = "var", value = "value", plot_title="",y_title="Percent", 
                                      caption_text = "", subtitle_text = "", rollmean = 1,
                                      break_settings = seq(2005, 2015, 2), xmin = 1996, xmax = 2016,
                                      labels, color_pal){
  data_long$var <- data_long[[var]]
  data_long$value<-data_long[[value]]
  data_long %<>% select(year, var, value)
  data_long <- arrange(data_long, as.character(var))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=var))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- color_pal
  p <- p + scale_colour_manual(values = cPalette, labels = labels)
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}

#MAPS

make_map <- function(var, name, units = "Percent",
                     map_style = "sequential", legend_title = ""){
  
  #renames var for use with the '$' operator
  map_jc@data$var <- map_jc@data[[var]]
  
  #concatenate third line of text for tract labels using units parameter
  if(units == "Percent"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2),"%", sep = "")
  }
  if(units == "Dollars"){
    map_jc@data$l_line3 <- paste(name, ": $",
                                 prettyNum(
                                   signif(map_jc@data$var, 3),
                                   big.mark = ",",
                                   preserve.width = "none"
                                 ),
                                 sep = "")
  }
  if(units == "none"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2), sep = "")
  }
  
  #combine lines of text into full formatted label
  labels <- sprintf("%s<br/>%s<br/>%s",
                    map_jc@data$l_line1, map_jc@data$l_line2, map_jc@data$l_line3
                   ) %>% 
            lapply(htmltools::HTML)
  
  labels[[190]] <- htmltools::HTML(sprintf("%s<br/>%s<br/>%s",
                                           "Tract #: 980000",
                                           "Louisville International Airport",
                                           "No residents"
                                          )
                                  )
                 
  #Define palette using map_style parameter
  if(map_style == "sequential" | map_style == "Sequential"){
    col_palette = "BuPu"
  }
  if(map_style == "divergent" | map_style == "Divergent"){
    col_palette = "RdYlGn"
  }
  pal <- brewer.pal(11, col_palette)
  pal <- colorNumeric(
    palette = pal,
    domain = map_jc@data$var
  )
  
  #Create map title using legend_title parameter
  if(units == "Percent") {
    title_text <- paste(legend_title, "(%)", sep = ' ')
  }
  if(units == "Dollars") {
    title_text <- paste(legend_title, "($)", sep = ' ')
  }
  if(units == "none"){
    title_text <- legend_title
  }
  
  #create map
  m <- leaflet(map_jc) %>%
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~pal(var),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = title_text,
              position = "bottomright")
  
  m
}