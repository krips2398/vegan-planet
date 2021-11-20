# Developing the function

data.missing <- function(df, percent = 'FALSE'){
  
  missing_patterns <- data.frame(is.na(df)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  if(percent){
    missing_patterns$count = missing_patterns$count*100/nrow(df)
  }
  # Creating New missing patterns dataframe using the old one, this turns the TRUE/FALSE missing data into 1's and 0s. The adorn_totals("col") makes it so that a toal column for the sum of missing patterns in each row is appended to the entire dataframe. 
  
  missing_patterns_new<- missing_patterns %>% mutate_all((as.integer)) %>% adorn_totals("col")
  
  # Creates the pattern identiication. Appends a column called pattern_id that assigns a 0, 0,5, or 1 given the total number missing patterns - the count of missing patterns for each row. This will help in the colors of the overall map
  missing_patterns_new <- missing_patterns_new %>%
    mutate(pattern_id=ifelse(missing_patterns_new$Total-missing_patterns_new$count==0,1,0.5))
  
  #Creates a first identification column for each number of rows. there are 9 rows
  # This is the final dataframe needed for the entire map
  missing_patterns_new <- missing_patterns_new %>% rownames_to_column("id")
  missing_patterns_new$id <- as.factor(as.integer(missing_patterns_new$id))
  
  if(percent){
    missing_patterns$count = missing_patterns$count*100/nrow(df)
    p1 <- ggplot(data=missing_patterns_new,aes(x=fct_rev(reorder(id,-count)),y=count)) + 
      geom_bar(aes(alpha=factor(missing_patterns_new$pattern_id)),stat = 'identity',fill = "#03b1fc") + 
      
      xlab("") + ylab("% rows") +
      ylim(0,100) + 
      scale_alpha_manual(values = c("0.5"=0.5,"1.0"=1), guide="none") +
      theme(axis.title.y=element_blank(), legend.position = "none") + 
      coord_flip() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 20), axis.title.y
            =element_text(size = 20), axis.title.x =      element_text(size = 20))
  }
  else
  {
    p1 <- ggplot(data=missing_patterns_new,aes(x=fct_rev(reorder(id,-count)),y=count)) + 
      geom_bar(aes(alpha=factor(missing_patterns_new$pattern_id)),stat = 'identity',fill = "#03b1fc") + 
      xlab("") + ylab("row count") +
      scale_alpha_manual(values = c("0.5"=0.5,"1.0"=1), guide="none") +
      theme(axis.title.y=element_blank(), legend.position = "none") + 
      coord_flip() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 20), axis.title.y
            =element_text(size = 20), axis.title.x =      element_text(size = 20))
  }
  
  
  complete_case_id <- missing_patterns_new %>% 
    filter(pattern_id == 1) %>% 
    select(id) %>% 
    as.character()
  
  missing_col <- df %>% select(everything()) %>%
    summarise_all(funs(sum(is.na(.))))
  
  
  miss_col <- data.frame(col=names(missing_col))
  
  f <- c(missing_col[1,])
  
  miss_col$count <- as.integer(unlist(f))
  
  miss_col <- miss_col[order(-miss_col$count),]
  
  rownames(miss_col) <- 1:nrow(miss_col)
  
  miss_col <- miss_col %>% mutate(col = fct_reorder(col, desc(count)))
  
  if(percent){
    miss_col$count = miss_col$count*100/nrow(df)
    p2 <- ggplot(data= miss_col, aes(x=col,y=count)) + 
      geom_bar(stat = 'identity', fill = "#03b1fc", alpha=0.5) +
      scale_x_discrete(label=function(x) abbreviate(x, minlength = 3)) +
      xlab("") +
      ylim(0,100) + 
      ylab("% rows missing") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 20), axis.title.y
            =element_text(size = 20), axis.title.x =      element_text(size = 20))
  }
  else{
    p2 <- ggplot(data= miss_col, aes(x=col,y=count)) + 
      geom_bar(stat = 'identity', fill = "#03b1fc", alpha=0.5) +
      scale_x_discrete(label=function(x) abbreviate(x, minlength = 3)) +
      xlab("") +
      ylab("num rows missing") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 20), axis.title.y
            =element_text(size = 20), axis.title.x =      element_text(size = 20))
  }
  
  missing_map <- data.frame(is.na(df)) %>% 
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  missing_map <- subset(missing_map, select = -c(count))
  
  tidycars <- missing_map %>% 
    rownames_to_column("id") %>% 
    gather(key, value, -id) %>% #gather key and value except id
    mutate(missing = ifelse(value==1, "yes", "no"))
  
  tidycars <- tidycars %>% mutate(miss2=ifelse(missing=="yes",1,0))
  
  tidycars$miss3 <- as.factor(ifelse(tidycars$id == complete_case_id,0.5,tidycars$miss2))
  
  tidycars$key <- factor(tidycars$key, levels = levels(miss_col$col))
  tidycars$id <- factor(tidycars$id, levels = levels(missing_patterns_new$id))
  
  if(complete_case_id == 0) {
    g <- ggplot(tidycars, aes(x = key, y = fct_rev(id), fill = miss3)) +
      geom_tile(color = "white") +
      #scale_fill_brewer(palette = "Blues") +
      scale_fill_manual(values=c("#cacaca","#b2a0e1","darkgray")) +
      scale_x_discrete(label=function(x) abbreviate(x, minlength = 3)) +
      xlab("variable") +
      ylab("missing data pattern") +
      #theme(legend.position = "none") + 
      theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 20), axis.title.y=element_text(size = 20), axis.title.x =      element_text(size = 20)) +
      annotate("text", x=length(unique(tidycars$key))/2, y=complete_case_id, label="complete cases")
  }
  
  else{
    g <- ggplot(tidycars, aes(x = key, y = fct_rev(id), fill = miss3)) +
      geom_tile(color = "white") +
      #scale_fill_brewer(palette = "Blues") +
      scale_fill_manual(values=c("#cacaca","#b2a0e1","darkgray")) +
      scale_x_discrete(label=function(x) abbreviate(x, minlength = 3)) +
      xlab("variable") +
      ylab("missing data pattern") +
      #theme(legend.position = "none") + 
      theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 20), axis.title.y=element_text(size = 20), axis.title.x =      element_text(size = 20))
    
  }
  
  p2 + plot_spacer() + g + p1 + plot_layout(widths = c(4,1), heights = c(1,3))
  
}