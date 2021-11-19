---
title: "data.missing_function"
author: "Kailande"
date: "11/19/2021"
output: html_document
---

Developing the function
```{r}
data.missing <- function(df, percent = 'FALSE'){
  
  missing_patterns <- data.frame(is.na(df)) %>%
  group_by_all() %>%
  count(name = "count", sort = TRUE) %>%
  ungroup()
  
  # Creating New missing patterns dataframe using the old one, this turns the TRUE/FALSE missing data into 1's and 0s. The adorn_totals("col") makes it so that a toal column for the sum of missing patterns in each row is appended to the entire dataframe. 

  missing_patterns_new<- missing_patterns %>% mutate_all((as.integer)) %>%
    adorn_totals("col")

# Creates the pattern identiication. Appends a column called pattern_id that assigns a 0, 0,5, or 1 given the total number missing patterns - the count of missing patterns for each row. This will help in the colors of the overall map
  missing_patterns_new <- missing_patterns_new %>%
    mutate(pattern_id=ifelse(missing_patterns_new$Total-missing_patterns_new$count==0,1,0.5))


#Creates a first identification column for each number of rows. there are 9 rows
# This is the final dataframe needed for the entire map
  missing_patterns_new <- missing_patterns_new %>% rownames_to_column("id")


  #p1 <- ggplot(data=missing_patterns_new,aes(x=reorder(id,desc(-count)),y=count)) +
   # geom_bar(aes(alpha=factor(missing_patterns_new$pattern_id)),stat = 'identity')+
    #scale_alpha_manual(values = c("0.5"=0.5,"1.0"=1), guide="none") +
    #theme(axis.title.y=element_blank()) + coord_flip() 
  
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
  
  miss_col_df <- miss_col %>%
    mutate(col = fct_reorder(col, desc(count)))
  
  #p2 <- miss_col %>% mutate(col = fct_reorder(col, desc(count))) %>%
    #ggplot(aes(x=col,y=count)) + 
    #geom_bar(stat = 'identity')
  
  missing_map <- data.frame(is.na(df)) %>% group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  missing_map <- subset(missing_map, select = -c(count))
  
  tidycars <- missing_map %>% 
    rownames_to_column("id") %>% 
    gather(key, value, -id) %>% #gather key and value except id
    mutate(missing = ifelse(value==1, "yes", "no"))

  tidycars <- tidycars %>% mutate(miss2=ifelse(missing=="yes",1,0))
  
  tidycars$miss3 <- as.factor(ifelse(tidycars$id ==
                                       complete_case_id,0.5,tidycars$miss2))
  
  main <- ggplot(tidycars, aes(x = fct_reorder(key, -miss2, sum), y = fct_rev(id),
                            fill = miss3)) +
    geom_tile(color = "white") +
    scale_fill_brewer(palette = "Blues") +
    xlab("variable name") +
    ylab("missing data pattern") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 10), axis.title.y
          =element_text(size = 10), axis.title.x =      element_text(size = 10))
    #{if(tidycars$miss3 == complete_case_id) geom_text(data = tidycars,
                                                      #aes(label = "complete cases"))}
  
  # plot for row count for patterns 
  right_frame <- missing_patterns_new
  if(percent) {
    right_frame$count = right_frame$count*100/nrow(df)
  }
  
  right_plot <- right_frame %>%
    ggplot(aes(x=reorder(id,desc(-count)),y=count)) +
    geom_bar(aes(alpha=factor(pattern_id)),stat = 'identity',
             fill = "#9ecae1")+
    scale_alpha_manual(values = c("0.5"=0.5,"1.0"=1), guide="none") +
    theme(axis.title.y=element_blank()) + coord_flip() +
    xlab("") +
    ylab(ifelse(percent, "% rows", "row count")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 10), axis.title.y
          =element_text(size = 10), axis.title.x =      element_text(size = 10))
    
  
  if(percent) {
    miss_col_df$count =  miss_col_df$count*100/nrow(df)
  }
  
  top_plot <- ggplot(miss_col_df, aes(x=col,y=count)) + 
    geom_bar(stat = 'identity', fill = "#9ecae1") +
    xlab("") +
    ylab(ifelse(percent, "% rows missing", "num rows missing")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), axis.text.y = element_text(vjust = 0.5, hjust=1, size = 10), axis.title.y
          =element_text(size = 10), axis.title.x =      element_text(size = 10))

  style <- "
    1#
    23
  "
  
  top_plot + main + right_plot + plot_layout(design = style, widths = c(3,1), heights = c(1,3))
}
```