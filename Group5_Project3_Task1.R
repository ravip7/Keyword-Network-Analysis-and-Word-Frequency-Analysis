# PROJECT 2
# Group 5
# Group Members:
# Anjali Dayaram Kshirsagar (NUID : 002743547)
# Aryan Deore (NUID : 002724785)
# Ravi Patel (NUID : 002655300)

# sources: https://ggraph.data-imaginist.com/
# sources: https://igraph.org/r/html/latest/aaa-igraph-package.html
# sources: https://eehh-stanford.github.io/SNA-workshop/intro-igraph.html
# sources: https://rpubs.com/yanalytics/network-analysis-directed1

library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(ggraph)
library(igraph)
library(RColorBrewer)

task1_data= read.csv("task1_data.csv")
terms = subset(task1_data, select = -c(Title))
terms = as.data.frame(map(terms, str_to_title))

# convert multiple column to 1 column
stack_terms = stack(terms)
keywords = subset(task1_data, select = -c(Title))

# Eliminate duplicate words
unique_terms = unique(stack_terms$values)

# create empty matrix
zeromatrix = matrix(0, 
                    nrow=length(unique_terms), 
                    ncol=length(unique_terms))

s=lapply(keywords, str_to_title)
keywords = as.data.frame(s)

colnames(zeromatrix) = unique_terms
rownames(zeromatrix) = unique_terms

rownumber = dim(terms)[1]
columnnumber = dim(terms)[2]

terms1 = task1_data[!is.na(task1_data$`Keyword.1`),]

number = length(terms1)

lu=stri_remove_empty(unique_terms, na_empty = FALSE)
lowerunique = tolower(lu)

l_lu = length(lowerunique)
l_pu = length(lowerunique)
zeromatrix = matrix(0,nrow = l_lu ,ncol=l_pu)
rownames(zeromatrix) = c(lowerunique)
colnames(zeromatrix) = c(lowerunique)

rownumber = dim(keywords)[1]
columnnumber = dim(keywords)[2]

termes1 = task1_data[!is.na(task1_data$`Keyword.1`),]
number = length(termes1)

#populate the matrix
for(ind_row in 1:rownumber){
  for(ind_col in 1:columnnumber) {
    for(ind_col_pair in 1:columnnumber) {
      firstpair = tolower(terms[[ind_row, ind_col]])
      second_pair = tolower(terms[[ind_row, ind_col_pair]])
      if(firstpair !=""){
        if(second_pair != ""){
          if(firstpair != second_pair){
            zeromatrix[firstpair,second_pair] = zeromatrix[firstpair,second_pair] + 1
          }
        }
      }
    }
  }
}

# 3 Find NGraph
ngplot =
  graph_from_adjacency_matrix(
    weighted = TRUE, 
    zeromatrix,
    mode="undirected")

ngplot

# 4a Find Degree
dg_n_plt = degree(mode="all",
                  ngplot)

dg_n_plt = as.data.frame(dg_n_plt)
#colnames(dg_n_plt) = c(words, dg_n_plt)
dg_n_plt 

#4b Find Strength
str_n_plt = strength(ngplot, mode="all")
str_n_plt = data.frame(str_n_plt)
str_n_plt

#5 Topdegree of NGraph
tp_deg = dg_n_plt %>% arrange(desc(dg_n_plt)) 
tp_deg = tp_deg %>% head(10)
tp_deg

# Top Strength
tp_str = str_n_plt  %>%
  arrange(desc(str_n_plt)) %>% 
  head(10)
tp_str

# 6 Top 10 node pairs by weight
data.frame(to_tn_node_by_str_n_plt = tp_str, to_tn_node_by_dg_n_plt = tp_deg)

limit = 248

to_tn_pairs_weight = data.frame()
for (first_counter in 1:limit){
  for (second_counter in 1:limit){
    if (first_counter != second_counter){ 
      if(first_counter > second_counter) {
        to_tn_pairs_weight = rbind(to_tn_pairs_weight, data.frame(N1 = row.names(zeromatrix)[first_counter], N2 = row.names(zeromatrix)[second_counter], Count = zeromatrix[first_counter,second_counter]))
      }
    }
  }
}

to_tn_pairs_weight %>% 
  arrange(desc(Count)) %>% 
  head(10)

#7 Strength vs Degree

temptable= merge(dg_n_plt,
                 str_n_plt, 
                 by=0, 
                 all=TRUE)

Avg_str_n_plt = temptable %>% 
  group_by(dg_n_plt) %>% 
  summarise(
    Avg_str_n_plt = mean(str_n_plt)
  )

plot = ggplot(Avg_str_n_plt, 
              aes(x=(dg_n_plt), 
                  y=(Avg_str_n_plt))) + 
  geom_point()

plot
