library(dagitty)
library(ggplot2)


## data ####

data_dag <- coords_matched_46 %>% 
  left_join(pred_resp_phylo %>% select(soc_id, complexity, agriculture_cereals))

## Functions ####

overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p %>% round(digits = 3))
}


dag_to_character_chain <- function(fdag){
  e <- edges(fdag)
  model <- paste(c(e$v[1], e$w), collapse = " \u2192 ")
} 

dag_to_character_edges <- function(fdag) {
  e <- edges(fdag)
  paste(sprintf("%s → %s", e$v, e$w), collapse = "; ")
}

dag_to_character <- function(fdag) {
  e <- edges(fdag)
  
  if (nrow(e) == 0) return("")
  
  # A simple chain has one start, one end, and no branching or merging.
  nodes <- unique(c(e$v, e$w))
  starts <- setdiff(e$v, e$w)
  ends   <- setdiff(e$w, e$v)
  
  is_chain <- length(starts) == 1 &&
    length(ends) == 1 &&
    all(tabulate(match(e$v, nodes), nbins = length(nodes)) <= 1) &&
    all(tabulate(match(e$w, nodes), nbins = length(nodes)) <= 1)
  
  if (is_chain) {
    # Follow the edges from the start, rather than relying on edge order.
    current <- starts
    chain <- current
    
    while (current != ends) {
      next_node <- e$w[e$v == current]
      if (length(next_node) != 1) break
      current <- next_node
      chain <- c(chain, current)
    }
    
    if (current == ends && length(chain) == nrow(e) + 1) {
      return(paste(chain, collapse = " \u2192 "))
    }
  }
  
  paste(sprintf("%s → %s", e$v, e$w), collapse = "; ")
}

ci_to_character <- function(fdag) { # conditional independency
  ci <- impliedConditionalIndependencies(fdag)
  
  vapply(ci, function(x) {
    if (length(x$Z) > 0) {
      paste(x$X, "_||_", x$Y, "|", paste(x$Z, collapse = ", "))
    } else {
      paste(x$X, "_||_", x$Y)
    }
  }, character(1))
}

ci_to_formula <- function(fdag, i = 1, response) {
  ci <- impliedConditionalIndependencies(fdag)
  x <- ci[[i]]
  
  response <- unname(as.character(response)[1])
  variables <- c(x$X, x$Y)
  
  if (!response %in% variables) {
    stop("response must be either ", x$X, " or ", x$Y)
  }
  
  predictors <- c(setdiff(variables, response), x$Z)
  predictors <- unname(as.character(predictors))
  
  reformulate(predictors, response = response)
}


rename_variables <- function(character){
  character %>% 
    # str_replace_all("complexity", "SC") %>% 
    # str_replace_all("height_ratio", "HR") %>% 
    str_replace_all(pattern = "Ax1", "women's_status")
}

dag_to_table_row <- function(fdag){
  graph <- dag_to_character(fdag) %>% 
    rename_variables
  
  ci <- ci_to_character(fdag)
  ci_renamed <- ci %>% rename_variables()
  
  resp_tested <- sub(" _\\|\\|_.*$", "", ci)
  pred_tested <- trimws(sub(".*_\\|\\|_\\s*([^|]+).*", "\\1", ci))
  
  formula <- ci_to_formula(fdag, response = resp_tested)
  
  CHAR <- as.character(formula)
  formula_character <- as.character(paste(CHAR[2], CHAR[1], CHAR[3])) %>% 
    rename_variables
  
  
  mod <- lm(formula, data = data_dag)
  sum <- summary(mod)
  
  pval_mod <- overall_p(mod)
  
  preds <- CHAR[3] %>% 
    stringr::str_split(pattern = "\\s*\\+\\s*")
  pred1 <- preds[[1]][1]

  est1 <- sum$coefficients[pred_tested, 1] %>%  # estimate
    round(digits = 1)
  pval1 <- sum$coefficients[pred_tested, 4] %>%  # p-value of the coefficient
    round(digits = 3)
  info <- paste0(pred_tested, ": coefficient = ", est1, ", p = ", pval1)
  
  tibble(graph, 
         ci_renamed,
         formula_character ,
         pval_mod,
         info,
         conclusion = ""
         )
}



## DAGs ####

# Linear DAGs
dag1 <- dagitty('dag {
  complexity -> Ax1
  Ax1 -> height_ratio
}')


dag2 <- dagitty('dag {
  complexity -> height_ratio
  height_ratio -> Ax1
}')

dag3 <- dagitty('dag {
  height_ratio -> complexity
  complexity -> Ax1
}')


# collider
dag_col_1 <- dagitty('dag {
  height_ratio -> Ax1
  complexity -> Ax1
}')

dag_col_2 <- dagitty('dag {
  height_ratio -> complexity
  Ax1 -> complexity
}')

dag_col_3 <- dagitty('dag {
  Ax1 -> height_ratio
  complexity -> height_ratio
}')


# fork
dag_fork_1 <- dagitty('dag {
  complexity -> height_ratio
  complexity -> Ax1
}')

dag_fork_2 <- dagitty('dag {
  height_ratio -> complexity
  height_ratio -> Ax1
}')

dag_fork_3 <- dagitty('dag {
  Ax1 -> height_ratio
  Ax1 -> complexity
}')


# full graph (equivalent to all possible ful dags)
dag6 <- dagitty('dag {
  complexity -> height_ratio
  complexity -> Ax1
  height_ratio -> Ax1
}')


# I also omit partial graphs



# Table ####

lapply(list(dag1, dag2, dag3), dag_to_table_row) %>% 
  bind_rows() %>% 
  flextable::flextable()

lapply(list(dag_col_1, dag_col_2, dag_col_3), dag_to_table_row) %>% 
  bind_rows() %>% 
  flextable::flextable()

lapply(list(dag_fork_1, dag_fork_2, dag_fork_3), dag_to_table_row) %>% 
  bind_rows() %>% 
  flextable::flextable()


table_dag <- lapply(list(dag1, dag2, dag3,
            dag_fork_1, dag_fork_2, dag_fork_3,
            dag_col_1, dag_col_2, dag_col_3), dag_to_table_row) %>% 
  bind_rows() %>% 
  cbind(graph_structure = c(rep("chain", 3),
                            rep("fork", 3),
                            rep("collider", 3)), .) %>% 
  cbind(graph_number = c(1:9), .) %>% 
  select(-pval_mod) %>% 
  flextable::flextable() %>% 
  flextable::set_header_labels(
    graph_structure = "Graph structure",
    graph_number = "Graph number",
    graph = "Causal links",
    ci_renamed = "Predicted conditional independence",
    formula_character = "Model",
    # pval_mod = "Model's p-value",
    info = "Test of the predicted conditional independence",
    conclusion = "Conclusion"
  ) %>% 
  width(j = 1, width = 1.3, "cm") %>%
  width(j = 2, width = 1.3, "cm") %>%
  width(j = 3, width = 3.5, "cm") %>%
  width(j = 4, width = 3.5, "cm") %>%
  width(j = 5, width = 3, "cm") %>%
  flextable::set_caption("Table S8. Putative causal graphs linking social complexity, women's status, and height ratios, 
  and tests of the related conditional independence predictions.  
  Putative causal links, of the form of either a chain (A causing B causing C), a fork (A causing both B and C), 
  or a collider (A and B jointly causing C) are presented. 
  The predicted conditional independence resulting from each graph were computed using the `impliedConditionalIndependencies` 
  function of the `dagitty` R package. A conditional independence statement 'C _||_ A | B' reads: 'C is independent of A knowing B'. 
  A potential graph structure is rejected if the predicted conditional independence is not verified in the data 
  (in the previous example, if A still predicts C when accounting for B).
  
  Note that graph A -> B -> C generates the same conditional indendence prediction as C -> B -> A, 
  and they are are therefore not distinguishable based on this method.  
  
  To test the conditional independence prediction of each graph, we used linear modeling, whose formulas are reported. 
  The coefficient and p-value of the conditional independence test are given. 
  The last column states whether the graph structure is rejected with the data. 
  
  Here, data consists in the complexity variable, women's status index, and height ratios, 
  for the 46 societies for which the latter was available. 

  Two graphs were not rejected by the conditional independence tests. The first, graph 1, proposes that 
  increased complexity leads to a decrease in women's status, 
  which further increases sex differences in height (or the equivalent opposite, from height ratio to 
  women's status and to complexity). The second, graph 6, proposed that women's status drives 
  both social complexity and sex differences in height. 

  N.B. Graph structures may be rejected not because the causal links displayed are false, 
  but because of omitted variables. For more details, see McElreath, R. (2020). Statistical rethinking: 
  A Bayesian course with examples in R and Stan (Second edition). CRC Press. 
  ")


save_as_docx(
  table_dag,
  pr_section = officer::prop_section(
    page_size = officer::page_size(orient = "portrait")
  ),
  path = "draft/tbl/talbe_dag.docx"
)




