library(cluster)
library(clusterCrit)

cluster_select <- function(data, try_clusters){
  ### Decide on number of clusters: run between 2 and 7
  clusterings <- lapply(try_clusters, function(x) pam(data, x))
  DB_values <- sapply(seq_along(clusterings), function(x) 
    intCriteria(data, as.integer(clusterings[[x]]$clustering),
                c("Davies_Bouldin")))
  plot <- ggplot(data.frame(Clusters = try_clusters, DBindex = unlist(DB_values)),
         aes(Clusters, DBindex)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    theme_bw()
  
  num_clusters_index <- which.min(unlist(DB_values))
  num_clusters <- try_clusters[num_clusters_index]
  
  return(list(plot = plot,
              clusterings = clusterings,
              num_clusters_index = num_clusters_index,
              num_clusters = num_clusters))
}

prepare_data <- function(data, clusterings, num_clusters_index){
  data_plot_t <- data.frame(class = as.factor(clusterings[[num_clusters_index]]$clustering),
                            data) %>%
    tidyr::pivot_longer(cols = starts_with('X'),
                        names_to = 'variable',
                        values_to = 'value') %>%
    dplyr::arrange(variable) %>%
    dplyr::mutate(Time = gsub('X', '', variable),
                  ID = rep(1:nrow(att_raw), ncol(att_raw))) %>%
    dplyr::mutate(Time = as.Date(Time, format = '%Y.%m.%d'))
  return(data_plot_t)
}

prepare_centers <- function(clusterings, num_clusters_index, num_clusters){
  centers_t <- data.frame(clusterings[[num_clusters_index]]$medoids,
                          class = 1:num_clusters) %>%
    tidyr::pivot_longer(cols = X2019.04.01:X2020.02.01,
                        names_to = 'variable',
                        values_to = 'value') %>%
    dplyr::arrange(variable, class) %>%
    dplyr::rename(Time = variable) %>%
    dplyr::mutate(Time = as.Date(gsub('X', '', Time), format = '%Y.%m.%d'),
                  ID = value)
  return(centers_t)
}
