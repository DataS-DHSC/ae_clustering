source('load_data.R')

### Create timeseries
tot_att <- ae %>%
  dplyr::select(month, code, region, total_attendances) %>%
  dplyr::arrange(code, month) %>%
  dplyr::mutate(mon_name = paste0('mon', month),
                total_attendances = total_attendances + 0.5) %>%
  tidyr::pivot_wider(id_cols = code:region, 
                     names_from = month, 
                     values_from = total_attendances) %>%
  na.omit() %>%
  dplyr::mutate_if(is.numeric, log) %>%
  dplyr::mutate_if(is.numeric, scale)

library(cluster)
library(clusterCrit)
library(ggplot2)
library(data.table)
att_raw <- as.matrix(tot_att[, 3:ncol(tot_att)])
clusterings <- lapply(c(2:7), function(x) pam(att_raw, x))
DB_values <- sapply(seq_along(clusterings), function(x) 
  intCriteria(att_raw, as.integer(clusterings[[x]]$clustering),
              c("Davies_Bouldin")))
ggplot(data.frame(Clusters = 2:7, DBindex = unlist(DB_values)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()

data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[3]]$clustering),
                                        tot_att)))
data_plot[, Time := as.Date(variable, origin = '1970-01-01')]
data_plot[, ID := rep(1:nrow(att_raw), ncol(att_raw))]

# prepare medoids
centers <- data.table(melt(data.table(clusterings[[3]]$medoids,
                                      class = 1:4),
                           id.vars = 'class'))
setnames(centers, c('class', "variable", "value"), c('class', "Time", "value"))
centers[, Time := as.Date(Time, origin = '1970-01-01')][, `:=`(ID = value)]

# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time, value, group= NULL), 
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  labs(x = "Time", y = "Load (normalised)") +
  theme_bw()

