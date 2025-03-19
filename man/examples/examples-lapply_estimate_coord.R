# library(data.table)
# library(patter)
# 
# map        <- dat_gebco()
# detections <- dat_detections
# unit_ids   <- unique(detections$individual_id)
# 
# # Define iteration data.table
# iteration <- data.table(unit_id = unit_ids, 
#                         delta_t = "1 hour")
# 
# # Define datasets list
# datasets <- list(detections_by_unit = split(detections, detections$individual_id))
#                         
# coord_coa <- lapply_estimate_coord_coa(iteration, 
#                                        map,
#                                        datasets, 
#                                        trial = FALSE,
#                                        coffeehouse = NULL,
#                                        log.folder = NULL, 
#                                        log.txt = NULL, 
#                                        verbose = TRUE)
