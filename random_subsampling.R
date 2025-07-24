dt_group_keys <- unique(standardized_dt[
  , .BY, by = .(monitoring_location_id, activity_start_date, activity_start_time) # replace column names with your own
  ][
    , !"BY"
  ])

subsampled_dt <- data.table()

for(i in 1:nrow(dt_group_keys)){
  # Using the group keys defined above, select group i
  temp_dt <- standardized_dt[][
    na.omit(standardized_dt[dt_group_keys[i], on = .(monitoring_location_id, activity_start_date, activity_start_time), which = T])
  ]
  
  if(sum(temp_dt[, .(count)]) > 360){
    temp_dt <- unique(temp_dt[][
      rep(1:.N, count) # duplicate rows 'count' times
    ][
      , count := 1
    ][
      sample(.N, 360) # randomly sample rows 360 times
    ][
      , count := sum(count), by = .(ID, monitoring_location_id, activity_start_date, activity_start_time)
    ])

  }
  
  subsampled_dt <- rbind(subsampled_dt, temp_dt)
}
