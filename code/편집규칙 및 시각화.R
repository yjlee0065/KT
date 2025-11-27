library(readxl)
library(dplyr)
library(lubridate)

# 파일 불러오기
setwd("C:/Users/yjlee/Downloads")
lv1 = as.data.frame(read_excel("data.xlsx"))

# lv1 시작-종료 형태의 시청로그 불러오기
return_lv1 = function(lv1){
  startdate = format(as.POSIXct(lv1$시작시각, format="%Y-%m-%d %H:%M:%S"), "%Y%m%d%H%M%S")
  currentdate = format(as.POSIXct(lv1$현재시각, format="%Y-%m-%d %H:%M:%S"), "%Y%m%d%H%M%S")
  lv1_df = data.frame(id = as.character(lv1$id), startdate, currentdate, ch_no = lv1$채널번호, base_date = lv1$base_date, base_hour = lv1$base_hour)
  
  #### NA 제거 ####
  lv1_df = na.omit(lv1_df)
  
  #### unixtime 변환 ####
  lv1_df = lv1_df %>%
    mutate(
      id = as.character(id),
      sd_unixtime = as.numeric(as.POSIXct(startdate, format="%Y%m%d%H%M%S")),
      cd_unixtime = as.numeric(as.POSIXct(currentdate, format="%Y%m%d%H%M%S"))
    )
  
  #### 2. 이상로그 제거 처리 ####
  lv1_df = lv1_df %>%
    filter(sd_unixtime < cd_unixtime
    )
  
  
  #### 3. 로그를 시작 종료 형태로 묶음 처리 ####
  
  # 정렬
  lv1_df = lv1_df %>%
    arrange(id, startdate, currentdate)
  
  # gap 생성, gap 음수 값 제거
  lv1_df = lv1_df %>%
    mutate(
      gap = cd_unixtime - sd_unixtime
    ) %>%
    filter(
      gap >= 0
    )
  
  # 최종 currentdate만 필터링
  lv1_df = lv1_df %>%
    group_by(id, startdate) %>%
    filter(currentdate == max(currentdate)) %>%
    ungroup()
  
  lv1_df = lv1_df %>%
    distinct(id, sd_unixtime, cd_unixtime, .keep_all = TRUE) %>%
    select(id, ch_no, startdate, currentdate, sd_unixtime, cd_unixtime)
  
  return(lv1_df)
}
lv1_df = as.data.frame(return_lv1(lv1))

# lv2 시청로그 불러오기
return_lv2 = function(lv2){
  lv2_df = lv2 %>%
    mutate(
      시작시각_unixtime = as.numeric(시작시각_unixtime),
      종료시각_unixtime = as.numeric(종료시각_unixtime),
      startdate = 시작시각,
      currentdate = 종료시각,
      ch_no = 채널번호,
      sd_unixtime = 시작시각_unixtime,
      cd_unixtime = 종료시각_unixtime
    ) %>%
    arrange(id, sd_unixtime, cd_unixtime) %>%
    select(id, ch_no, startdate, currentdate, sd_unixtime, cd_unixtime)
  
  return(lv2_df)
}
lv2_df = return_lv2(lv2)

# lv3 시청로그 불러오기
return_lv3 = function(lv2){
  max_session_hour = 8   # 데모용 상한 (실제와 무관)
  lv3_df = lv2 %>%
    mutate(
      시작시각_unixtime = as.numeric(시작시각_unixtime),
      종료시각_unixtime = as.numeric(종료시각_unixtime),
      class = ifelse(종료시각_unixtime - 시작시각_unixtime > max_session_hour*3600, 1, 0),
      종료시각_unixtime = ifelse(class==1, 시작시각_unixtime + max_session_hour*3600, 종료시각_unixtime),
      종료시각 = format(as.POSIXct(종료시각_unixtime), "%Y%m%d%H%M%S"),
    ) %>%
    mutate(
      startdate = 시작시각,
      currentdate = 종료시각,
      ch_no = 채널번호,
      sd_unixtime = 시작시각_unixtime,
      cd_unixtime = 종료시각_unixtime
    ) %>%
    arrange(id, sd_unixtime, cd_unixtime) %>%
    select(id, ch_no, startdate, currentdate, sd_unixtime, cd_unixtime)
  
  return(lv3_df)
}
lv3_df = return_lv3(lv2)

# 새 편집규칙 적용한 시청로그
lv1_to_lv2 = function(lv1, date){ #date는 YYYYmmdd형식의 텍스트
  
  # 기밀 유지를 위해 몇몇 편집규칙이 생략됨
  
  date_unixtime = as.numeric(as.POSIXct(date, format="%Y%m%d"))
  
  startdate = format(as.POSIXct(lv1$시작시각, format="%Y-%m-%d %H:%M:%S"), "%Y%m%d%H%M%S")
  currentdate = format(as.POSIXct(lv1$현재시각, format="%Y-%m-%d %H:%M:%S"), "%Y%m%d%H%M%S")
  lv1_df = data.frame(id = as.character(lv1$id), startdate, currentdate, ch_no = lv1$채널번호, base_date = lv1$base_date, base_hour = lv1$base_hour)
  
  #### NA 제거 ####
  lv1_df = na.omit(lv1_df)
  
  #### unixtime 변환 ####
  lv1_df = lv1_df %>%
    mutate(
      id = as.character(id),
      sd_unixtime = as.numeric(as.POSIXct(startdate, format="%Y%m%d%H%M%S")),
      cd_unixtime = as.numeric(as.POSIXct(currentdate, format="%Y%m%d%H%M%S"))
    )
  
  
  #### 1. 수집시점 필터 적용 ####
  # 예시: 기준일 전후 일정시간범위만 분석대상으로 사용
  window_before = 2 * 3600
  window_after = 18 * 3600
  
  lv1_df_1 = lv1_df %>%
    filter(
      (sd_unixtime >= date_unixtime-window_before) & (cd_unixtime <= date_unixtime+window_after)
    )
  
  
  #### 2. 이상로그 제거 처리 ####
  lv1_df_2 = lv1_df_1 %>%
    filter(sd_unixtime < cd_unixtime
    )
  
  
  #### 3. 로그를 시작 종료 형태로 묶음 처리 ####
  
  # 정렬
  lv1_df_2 = lv1_df_2 %>%
    arrange(id, startdate, currentdate)
  
  # gap 생성, gap 음수 값 제거
  lv1_df_3 = lv1_df_2 %>%
    mutate(
      gap = cd_unixtime - sd_unixtime
    ) %>%
    filter(
      gap >= 0
    )
  
  # 최종 currentdate만 필터링
  lv1_df_3 = lv1_df_3 %>%
    group_by(id, startdate) %>%
    filter(currentdate == max(currentdate)) %>%
    ungroup()
  
  lv1_df_3 = lv1_df_3 %>%
    distinct(id, sd_unixtime, cd_unixtime, .keep_all = TRUE)
  
  
  #### 5. 연속시청 보정 ####
  max_session_hour = 8   # 데모용 상한 (실제와 무관)
  lv1_df_4 = lv1_df_3 %>% 
    mutate(
      class1 = ifelse(gap >= max_session_hour*3600, 1, 0)  # 일정시간 이상 시청한 경우 class1 = 1
    )
  
  # 일정시간 이상 시청한 경우 시청기록을 일정시간 시청으로 보정
  lv1_df_5 = lv1_df_4 %>%
    mutate(
      cd_unixtime = ifelse(class1 == 1, sd_unixtime + max_session_hour*3600, cd_unixtime),
      currentdate = ifelse(class1 == 1, format(as.POSIXct(cd_unixtime), "%Y%m%d%H%M%S"), currentdate)  # 종료시간 보정에 따른 currentdate 업데이트
    )
  
  
  #### 6. 이틀에 걸친 시청내역 분리 ####
  
  lv1_df_6 = lv1_df_5 %>%
    filter(
      !((sd_unixtime < date_unixtime & cd_unixtime < date_unixtime)|
          (sd_unixtime >= date_unixtime + 24*3600 & cd_unixtime >= date_unixtime + 24*3600))
    ) %>%
    mutate(
      sd_unixtime = ifelse(sd_unixtime < date_unixtime, date_unixtime, sd_unixtime),
      startdate = format(as.POSIXct(sd_unixtime), "%Y%m%d%H%M%S"),
      cd_unixtime = ifelse(cd_unixtime >= date_unixtime + 24*3600, date_unixtime + 24*3600 - 1, cd_unixtime),
      currentdate = format(as.POSIXct(cd_unixtime), "%Y%m%d%H%M%S"),
      base_date = date
    ) %>%
    select(-class1, -gap)
  
  return(lv1_df_6)
}
return_new = function(lv1){
  lv2_01 = lv1_to_lv2(lv1, "20240601")
  lv2_02 = lv1_to_lv2(lv1, "20240602")
  lv2_03 = lv1_to_lv2(lv1, "20240603")
  lv2_04 = lv1_to_lv2(lv1, "20240604")
  lv2_05 = lv1_to_lv2(lv1, "20240605")
  lv2_06 = lv1_to_lv2(lv1, "20240606")
  lv2_07 = lv1_to_lv2(lv1, "20240607")
  lv2_08 = lv1_to_lv2(lv1, "20240608")
  lv2_09 = lv1_to_lv2(lv1, "20240609")
  lv2_10 = lv1_to_lv2(lv1, "20240610")
  
  ours = rbind(lv2_01, lv2_02, lv2_03, lv2_04, lv2_05,
               lv2_06, lv2_07, lv2_08, lv2_09, lv2_10)
  
  ours = ours %>%
    arrange(id, sd_unixtime, cd_unixtime) %>%
    select(id, ch_no, startdate, currentdate, sd_unixtime, cd_unixtime)
  
  return(ours)
}
new_df = as.data.frame(return_new(lv1))

################################################################################

# 개별 id 추출
id_list = unique(lv1_df$id)

get_ganttchart <- function(lv1, lv2, lv3, new, id, start, end, set.title=FALSE){
  
  library(ggplot2)
  
  # lv1에는 있는데 lv2에는 없는 id 존재할 수 있음
  lv1_id = lv1[lv1$id==id,]
  lv2_id = lv2[lv2$id==id,]
  lv3_id = lv3[lv3$id==id,]
  new_id = new[new$id==id,]
  
  lv1_id = lv1_id %>% arrange(sd_unixtime, cd_unixtime)
  lv2_id = lv2_id %>% arrange(sd_unixtime, cd_unixtime)
  lv3_id = lv3_id %>% arrange(sd_unixtime, cd_unixtime)
  new_id = new_id %>% arrange(sd_unixtime, cd_unixtime)
  
  data = rep("lv1", nrow(lv1_id)); lv1_id = cbind(lv1_id, data)
  data = rep("lv2", nrow(lv2_id)); lv2_id = cbind(lv2_id, data)
  data = rep("lv3", nrow(lv3_id)); lv3_id = cbind(lv3_id, data)
  data = rep("NEW", nrow(new_id)); new_id = cbind(new_id, data)
  
  timeline = rbind(lv1_id, lv2_id, lv3_id, new_id)
  
  # 채널별로 색상 지정
  ch_no = unique(lv1$ch_no)
  color_palette = rainbow(length(ch_no))
  color = color_palette[as.factor(ch_no)]
  colors = as.data.frame(cbind(ch_no, color))
  timeline = merge(timeline, colors, by = "ch_no", all.x = TRUE)
  
  timeline$data = factor(timeline$data, levels = c("NEW", "lv3", "lv2", "lv1"))
  
  timeline$sd_unixtime = as.POSIXct(as.numeric(timeline$sd_unixtime))
  timeline$cd_unixtime = as.POSIXct(as.numeric(timeline$cd_unixtime))
  
  start_time <- as.POSIXct(start)
  end_time <- as.POSIXct(end)
  
  labs_obj <- 
    if (set.title) {
      labs(
        title = paste("Time Range:",
                      format(start_time, "%Y-%m-%d %H:%M:%S"), "-",
                      format(end_time, "%Y-%m-%d %H:%M:%S")),
        subtitle = paste("ID:", id)
      )
    } else {
      labs(
        subtitle = paste("ID:", id)
      )
    }
  
  ggplot() +
    geom_segment(data=timeline, aes(x=sd_unixtime, xend=cd_unixtime, y=data, yend=data, color=color), 
                 linetype=1, linewidth=10) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    coord_cartesian(xlim = c(start_time, end_time)) +
    xlab("Time") +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
    theme(aspect.ratio = .2) +
    theme(legend.position="none") +
    theme(
      plot.title = element_text(hjust = 0.5), 
      plot.subtitle = element_text(hjust = 0.5) 
    ) + 
    labs_obj
}

get_ganttchart(lv1 = lv1_df, lv2 = lv2_df, lv3 = lv3_df, new = new_df, 
               id = id_list[1], start = "2024-06-01 00:00:00", end = "2024-06-10 23:59:59", 
               set.title = TRUE)
