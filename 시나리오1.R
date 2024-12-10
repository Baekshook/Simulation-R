# 데이터 불러오기
data_path <- "expanded_data_v2.csv"  # 데이터 경로
simulation_data <- read.csv(data_path, header = TRUE)

# 공통 난수 생성기(CRN) 정의
LCG <- function(Z) {
  a <- 214013
  c <- 2531011
  return((a * Z + c) %% (2^32))
}

# 지수분포 난수 생성 함수
R_Exp <- function(Z, Mean_Time) {
  m <- 2^32
  Z <- LCG(Z)
  return(data.frame(RN = -Mean_Time * log(Z / m), Z = Z))
}

# 중증 및 긴급 환자 우선 치료와 피로 관리 시뮬레이션
simulate_priority_and_fatigue <- function(data, arrival_rate, service_rate, num_doctors, fatigue_threshold, num_beds) {
  # 초기 자원 상태 설정
  doctor_availability <- rep(0, num_doctors)  # 각 의사의 초기 가용 상태
  doctor_fatigue <- rep(0, num_doctors)       # 각 의사의 초기 피로도
  bed_availability <- num_beds                # 병상의 초기 가용 상태
  results <- list()                           # 결과 저장 리스트
  CRN_Z <- 10000                              # CRN 초기값

  # 데이터 처리
  for (i in 1:nrow(data)) {
    # 환자 정보 읽기
    patient <- data[i, ]
    severity <- patient$Severity
    service_time <- patient$Service_Time
    age_group <- patient$Age_Group
    gender <- patient$Gender

    # CRN을 이용해 환자 도착 시간 계산
    arrival_result <- R_Exp(CRN_Z, 1 / arrival_rate)
    CRN_Z <- arrival_result$Z
    arrival_time <- arrival_result$RN

    # 중증 및 긴급 환자 우선 처리
    if (severity == "Critical" || severity == "Moderate") {
      priority <- 1  # 우선 치료
    } else {
      priority <- 2  # 후순위 (경증)
    }

    # 피로도 관리: 피로도가 임계값 초과 시 교대
    if (any(doctor_fatigue >= fatigue_threshold)) {
      fatigued_doctors <- which(doctor_fatigue >= fatigue_threshold)
      doctor_availability[fatigued_doctors] <- doctor_availability[fatigued_doctors] + 30  # 30분 휴식
      doctor_fatigue[fatigued_doctors] <- 0  # 피로도 초기화
    }

    # 병상 가용성 확인
    if (bed_availability > 0) {
      bed_wait_time <- 0
      bed_availability <- bed_availability - 1  # 병상 점유
    } else {
      bed_wait_time <- runif(1, 5, 20)  # 병상이 없을 경우 대기 시간
    }

    # 의사 가용성 확인
    earliest_available <- min(doctor_availability)
    doctor_idx <- which.min(doctor_availability)

    # 치료 시작 시간 및 대기 시간 계산
    start_time <- max(arrival_time, earliest_available) + bed_wait_time
    wait_time <- start_time - arrival_time
    completion_time <- start_time + service_time

    # 자원 상태 업데이트
    doctor_availability[doctor_idx] <- completion_time
    doctor_fatigue[doctor_idx] <- doctor_fatigue[doctor_idx] + service_time  # 피로도 증가
    bed_availability <- min(num_beds, bed_availability + 1)  # 치료 완료 후 병상 회복

    # 결과 저장
    results[[i]] <- data.frame(
      Patient_ID = patient$Patient_ID,
      Severity = severity,
      Arrival_Time = arrival_time,
      Wait_Time = wait_time,
      Service_Time = service_time,
      Completion_Time = completion_time,
      Priority = priority,
      Age_Group = age_group,
      Gender = gender
    )
  }

  return(do.call(rbind, results))
}

# 실행
result_scenario_1 <- simulate_priority_and_fatigue(
  data = simulation_data,
  arrival_rate = 1,
  service_rate = 2,
  num_doctors = 2,
  fatigue_threshold = 120,
  num_beds = 15
)
