# 혼잡 시간대 추가 인력 배치 시뮬레이션
simulate_additional_staff <- function(data, arrival_rate, service_rate, num_doctors_peak, num_doctors_offpeak, num_beds) {
  results <- list()
  CRN_Z <- 10000  # CRN 초기값

  for (i in 1:nrow(data)) {
    # 환자 정보 읽기
    patient <- data[i, ]
    severity <- patient$Severity
    service_time <- patient$Service_Time

    # CRN을 이용해 환자 도착 시간 계산
    arrival_result <- R_Exp(CRN_Z, 1 / arrival_rate)
    CRN_Z <- arrival_result$Z
    arrival_time <- arrival_result$RN

    # 시간대에 따른 의사 수 설정
    if (arrival_time >= 9 * 60 && arrival_time <= 18 * 60) {  # 혼잡 시간대: 9시~18시
      num_doctors <- num_doctors_peak
    } else {
      num_doctors <- num_doctors_offpeak
    }

    doctor_availability <- rep(0, num_doctors)  # 의사 초기 가용 상태

    # 병상 가용성 확인
    if (num_beds > 0) {
      bed_wait_time <- 0
      num_beds <- num_beds - 1  # 병상 점유
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
    num_beds <- min(15, num_beds + 1)  # 치료 완료 후 병상 회복

    # 결과 저장
    results[[i]] <- data.frame(
      Patient_ID = patient$Patient_ID,
      Severity = severity,
      Arrival_Time = arrival_time,
      Wait_Time = wait_time,
      Service_Time = service_time,
      Completion_Time = completion_time
    )
  }

  return(do.call(rbind, results))
}

# 실행
result_scenario_2 <- simulate_additional_staff(
  data = simulation_data,
  arrival_rate = 1,
  service_rate = 2,
  num_doctors_peak = 3,
  num_doctors_offpeak = 2,
  num_beds = 15
)
