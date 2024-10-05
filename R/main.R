# main control loop
repeat {

  # # say Hello to Gmail
  n_errors <- tryCatch(
    {
      gm_auth(token = gm_token_read(path = ".secrets/gmailr-token.rds"))
      0L
    },
    error = function(e1) {
      flog.error(sprintf("Accessing Gmail failed - can't report results. Msg = %s", conditionMessage(e1)),
                 name = "cz_df")
      return(1L)
    }
  )

  if (n_errors > 0) {
    break
  }

  if (exists("salsa_source_error")) {
    report_msg <- "Taak kon niet voltooid worden - zie 'woj_schedules.log' (Nipper/Desktop)."
  } else {
    report_msg <- "Taak voltooid, geen bijzonderheden."
  }

  # report the result ----
  task_report <- gm_mime() |>
    # gm_to(c(config$email_to_A, config$email_to_B)) |>
    gm_to(config$email_to_A) |>
    gm_from(config$email_from) |>
    gm_subject("CZ-diskfree TEST") |>
    gm_text_body(report_msg)

  rtn <- gm_send_message(task_report)

  # ======================
  # EXIT main control loop
  # ======================
  break
}

flog.info("<<< STOP", name = "cz_df")
