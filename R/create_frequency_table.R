create_freq_table <- function(data, group_var, var) {
  data %>%
    mutate(across(all_of(group_var), as.character)) %>%
    group_by(across(all_of(c(group_var, var)))) %>%
    summarise(Frequency = n(), .groups = "drop") %>%
    group_by(across(all_of(group_var))) %>%
    mutate(Percentage = 100 * Frequency / sum(Frequency)) %>%
    ungroup() %>%
    complete(!!sym(group_var), !!sym(var), fill = list(Frequency = 0, Percentage = 0)) %>%
    bind_rows(
      data %>%
        mutate(across(all_of(group_var), as.character)) %>%
        group_by(across(all_of(var))) %>%
        summarise(Frequency = n(), .groups = "drop") %>%
        mutate(Percentage = 100 * Frequency / sum(Frequency)) %>%
        mutate(!!group_var := "Total")
    ) %>%
    arrange(!!sym(group_var), !!sym(var))
}