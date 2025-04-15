super_table = function(input_dataframe,
                       data_dictionary_variables,
                       data_dictionary_values,
                       group_variable,
                       omit_variables,
                       subset_list=c(),
                       save_location) {
  library(tidyverse)
  library(openxlsx)
  # library(foreign)
  library(stringr)
  # library(haven)
  library(magrittr)

  supertable = createWorkbook()

  # Merge baseline and followup questions into one table
  input_dataframe_table = data_dictionary_values %>%
    rename(all_of(c(Level = "X2",Response = "Label", Variable = "Value"))) %>%
    full_join(data_dictionary_variables, by="Variable") %>%
    select(Variable, Level, Response) %>%
    group_by(Variable) %>%
    mutate(Label = case_when(!is.na(Variable) ~ data_dictionary_variables$Label[data_dictionary_variables$Variable==Variable],
                             TRUE ~ Variable)) %>%
    relocate(Label, .after=Variable) %>%
    add_column(Overall = NA, `Smart-T` = NA, `Quit Guide` = NA) # TODO: dynamically add columns for each level in grouping variable

  if (length(subset_list)>0) {
    omit_variables = unique(c(omit_variables,setdiff(names(base_follow_df),subset_list)))
  }

  var_row_idx = which(!is.na(base_follow_table$Variable)) %>%
    as.data.frame
  nominal_rows = data.frame(Variable = c(NA,NA),
                            Label = c(NA,NA),
                            Level = c(NA,NA),
                            Response = c("# Asked","Missing among asked")) %>%
    add_column(Overall = NA, `Smart-T` = NA, `Quit Guide` = NA)
  scale_rows = data.frame(Variable = c(NA,NA,NA,NA,NA),
                          Label = c(NA,NA,NA,NA,NA),
                          Level = c(NA,NA,NA,NA,NA),
                          Response = c("# Asked","Missing among asked","Mean (SD)","Median [IQR]","Range")) %>%
    add_column(Overall = NA, `Smart-T` = NA, `Quit Guide` = NA)
  # Add # Asked, Missing, and scale responses to each variable as appropriate
  for (row_num in rev(1:nrow(var_row_idx))) {
    start_row = var_row_idx[row_num,1]
    end_row = var_row_idx[row_num+1,1]-1
    variable = base_follow_table$Variable[start_row]
    label = base_follow_table$Label[start_row]

    if ((variable %in% omit_variables) | !(variable %in% names(base_follow_df))) next # skip variables that aren't being considered
    #
    variable_type = data_dictionary_variables$Measurement.Level[data_dictionary_variables$Variable==variable]
    if (variable_type=="Nominal" | variable_type=="Ordinal"){
      base_follow_table$Variable[start_row] = NA
      base_follow_table$Label[start_row] = NA
      base_follow_table = rbind(base_follow_table[1:(start_row-1),],
                                nominal_rows,
                                base_follow_table[-(1:(start_row-1)),])
      base_follow_table$Variable[start_row] = variable
      base_follow_table$Label[start_row] = label

      if (NA %in% (base_follow_table[start_row:(end_row+2),"Response"] %>% deframe)) {
        # if (!(variable %in% names(base_follow_df))) next # TODO remove after demonstration
        unique_responses = base_follow_df[variable] %>%
          unique %>%
          na.omit %>%
          deframe %>%
          sort %>%
          as.character
        na_mat = rep(NA,length(unique_responses))
        response_rows = data.frame(Variable = na_mat,
                                   Label = na_mat,
                                   Level = na_mat,
                                   Response = unique_responses) %>%
          add_column(Overall = NA, `Smart-T` = NA, `Quit Guide` = NA)
        base_follow_table = rbind(base_follow_table[1:(start_row+1),],
                                  response_rows,
                                  base_follow_table[-(1:(start_row+2)),])
      }
    } else if (variable_type=="Scale" | variable_type=="Datetime") {
      base_follow_table$Variable[start_row] = NA
      base_follow_table$Label[start_row] = NA
      base_follow_table = rbind(base_follow_table[1:(start_row-1),],
                                scale_rows,
                                base_follow_table[-(1:(start_row)),])
      base_follow_table$Variable[start_row] = variable
      base_follow_table$Label[start_row] = label
    }
  }
  #   write.csv(base_follow_table,base_follow_table_path)
  # }


  ### Baseline

  # try(addWorksheet(supertable,"Baseline"))
  # base_follow_df$Timepoint = trimws(base_follow_df$Timepoint)
  # timepoints = c("Baseline","Final Survey")

  baseline_df = base_follow_df %>%
    filter(is_valid_baseline_record=="Yes")

  baseline_supertable = base_follow_table

  baseline_drop_rows = c()
  missing_rows = c()

  bl_accidentally_excluded_list = c()
  var_row_idx = which(!is.na(baseline_supertable$Variable)) %>%
    as.data.frame
  groups = c("Overall","Smart-T","Quit Guide")
  for (row_num in 1:nrow(var_row_idx)) {
    start_row = var_row_idx[row_num,1]
    # end_row = var_row_idx[row_num+1,1]-1
    end_row = min(nrow(baseline_supertable),var_row_idx[row_num+1,1]-1,na.rm = T)
    variable = baseline_supertable$Variable[start_row]
    # print(variable)
    # if (variable %in% omit_variables) next
    if ((variable %in% omit_variables) | !(variable %in% names(baseline_df))) {
      if (is.na(end_row)) end_row = start_row
      baseline_drop_rows = c(baseline_drop_rows,seq(start_row,end_row))
      next
    }
    variable_type = data_dictionary_variables$Measurement.Level[data_dictionary_variables$Variable==variable]
    for (group in groups) {
      ###############################
      # Steps for getting n_asked and n_missing for nested questions:
      #   1. Create var_data based on everyone who meets the criteria (i.e. sq_11==4)
      #   2. mark NA based on known missing codes
      #   3. n_asked = the length of the new vector
      #   4. n_missing = sum(is.na(var_data))
      ###############################
      branching_logic = data_dictionary_variables$Branching.Logic[data_dictionary_variables$Variable==variable]
      if(!is.na(branching_logic)) {
        parse_string = paste0("baseline_df %>% filter(",branching_logic,")")
        exclude_parse_string = paste0("baseline_df %>% filter(!(",branching_logic,")) %>% select(!!variable) %>% deframe")
        filtered_data = eval(parse(text=parse_string)) #TODO Don't use eval here, need to implement a different system when there's time
        excluded_data = eval(parse(text=exclude_parse_string))
        if(!all(is.na(excluded_data))) {
          # print(paste0(variable," branching excluded non-missing values from Baseline dataset"))
          bl_accidentally_excluded_list = c(bl_accidentally_excluded_list,variable)
        }
      } else {
        filtered_data = baseline_df
      }
      if (group == "Overall") {
        var_data = filtered_data %>% select(!!variable) %>% deframe
        n_asked = length(var_data)
      } else {
        var_data = filtered_data %>%
          filter(treatment_group==group) %>%
          select(!!variable) %>%
          deframe
        n_asked = length(var_data)
      }

      missing_code = as.numeric(data_dictionary_variables$Missing.Values[data_dictionary_variables$Variable==variable])
      if (!is.na(missing_code)) {
        var_data[var_data==missing_code] = NA
      }
      n_missing = sum(is.na(var_data))

      #TODO Make sure the code below actually makes sense and isn't corrupting the counts
      if ((n_asked<=n_missing | n_asked==0) & group=="Overall") {
        if (is.na(end_row)) end_row = start_row
        baseline_drop_rows = c(baseline_drop_rows,seq(start_row,end_row))
        next
      }

      if(n_missing>0 & group=="Overall") {
        missing_rows = c(missing_rows, seq(start_row,start_row+1))
        # missing_rows = c(missing_rows, seq(start_row,end_row))
      }

      if (variable_type=="Nominal" | variable_type=="Ordinal"){
        responses = baseline_supertable[start_row:end_row,"Response"]
        for (response_num in 1:nrow(responses)) {
          response = responses[response_num,1] %>% deframe
          # response = responses[response_num]
          if(response=="# Asked") {
            response_count = n_asked
            response_string = as.character(response_count)
          } else if (response=="Missing among asked") {
            response_count = n_missing
            response_percent = round((response_count/n_asked)*100,digits = 2)
            response_string = paste0(as.character(response_count)," ",
                                     "(",as.character(response_percent),"%)")
          } else {
            response_count = sum(var_data==response, na.rm = T)
            response_percent = round((response_count/n_asked)*100,digits = 2)
            response_string = paste0(as.character(response_count)," ",
                                     "(",as.character(response_percent),"%)")
          }
          baseline_supertable[start_row+(response_num-1),group] = response_string
        }
      }
      else if (variable_type=="Scale") {
        if (class(var_data)!="Datetime") {
          var_data = as.numeric(var_data)
          quantile_type = 7
        } else {
          quantile_type = 1
        }

        baseline_supertable[start_row,group] = as.character(n_asked)

        missing_percent = as.character(round((n_missing/n_asked)*100,digits = 2))
        baseline_supertable[start_row+1,group] = paste0(as.character(n_missing)," ",
                                                        "(",missing_percent,"%)")
        var_mean = round(mean(var_data,na.rm=T),digits = 2)
        var_sd = round(sd(var_data,na.rm=T),digits = 2)
        baseline_supertable[start_row+2,group] = paste0(as.character(var_mean),
                                                        " ","(",as.character(var_sd),")")

        var_median = round(median(var_data,na.rm=T),digits = 2)
        quantile_vector = quantile(var_data,na.rm = T, type = quantile_type)
        q1 = quantile_vector[2]
        q3 = quantile_vector[4]
        baseline_supertable[start_row+3,group] = paste0(as.character(var_median)," ",
                                                        "[",as.character(round(q1, digits=2)),
                                                        ", ",as.character(round(q3, digits=2)),"]")

        var_range = round(range(var_data,na.rm=T),digits = 2)
        range_lower = as.character(var_range[1])
        range_upper = as.character(var_range[2])
        baseline_supertable[start_row+4,group] = paste0("(",range_lower,", ",range_upper,")")
      }
    }
  }

  # cleanup!
  baseline_missing_table = baseline_supertable[missing_rows,] %>%
    select(-Level)
  baseline_supertable = baseline_supertable[-baseline_drop_rows,] %>%
    select(-Level)


  ### Final Survey

  # try(addWorksheet(supertable,"Final Survey"))
  fs_accidentally_excluded_list = c()
  final_survey_df = base_follow_df %>%
    filter(final_survey_type=="Final Survey" & is_valid_final_record=="Yes")
  # filter(Timepoint=="Final Survey")  #%>%
  # select(where(~sum(!is.na(.x))>0))

  final_survey_supertable = base_follow_table

  final_survey_drop_rows = c()
  missing_rows = c()

  var_row_idx = which(!is.na(final_survey_supertable$Variable)) %>%
    as.data.frame
  groups = c("Overall","Smart-T","Quit Guide")
  for (row_num in 1:nrow(var_row_idx)) {
    start_row = var_row_idx[row_num,1]
    # end_row = var_row_idx[row_num+1,1]-1
    end_row = min(nrow(final_survey_supertable),var_row_idx[row_num+1,1]-1,na.rm = T)
    variable = final_survey_supertable$Variable[start_row]
    if ((variable %in% omit_variables) | !(variable %in% names(final_survey_df))) {
      if (is.na(end_row)) end_row = start_row
      final_survey_drop_rows = c(final_survey_drop_rows,seq(start_row,end_row))
      next
    }
    variable_type = data_dictionary_variables$Measurement.Level[data_dictionary_variables$Variable==variable]
    for (group in groups) {
      branching_logic = data_dictionary_variables$Branching.Logic[data_dictionary_variables$Variable==variable]
      if(!is.na(branching_logic)) {
        parse_string = paste0("final_survey_df %>% filter(",branching_logic,")")
        exclude_parse_string = paste0("final_survey_df %>% filter(!(",branching_logic,")) %>% select(!!variable) %>% deframe")
        filtered_data = eval(parse(text=parse_string)) #TODO Don't use eval here, need to implement a different system when there's time
        excluded_data = eval(parse(text=exclude_parse_string))
        if(!all(is.na(excluded_data))) {
          # print(paste0(variable," branching excluded non-missing values from Final Survey dataset"))
          fs_accidentally_excluded_list = c(fs_accidentally_excluded_list,variable)
        }
      } else {
        filtered_data = final_survey_df
      }

      if (group == "Overall") {
        var_data = filtered_data %>% select(!!variable) %>% deframe
        n_asked = length(var_data)
      } else {
        var_data = filtered_data %>%
          filter(treatment_group==group) %>%
          select(!!variable) %>%
          deframe
        n_asked = length(var_data)
      }

      # missing_code = data_dictionary_variables$Missing.Values[data_dictionary_variables$Variable==variable]
      missing_code = as.numeric(data_dictionary_variables$Missing.Values[data_dictionary_variables$Variable==variable])
      if (!is.na(missing_code)) {
        var_data[var_data==missing_code] = NA
      }
      n_missing = sum(is.na(var_data))

      if ((n_asked<=n_missing | n_asked==0) & group=="Overall") {
        if (is.na(end_row)) end_row = start_row
        final_survey_drop_rows = c(final_survey_drop_rows,seq(start_row,end_row))
        next
      }

      if(n_missing>0 & group=="Overall") {
        missing_rows = c(missing_rows, seq(start_row,start_row+1))
        # missing_rows = c(missing_rows, seq(start_row,end_row))
      }

      if (variable_type=="Nominal" | variable_type=="Ordinal"){
        responses = final_survey_supertable[start_row:end_row,"Response"]
        for (response_num in 1:nrow(responses)) {
          response = responses[response_num,1] %>% deframe
          # response = responses[response_num]
          if(response=="# Asked") {
            response_count = n_asked
            response_string = as.character(response_count)
          } else if (response=="Missing among asked") {
            response_count = n_missing
            response_percent = round((response_count/n_asked)*100,digits = 2)
            response_string = paste0(as.character(response_count)," ",
                                     "(",as.character(response_percent),"%)")
          } else {
            response_count = sum(var_data==response, na.rm = T)
            response_percent = round((response_count/n_asked)*100,digits = 2)
            response_string = paste0(as.character(response_count)," ",
                                     "(",as.character(response_percent),"%)")
          }
          final_survey_supertable[start_row+(response_num-1),group] = response_string
        }
      }
      else if (variable_type=="Scale") {
        if (class(var_data)!="Date") {
          var_data = as.numeric(var_data)
          quantile_type = 7
        } else {
          quantile_type = 1
        }

        final_survey_supertable[start_row,group] = as.character(n_asked)

        missing_percent = as.character(round((n_missing/n_asked)*100,digits = 2))
        final_survey_supertable[start_row+1,group] = paste0(as.character(n_missing)," ",
                                                            "(",missing_percent,"%)")
        var_mean = round(mean(var_data,na.rm=T),digits = 2)
        var_sd = round(sd(var_data,na.rm=T),digits = 2)
        final_survey_supertable[start_row+2,group] = paste0(as.character(var_mean),
                                                            " ","(",as.character(var_sd),")")

        var_median = round(median(var_data,na.rm=T), digits = 2)
        quantile_vector = quantile(var_data,na.rm = T, type = quantile_type)
        q1 = quantile_vector[2]
        q3 = quantile_vector[4]
        final_survey_supertable[start_row+3,group] = paste0(as.character(var_median)," ",
                                                            "[",as.character(round(q1, digits=2)),
                                                            ", ",as.character(round(q3, digits=2)),"]")

        var_range = round(range(var_data,na.rm=T), digits = 2)
        range_lower = as.character(var_range[1])
        range_upper = as.character(var_range[2])
        final_survey_supertable[start_row+4,group] = paste0("(",range_lower,", ",range_upper,")")
      }
    }
  }

  final_survey_missing_table = final_survey_supertable[missing_rows,] %>%
    select(-Level)
  final_survey_supertable = final_survey_supertable[-final_survey_drop_rows,] %>%
    select(-Level)


  # Format excel file

  header_row = data.frame(Variable = c("Variable"),
                          Label = c("Label"),
                          Response = c("Response")) %>%
    add_column(Overall = "Overall", `Smart-T` = "Smart-T", `Quit Guide` = "Quit Guide")
  separator_row = data.frame(Variable = c("Final Survey"),
                             Label = c(NA),
                             Response = c(NA)) %>%
    add_column(Overall = NA, `Smart-T` = NA, `Quit Guide` = NA)

  super_table = rbind(baseline_supertable,separator_row,header_row,final_survey_supertable)
  # super_table = baseline_supertable
  try(addWorksheet(supertable,"Baseline and Final Survey"))
  writeData(supertable,"Baseline and Final Survey",super_table)

  missingtable = createWorkbook()
  missing_table = rbind(baseline_missing_table,separator_row,header_row,final_survey_missing_table)
  try(addWorksheet(missingtable,"Baseline and Final Survey"))
  writeData(missingtable,"Baseline and Final Survey",missing_table)

  # Merge cells
  if (test_supertable==T){
    supertable = createWorkbook()
    # try(addWorksheet(supertable,"Baseline"))
    # writeData(supertable,"Baseline",baseline_supertable)
    # try(addWorksheet(supertable,"Final Survey"))
    # writeData(supertable,"Final Survey",final_survey_supertable)
    # try(addWorksheet(supertable,"Baseline and Final Survey"))
    # writeData(supertable,"Baseline and Final Survey",base_follow_table)
    try(addWorksheet(supertable,"Baseline and Final Survey"))
    writeData(supertable,"Baseline and Final Survey",super_table)
  }
  ## Find all individual variables in column A, determine how many rows are
  ## connected to each, then merge those rows in both column A and B
  sheets = supertable$sheet_names

  header_style = createStyle(textDecoration = c("bold"),
                             border = "bottom",
                             halign = "center",
                             valign = "center",
                             fgFill = "#779EB2")
  text_style = createStyle(halign = "left",
                           valign = "center",
                           wrapText = T)
  number_style = createStyle(halign = "right",
                             valign = "center")
  observed_missing_style = createStyle(fgFill = "#D3D3D3")
  separator_style = createStyle(textDecoration = c("bold"),
                                fgFill = "#779EB2",
                                halign = "center",
                                valign = "center")

  observed_missing_rows = c()

  for(sheet_name in sheets) {
    df = readWorkbook(supertable,sheet = sheet_name)

    merge_row_idx = which(!is.na(df$Variable)) %>%
      as.data.frame  %>%
      + 1
    merge_row_idx[nrow(merge_row_idx)+1,] = nrow(df) + 1

    non_header_rows = seq(2,nrow(df))

    # use mergeCells(wb, sheet, cols, rows) https://rdrr.io/cran/openxlsx/man/mergeCells.html
    for (idx in 1:(nrow(merge_row_idx)-1)) {
      start_row = merge_row_idx[idx,]
      end_row = merge_row_idx[idx+1,]-1
      if (start_row==nrow(baseline_supertable)+2){ # the +2 is because +1 gets you the next row after the baseline super table and another +1 accounts for the header row.
        # Merge separator row
        mergeCells(supertable,
                   sheet = sheet_name,
                   cols = seq(1,6),
                   rows = start_row)
      } else if (start_row==end_row) {
        # skip if there are no rows to merge
      } else {
        mergeCells(supertable,
                   sheet = sheet_name,
                   cols = 1,
                   rows = start_row:end_row)
        mergeCells(supertable,
                   sheet = sheet_name,
                   cols = 2,
                   rows = start_row:end_row)
        observed_missing_rows = c(observed_missing_rows,c(start_row,start_row+1))
      }
    }
    setColWidths(supertable,
                 sheet = sheet_name,
                 cols = seq(1,6),
                 widths = c(10,23.5,18,17,17,17))
    setRowHeights(supertable,
                  sheet = sheet_name,
                  rows = 1,
                  heights = 16)
    setRowHeights(supertable,
                  sheet = sheet_name,
                  rows = non_header_rows,
                  heights = 40)
    addStyle(supertable,
             sheet = sheet_name,
             style = text_style,
             rows = non_header_rows,
             cols = seq(1,3),
             gridExpand = T,
             stack = T)
    addStyle(supertable,
             sheet = sheet_name,
             style = number_style,
             rows = non_header_rows,
             cols = seq(4,6),
             gridExpand = T,
             stack = T)
    addStyle(supertable,
             sheet = sheet_name,
             style = header_style,
             rows = c(1,nrow(baseline_supertable)+3),
             cols = seq(1,6),
             gridExpand = T,
             stack = T)
    addStyle(supertable,
             sheet = sheet_name,
             style = observed_missing_style,
             rows = observed_missing_rows,
             cols = seq(3,6),
             gridExpand = T,
             stack = T)
    # Add separator row style
    addStyle(supertable,
             sheet = sheet_name,
             style = separator_style,
             rows = nrow(baseline_supertable)+2,
             cols = 1,
             gridExpand = T,
             stack = T)
  }

  output_directory = r"(\\oup-share\OUP\OTRC\Research\Projects\Businelle\9860 ST3\DATA\Super_Table\Output)"

  if (test_supertable==T){
    file_name = paste0("Super_table_test_",format(Sys.time(),"%m%d%y_%H%M%S"),".xlsx")
  } else {
    file_name = paste0("Super_table_",format(Sys.time(),"%m%d%y_%H%M%S"),".xlsx")
  }

  saveWorkbook(supertable,
               file = file.path(output_directory,file_name),
               overwrite = TRUE)

  saveWorkbook(missingtable,
               file = file.path(output_directory,paste0("missing_table_",format(Sys.time(),"%m%d%y_%H%M%S"),".xlsx")),
               overwrite = TRUE)
}
