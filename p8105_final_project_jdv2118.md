p8105_nr2774
================
Justin Vargas
2022-11-28

### Problem 1

``` r
payroll_df = 
    read_csv("data/Citywide_Payroll_Data__Fiscal_Year_.csv") |> 
    clean_names() |>
    filter(fiscal_year == "2022") |>
    select(-payroll_number, -agency_start_date) |> 
    mutate(pay_basis = recode(pay_basis, "per Annum" = "Annually"),
           pay_basis = recode(pay_basis, "per Day" = "Daily"),
           pay_basis = recode(pay_basis, "per Hour" = "Hourly"),
           leave_status = leave_status_as_of_june_30,
           total_pay = regular_gross_paid + total_ot_paid + total_other_pay) |>
    filter(work_location_borough != "NA",work_location_borough != "OTHER") |> select(-leave_status_as_of_june_30)
```

    ## Rows: 5109775 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): Agency Name, Last Name, First Name, Mid Init, Agency Start Date, Wo...
    ## dbl (2): Fiscal Year, Payroll Number
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Need to create a new variable for borough and possibly look into removing decimal places for salary, and check NAs in first and last name
```

``` r
median_total_df =
  payroll_df %>%
  group_by(work_location_borough) %>%
  summarize(
      median_total_pay = median(total_pay, na.rm = TRUE)) %>%
        ggplot(aes(x = work_location_borough, y = median_total_pay, fill = work_location_borough)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "County Names",
    y = "Median Total Pay",
    title = "The Median Total Pay By County"
  )

median_base_df =
  payroll_df %>%
  group_by(work_location_borough) %>%
  summarize(
      median_base_salary = median(base_salary, na.rm = TRUE)) %>%
        ggplot(aes(x = work_location_borough, y = median_base_salary , fill = work_location_borough)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "County Names",
    y = "Median Base Salary",
    title = "The Median Base Salary By County"
  )

mean_total_df =
  payroll_df %>%
  group_by(work_location_borough) %>%
  summarize(
      mean_total_pay = mean(total_pay, na.rm = TRUE)) %>%
        ggplot(aes(x = work_location_borough, y = mean_total_pay, fill = work_location_borough)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "County Names",
    y = "Mean Total Pay",
    title = "The Mean Total Pay By County"
  )

total_ls = table(payroll_df$leave_status)
total_ls
```

    ## 
    ##              ACTIVE              CEASED            ON LEAVE ON SEPARATION LEAVE 
    ##              481788              102364               10093                1617 
    ##            SEASONAL 
    ##                5233

``` r
labels = c("Active", "Ceased", "On Leave", "On Separation Leave", "Seasonal")

piepercent = round(100 * total_ls / sum(total_ls), 1)

pie(total_ls, labels = paste(labels, sep = " ", piepercent, "%"),
    main = "Leave Status Pie Chart", col = rainbow(length(total_ls)))
legend("topright", c("Active", "Ceased", "On Leave", "On Separation Leave", "Seasonal"),cex = 1, fill = rainbow(length(total_ls)))
```

<img src="p8105_final_project_jdv2118_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
library(plotrix)

pie3D(total_ls, labels = piepercent,
    main = "Leave Status Pie Chart", col = rainbow(length(total_ls)))
legend("topright", c("Active", "Ceased", "On Leave", "On Separation Leave", "Seasonal"),cex = 1, fill = rainbow(length(total_ls)))
```

<img src="p8105_final_project_jdv2118_files/figure-gfm/unnamed-chunk-2-2.png" width="90%" />

``` r
Ls_boro = payroll_df %>%
  group_by(work_location_borough, leave_status) %>%
  summarise(
    count = n())
```

    ## `summarise()` has grouped output by 'work_location_borough'. You can override
    ## using the `.groups` argument.

``` r
Ls_boro
```

    ## # A tibble: 57 × 3
    ## # Groups:   work_location_borough [17]
    ##    work_location_borough leave_status        count
    ##    <chr>                 <chr>               <int>
    ##  1 ALBANY                ACTIVE                  9
    ##  2 ALBANY                CEASED                  4
    ##  3 ALBANY                ON SEPARATION LEAVE     1
    ##  4 BRONX                 ACTIVE              22504
    ##  5 BRONX                 CEASED               3617
    ##  6 BRONX                 ON LEAVE              590
    ##  7 BRONX                 ON SEPARATION LEAVE   109
    ##  8 BRONX                 SEASONAL              787
    ##  9 BROOKLYN              ACTIVE              40755
    ## 10 BROOKLYN              CEASED               7994
    ## # … with 47 more rows

``` r
ggplot(Ls_boro, aes(x = leave_status, y = count, fill = work_location_borough)) + geom_bar(position = "dodge", stat = "identity") + labs(x = "Leave Status", y = "Count")
```

<img src="p8105_final_project_jdv2118_files/figure-gfm/unnamed-chunk-2-3.png" width="90%" />

``` r
  # visualizations Number of employees by county separated by leave status
  # Number of agencies separated by county scatterplot Nerugi
```
