# consec_cluster

An R Shiny app (`app.R`) for processing actigraphy (activity) data (.csv input). The app allows for conditional threshold setting for clustering up to 10 observations over time.

The output is a plot to guide manual threshold setting and downloadable tabular data (.csv output)

The file `ex_data.rdata` contains default data to run or troubleshoot the app in lieu of actigraphy data.

Columns in the output table:

| Column         | Description                                                                                   |
|---------------|------------------------------------------------------|
| `time`         | the x value (e.g., time) for a cluster component.                                             |
| `name`         | the column names from the input file (.csv).                                                  |
| `value`        | the y value for a cluster component.                                                          |
| `name_id`      | a number given to each name so the user can specify custom thresholds.                        |
| `max`          | the maximum y value for `name`.                                                               |
| `threshold`    | a proportion of `max` used to filter `value` (default is 0.1).                                |
| `auc`          | total area under the curve calculated with trapezoidal rule using `DescTools::AUC()`.         |
| `clust_id`     | an arbitrary number indicating `time` and `value` cluster components when `clust_id` matches. |
| `n`            | the size of the cluster.                                                                      |
| `clust_time`   | the time of the cluster is 5\*`n` for 5-minute bins.                                          |
| `total_time`   | the total time summed for `name.`                                                             |
| `largest_bout` | the `clust_time` of the cluster with the largest `n`.                                         |
| `total_bouts`  | the number of individual clusters.                                                            |

The on-screen summary table displays `max`, `threshold`, `auc`, `total_time`, `largest_bout`, and `total_bouts` grouped by `name`. Total `auc` as an estimate of overall activity does not change with threshold setting.
