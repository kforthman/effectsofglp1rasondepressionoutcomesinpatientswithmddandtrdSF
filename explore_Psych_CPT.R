# %% [markdown]
# # Setup

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %% [markdown]
# ## DTE data

# %%
load("dte_cohort_visits.rds")

dte_cohort_visits <- visit_data

dte_cohort_visits %>% head(10)

# %%
load("dte_cohort_psych_visits.rds")

dte_cohort_psych_visits <- psych_visit_data

dte_cohort_psych_visits %>% head(10)

# %%
n_t <- dte_cohort_psych_visits %>% nrow
n_1 <- sum(dte_cohort_psych_visits$visit_occurrence_id %in% dte_cohort_visits$visit_occurrence_id)
n_2 <- sum(!dte_cohort_psych_visits$visit_occurrence_id %in% dte_cohort_visits$visit_occurrence_id)

cat("Total psych visits in the visit table: \n")
paste0(comma(n_1), " / ", comma(n_t), "    (", percent(n_1/n_t),")") %>% cat

cat("\nTotal psych visits not in the visit table: \n")
paste0(comma(n_2), " / ", comma(n_t), "    (", percent(n_2/n_t),")") %>% cat

# %%
dte_cohort_psych_visits_2 <- dte_cohort_psych_visits %>% 
mutate(in_visit_table = visit_occurrence_id %in% dte_cohort_visits$visit_occurrence_id) %>%
left_join(dte_cohort_visits, by = join_by(person_id, visit_occurrence_id))

dte_cohort_psych_visits_2 %>% head(10)

# %%
dte_cohort_psych_visits_2 %>% dplyr::select(source_concept_code, standard_concept_name)

# %% [markdown]
# ## All data

# %%
load("Data_Prepped/Prepped_Data-All_Participants-All_Visits-Visit.rds", verbose = T)

all_visits <- data_prep

all_visits %>% head(10)

# %%
nrow(all_visits)

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Psychologic_or_Psychiatric_Visits_v2-Combined.rds", verbose = T)

all_psych_visits <- data_prep

all_psych_visits %>% head(10)

# %%
all_psych_visits$visit_occurrence_id %in% all_visits$visit_occurrence_id

# %%
n_t <- all_psych_visits %>% nrow
n_nunique <- unique(all_psych_visits$visit_occurrence_id) %>% length
n_tv <- all_visits %>% nrow
n_vnunique <- unique(all_visits$visit_occurrence_id) %>% length
n_1 <- sum(all_psych_visits$visit_occurrence_id %in% all_visits$visit_occurrence_id)
n_1unique <- sum(unique(all_psych_visits$visit_occurrence_id) %in% unique(all_visits$visit_occurrence_id))
n_2 <- sum(!all_psych_visits$visit_occurrence_id %in% all_visits$visit_occurrence_id)
n_3 <- sum(is.na(all_psych_visits$visit_occurrence_id))
n_4 <- sum(is.na(all_visits$visit_occurrence_id))

cat("Total psych visits in the visit table: \n")
paste0(comma(n_1), " / ", comma(n_t), "    (", percent(n_1/n_t),")") %>% cat

cat("\nTotal unique psych visits in the visit table: \n")
paste0(comma(n_1unique), " / ", comma(n_nunique), "    (", percent(n_1unique/n_nunique),")") %>% cat

cat("\nTotal psych visits not in the visit table: \n")
paste0(comma(n_2), " / ", comma(n_t), "    (", percent(n_2/n_t),")") %>% cat

cat("\nTotal na visits in the psych table: \n")
paste0(comma(n_3), " / ", comma(n_t), "    (", percent(n_3/n_t),")") %>% cat

cat("\nTotal na visits in the visit table: \n")
paste0(comma(n_4), " / ", comma(n_tv), "    (", percent(n_4/n_tv),")") %>% cat

# %%
all_psych_visits %>% pull(origin_table_type) %>% table

# %%
all_psych_visits_2 <- all_psych_visits %>%
mutate(in_visit_table = visit_occurrence_id %in% all_visits$visit_occurrence_id)

# %%
all_psych_visits_2 %>% filter(in_visit_table == FALSE) %>% arrange(visit_occurrence_id) %>% pull(visit_occurrence_id)

# %%
all_visits%>% arrange(visit_occurrence_id) %>% pull(visit_occurrence_id)

# %%
options(repr.plot.width=37, repr.plot.height=12)

# %%
ggplot(all_psych_visits_2, aes(as.numeric(visit_occurrence_id), fill = in_visit_table)) +
geom_histogram(bins = 12, alpha=0.6, position = 'identity')+
theme_minimal(base_size = 25)

# %%
ggplot(all_psych_visits_2, aes(nchar(visit_occurrence_id), fill = in_visit_table)) +
geom_histogram(bins = 12, alpha=0.6, position = 'identity')+
theme_minimal(base_size = 25)

# %%
ggplot(all_psych_visits_2, aes(year(start_datetime), fill = in_visit_table)) +
geom_histogram(bins = 12, alpha=0.6, position = 'identity')+
theme_minimal(base_size = 25)

# %%
ggplot(all_psych_visits_2, aes(visit_occurrence_concept_name, fill = in_visit_table)) +
geom_histogram(stat="count", bins = 12, alpha=0.6, position = 'identity')+
theme_minimal(base_size = 25)

# %%
all_psych_visits_2$visit_occurrence_concept_name %>% table(useNA = "always")

# %%
proc_tbl      <- str_glue("`procedure_occurrence`")
visit_tbl     <- str_glue("`visit_occurrence`")
visit_det_tbl <- str_glue("`visit_detail`")
concept_tbl   <- str_glue("`concept`")
cbcrit_tbl    <- str_glue("`cb_criteria`")

concept_ids <- c(
  2213518,2213519,2213520,2213521,2213522,2213523,2213524,2213525,2213526,2213527,
  2213528,2213529,2213530,2213531,2213532,2213533,2213534,2213535,2213536,2213537,
  2213538,2213539,2213540,2213542,2213543,2213544,2213545,2213546,2213547,2213548,
  2213549,2213550,2213551,2213552,2213554,2213555,2213556,2213557,2213558,2213559,
  2213560,2213561,40756837,40757109,42742381,43527904,43527905,43527983,43527984,
  43527985,43527986,43527987,43527988,43527989,43527990,43527991,43527992
)

psych_procs_cte <- str_glue("
  psych_procs AS (
    SELECT p.*
    FROM {proc_tbl} p
    WHERE p.procedure_source_concept_id IN (
      SELECT DISTINCT c.concept_id
      FROM {cbcrit_tbl} c
      JOIN (
        SELECT CAST(cr.id AS STRING) AS id
        FROM {cbcrit_tbl} cr
        WHERE concept_id IN ({paste(concept_ids, collapse = ', ')})
          AND full_text LIKE '%_rank1]%'
      ) a
      ON (c.path LIKE CONCAT('%.', a.id, '.%')
       OR c.path LIKE CONCAT('%.', a.id)
       OR c.path LIKE CONCAT(a.id, '.%')
       OR c.path = a.id)
      WHERE c.is_standard = 0 AND c.is_selectable = 1
    )
  )
")

# %% [markdown]
# ### Headline reconciliation (where does the 57% come from?)

# %%
sql1 <- str_glue("
WITH
{psych_procs_cte},
joined AS (
  SELECT
    p.*,
    v.visit_occurrence_id AS v_id,
    v.person_id           AS v_person_id,
    v.visit_start_datetime,
    v.visit_end_datetime
  FROM psych_procs p
  LEFT JOIN {visit_tbl} v
    ON p.visit_occurrence_id = v.visit_occurrence_id
)
SELECT
  COUNT(*) AS n_total,
  COUNTIF(p.visit_occurrence_id IS NULL) AS n_visitid_null,
  SAFE_DIVIDE(COUNTIF(p.visit_occurrence_id IS NULL), COUNT(*)) AS pct_visitid_null,
  COUNTIF(p.visit_occurrence_id IS NOT NULL AND v_id IS NULL) AS n_visitid_present_but_missing_in_visits,
  SAFE_DIVIDE(COUNTIF(p.visit_occurrence_id IS NOT NULL AND v_id IS NULL), COUNT(*)) AS pct_visitid_present_but_missing_in_visits,
  COUNTIF(v_id IS NOT NULL AND p.person_id != v_person_id) AS n_visitid_cross_person,
  SAFE_DIVIDE(COUNTIF(v_id IS NOT NULL AND p.person_id != v_person_id), COUNT(*)) AS pct_visitid_cross_person,
  COUNTIF(v_id IS NOT NULL AND p.person_id = v_person_id) AS n_clean_match,
  SAFE_DIVIDE(COUNTIF(v_id IS NOT NULL AND p.person_id = v_person_id), COUNT(*)) AS pct_clean_match
FROM joined p
")
headline <- grab_data(sql1)
headline

# %%
visit_ids <- grab_data("
SELECT 
    CAST(visit_occurrence.visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    visit_occurrence.person_id
FROM
    `visit_occurrence`
LEFT JOIN
    `visit_occurrence_ext` v_ext
    ON visit_occurrence.visit_occurrence_id = v_ext.visit_occurrence_id
WHERE LOWER(v_ext.src_id) LIKE 'ehr site%'
")

visit_ids %>% head

# %%
visit_ids <- grab_data("
SELECT 
    CAST(visit_occurrence.visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    visit_occurrence.visit_occurrence_id,
    visit_occurrence.person_id,
    visit_occurrence.visit_start_datetime
FROM
    `visit_occurrence`
LEFT JOIN
    `visit_occurrence_ext` v_ext
    ON visit_occurrence.visit_occurrence_id = v_ext.visit_occurrence_id
WHERE LOWER(v_ext.src_id) LIKE 'ehr site%'
")

visit_ids %>% head

# %%
visit_ids %>% filter(!is.na(visit_occurrence_id)) %>% head

# %%
visit_ids %>% filter(is.na(visit_start_datetime)) %>% head

# %%
po_visit_ids <- grab_data("
SELECT 
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    visit_occurrence_id,
    person_id
FROM
    `procedure_occurrence`
")

po_visit_ids %>% head

# %%
n1 <- sum(po_visit_ids$visit_occurrence_id_str %in% visit_ids$visit_occurrence_id_str)
n2 <- nrow(po_visit_ids)

cat("Total psych visits in the visit table: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
n1 <- sum(is.na(po_visit_ids$visit_occurrence_id_str))
n2 <- nrow(po_visit_ids)

cat("Total psych visits that are na: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
po_visit_ids_2 <- grab_data("
SELECT 
    CAST(po.visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    po.visit_occurrence_id,
    po.person_id,
    v.visit_start_datetime
FROM
    `procedure_occurrence` po
LEFT JOIN
    `visit_occurrence` v 
    ON po.visit_occurrence_id = v.visit_occurrence_id 
")

po_visit_ids_2 %>% head

# %%
n1 <- sum(is.na(po_visit_ids_2$visit_start_datetime))
n2 <- nrow(po_visit_ids_2)

cat("Total psych visits that have no visit date: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
n1 <- po_visit_ids_2 %>% filter(is.na(visit_start_datetime) & is.na(visit_occurrence_id_str)) %>% nrow
n2 <- nrow(po_visit_ids_2)

cat("Total psych visits that have no visit date and no visit id: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
n1 <- po_visit_ids_2 %>% filter(is.na(visit_start_datetime) & !is.na(visit_occurrence_id_str)) %>% nrow
n2 <- nrow(po_visit_ids_2)

cat("Total psych visits that have no visit date but do have a visit id: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
n1 <- po_visit_ids_2 %>% filter(!is.na(visit_start_datetime) & is.na(visit_occurrence_id_str)) %>% nrow
n2 <- nrow(po_visit_ids_2)

cat("Total psych visits that have a visit date but not a visit id: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
n1 <- po_visit_ids_2 %>% filter(is.na(visit_start_datetime) & (visit_occurrence_id_str %in% visit_ids$visit_occurrence_id_str)) %>% nrow
n2 <- nrow(po_visit_ids_2)

cat("Total psych visits that have a match in the visit table but no start date: \n")
paste0(comma(n1), " / ", comma(n2), "    (", percent(n1/n2),")") %>% cat

# %%
po_visit_ids_2 %>% filter(is.na(visit_start_datetime) & !is.na(visit_occurrence_id_str)) %>% head(10)

# %%
# procedure visit ids with no match in the visit table
ids_case_1 <- po_visit_ids_2 %>% filter(is.na(visit_start_datetime) & !is.na(visit_occurrence_id_str)) %>% pull(person_id)
ids_case_1 %>% length %>% comma
# procedure visits with no visit id
ids_case_2 <- po_visit_ids_2 %>% filter(is.na(visit_occurrence_id_str)) %>% pull(person_id)
ids_case_2 %>% length %>% comma
# procedure visits with a match in the visit table
ids_case_3 <-  po_visit_ids_2 %>% filter(!is.na(visit_start_datetime) & !is.na(visit_occurrence_id_str)) %>% pull(person_id)
ids_case_3 %>% length %>% comma

# %%
