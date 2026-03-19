# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("participant_survey_answers_collapsed_prep.RData", verbose = T)
participant_survey_answers_collapsed_prep %>% colnames

# %%
df <- participant_survey_answers_collapsed_prep

# %% [markdown]
# ### Income

# %%
df %>%
count(Income..Annual.Income.factor) %>%
mutate(pct = percent(n/sum(n), accuracy = 0.1), n = comma(n))

# %% [markdown]
# ### Education Level

# %%
df %>%
count(Education.Level) %>%
mutate(pct = percent(n/sum(n), accuracy = 0.1), n = comma(n))

# %% [markdown]
# ### Education Level, 2-Level Factor

# %%
df %>%
count(Education.Level.bool) %>%
mutate(pct = percent(n/sum(n), accuracy = 0.1), n = comma(n))

# %% [markdown]
# ### Employment Status

# %%
df %>%
count(employment_status_trump) %>%
mutate(pct = percent(n/sum(n), accuracy = 0.1), n = comma(n))

# %% [markdown]
# ### Employment Status, 2-Level Factor

# %%
df %>%
count(employed) %>%
mutate(pct = percent(n/sum(n), accuracy = 0.1), n = comma(n))
