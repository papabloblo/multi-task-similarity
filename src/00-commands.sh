#!/bin/bash

folder="empirical-work/synthetic-data-1"

Rscript src/01-tasks_data_simulation.R \
          --task_config $folder/task_config.json \
          --seed 2023 \
          --output $folder/data/tasks_data.RDS

Rscript src/02-tasks_training.R \
          --seed 2023 \
          --tasks_data data/tasks_data.RDS \
          --predictors all \
          --response y \
          --output data/models.RDS

Rscript src/03-tasks_ale-curves.R \
          --tasks_data data/tasks_data.RDS \
          --models data/models.RDS \
          --out_file data/ale_by_task_var.RDS

Rscript src/03-meta_explain_similarity.R \
        --ale_curves data/ale_by_task_var.RDS \
        --models data/models.RDS \
        --task_var_similarity data/similarity/similarity_task_var_summary.RDS \
        --task_similarity data/similarity/similarity_task_summary.RDS