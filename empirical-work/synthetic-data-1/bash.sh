#!/bin/bash

folder="empirical-work/synthetic-data-1"

# echo "Step 1: Data simulation"
# Rscript src/01-tasks_data_simulation.R \
#           --task_config $folder/task_config.json \
#           --seed 2023 \
#           --output $folder/data/tasks_data.RDS
#         
# echo "Step 2: Training models"
# Rscript src/02-tasks_training.R \
#           --seed 2023 \
#           --tasks_data $folder/data/tasks_data.RDS \
#           --predictors all \
#           --response y \
#           --output $folder/data/models.RDS
# 
# echo "Step 3: Computing ALE curves"
# Rscript src/03-tasks_ale-curves.R \
#           --tasks_data $folder/data/tasks_data.RDS \
#           --models $folder/data/models.RDS \
#           --out_file $folder/data/ale_by_task_var.RDS
# 
# echo "Step 4: Multi-task similarity"
# Rscript src/04-meta_explain_similarity.R \
#         --ale_curves $folder/data/ale_by_task_var.RDS \
#         --models $folder/data/models.RDS \
#         --task_var_similarity $folder/data/similarity/similarity_task_var_summary.RDS \
#         --task_similarity $folder/data/similarity/similarity_task_summary.RDS
#         
#         
echo "Results: ALE plots"
Rscript src/plots/ale_plots.R \
        --ale_curves $folder/data/ale_by_task_var.RDS \
        --out_plot $folder/plots/ale_plots.png \
        --ymax 4.5 \
        --ymin=-4.5 \
        --ribbon 1\
        --width 25 \
        --height 20 