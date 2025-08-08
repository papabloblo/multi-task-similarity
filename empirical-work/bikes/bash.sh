#!/bin/bash

folder="empirical-work/bikes"

mkdir -p $folder/data/similarity
mkdir -p $folder/plots
mkdir -p $folder/latex-tables


echo "Step 2: Training models"
Rscript src/02-tasks_training.R \
          --seed 2023 \
          --tasks_data $folder/data/dataset_model.RDS \
          --predictors all \
          --response n \
          --id_task station_unlock \
          --output $folder/data/models.RDS

echo "Step 3: Computing ALE curves"
Rscript src/03-tasks_ale-curves.R \
          --tasks_data $folder/data/tasks_data.RDS \
          --id_task id_task \
          --response total_UPDRS \
          --n 10\
          --models $folder/data/models.RDS \
          --out_file $folder/data/ale_by_task_var.RDS

echo "Step 4: Multi-task similarity"
Rscript src/04-meta_explain_similarity.R \
        --ale_curves $folder/data/ale_by_task_var.RDS \
        --models $folder/data/models.RDS \
        --same_features TRUE\
        --similarity $folder/data/similarity/similarity.RDS \
        --task_var_similarity $folder/data/similarity/similarity_task_var_summary.RDS \
        --task_similarity $folder/data/similarity/similarity_task_summary.RDS


echo "Results: ALE plots"
Rscript src/plots/ale_plots.R \
        --ale_curves $folder/data/ale_by_task_var.RDS \
        --out_plot $folder/plots/ale_synthetic1.png \
        --ymax 4 \
        --ymin=-4 \
        --ribbon 2\
        --width 25 \
        --height 20\
        --free_x FALSE

echo "Results: Latex tables"
Rscript src/tables/similarity-table-latex.R \
         --task_var_similarity $folder/data/similarity/similarity_task_var_summary.RDS \
         --task_similarity $folder/data/similarity/similarity_task_summary.RDS \
         --out_table $folder/latex-tables/similarity.txt
