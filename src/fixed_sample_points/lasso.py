import pickle
from collections import defaultdict
from itertools import chain

import seaborn as sns
import numpy as np
import pandas as pd
from group_lasso import LogisticGroupLasso
from sklearn.compose import make_column_transformer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import brier_score_loss, make_scorer
from sklearn.model_selection import GroupKFold, cross_val_score, cross_val_predict, GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.model_selection import GridSearchCV
from sklearn.utils.fixes import loguniform

from fixed_sample_points.config import *
from util.aspect import angle_to_direction
from util.config import Config
import matplotlib.pyplot as plt

from util.grid import generate_gird


np.random.seed(42)

config = Config.default()
output_dir = os.path.join(os.path.dirname(__file__), "out")
debug_dir = os.path.join(output_dir, "debug")
n_bootstrap = 20
n_cv = 5
label_col = 'landslide'
group_col = 'grid_id'

results = list()


for site in config.sites:

    # Load data of site
    with open(os.path.join(output_dir, f'{site.name}_geo_df.pickle'), 'rb') as handle:
        gdf_points = pickle.load(handle)

    # Encode aspect
    gdf_points['aspect_factor'] = gdf_points['aspect'].apply(angle_to_direction)

    # TODO check why not cross val 100?
    # Bootstrap 20 times -> generate grid -> Then 5 way cross validate with grid.

    grid = generate_gird(1000, 1000, gdf_points.crs, *gdf_points.total_bounds)  # generate grid inside bounds
    grid = grid.loc[grid.intersects(gdf_points.unary_union)]  # filter grid cells that do not contain any point

    for boostrap_run_number in range(1, n_bootstrap + 1):
        print(f"{site.name} run {boostrap_run_number}/20")
        grid['grid_id'] = np.random.randint(0, n_cv, size=len(grid))

        # plot grid to debug
        grid.plot('grid_id', categorical=True, cmap='Spectral', legend=True)
        plt.savefig(os.path.join(debug_dir, f"{site.name}_{boostrap_run_number}_grid.png"))
        plt.close()

        gdf_points[group_col] = gdf_points \
            .sjoin(grid, how="left", predicate='intersects')['grid_id']

        data = gdf_points[[label_col] + [group_col] + features].dropna()

        X = data[features]
        y = data[label_col]

        n_groups = data['grid_id'].nunique()

        if n_groups < n_cv:
            print(f"Note: {n_groups} < {n_cv}")

        gkf = GroupKFold(n_splits=n_groups)
        y_hat_prob = np.zeros(y.shape, dtype=float)

        # Custom implementation of cross_val_predict, because we need to access number of one-hot-encoding categories to get group size
        for train_index, test_index in gkf.split(X, y, groups=data[group_col]):
            X_train = X.iloc[train_index]
            X_train, y_train = X.iloc[train_index].copy(), y.iloc[train_index].copy()
            X_test, y_test = X.iloc[test_index].copy(), y.iloc[test_index].copy()
            standard_scalers = {col: StandardScaler() for col in numeric_cols}
            # TODO Check dummy variables!
            one_hot_encoders = {col: OneHotEncoder(handle_unknown='ignore') for col in categorical_cols}

            for feature, ss in standard_scalers.items():
                X_train[feature] = ss.fit_transform(X_train[[feature]])
            for feature, ohe in one_hot_encoders.items():
                X_train_ohe_species = ohe.fit_transform(X_train[[feature]])
                X_train_species = pd.DataFrame(data=X_train_ohe_species.toarray(), index=X_train.index, columns=ohe.categories_[0])
                X_train = pd.concat([X_train.drop(columns=feature), X_train_species], axis=1)

            for feature, ss in standard_scalers.items():
                X_test[feature] = ss.transform(X_test[[feature]])
            for feature, ohe in one_hot_encoders.items():
                X_test_ohe_species = ohe.transform(X_test[[feature]])
                X_test_species = pd.DataFrame(data=X_test_ohe_species.toarray(), index=X_test.index, columns=ohe.categories_[0])
                X_test = pd.concat([X_test.drop(columns=feature), X_test_species], axis=1)

            group_sizes = [1] * len(standard_scalers) + list(len(ohe.categories_[0]) for ohe in one_hot_encoders.values())
            groups = np.concatenate([size * [i] for i, size in enumerate(group_sizes)])

            # TODO Set lambda parameter instead of default -> How?
            model = LogisticGroupLasso(
                groups=groups,
                group_reg=0.05,
                l1_reg=0, # only group regularization
                n_iter=1000, 
                supress_warning=True
            )

            print(np.logspace(1e-4, 1e-2, num=10))

            gs_model = GridSearchCV(
                model, 
                {'group_reg': np.logspace(1e-4, 1, num=10)},
                cv=10,
                # scoring=make_scorer(brier_score_loss, needs_proba=True)
            )
            gs_model.fit(X_train, y_train)

            print(gs_model.cv_results_)
            print(gs_model.best_params_)
            exit(1)
            
            model.fit(X_train, y_train)
            y_test_hat_prob = model.predict_proba(X_test)[:, 1]
            y_hat_prob[test_index] = y_test_hat_prob

        results.append(dict(
            site=site.name,
            bootstrap_run_number=boostrap_run_number,
            group=data[group_col],
            y_hat_prob=y_hat_prob,
            y=y,
        ))

        gdf_points = gdf_points.drop(columns=[group_col])

df_results = pd.DataFrame(results)

print(df_results)
df_results_flatten = pd.DataFrame({
    "site": np.repeat(df_results.site.values, df_results.y.str.len()),
    "bootstrap_run_number": np.repeat(df_results.bootstrap_run_number.values, df_results.y.str.len()),
    "group": list(chain.from_iterable(df_results.group)),
    "y_hat_prob": list(chain.from_iterable(df_results.y_hat_prob)),
    "y": list(chain.from_iterable(df_results.y)),
})

# Store data (serialize)
with open(os.path.join(output_dir, 'all_sites_all_features_results.pickle'), 'wb') as handle:
    pickle.dump(df_results_flatten, handle, protocol=pickle.HIGHEST_PROTOCOL)

print(df_results_flatten)
