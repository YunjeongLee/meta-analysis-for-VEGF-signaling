clear; close all; clc;

%% Load data
filename = '../data/parameters.xlsx';
table_adip_size = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "Adipocyte diameter", "Range", "A1:E6", "TreatAsMissing", "NaN");
table_cbm = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "CBM thickness", "Range", "A1:E9", "TreatAsMissing", "NaN");
table_vegfr1 = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "VEGFA165_VEGFR1", "Range", "A1:D5", "TreatAsMissing", "NaN");
table_vegfr2 = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "VEGFA165_VEGFR2", "Range", "A1:D9", "TreatAsMissing", "NaN");
table_nrp1 = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "VEGFA165_NRP1", "Range", "A1:D8", "TreatAsMissing", "NaN");

%% Compute weighted mean and weighted error for geometric parameters
% Adipocyte size for lean and obese mouse
adip_lean = compute_weighted_stats(table_adip_size{:, "Lean average"}, table_adip_size{:, "Lean SE"});
adip_obese = compute_weighted_stats(table_adip_size{:, "Obese average"}, table_adip_size{:, "Obese SE"});

% CBM thickness of lean and obese mouse
cbm_lean = compute_weighted_stats(table_cbm{:, "Lean average"}, table_cbm{:, "Lean SD"});
cbm_obese = compute_weighted_stats(table_cbm{:, "Obese average"}, table_cbm{:, "Obese SD"});

%% Compute weighted mean and weighted error for binding affinities
% VEGFA-165:VEGFR1 SPR vs radioligand
vegfr1_spr = compute_weighted_stats(table_vegfr1{table_vegfr1.Method == "SPR", "Kd average"}, ...
    table_vegfr1{table_vegfr1.Method == "SPR", "Kd SE"});
vegfr1_radio = compute_weighted_stats(table_vegfr1{table_vegfr1.Method == "Radioligand", "Kd average"}, ...
    ones(size(table_vegfr1{table_vegfr1.Method == "Radioligand", "Kd average"}, 1), 1));

% VEGFA-165:VEGFR2 SPR vs radioligand
vegfr2_spr = compute_weighted_stats(table_vegfr2{table_vegfr2.Method == "SPR", "Kd average"}, ...
    table_vegfr2{table_vegfr2.Method == "SPR", "Kd SE"});
vegfr2_radio = compute_weighted_stats(table_vegfr2{table_vegfr2.Method == "Radioligand", "Kd average"}, ...
    ones(size(table_vegfr2{table_vegfr2.Method == "Radioligand", "Kd average"}, 1), 1));

% VEGFA-165:NRP1 SPR vs radioligand
nrp1_spr = compute_weighted_stats(table_nrp1{table_nrp1.Method == "SPR", "Kd average"}, ...
    table_nrp1{table_nrp1.Method == "SPR", "Kd SE"});
nrp1_radio = compute_weighted_stats(table_nrp1{table_nrp1.Method == "Radioligand", "Kd average"}, ...
    ones(size(table_nrp1{table_nrp1.Method == "Radioligand", "Kd average"}, 1), 1));

%% Perform Student's t-test
alpha = 0.05;

% Adipocyte size
[t_adip, pval_adip, reject_adip] = students_t_test(adip_obese, adip_lean, alpha, "one-side");

% CBM thickness
[t_cbm, pval_cbm, reject_cbm] = students_t_test(cbm_obese, cbm_lean, alpha, "one-side");

% VEGFA-165:VEGFR1
[t_vegfr1, pval_vegfr1, reject_vegfr1] = students_t_test(vegfr1_radio, vegfr1_spr, alpha, "two-side");

% VEGFA-165:VEGFR2
[t_vegfr2, pval_vegfr2, reject_vegfr2] = students_t_test(vegfr2_radio, vegfr2_spr, alpha, "two-side");

% VEGFA-165:NRP1
[t_nrp1, pval_nrp1, reject_nrp1] = students_t_test(nrp1_spr, nrp1_radio, alpha, "two-side");

%% Plot Kd values into one figure
fig = figure('pos', [10 10 1200 900]);
set(fig, 'DefaultAxesColorOrder', [0 0 0; 0 0 0])

% Default settings
set(gca, 'fontsize', 25, 'yscale', 'log');
xlim([0 4])
ylim([1e-1, 1e6])
xticks(0:4)
xticklabels(["", "VEGFR1", "VEGFR2", "NRP1"]);
ylabel("Equilibrium dissociation constant (K_d, pM)")

% Define colormaps
colormap default
radio_cmap = summer(11);
spr_cmap = autumn(11);

% VEGF-A:VEGFR1
hold on;
waltenberger1 = plot(1, table_vegfr1{"Waltenberger1994", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(2, :), 'MarkerFaceColor', radio_cmap(2, :));
waltenberger2 = plot(1, table_vegfr1{"Waltenberger1994_1", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(3, :), 'MarkerFaceColor', radio_cmap(3, :));
radio_mean = plot(1, vegfr1_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [49, 87, 44]/255, 'linewidth', 7);
tiedemann = plot(1, table_vegfr1{"Tiedemann2002", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(2, :), 'MarkerFaceColor', spr_cmap(2, :));
mamer = plot(1, table_vegfr1{"Mamer2020", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(3, :), 'MarkerFaceColor', spr_cmap(3, :));
spr_mean = plot(1, vegfr1_spr.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);
text(1, vegfr1_spr.mean, sprintf('   p=%.3f', pval_vegfr1), 'color', 'k', 'fontsize', 25)

% VEGF-A:VEGFR2
plot(2, table_vegfr2{"Waltenberger1994", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(2, :), 'MarkerFaceColor', radio_cmap(2, :));
plot(2, table_vegfr2{"Waltenberger1994_1", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(3, :), 'MarkerFaceColor', radio_cmap(3, :));
whitaker = plot(2, table_vegfr2{"Whitaker2001", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(4, :), 'MarkerFaceColor', radio_cmap(4, :));
plot(2, vegfr2_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [49, 87, 44]/255, 'linewidth', 7);
huang = plot(2, table_vegfr2{"Huang1998", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(4, :), 'MarkerFaceColor', spr_cmap(4, :));
cunningham1 = plot(2, table_vegfr2{"Cunningham1999", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(5, :), 'MarkerFaceColor', spr_cmap(5, :));
cunningham2 = plot(2, table_vegfr2{"Cunningham1999_1", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(6, :), 'MarkerFaceColor', spr_cmap(6, :));
plot(2, table_vegfr2{"Mamer2020", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(3, :), 'MarkerFaceColor', spr_cmap(3, :));
shobhan = plot(2, table_vegfr2{"Shobhan2023", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(7, :), 'MarkerFaceColor', spr_cmap(7, :));
plot(2, vegfr2_spr.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);
text(2, vegfr2_spr.mean, ' *', 'color', 'k', 'fontsize', 25)
text(2, vegfr2_spr.mean, sprintf('   p=%.3f', pval_vegfr2), 'color', 'k', 'fontsize', 25)

% VEGF-A:NRP1
soker1996_1 = plot(3, table_nrp1{"Soker1996", "Kd average"}*1e3, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(5, :), 'MarkerFaceColor', radio_cmap(5, :));
soker1996_2 = plot(3, table_nrp1{"Soker1996_1", "Kd average"}*1e3, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(6, :), 'MarkerFaceColor', radio_cmap(6, :));
soker1998 = plot(3, table_nrp1{"Soker1998", "Kd average"}*1e3, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(7, :), 'MarkerFaceColor', radio_cmap(7, :));
plot(3, table_nrp1{"Whitaker2001", "Kd average"}*1e3, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(4, :), 'MarkerFaceColor', radio_cmap(4, :));
plot(3, nrp1_radio.mean*1e3, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [49, 87, 44]/255, 'linewidth', 7);
fuh = plot(3, table_nrp1{"Fuh2000", "Kd average"}*1e3, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(8, :), 'MarkerFaceColor', spr_cmap(8, :));
teran1 = plot(3, table_nrp1{"Teran2019", "Kd average"}*1e3, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(9, :), 'MarkerFaceColor', spr_cmap(9, :));
teran2 = plot(3, table_nrp1{"Teran2019", "Kd average"}*1e3, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(10, :), 'MarkerFaceColor', spr_cmap(10, :));
plot(3, nrp1_spr.mean*1e3, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);
text(3, nrp1_spr.mean*1e3, ' *', 'color', 'k', 'fontsize', 25)
text(3, nrp1_spr.mean*1e3, sprintf('   p=%.3f', pval_nrp1), 'color', 'k', 'fontsize', 25)

hold off;
lgd1 = legend([waltenberger1, waltenberger2, whitaker, soker1996_1, soker1996_2, soker1998, radio_mean], ...
    {"Waltenberger, 1998"+newline+"(PAE)", "Waltenberger, 1994"+newline+"(HUVEC)", "Whitaker, 2001", ...
    "Soker, 1996"+newline+"(HUVEC)", "Soker, 1996"+newline+"(231 cell)", "Soker, 1998", "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Cell-based"+newline+"(Radioligand)");


foldername = '../results/figures';
mkdir(foldername)
