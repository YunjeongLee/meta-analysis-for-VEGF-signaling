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
    "VariableNamingRule", "preserve", "Sheet", "VEGFA165_VEGFR2", "Range", "A1:D8", "TreatAsMissing", "NaN");
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

%% Draw figures for geometric parameters
figure('pos', [10 10 1200 500]);
sgtitle('Geometric parameters', 'fontsize', 20)

subplot(1, 2, 1);
set(gca, 'fontsize', 13);
title("Adipocyte size (µm)", " ")
hold on;
lean1 = plot(1, table_adip_size{"Morange2000", "Lean average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean2 = plot(1, table_adip_size{"Lijnen2001", "Lean average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean3 = plot(1, table_adip_size{"Voros2005", "Lean average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean4 = plot(1, table_adip_size{"Lijnen2006", "Lean average"}, 'kdiamond', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean5 = plot(1, adip_lean.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
obese1 = plot(2, table_adip_size{"Morange2000", "Obese average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese2 = plot(2, table_adip_size{"Lijnen2001", "Obese average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese3 = plot(2, table_adip_size{"Maquoi2002", "Obese average"}, 'kpentagram', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese4 = plot(2, table_adip_size{"Voros2005", "Obese average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese5 = plot(2, table_adip_size{"Lijnen2006", "Obese average"}, 'kdiamond', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese6 = plot(2, adip_obese.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
text(2, adip_obese.mean, ' *', 'color', 'r', 'fontsize', 20)
hold off;
xlim([0, 3])
ylim([0, 100])
xticks(0:3)
xticklabels({"", "Lean", "Obese", ""})
lgd1 = legend([lean1, lean2, lean3, lean4, lean5], ...
    {"Morange, 2000"; "Lijnen, 2001"; "Voros, 2005"; "Lijnen, 2006"; "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Lean mouse")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 13);
lgd2 = legend(a, [obese1, obese2, obese3, obese4, obese5, obese6], ...
    {"Morange, 2000"; "Lijnen, 2001"; "Maquoi, 2002"; "Voros, 2005"; "Lijnen, 2006"; "Weighted average"}, ...
    'Location', 'Eastoutside');
title(lgd2, "Obese mouse")

subplot(1, 2, 2);
title("Capillary basement membrane thickness (nm)", " ")
set(gca, 'fontsize', 13);
hold on;
lean1 = plot(1, table_cbm{"Creutzfeldt", "Lean average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean2 = plot(1, table_cbm{"Rodrigues1983", "Lean average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean3 = plot(1, table_cbm{"Lash1989", "Lean average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean4 = plot(1, table_cbm{"Lash1989_1", "Lean average"}, 'kdiamond', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean5 = plot(1, table_cbm{"Danis1993", "Lean average"}, 'kpentagram', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean6 = plot(1, table_cbm{"Calson2003", "Lean average"}, 'kx', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean7 = plot(1, table_cbm{"Calson2003_1", "Lean average"}, 'kv', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean8 = plot(1, table_cbm{"Ceafalan2019", "Lean average"}, 'khexagram', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
lean9 = plot(1, cbm_lean.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
obese1 = plot(2, table_cbm{"Lash1989", "Obese average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese2 = plot(2, table_cbm{"Lash1989_1", "Obese average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese3 = plot(2, table_cbm{"Danis1993", "Obese average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
obese4 = plot(2, cbm_obese.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
hold off;
xlim([0, 3])
ylim([30, 130])
xticks(0:3)
xticklabels({"", "Lean", "Obese", ""})
lgd1 = legend([lean1, lean2, lean3, lean4, lean5, lean6, lean7, lean8, lean9], ...
    {"Creutzfeldt, 1970"; "Rodrigues, 1983"; "Lash, 1989 (11 wk.)"; "Lash, 1989 (18 wk.)"; "Danis, 1993"; ...
    "Calson, 2003 (retina)"; "Calson, 2003 (muscle)"; "Ceafalan2019"; "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Lean mouse")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 13);
lgd2 = legend(a, [obese1, obese2, obese3, obese4], ...
    {"Lash, 1989 (11 wk.)"; "Lash, 1989 (18 wk.)"; "Danis, 1993"; "Weighted average"}, ...
    'Location', 'eastoutside');
title(lgd2, "Obese mouse")

% Save figure
foldername = '../results/figures';
mkdir(foldername)
saveas(gca, sprintf('%s/geometric_parameters.png', foldername), 'png')

%% Draw figures for binding affinities
figure('pos', [10 10 2000 500]);
sgtitle('Binding affinities of VEGF to its receptors', 'fontsize', 20)

subplot(1, 3, 1);
title("Binding affinity of VEGF to VEGFR1 (pM)", " ")
set(gca, 'fontsize', 13);
hold on;
radio1 = plot(1, table_vegfr1{"Waltenberger1994", "Kd average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio2 = plot(1, table_vegfr1{"Waltenberger1994_1", "Kd average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio3 = plot(1, vegfr1_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
spr1 = plot(2, table_vegfr1{"Tiedemann2002", "Kd average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr2 = plot(2, table_vegfr1{"Mamer2020", "Kd average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr3 = plot(2, vegfr1_spr.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
hold off;
xlim([0, 3])
ylim([0, 25])
xticks(0:3)
xticklabels({"", "Radioligand", "SPR", ""})
lgd1 = legend([radio1, radio2, radio3], ...
    {"Waltenberger, 1994"+ newline + "(PAE)"; "Waltenberger, 1994"+ newline + "(HUVEC)"; "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Radioligand")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 13);
lgd2 = legend(a, [spr1, spr2, spr3], ...
    {"Tiedemann, 2002"; "Mamer, 2020"; "Weighted average"}, ...
    'Location', 'Eastoutside');
title(lgd2, "SPR")

subplot(1, 3, 2);
title("Binding affinity of VEGF to VEGFR2 (pM)", " ")
set(gca, 'fontsize', 13);
hold on;
radio1 = plot(1, table_vegfr2{"Waltenberger1994", "Kd average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio2 = plot(1, table_vegfr2{"Waltenberger1994_1", "Kd average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio3 = plot(1, table_vegfr2{"Whitaker2001", "Kd average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio4 = plot(1, vegfr2_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
spr1 = plot(2, table_vegfr2{"Huang1998", "Kd average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr2 = plot(2, table_vegfr2{"Cunningham1999", "Kd average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr3 = plot(2, table_vegfr2{"Cunningham1999_1", "Kd average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr4 = plot(2, table_vegfr2{"Mamer2020", "Kd average"}, 'kpentagram', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr5 = plot(2, vegfr2_spr.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
text(2, vegfr2_spr.mean, ' *', 'color', 'r', 'fontsize', 20)
hold off;
xlim([0, 3])
ylim([0, 1000])
xticks(0:3)
xticklabels({"", "Radioligand", "SPR", ""})
lgd1 = legend([radio1, radio2, radio3, radio4], ...
    {"Waltenberger, 1994" + newline + "(PAE)"; "Waltenberger, 1994" + newline + "(HUVEC)"; "Whitaker, 2001"; "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Radioligand")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 13);
lgd2 = legend(a, [spr1, spr2, spr3, spr4, spr5], ...
    {"Huang, 1998"; "Cunningham, 1999" + newline + "(pre-dimer)"; "Cunningham, 1999" + newline + "(monomer)"; "Mamer, 2020"; "Weighted average"}, ...
    'Location', 'eastoutside');
title(lgd2, "SPR")

subplot(1, 3, 3);
title("Binding affinity of VEGF to NRP1 (nM)", " ")
set(gca, 'fontsize', 13);
hold on;
radio1 = plot(1, table_nrp1{"Soker1996", "Kd average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio2 = plot(1, table_nrp1{"Soker1996_1", "Kd average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio3 = plot(1, table_nrp1{"Soker1998", "Kd average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio4 = plot(1, table_nrp1{"Whitaker2001", "Kd average"}, 'kpentagram', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
radio5 = plot(1, nrp1_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
spr1 = plot(2, table_nrp1{"Fuh2000", "Kd average"}, 'ko', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr2 = plot(2, table_nrp1{"Teran2019", "Kd average"}, 'ksquare', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr3 = plot(2, table_nrp1{"Teran2019", "Kd average"}, 'k^', 'Markersize', 7, 'MarkerFaceColor', 'none', 'linewidth', 1.5);
spr4 = plot(2, nrp1_spr.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 7, 'MarkerEdgeColor', 'r', 'linewidth', 1.5);
text(2, nrp1_spr.mean, ' *', 'color', 'r', 'fontsize', 20)
hold off;
xlim([0, 3])
ylim([0, 150])
xticks(0:3)
xticklabels({"", "Radioligand", "SPR", ""})
lgd1 = legend([radio1, radio2, radio3, radio4, radio5], ...
    {"Soker, 1996" + newline + "(HUVEC)"; "Soker, 1996" + newline + "(231 cell)"; "Soker, 1998"; "Whitaker, 2001"; "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Radioligand")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 13);
lgd2 = legend(a, [spr1, spr2, spr3, spr4], ...
    {"Fuh, 2000"; "Teran, 2019" + newline + "(chimera)"; "Teran, 2019" + newline + "(monomer)"; "Weighted average"}, ...
    'Location', 'eastoutside');
title(lgd2, "SPR")

% Save figure
foldername = '../results/figures';
mkdir(foldername)
saveas(gca, sprintf('%s/binding_affinity.png', foldername), 'png')

