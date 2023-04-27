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

%% Plot geometric parameters
figure('pos', [10 10 1800 600]);

subplot(1, 2, 1);

% Define colormaps
colormap default
lean_cmap = winter(4);
obese_cmap = autumn(5);

% Default settings
set(gca, 'fontsize', 25);
ylabel("Adipocyte size (µm)")
xlim([0, 3])
ylim([0, 100])
xticks(0:3)
xticklabels({"", "Lean", "Obese", ""});

% Plot
hold on;
lean1 = plot(1, table_adip_size{"Morange2000", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(1, :), 'MarkerFaceColor', lean_cmap(1, :));
lean2 = plot(1, table_adip_size{"Lijnen2001", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(2, :), 'MarkerFaceColor', lean_cmap(2, :));
lean3 = plot(1, table_adip_size{"Voros2005", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(3, :), 'MarkerFaceColor', lean_cmap(3, :));
lean4 = plot(1, table_adip_size{"Lijnen2006", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(4, :), 'MarkerFaceColor', lean_cmap(4, :), 'linewidth', 1.5);
lean5 = plot(1, adip_lean.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', 'k', 'linewidth', 7);
obese1 = plot(2, table_adip_size{"Morange2000", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(1, :), 'MarkerFaceColor', obese_cmap(1, :));
obese2 = plot(2, table_adip_size{"Lijnen2001", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(2, :), 'MarkerFaceColor', obese_cmap(2, :));
obese3 = plot(2, table_adip_size{"Maquoi2002", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(3, :), 'MarkerFaceColor', obese_cmap(3, :));
obese4 = plot(2, table_adip_size{"Voros2005", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(4, :), 'MarkerFaceColor', obese_cmap(4, :));
obese5 = plot(2, table_adip_size{"Lijnen2006", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(5, :), 'MarkerFaceColor', obese_cmap(5, :));
obese6 = plot(2, adip_obese.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);
text(2, adip_obese.mean, '  *', 'color', 'k', 'fontsize', 25)
text(2, adip_obese.mean, sprintf('   p=%.3f', pval_adip), 'color', 'k', 'fontsize', 25)
hold off;

% Add legend
lgd1 = legend([lean1, lean2, lean3, lean4, lean5], ...
    {"Morange, 2000", "Lijnen, 2001", "Voros, 2005", "Lijnen, 2006", "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Lean mouse")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 25);
lgd2 = legend(a, [obese1, obese2, obese3, obese4, obese5, obese6], ...
    {"Morange, 2000", "Lijnen, 2001", "Maquoi, 2002", "Voros, 2005", "Lijnen, 2006", "Weighted average"}, ...
    'Location', 'southeastoutside');
title(lgd2, "Obese mouse")

subplot(1, 2, 2);

% Define colormaps
colormap default
lean_cmap = winter(8);
obese_cmap = autumn(3);

% Default settings
set(gca, 'fontsize', 25);
ylabel("Capillary basement membrane thickness (nm)")
xlim([0, 3])
ylim([30, 130])
xticks(0:3)
xticklabels({"", "Lean", "Obese", ""})

% Plot
hold on;
lean1 = plot(1, table_cbm{"Creutzfeldt", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(1, :), 'MarkerFaceColor', lean_cmap(1, :));
lean2 = plot(1, table_cbm{"Rodrigues1983", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(2, :), 'MarkerFaceColor', lean_cmap(2, :));
lean3 = plot(1, table_cbm{"Lash1989", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(3, :), 'MarkerFaceColor', lean_cmap(3, :));
lean4 = plot(1, table_cbm{"Lash1989_1", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(4, :), 'MarkerFaceColor', lean_cmap(4, :));
lean5 = plot(1, table_cbm{"Danis1993", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(5, :), 'MarkerFaceColor', lean_cmap(5, :));
lean6 = plot(1, table_cbm{"Calson2003", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(6, :), 'MarkerFaceColor', lean_cmap(6, :));
lean7 = plot(1, table_cbm{"Calson2003_1", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(7, :), 'MarkerFaceColor', lean_cmap(7, :));
lean8 = plot(1, table_cbm{"Ceafalan2019", "Lean average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', lean_cmap(8, :), 'MarkerFaceColor', lean_cmap(8, :));
lean9 = plot(1, cbm_lean.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', 'k', 'linewidth', 7);
obese1 = plot(2, table_cbm{"Lash1989", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(1, :), 'MarkerFaceColor', obese_cmap(1, :));
obese2 = plot(2, table_cbm{"Lash1989_1", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(2, :), 'MarkerFaceColor', obese_cmap(2, :));
obese3 = plot(2, table_cbm{"Danis1993", "Obese average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', obese_cmap(3, :), 'MarkerFaceColor', obese_cmap(3, :));
obese4 = plot(2, cbm_obese.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);
text(2, cbm_obese.mean*0.97, sprintf('  p=%.3f', pval_cbm), 'color', 'k', 'fontsize', 25)
hold off;

% Add legend
lgd1 = legend([lean1, lean2, lean3, lean4, lean5, lean6, lean7, lean8, lean9], ...
    {"Creutzfeldt, 1970"; "Rodrigues, 1983"; "Lash, 1989 (11 wk.)"; "Lash, 1989 (18 wk.)"; "Danis, 1993"; ...
    "Calson, 2003 (retina)"; "Calson, 2003 (muscle)"; "Ceafalan2019"; "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Lean mouse")
a = axes('position',get(gca,'position'),'visible','off');
set(gca, 'fontsize', 25);
lgd2 = legend(a, [obese1, obese2, obese3, obese4], ...
    {"Lash, 1989 (11 wk.)"; "Lash, 1989 (18 wk.)"; "Danis, 1993"; "Weighted average"}, ...
    'Location', 'southeastoutside');
title(lgd2, "Obese mouse")

% Save figure
foldername = '../results/figures';
mkdir(foldername)
saveas(gca, sprintf('%s/geometric_parameters2.png', foldername), 'png')

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
radio_cmap = winter(7);
spr_cmap = autumn(11);

% VEGF-A:VEGFR1
hold on;
waltenberger1 = plot(1, table_vegfr1{"Waltenberger1994", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(2, :), 'MarkerFaceColor', radio_cmap(2, :));
waltenberger2 = plot(1, table_vegfr1{"Waltenberger1994_1", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(3, :), 'MarkerFaceColor', radio_cmap(3, :));
radio_mean = plot(1, vegfr1_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', 'k', 'linewidth', 7);
tiedemann = plot(1, table_vegfr1{"Tiedemann2002", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(2, :), 'MarkerFaceColor', spr_cmap(2, :));
errorbar(1, table_vegfr1{"Tiedemann2002", "Kd average"}, table_vegfr1{"Tiedemann2002", "Kd SE"}, ...
    'Color', spr_cmap(2, :), 'LineWidth', 3)
mamer = plot(1, table_vegfr1{"Mamer2020", "Kd average"}, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(3, :), 'MarkerFaceColor', spr_cmap(3, :));
errorbar(1, table_vegfr1{"Mamer2020", "Kd average"}, table_vegfr1{"Mamer2020", "Kd SE"}, ...
    'Color', spr_cmap(3, :), 'LineWidth', 3)
spr_mean = plot(1, vegfr1_spr.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);

% VEGF-A:VEGFR2
plot(2, table_vegfr2{"Waltenberger1994", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(2, :), 'MarkerFaceColor', radio_cmap(2, :));
plot(2, table_vegfr2{"Waltenberger1994_1", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(3, :), 'MarkerFaceColor', radio_cmap(3, :));
whitaker = plot(2, table_vegfr2{"Whitaker2001", "Kd average"}, 'o', 'Markersize', 20, ...
    'MarkerEdgeColor', radio_cmap(4, :), 'MarkerFaceColor', radio_cmap(4, :));
plot(2, vegfr2_radio.mean, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', 'k', 'linewidth', 7);
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
text(2, vegfr2_spr.mean, '  *', 'color', 'k', 'fontsize', 25)
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
    'MarkerEdgeColor', 'k', 'linewidth', 7);
fuh = plot(3, table_nrp1{"Fuh2000", "Kd average"}*1e3, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(8, :), 'MarkerFaceColor', spr_cmap(8, :));
teran1 = plot(3, table_nrp1{"Teran2019", "Kd average"}*1e3, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(9, :), 'MarkerFaceColor', spr_cmap(9, :));
teran2 = plot(3, table_nrp1{"Teran2019", "Kd average"}*1e3, '^', 'Markersize', 20, ...
    'MarkerEdgeColor', spr_cmap(10, :), 'MarkerFaceColor', spr_cmap(10, :));
plot(3, nrp1_spr.mean*1e3, 'LineStyle', 'none', 'Marker', '_', 'Markersize', 25, ...
    'MarkerEdgeColor', [155, 34, 38]/255, 'linewidth', 7);
text(3, nrp1_spr.mean*1e3, '  *', 'color', 'k', 'fontsize', 25)
text(3, nrp1_spr.mean*1e3, sprintf('   p=%.3f', pval_nrp1), 'color', 'k', 'fontsize', 25)

hold off;
lgd1 = legend([waltenberger1, waltenberger2, whitaker, soker1996_1, soker1996_2, soker1998, radio_mean], ...
    {"Waltenberger, 1998"+newline+"(PAE)", "Waltenberger, 1994"+newline+"(HUVEC)", "Whitaker, 2001", ...
    "Soker, 1996"+newline+"(HUVEC)", "Soker, 1996"+newline+"(231 cell)", "Soker, 1998", "Weighted average"}, ...
    'Location', 'northeastoutside');
title(lgd1, "Cell-based"+newline+"(Radioligand)");

yyaxis right
set(gca, 'Yscale', 'log')
ylim([1e-4, 1e3])
ylabel('Equilibrium dissociation constant (Kd, nM)')

a = axes('position', get(gca, 'position'), 'visible', 'off');
set(gca, 'fontsize', 25);
lgd2 = legend(a, [tiedemann, mamer, huang, cunningham1, cunningham2, shobhan, fuh, teran1, teran2, spr_mean], ...
    {"Tiedemann, 2002", "Mamer, 2020", "Huang, 1998", "Cunningham, 1999"+newline+"(pre-dimer)", "Cunningham, 1999"+newline+"(monomer)", ...
    "Shobhan, 2023", "Fuh, 2000", "Teran, 2019"+newline+"(chimera)", "Teran, 2019"+newline+"(monomer)", "Weighted average"}, ...
    'Location', 'southeastoutside');
title(lgd2, "Chip-based"+newline+"(SPR)");

saveas(gca, sprintf('%s/binding_affinity2.png', foldername), 'png');
