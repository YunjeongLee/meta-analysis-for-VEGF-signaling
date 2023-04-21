clear; close all; clc;

%% Load data
filename = '../data/parameters.xlsx';
table_adip_size = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "Adipocyte diameter", "Range", "A1:E6", "TreatAsMissing", "NaN");
table_cbm = readtable(filename, "ReadVariableNames", true, "ReadRowNames", true, ...
    "VariableNamingRule", "preserve", "Sheet", "CBM thickness", "Range", "A1:E11", "TreatAsMissing", "NaN");
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

