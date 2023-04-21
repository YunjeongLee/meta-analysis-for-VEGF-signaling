function [tval, pval, RejectOrNot] = students_t_test(sample1, sample2, alpha, OneSideOrTwoSide)
%% Calculate tvalue
sample1.squared_se = sample1.sd^2/sample1.size;
sample2.squared_se = sample2.sd^2/sample2.size;

mean_difference = sample1.mean - sample2.mean;
sd_difference = sqrt(sample1.squared_se + sample2.squared_se);

% Calculate degree of freedom (df)
df = (sample1.squared_se + sample2.squared_se)^2 ...
    / (sample1.squred_se^2/(sample1.size - 1) ...
    + sample2.squared_se^2/(sample2.size - 1));

% Round down
df = floor(df);

tval = mean_difference/sd_difference;

%% Find critical value
if OneSideOrTwoSide == "one-side"
    pval = tcdf(-tval, df);
    if pval <= alpha
        RejectOrNot = "reject";
    else
        RejectOrNot = "accept";
    end
elseif OneSideOrTwoSide == "two-side"
    pval = 2 * tcdf(-abs(tval), df);
    if pval <= alpha
        RejectOrNot = "reject";
    else
        RejectOrNot = "accept";
    end
else
    sprintf('You should put either "one-side" or "two-side" into "OneSideOrTwoSide" input.');
end
