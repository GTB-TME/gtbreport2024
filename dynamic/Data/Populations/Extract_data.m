clear all; warning off;

C = readtable('pop_2022-08-02.csv');

colnames = C.Properties.VariableNames;

% --- Filter on 2019 year
rows    = find(C.year==2019);
C1      = C(rows,:);

cols_needed = {'iso3','g_whoregion','e_pop_num'};

estimcols = zeros(1,length(cols_needed));
for ii = 1:length(estimcols)
   estimcols(ii) = find(strcmp(colnames,cols_needed{ii})); 
end
C2 = C1(:,estimcols);

popns = C2;
save popn_data popns;

