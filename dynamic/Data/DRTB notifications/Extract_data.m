clear all; 

C = readtable('TB_dr_surveillance_2022-03-18.csv');

colnames = C.Properties.VariableNames;

% --- Filter on 2019 year
rows    = find(C.year==2019);
C1      = C(rows,:);

% --- Pull out the relevant estimate data
cols_needed = {'iso3', 'rr_new', 'rr_ret'};

estimcols = zeros(1,length(cols_needed));
for ii = 1:length(estimcols)
   estimcols(ii) = find(strcmp(colnames,cols_needed{ii})); 
end
C2 = C1(:,estimcols);
C2.allRR = C2.rr_new + C2.rr_ret;

DR_data = C2(:,[1,end]);
save DR_data DR_data;