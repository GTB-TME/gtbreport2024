clear all;

% --- Pull the data

[num, txt, raw] = xlsread('Covid_incidence.xlsx');
incd = num(:,2)'/1e6; 

% --- Make a timeseries, padding with zeroes for other dates

ndays     = 365*(2035-2019);
time      = 2019+(0:1:(ndays-1))/365;
covseries = [time; zeros(1,ndays)];

% First date of projections is 12 Dec 2019, which is 345 days from 1 Jan 2019
inds = 344 + [1:length(incd)];
covseries(2,inds) = incd;

save covseries covseries;