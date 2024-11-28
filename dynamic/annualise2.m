% v2: Initial version was getting annoying for calculating percentiles, as
% these were only available as a second (not primary) output. Here,
% recoding so percentiles are made a primary output, if specified

function out = annualise(inmat, annudim, take_pct, pctdim)

dims = size(inmat);
if annudim == 1
    shape = [12, dims(1)/12, dims(2:end)];
elseif annudim == length(dims)
    shape = [dims(1:annudim-1), 12, dims(end)/12];
else
    shape = [dims(1:annudim-1), 12, dims(annudim)/12, dims(annudim+1:end)];
end

tmp1 = reshape(inmat, shape);
tmp2 = sum(tmp1,annudim);
out  = squeeze(tmp2);

% keyboard;

% Also take percentiles if needed
if nargin > 2
    if take_pct
        out = squeeze(prctile(out,[2.5,50,97.5],pctdim));
    end
end