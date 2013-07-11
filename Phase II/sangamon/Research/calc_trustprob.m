function [p] = calc_trustprob( reg, pt )
mins = reg.mins;
maxs = reg.maxs;
n = size(mins,2);

lA = pt > mins;
lB = pt < maxs;
if( sum(lA)+sum(lB)==2*n )
    %pt is in the boundary box. try to compute the probability.
    dist = pt - reg.center;
    p = sum( (dist.^2) ./ (reg.radius.^2) );
    p = 1.0 - min( 1.0, p );
else
    p = 0.0;
end

if( p > 0.0 )
%    p = 1.0;
end
    
