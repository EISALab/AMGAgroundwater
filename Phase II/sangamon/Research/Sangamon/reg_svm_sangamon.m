%reg_svm_sangamon creates hierarchical svm models and uses trust region tests to predict the sangamon case study.
%errors = [corr_norm nmse_norm rmse fp fn eall];
function [errors] = reg_svm_sangamon

%load sangamon data set.
[minaxp minaxpn minaxt minaxtn pn tn vp vt train_idx test_idx centers] = load_sangamon;

%options = '-c 1 -g 0.6 -p 0.02 -s 3 -t 2';
options = '-c 1.5 -g 40 -p 1.5 -s 3 -t 2';
%options = '-c 10 -g 0.001 -s 4 -t 3';
pn = unnormalize( pn, minaxp, minaxpn );
tn = unnormalize( tn, minaxt, minaxtn );
vp = unnormalize( vp, minaxp, minaxpn );
vt = unnormalize( vt, minaxt, minaxtn );

regs = region_svm_train( options, train_idx, pn, tn, minaxpn, test_idx, vp, vt, centers );

tp = region_svm_predict( regs, vp );

[corr_norm nmse_norm rmse_norm] = test_errors( tp, vt );

%unnormalize the data.
%tp = unnormalize( tp, minaxt, minaxtn );
%vt = unnormalize( vt, minaxt, minaxtn );
[corr nmse rmse] = test_errors( tp, vt );

disp( 'corr_norm, nmse_norm, rmse' );
disp( [corr_norm nmse_norm rmse] );

[fp, fn, eall] = count_false( tp, vt );
disp( 'false_positive, false_negative, accuracy' );
disp( [fp fn eall] );

errors = [corr_norm nmse_norm rmse fp fn eall];

return;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               subs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%tp - the predicted result of model
%tn - the true results.
function [corr_norm mse_norm rmse] = test_errors( tp, tn )
%compute normalized MSE
mse_norm = sum( (tp-tn).^2 ) / size(tn,2) / var(tn');

%compute coefficient.
corr_norm = corrcoef( tp', tn' );
corr_norm = corr_norm(1, 2);

%compute the RMSE.
rmse = sqrt( sum( (tp-tn).^2 )/size(tn,2) );

%find the first element in array which is bigger than x. return the id.
%array must be sorted.
function [id] = find_if( array, x )
for i=1:size(array, 1)
    if( x<array(i) )
        id = i;
        return;
    end
end
id = size(array,1)+1;

function [mse]=testnn( net, p, t )
tp = sim( net, p );
%calculate the MSE
sz = size( tp );
mse = sum( (t-tp).^2, 1 );
mse = sum( mse ) / sz(1) / sz(2);

function [fp, fn, e] = count_false( p, o)
n1 = 0;
n2 = 0;
n3 = 0;
for i=1:size(p,2)
    if( p(i)>10 && o(i)<10 )
        n1 = n1+1;
        n3 = n3 + 1;
    elseif( p(i)<10 && o(i)>10 )
        n2 = n2 +1;
        n3 = n3 + 1;
    end
end
fp = n1 / (n1+n2);
fn = n2 / (n1+n2);
e = 1 - n3 / size(p,2);

%p[n,m] is a matrix with m points, each point has n dimensions.
%minaxp[n, 2] minaxp(:,1) the minimum of p(:), minaxp(:,2) the maximum of p(:)
function [pn] = normalize( p, minaxp, minaxn )
sz = size(minaxp);
for i=1:sz(1)
    pn(i, :) = ( p(i, :)-minaxp(i, 1) )/( minaxp(i, 2)-minaxp(i, 1) ) * ( minaxn(i, 2)-minaxn(i, 1) ) + minaxn(i, 1);
end

%p[n,m] is a matrix with m points, each point has n dimensions.
%minaxp[n, 2] minaxp(:,1) the minimum of p(:), minaxp(:,2) the maximum of p(:)
function [pn] = unnormalize( p, minaxp, minaxn )
sz = size(minaxp);
for i=1:sz(1)
    pn(i, :) = ( p(i, :) -minaxn(i, 1) )/ ( minaxn(i, 2)-minaxn(i, 1) ) * ( minaxp(i, 2)-minaxp(i, 1) ) + minaxp(i, 1);
end

%generate the [min max] matrix [n, 2]. n is the dimension. [:,1] is min
%values, and [:,2] is max values
function [z] = bounds( n, xmin, xmax )
z = [xmin, xmax];
for i=1:n-1
    z = [z; [xmin, xmax] ];
end

function [d] = distance( x, y )
d = sqrt( sum( (x-y).^2 ) );

