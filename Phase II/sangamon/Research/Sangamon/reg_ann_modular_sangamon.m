%errors = [corr_norm nmse_norm rmse fp fn eall];
function [errors] = reg_ann_modular_sangamon( sed )

%load sangamon data set.
[minaxp minaxpn minaxt minaxtn pn tn vp vt train_idx test_idx] = load_sangamon;

rand('seed', sed);
nodes = [4, 1];
regs = region_ann_train( nodes, train_idx, pn, tn, minaxpn, test_idx, vp, vt );

%assembly the cluster matrix
n_networks = size( train_idx, 2 );
train_cluster = zeros( size(pn, 2), n_networks );
test_cluster = zeros( size(vp, 2), n_networks );
for i=1:n_networks
    idx = train_idx{1, i};
    n = size(idx, 1);
    for j=1:n
        train_cluster( idx(j), i ) = 1;
    end
    idx = test_idx{1, i};
    n = size(idx, 1);
    for j=1:n
        test_cluster( idx(j), i ) = 1;
    end
end

train_cluster = calc_clustermx( regs, pn );
test_cluster = calc_clustermx( regs, vp );
%train_cluster = ones( size(pn,2), n_networks );
%test_cluster = ones( size(vp, 2), n_networks );
gate_w = region_ann_train_gate( regs, pn, tn, train_cluster, vp, vt, test_cluster );
tp = region_ann_modular_predict( regs, gate_w, [vp; vt] );
%tp = region_ann_modular_predict( regs, gate_w, [vp; vt] , test_cluster );
%tp = region_predict( regs, vp );

[corr_norm nmse_norm rmse_norm] = test_errors( tp, vt );
%unnormalize the data.
tp = unnormalize( tp, minaxt, minaxtn );
vt = unnormalize( vt, minaxt, minaxtn );
[corr nmse rmse] = test_errors( tp, vt );

disp( 'corr_norm, nmse_norm, rmse' );
disp( [corr_norm nmse_norm rmse] );

y = [tp', vt'];

%%{
figure(1);
plot( vt', tp', 'x' );
hold on;
plot( [0; 16], [0; 16], 'r' );
plot( [10; 10], [0; 16], ':' );
plot( [0; 16], [10; 10], ':' );
axis( [0 16 0 16] );
hold off;
%%}

[fp, fn, eall] = count_false( tp, vt );
disp( 'false_positive, false_negative, accuracy' );
disp( [fp fn eall] );

errors = [corr_norm nmse_norm rmse fp fn eall];

%test local set error

%normalize the data.
tp = normalize( tp, minaxt, minaxtn );
vt = normalize( vt, minaxt, minaxtn );

n_networks = size( train_idx, 2 );
vv_i = [];
vv_o = [];
for i=2:n_networks
    idx = test_idx{1, i};
    n = size(idx, 1);
    for j=1:n
        vv_i = [vv_i vp(:, idx(j))];
        vv_o = [vv_o vt(:, idx(j))];
    end
    %p_o = region_predict( regs, vv_i );
    p_o = region_ann_modular_predict( regs, gate_w, [vv_i; vv_o] );
    
    [corr_norm nmse_norm rmse_norm] = test_errors( p_o, vv_o );
    %unnormalize the data.
    p_o = unnormalize( p_o, minaxt, minaxtn );
    vv_o = unnormalize( vv_o, minaxt, minaxtn );
    [corr nmse rmse] = test_errors( p_o, vv_o );
    
    disp( 'region corr_norm, nmse_norm, rmse' );
    disp( [corr_norm, nmse_norm, rmse] );
   
    vv_i = [];
    vv_o = [];
end

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

function [z] = SimNet( net, x, minaxp, minaxpn, minaxt, minaxtn )
pn = normalize( x, minaxp, minaxpn );
tn = sim( net, pn );
z = unnormalize( tn, minaxt, minaxtn );

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

