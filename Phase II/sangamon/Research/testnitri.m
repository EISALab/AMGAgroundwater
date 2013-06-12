function testnitri
minaxp = [
0   0.2	0   0	0	0   ;
0.9 0.9	0.6	0.6	0.6	1.0 ];
minaxp = minaxp';
minaxpn = bounds(6, -0.9, 0.9);
minaxt = [0 15.0];
minaxtn = [0.0 1.0];

center = [-0.8703    0.1877   -0.7609   -0.7619   -0.7297    0.0657];

ntrains = 155;
data = load('93odd.dat');

%ntrains = 151;
%data = load('93half.dat');

p = data(:,1:6);
t = data(:,7);
p = p';
p = normalize( p, minaxp, minaxpn );
t = t';
t = normalize( t, minaxt, minaxtn );

vp = p(: , ntrains+1:size(p,2));
vt = t(: , ntrains+1:size(p,2));
pn = p(:, 1:ntrains);
tn = t(:, 1:ntrains);

nodes = [4, 1];
funcs = { 'tansig', 'tansig' };
net = newff( minaxpn, nodes, funcs );

net.trainParam.show = 50;
net.trainParam.lr = 0.05;
net.trainParam.mc = 0.9;
net.trainParam.epochs = 100;
net.trainParam.goal = 1e-5;

vv.P = vp;
vv.T = vt;

minmse = 1e10;
savenet = net;
for i=1:5
%    [net, tr] = train( net, pn, tn );
    [net, tr] = train( net, pn, tn, [], [], vv );
    mse = testnn( net, vp, vt );
    if( minmse>mse )
        minmse = mse;
        savenet = net;
    end
    net = init(net);
end

%[net, tr] = train( net, pn, tn );
%[net, tr] = train( net, pn, tn, [], [], vv );

%data = load('valid_1_c.dat');
%vp = data(:,1:6);
%vt = data(:,7);
%vp = vp';
%vt = vt';

tp = sim( savenet, vp );

%unnormalize the data.
tp = unnormalize( tp, minaxt, minaxtn );
vt = unnormalize( vt, minaxt, minaxtn );

%compute the distance of the points to the center
center = [-0.8703    0.1877   -0.7609   -0.7619   -0.7297    0.0657]';
%center = [-0.6950    0.2831   -0.2613   -0.1752   -0.2144   -0.0539]';
d = zeros(size(tp,2),1);
for i=1:size(tp,2)
    d(i,1) = distance( vp(:,i), center );
end
error = (tp-vt).^2;
error = error';
error = [d, error];
y = sortrows( error, 1 );
figure(1);
plot( y(:,1), y(:,2), 'x' );

%compute the RMSE.
err = sum( (tp-vt).^2 )/size(vt,2);
disp( 'error is:' );
disp( sqrt(err) );

y = [tp', vt'];

figure(2);
plot( y );

figure(2);
plot( vt', tp', 'x' );
min(tp)
hold on;
plot( [0; 16], [0; 16], 'r' );
plot( [10; 10], [0; 16], ':' );
plot( [0; 16], [10; 10], ':' );
axis( [0 16 0 16] );
hold off;

[fp, fn, eall] = count_false( tp, vt );
[fp fn eall]
return;

p = p';
sils = zeros(7,1);
for i=1:7
    idx = kmeans(p, i+1, 'distance', 'city');
    sil = silhouette(p, idx, 'city');
    sils(i) = mean(sil);
end

plot(sils);

sils

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

function [d] = distance( x, y )
d = sqrt( sum( (x-y).^2 ) );

%generate the [min max] matrix [n, 2]. n is the dimension. [:,1] is min
%values, and [:,2] is max values
function [z] = bounds( n, xmin, xmax )
z = [xmin, xmax];
for i=1:n-1
    z = [z; [xmin, xmax] ];
end