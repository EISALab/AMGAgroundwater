function test_umatilla_cluster( sed )
data = load('train.dat');

p = data(:,1:28);
t = data(:,29:31);
p = p';
t = t';

%compute the cluster center.
alpha = 1.0;
cluster = [p; alpha*t]';
[idx, c] = kmeans(cluster, 2);
[sil, h] = silhouette(cluster, idx);
c
%hist(idx);
