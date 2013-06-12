function batch_sangamon( sed, n )

rand('seed', sed );

fid1 = fopen( 'single_model.txt', 'w' );
fid2 = fopen( 'region_model.txt', 'w' );
for i=1:n
    sed = floor( rand()*100000 );
    %{
    fprintf( fid1, '%d\t', sed );
    rand('seed', sed );
    [errors] = reg_nitr(150);
    fprintf( fid1, '%f\t', errors );
    fprintf( fid1, '\n' );
    %}
    fprintf( fid2, '%d\t', sed );
%    [errors] = reg_ann_sangamon( seds(i) )
%    [errors] = reg_ann_modular_sangamon( sed )
    [errors] = reg_svm_modular_sangamon
%    [errors] = reg_svm_sangamon
    fprintf( fid2, '%f\t', errors );
    fprintf( fid2, '\n' );
end

fclose( fid1 );
fclose( fid2 );
