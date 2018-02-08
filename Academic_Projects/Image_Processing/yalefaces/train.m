clear 
clc
trainig_set_filenames = 'filelist_train.txt' ; % Set the name of file containing names of img files to train

filelist=importdata(trainig_set_filenames,'');
nof_images = length(filelist);
% width and height of images
[im_W im_H]=size(imread(char(filelist(1))));

img=[];
for i=1:nof_images
    img = [ img reshape( double( imread( char( filelist(i) ) ) ),[],1 ) ]; % Generate the big matrix
end

% Calculate the average face
avgface = mean( img,2 );
% Plot the average face
figure(1);
imagesc(reshape(avgface,[im_W im_H]));
axis image;
axis off;
colormap(gray);

% Subtract average face from each column of img
img0 = img - repmat(avgface, [1 nof_images]);

%Verify the new mean is zero
newmean = uint8(mean(img0,2));


%Computing PCA using redced SVD
[U S V] = svd(img0,0);

%Plot frst 4 eigenfaces
figure(2);
for k=1:4
  
    subplot(2,2,k);
    imagesc(reshape(U(:,k),[im_W,im_H]));
    axis image;
    axis off;
    colormap(gray);
end