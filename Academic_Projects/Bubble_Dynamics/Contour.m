link =  'Outputs\';
n = 30;
x = csvread([link 'grid.csv'],0,0,[0 0 n 0]);
y = csvread([link 'grid.csv'],0,1,[0 1 n 1]);

dx = 1/(n+2);
alp = dx/4;
value = 1.5;
value1 = -alp:0.1*alp:alp;
for k = 0:30
    phi = csvread([link 'dist_fn\' num2str(k) '.csv'],0,0,[0 0 n n]);  

    hold off,contour(x,y,flipud(rot90(phi)),value1),axis equal,axis([0 1 0 1]);
    print(num2str(k),'-djpeg')
    pause(0.01)
end

% for k = 0:9
%    figure
%     subplot(3,3,k+1)
%     phi = csvread([link 'dist_fn\' num2str(k*3) '.csv'],0,0,[0 0 n-2 n-2]);    
%     hold off, contour(x,y,flipud(rot90(phi)),val),axis equal,axis([0 1 0 1]);
%     pause(0.01)
%     title(['at t = ' num2str(k) ])
%     
% end