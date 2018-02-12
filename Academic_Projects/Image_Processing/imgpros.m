A=zeros(243*320,11,15);
B=zeros(243,320,11,15);
C=zeros(243*320,10,15);

 for subid= 1:15
    if(subid<10)
        text=strcat('yalefaces\subject','0',num2str(subid),'.*');
    else 
        
        text=strcat('yalefaces\subject',num2str(subid),'.*');
    end
files=dir(text);

for k = 1:length(files)
  B(:,:,k,subid)=  imread(fullfile('yalefaces',files(k).name));
end

%vectorization
for i=1:243
    for j=1:320
        A((i-1)*320+j,:,subid)=B(i,j,:,subid);
    end
end

Cwm(:,(1:10),subid)=A(:,(1:10),subid);
Cm=sum(C,2)/10; 
for x=1:10
    C(:,x,subid)=Cwm(:,x,subid)-Cm(:,1,subid);% mean removed
end

[V(:,:,subid), D(:,:,subid)]= eig(C(:,:,subid)'*C(:,:,subid));
V1(:,:,subid)=V(:,(8:10),subid);
T(:,:,subid)=C(:,:,subid)*V1(:,:,subid);
end
 for test=1:15
    for subid=1:15
% 	since going ou of memory test photo has
%     not been weighed using inverse of covariance matrix
    n(subid)=norm(T(:,1,subid)'-A(:,11,test)'+Cm(:,1,subid)'); 

    end
%     find subid for min(norm)
    count=1;
    while (n(count) ~= min(n))
        count=count+1;
    end
    if (test==count)
    fprintf('the test photo num %d corresponds to subject %d \n',test,count);
    else
     fprintf('the test photo num %d is mis classified as %d \n',test,count);   
    end
 end
