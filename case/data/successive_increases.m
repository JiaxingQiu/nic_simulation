function p=successive_increases(hr)

n=length(hr);
up=false(n-1,1);
for i=1:n-1
    up(i)=hr(i+1)>hr(i);
end
uu=false(n-2,1);
for i=1:n-2
    uu(i)=up(i)&up(i+1);    
end
p=mean(uu);
