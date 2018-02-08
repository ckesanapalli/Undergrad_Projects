# Make Commands

# This command is not working..
start:
	 export PS1='\W $> '

# Code Commands

plott: code
	$(MAKE) -C codes plot
	exit

plot: 
	$(MAKE) -C codes plot

codec: 
	$(MAKE) -C codes clean

code:
	$(MAKE) -C codes output
	
ps:
	export PS1='$$ '

gdb:
	$(MAKE) -C codes gdb

# Git Commands

pushh: commit codec push 

commit: add
	 @read -p " Commit Title: " commits; \
	git commit -m "$$commits"

add:
	git add codes handy_commands.txt makefile Parameters.xlsx README.md
	
#git add --all

init: 
	git init

push: codec list
	@read -p " Branch Name: " branch; \
	git push -u origin "$$branch"
	
merge: list
	git checkout master; \
	@read -p " Branch Name: " branch; \
	git merge 

fetch: list
	git fetch
	@read -p " Branch Name: " branch; \
	git checkout "$$branch"

list:
	git branch

remote:
	git remote add origin https://na12b014@bitbucket.org/na12b014/Ribbon-fins-2.0.git