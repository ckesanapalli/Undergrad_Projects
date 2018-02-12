# Git Commands

push: commit
	git push -f origin master

commit: add
	 @read -p " Commit Title: " commits; \
	git commit -m "$$commits"

add:
	git add --all

remote: init
	git remote add origin https://github.com/ckesanapalli/Undergrad_Projects.git

init: 
	git init