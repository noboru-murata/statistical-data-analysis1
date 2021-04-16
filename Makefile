MSG = "2021 version"

all: html

html:
	hugo

push:
	git add --all
	git commit -m ${MSG}
	git push -u origin master

stat:
	git status

clean:
	rm -f content/*~

