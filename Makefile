default: local

local:
	git pull
	runghc Generate.hs

deploy: local
	smbclient //msp.cis.strath.ac.uk/msp -c "lcd _build; prompt; recurse; mput *"

upload:
	git add --all
	git commit
	git push
