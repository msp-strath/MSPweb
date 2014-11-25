SMBCOMMAND="lcd _build; prompt; recurse; mput *"

default: local

local:
	git pull
	runghc Generate101.hs
	runghc Generate.hs

deploy: local
	smbclient //msp.cis.strath.ac.uk/msp -c $(SMBCOMMAND)

auto-deploy: local
	smbclient //msp.cis.strath.ac.uk/msp -A authsmb.txt -c $(SMBCOMMAND)

upload:
	git add --all
	git commit
	git push
