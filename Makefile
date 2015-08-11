SMBCOMMAND="lcd _build; prompt; recurse; mput *"

default: local

local:
	git pull
	runghc Generate101.hs
	runghc Generate.hs

deploy: local
	smbclient //msp.cis.strath.ac.uk/msp -c $(SMBCOMMAND)

auto-deploy: local
# To use this target, make sure you have a text file authsmb.txt which
# contains the lines
#
#   username = $yourUsername
#   password = $yourPassword
#
# (no spaces at the beginning of the lines)
	smbclient //msp.cis.strath.ac.uk/msp -d 0 -A authsmb.txt -c $(SMBCOMMAND)

upload:
	git add --all
	git commit
	git push
