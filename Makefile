default: local

local:
	runghc Generate.hs

deploy: local
	smbclient //msp.cis.strath.ac.uk/msp -c "recurse; mput _build/*"
