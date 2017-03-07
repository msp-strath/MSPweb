SMBCOMMAND     = "lcd _build; prompt; recurse; mput *"

LOGDIR         = _logs # underscore means Generate.hs won't copy them to the server!
SMBLOGSCOMMAND = "lcd $(LOGDIR); prompt; mget *"

### Authentication file ###
#
# If the text file

AUTHFILE       = _authsmb.txt

# is found, it will be used by smbclient for authentication. It should
# contain the lines
#
#   username = $yourUsername
#   password = $yourPassword
#
# (no spaces at the beginning of the lines).
#
# If the file is not present, you will be asked for your credentials
# instead.

ifneq ($(wildcard $(AUTHFILE)),)
	AUTHSTRING = -A $(AUTHFILE)
else
        AUTHSTRING =
endif
###########################


default: local

local:
	git pull
	runghc Generate101.hs
	runghc Generate.hs

deploy autodeploy: local
	smbclient //msp.cis.strath.ac.uk/msp -d 0 $(AUTHSTRING) -c $(SMBCOMMAND)

get-logs:
	@echo "Storing logs in the $(LOGDIR) directory"
	mkdir -p $(LOGDIR)
	smbclient //msp.cis.strath.ac.uk/msp-logs $(AUTHSTRING) -c $(SMBLOGSCOMMAND)

upload:
	git add --all
	git commit
	git push
