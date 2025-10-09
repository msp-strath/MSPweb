{ mkDerivation, aeson, array, base, bytestring, containers
, crypton-connection, crypton-x509-system, data-default, directory
, feed, filepath, http-client, http-client-tls, http-types, lib
, pandoc, regex-pcre-builtin, text, time, tls, xml-types, yaml
}:
mkDerivation {
  pname = "MSPweb";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array base bytestring containers crypton-connection
    crypton-x509-system data-default directory feed filepath
    http-client http-client-tls http-types pandoc regex-pcre-builtin
    text time tls xml-types yaml
  ];
  homepage = "https://github.com/msp-strath/MSPweb";
  description = "Generator for the MSP website";
  license = "unknown";
}
