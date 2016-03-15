ORIG_EXE = torbrowser-install-5.0.3_en-US.exe
# This is the "old" Tor Launcher interface, not changed except for added
# instrumentation.
OUTPUT_OLD_EXE = torbrowser-old-5.0.3_en-US.exe
# This is the "new" Tor Launcher interface, with a modified UI plus
# instrumentation.
OUTPUT_NEW_EXE = torbrowser-new-5.0.3_en-US.exe

WORKDIR = work

all: $(OUTPUT_OLD_EXE) $(OUTPUT_NEW_EXE)

# This is a browser bundle altered to include Tor Launcher
# instrumentation logging.
torbrowser-%-5.0.3_en-US.exe: $(ORIG_EXE) tor-launcher-%.xpi torbrowser.nsi torbrowser.ico
	rm -rf "$(WORKDIR)"
	mkdir -p "$(WORKDIR)"

# Unpack the original bundle.
	7z x -o"$(WORKDIR)" "$(ORIG_EXE)"

# This is a hack. The extracted bundle contains two copies of
# libssp-0.dll, one in Browser/ and one in Browser/TorBrowser/Tor/. The
# NSIS process seems to deduplicate these files and leave only the one
# in Browser/. Naive extraction and repacking will leave tor.exe unable
# to run. Therefore copy libssp-0.dll into both places before repacking.
# libssp-0.dll is the only file where this happens in Tor Browser 5.0.3.
	cp -f "$(WORKDIR)/\$$_OUTDIR/Browser/libssp-0.dll" "$(WORKDIR)/\$$_OUTDIR/Browser/TorBrowser/Tor/libssp-0.dll"

# Fix the meek-amazon bridge fingerprint that has changed since 5.0.3
# was current.
# https://bugs.torproject.org/17473
	sed -i -e 's/4EE0CC769EB4B15A872F742EDE27D298A59DCADE/B9E7141C594AF25699E0079C1F0146F409495296/' "$(WORKDIR)/\$$_OUTDIR/Browser/TorBrowser/Data/Browser/profile.default/preferences/extension-overrides.js"

# Install the altered tor-launcher.xpi.
	cp -f "tor-launcher-$*.xpi" "$(WORKDIR)/\$$_OUTDIR/Browser/TorBrowser/Data/Browser/profile.default/extensions/tor-launcher@torproject.org.xpi"

# Rebuild with NSIS.
	cp -f torbrowser.nsi torbrowser.ico "$(WORKDIR)/"
	ln -s "\$$_OUTDIR" "$(WORKDIR)/Tor Browser"
	cd "$(WORKDIR)" && makensis torbrowser.nsi

	mv "$(WORKDIR)/torbrowser-install.exe" "$@"

torbrowser-install-%_en-US.exe:
	@echo "Missing $@. Download it from"
	@echo "https://archive.torproject.org/tor-package-archive/torbrowser/$*/$@"
	@false

clean:
	rm -f "$(OUTPUT_OLD_EXE)" "$(OUTPUT_NEW_EXE)"

.PHONY: all clean
